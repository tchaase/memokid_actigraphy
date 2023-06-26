## Organizational stuff ---- 

  # The following two packages are used to the analyses. Firstly, the PhysicalActivity package that is mainly used for Wear-Time-Validation (WTV) algorithms
#install.packages(c("remotes", "RSQLite", "PhysicalActivity"))
  # This package is mostly used for sleep analyses. The package is directly installed from github. 
#remotes::install_github("dipetkov/actigraph.sleepr")
  # Loading the packages....
library(PhysicalActivity)
library(actigraph.sleepr)
library(dplyr)

#ID of participant
part_id <- XXXXX # Participant's idea is inserted here. Use the search function to replace this across participants! 

## Wear-Time Validation ----
  # Firstly the usual R-set up related to the working directory. 
getwd()
setwd("/home/tchaase/Documents/Universitaet/Bachelorarbeit/Actigraphy-Analyses/Actigraphy_2/Young_Adults/")

  # Here I mark the data for every 10s indicating whether the device was worn or not. Firstly the data is loaded via the R-package provided by the authors Choi et al. 
PhysicalActivity::readActigraph("XXXXX_10sec.agd") -> Wear_Time
  # perMinuteCts how much data is valid. This is relevant but only makes sense if you set it in relation to how much data there SHOULD be, which can easily be done with the next row of code.
PhysicalActivity::wearingMarking(Wear_Time, perMinuteCts = 6) -> Wear_Time_Marked

  # This commands makes it easy to see a summary of the WTV. Output either wear or non wear time - pay attention! This code already gives WHEN the device was worn.
PhysicalActivity::sumVct(datavct = Wear_Time_Marked) -> Wear_Time_Summary 
PhysicalActivity::sumVct(Wear_Time_Marked, wearing = "wearing", TS = getOption("pa.timeStamp"),
                         markingString = "nw", by = "days", id = NULL) -> Non_Wear_Time_Summary # this line to see the episodes of non-wear-time. The criterion for non-wear-time is 4hours per night from 7pm to 12pm

# Checking if wear time is too low on certain days: 
Non_Wear_Time_Summary$duration /6/60 > 4
Non_Wear_Time_Summary$duration /6/60  # See the duration of the No-WT

  # Manually setting the days with two high wear time: 
wt <- 2 #0 = everything is fine, and then the numbers indicate how many days had to be cut out. This is refers to both whole days that are cut out and nights that are deemed to be *not reliable* due to large chuncks of missing data according to the wear-time algorithm. The criterion for this is 4 hours!
wt_old <- 1 # how many had to be excluded due to the old criterion of 8 hours non wear-time per night

## Sleep-analyses ----
dir()  # Checking what files are available and to copy the relevant files. 
actigraph.sleepr::read_agd("XXXXX_10sec.agd") -> Sleep # Copy the file that corresponds to the relevant test subject. 

# Changing a few variable names:
data.frame(Wear_Time$TimeStamp, Sleep[2:10]) -> Sleep
colnames(Sleep) <- c('timestamp',"axis1", "axis2","axis3","steps", "lux", "inclineoff",  "inclinestanding", "inclinesitting", "inclinelying")  
actigraph.sleepr::collapse_epochs(Sleep, epoch_len_out = 60)-> Sleep # Collapsing the epochs to get values per minute as the algorithm requires. 
actigraph.sleepr::apply_sadeh(Sleep) -> Sleep_Sadeh
  # -> Now I have multiple small windows of sleep.
  # There is lots of room for error here and windows where the device isn't worn might count as sleep. Therefore, only windows that aren't also non wear time windows should count in, else the overall wear time could significantly be expanded if the device isn't worn in this period.
Sleep_Sadeh_Validated <- actigraph.sleepr::combine_epochs_periods(Sleep_Sadeh, Wear_Time_Summary, startTimeStamp, endTimeStamp)
  # This object now contains another column that indicates if the device was worn.

## Sleep duration computation -----
 # Firstly the timestamp is is changed to be a Posixlt. This way its a list object that I can easily use to create a variable that indexes each day.  
Sleep_Sadeh_Validated$timestamp_list <- as.POSIXlt.POSIXct(Sleep_Sadeh_Validated$timestamp)
day <- 1
Sleep_Sadeh_Validated['night_indicator'] <- NA
for (i in 1:nrow(Sleep_Sadeh_Validated)) {
  timestamp_variable <- as.POSIXlt(Sleep_Sadeh_Validated$timestamp[i])
  
  if(timestamp_variable$hour == 18 & timestamp_variable$min == 0 & timestamp_variable$sec == 0) {
  Sleep_Sadeh_Validated[c(i : (i+ 1439)),]$night_indicator <- day
  day <- day + 1}
}


 # Now I will compute the mean sleep for each day. Note that periods where the device wasn't worn aren't included:

#Calculate sleep duration per day
sleep_duration <- aggregate(
  ifelse(Sleep_Sadeh_Validated$sleep == "S" & !is.na(Sleep_Sadeh_Validated$period_id), 1, 0),
  by = list(night_indicator = Sleep_Sadeh_Validated$night_indicator),
  FUN = sum)
print(sleep_duration) # Print sleep duration per day
 

# Sleep quality computation -----
 # Next sleep quality will be computed. Sleep quality will be defined as the mean value of the sleep fragmentation index across periods of nightly sleep.
 # Nightly sleep will thusly be defined as sleep periods within the window of 7pm to 12 am.
Sleep <- actigraph.sleepr::apply_tudor_locke(Sleep_Sadeh_Validated[,], min_sleep_period = 60)
 # For convenience I am just going to define a function that will be used to filter the nonwear time vs. the sleep time as indicated by the algorithm above. 
overlap <- mapply(function(sleep_start, sleep_end, nonwear_start, nonwear_end) 
    {any(sleep_start <= nonwear_end & sleep_end >= nonwear_start)}, 
    Sleep$onset, Sleep$out_bed_time, Non_Wear_Time_Summary$startTimeStamp, Non_Wear_Time_Summary$endTimeStamp)

 # Filter the sleep periods based on the overlap vector
filtered_sleep_data <- Sleep[!overlap, ]
filtered_sleep_data

 # Filter based on the time to get the nightly sleep.
  # Firstly convert to appropriate format:
filtered_sleep_data$in_bed_time_hours <- format(as.POSIXct(filtered_sleep_data$in_bed_time), "%H:%M:%OS")
filtered_sleep_data$out_bed_time_hours <- format(as.POSIXct(filtered_sleep_data$out_bed_time), "%H:%M:%OS")
filtered_sleep_data[filtered_sleep_data$in_bed_time_hours <= "12:00:00" | filtered_sleep_data$in_bed_time_hours >= "19:00:00", ] -> Sleep_night
Sleep_night[Sleep_night$out_bed_time_hours >= "19:00:00" | Sleep_night$in_bed_time_hours <= "12:00:00", ] -> Sleep_night
 # Compute sleep quality, I will use a for loop similarly to the one above. Again, its easier to do such a loop via Posixlt.

night_border <- as.POSIXlt("19:00:00", format = "%H:%M:%S")
morning_border <- as.POSIXlt("12:00:00", format = "%H:%M:%S")
Sleep_night$night_indicator <- NA  # Initialize the night_indicator column
night <- 1  # Initialize the night index
current_day <- as.Date(Sleep_night$onset[1])  # Get the date of the first night
skip_counter = FALSE
Sleep_night$onsetlt <- as.POSIXlt(Sleep_night$onset, format = "%H:%M:%S") ; Sleep_night$out_bed_timelt <- as.POSIXlt(Sleep_night$out_bed_time, format = "%H:%M:%S")

# Firstly I need to set the current day. Why does the counter sometimes equal the current day -1? Because per my definition a day starts at 12. 
# setting the first day as one day before makes it so my loops actually work easier, because this is the same set up I would have in the mid. 
if (Sleep_night$onset[1] <= as.POSIXlt("12:00:00", format = "%H:%M:%S")){current_day - 1 -> current_day}

for (i in 1:nrow(Sleep_night)){  
  if (as.Date(Sleep_night$onset[i]) == current_day & as.numeric(Sleep_night$onsetlt[i]$hour) >= 19 & skip_counter == FALSE){
      Sleep_night$night_indicator[i] <- night
      skip_counter <- FALSE
      message("Code 1: Through sleep. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
              " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
              " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
     # Won't happen often. Only on the first night. 
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 && as.numeric(Sleep_night$out_bed_timelt[i]$hour) <= 12 && (current_day + 1) == as.Date(Sleep_night$onset[i]) && skip_counter == FALSE) {
    current_day <- as.Date(Sleep_night$onset[i])
    Sleep_night$night_indicator[i] <- night
    skip_counter <- TRUE
    message("Code 2: Late sleep. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
            " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
            " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 && current_day == as.Date(Sleep_night$onset[i]) && skip_counter == TRUE) {
    Sleep_night$night_indicator[i] <- night
    message("Code 3: Wake up sleep, after short sleep after midnight. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
            " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
            " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
    skip_counter <- TRUE
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 && skip_counter == FALSE & as.Date(Sleep_night$onset[i]) == as.Date(Sleep_night$onset[i-1])) {
      Sleep_night$night_indicator[i] <- night
      message("Code 4: Wake up sleep. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
              " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
              " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
      skip_counter <- TRUE 
  } else if (as.Date(Sleep_night$onset[i]) == (current_day + 1) && as.numeric(Sleep_night$onsetlt[i]$hour) >= 19 & skip_counter == FALSE) {
    current_day <- as.Date(Sleep_night$onset[i])  # Update current_day to the new day
    night <- night + 1
    Sleep_night$night_indicator[i] <- night
    message("Code 5: Through sleep, but next day: Through sleep. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
          " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
          " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
    skip_counter <- FALSE
 } else if (as.Date(Sleep_night$onset[i]) == current_day && as.numeric(Sleep_night$onsetlt[i]$hour) >= 19 & skip_counter == TRUE) {
      current_day <- as.Date(Sleep_night$onset[i])  # Update current_day to the new day
      night <- night + 1
      Sleep_night$night_indicator[i] <- night
      message("Code 6: Through sleep, but skip: Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
              " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
              " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
      skip_counter <- FALSE
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 && (current_day + 2) == as.Date(Sleep_night$onset[i]) && skip_counter == TRUE) {
    current_day <- as.Date(Sleep_night$onset[i])  # Update current_day to the new day
    night <- night + 1
    Sleep_night$night_indicator[i] <- night
    message("Code 7: Late sleep, but skip. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
            " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
            " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
    skip_counter <- TRUE
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 & as.numeric(Sleep_night$out_bed_timelt[i]$hour) <= 12 & (current_day + 2) == as.Date(Sleep_night$onset[i]) && skip_counter == FALSE) {
    current_day <- as.Date(Sleep_night$onset[i])  # Update current_day to the new day
    night <- night + 1
    Sleep_night$night_indicator[i] <- night
    message("Code 8: Skip sleep, two nights. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
            " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
            " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
    skip_counter <- TRUE
  } else if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12 && (current_day + 1) == as.Date(Sleep_night$onset[i]) && skip_counter == TRUE) {
      current_day <- as.Date(Sleep_night$onset[i])  # Update current_day to the new day
      night <- night + 1
      Sleep_night$night_indicator[i] <- night
      message("Code 9: Next day, with skip. Sleep period from ", format(Sleep_night$onset[i], "%H:%M:%S"),
              " to ", format(Sleep_night$out_bed_time[i], "%H:%M:%S"), " assigned to night ", night,
              " - The current date is: ", format(current_day, "%Y-%m-%d"), " - ", i, " - ", skip_counter)
      skip_counter <- TRUE
      # Most likely to occur if person goes to sleep after midnight each day. 
  } else if (as.Date(Sleep_night$onset[i]) == current_day +3) {
    if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 2
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = TRUE}
    else if (as.numeric(Sleep_night$onsetlt[i]$hour) >= 19){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 3
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = FALSE}
      message("Skip 3 nights in row: ","Night:  ", night, " Current day is ", current_day, " - ", skip_counter)
  } else if (as.Date(Sleep_night$onset[i]) == current_day +4) {
    if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 3
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = TRUE}
    else if (as.numeric(Sleep_night$onsetlt[i]$hour) >= 19){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 4
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = FALSE}
    message("Skip 4 nights in row: ","Night:  ", night, " Current day is ", current_day, " - ", skip_counter)
  } else if (as.Date(Sleep_night$onset[i]) == current_day +5) {
    if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 4
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = TRUE}
    else if (as.numeric(Sleep_night$onsetlt[i]$hour) >= 19){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 5
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = FALSE}
    message("Skip 5 nights in row: ","Night:  ", night, " Current day is ", current_day, " - ", skip_counter)
  } else if (as.Date(Sleep_night$onset[i]) == current_day + 6) {
    if (as.numeric(Sleep_night$onsetlt[i]$hour) <= 12){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 5
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = TRUE}
    else if (as.numeric(Sleep_night$onsetlt[i]$hour) >= 19){
      if (skip_counter == TRUE){night <- night +1}
      night <- night + 6
      Sleep_night$night_indicator[i] <- night
      current_day <- as.Date(Sleep_night$onset[i])
      skip_counter = FALSE}
    message("Skip 6 nights in row: ","Night:  ", night, " Current day is ", current_day, " - ", skip_counter)
  }
} # need to still work on adjusting the day count here.   
# Some of these combinations are unlikely to occur, but were added just so this can never fail. Over time I should get rid of the unnecessary options. 

weighted_avg_per_night <- Sleep_night %>%
  group_by(night_indicator) %>%
  summarize(weighted_avg = weighted.mean(sleep_fragmentation_index, w = duration, na.rm = TRUE))
# Inspect if this makes sense:
print(weighted_avg_per_night)

sleep_day_values <- merge(weighted_avg_per_night, sleep_duration, by = "night_indicator", all = TRUE)
colnames(sleep_day_values)[colnames(sleep_day_values) == "x"] <- "duration"


# Dataframe ----
# Now I have all the values. Firstly the ID, the respective sleep values per day. However, I still need to use string formatting to create the respective sleep variables
# to which the values will then be assigned. 

if (nrow(sleep_day_values) < 16) {
  # Generate night indicator values for the missing rows
  night_indicator <- (nrow(sleep_day_values) + 1):16
  
  # Create a data frame with missing nights
  missing_nights <- data.frame(
    night_indicator = night_indicator,
    weighted_avg = -99,
    duration = -99
  )
  
  # Bind the missing nights to the bottom of the data frame
  sleep_day_values <- rbind(sleep_day_values, missing_nights)
}

# Create an empty data frame for the wide format
wide_format <- data.frame(part_id = part_id,
                          stringsAsFactors = FALSE)

# Loop through each night
for (i in 1:16) {
  col_names_avg <- paste0("weighted_avg_night_", i)
  col_names_dur <- paste0("duration_night_", i)
  
  # Check if the index is within the available range
  if (i <= nrow(sleep_day_values)) {
    wide_format[[col_names_avg]] <- sleep_day_values$weighted_avg[i]
    wide_format[[col_names_dur]] <- sleep_day_values$duration[i]
  } else {
    wide_format[[col_names_avg]] <- -99
    wide_format[[col_names_dur]] <- -99
  }
}


##Dataframe
# Load the data from the CSV file using fread
library(data.table)
loaded_data <- data.table::fread(file = "/home/tchaase/Documents/Universitaet/Bachelorarbeit/Actigraphy-Analyses/Actigraphy_2/preprocessed/sleep_data.csv")

# Append the wide format data to the sleep_data data set
appended_data <- rbindlist(list(wide_format, loaded_data))

# Save the appended data to the CSV file using fwrite
data.table::fwrite(appended_data, "/home/tchaase/Documents/Universitaet/Bachelorarbeit/Actigraphy-Analyses/Actigraphy_2/preprocessed/sleep_data.csv")

data.table::fwrite(wide_format, "/home/tchaase/Documents/Universitaet/Bachelorarbeit/Actigraphy-Analyses/Actigraphy_2/preprocessed/sleep_data.csv")
