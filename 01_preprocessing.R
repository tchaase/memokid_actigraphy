## Organizational stuff ---- 

  # The following two packages are used to the analyses. Firstly, the PhysicalActivity package that is mainly used for Wear-Time-Validation (WTV) algorithms
install.packages(c("remotes", "RSQLite", "PhysicalActivity"))
  # This package is mostly used for sleep analyses. The package is directly installed from github. 
remotes::install_github("dipetkov/actigraph.sleepr")
  # Loading the packages....
library(PhysicalActivity)
library(actigraph.sleepr)

#ID of participant
part_id <- XXXXX # Participant's idea is inserted here. Use the search function to replace this across participants! 

## Wear-Time Validation ----
  # Firstly the usual R-set up related to the working directory. 
getwd()
setwd("/home/tchaase/Documents/Universitaet/Bachelorarbeit/Actigraphy-Analyses/Actigraphy_2/YK_Preterm/")

  # Here I mark the data for every 10s indicating whether the device was worn or not. Firstly the data is loaded via the R-package provided by the authors Choi et al. 
PhysicalActivity::readActigraph("12025_10sec.agd") -> Wear_Time
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
actigraph.sleepr::read_agd("12024_10sec.agd") -> Sleep # Copy the file that corresponds to the relevant test subject. 

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
Sleep_Sadeh_Validated['day_night'] <- NA
for (i in 1:nrow(Sleep_Sadeh_Validated)) {
  timestamp_variable <- as.POSIXlt(Sleep_Sadeh_Validated$timestamp[i])
  
  if(timestamp_variable$hour == 18 & timestamp_variable$min == 0 & timestamp_variable$sec == 0) {
  Sleep_Sadeh_Validated[c(i : (i+ 1439)),]$day_night <- day
  day <- day + 1}
}


 # Now I will compute the mean sleep for each day. Note that periods where the device wasn't worn aren't included:

#Calculate sleep duration per day
sleep_duration <- aggregate(
  ifelse(Sleep_Sadeh_Validated$sleep == "S" & !is.na(Sleep_Sadeh_Validated$period_id), 1, 0),
  by = list(day_night = Sleep_Sadeh_Validated$day_night),
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

 # Filter based on the time to get the nightly sleep.
  # Firstly convert to appropriate format:
filtered_sleep_data$in_bed_time <- format(as.POSIXct(filtered_sleep_data$in_bed_time), "%H:%M:%OS")
filtered_sleep_data$out_bed_time <- format(as.POSIXct(filtered_sleep_data$out_bed_time), "%H:%M:%OS")
filtered_sleep_data[filtered_sleep_data$in_bed_time <= "12:00:00" | filtered_sleep_data$in_bed_time >= "19:00:00", ] -> Sleep_night
Sleep_night[Sleep_night$out_bed_time >= "19:00:00" | Sleep_night$in_bed_time <= "12:00:00", ] -> Sleep_night
 # Compute sleep quality, I will use a for loop similarily to the one above. Again, its easier to do such a loop via Posixlt.
night <- 1
Sleep_night$night_indicator <- NA

  # This is a work in progress as the count "night" still doesnt work. It should increase only when the sleep period is on the next day...need to figure out how to do this or aggregate by day?
for (i in 1:nrow(Sleep_night)) {
    if (Sleep_night[i,]$onset >= 19:00:00 | Sleep_night[i,]$onset <= 12:00:00 || Sleep_night[i,]$out_bed_time > 19:00:00 | Sleep_night[i,]$out_bed_time <= 12:00:00) {
    Sleep_night$night_indicator[i] <- night
    night <- night + 1  }
}
as.POSIXlt(Sleep_Sadeh_Validated[1,]$timestamp)$hour




##Dataframe
#data.frame(matrix(ncol=47, nrow=0))-> sleep_data
#colnames(sleep_data) <- c("part_id", "wt", "wt_old", "comment" )
load("sleep_data_124.RData")
data.frame(matrix(c(part_id ,  wt ,  wt_old , comment), nrow = 1)) ->subfile
colnames(subfile) <- c("part_id", "wt", "wt_old", "comment" )
sleep_data<-rbind(sleep_data, subfile)
#sleep_data[-64,] -> sleep_data #Leaving this here in case I need to remove a participant. 

openxlsx::write.xlsx(sleep_data, file = "sleep_XXXXX.xlsx")

#End of session
save(sleep_data, file = "sleep_data_124.RData")
?actigraph.sleepr::apply_tudor_locke()
