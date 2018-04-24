#A modificiation of the nparACT package to use with missing data for IS and IV
#Modified by Qianyi Chen, Carnegie Mellon University

#INCLUDE ALL LIBRARIES HERE
#library(nparACT)
setwd("/Users/lucychen/Documents/Lab stuff/Franzen Sarah lab/Code")
source("ISIV_functions.R")

#Additional things that can be added
#-currently IS cuts off after the last 24 hour period, modify to be able to take into account last day
#^ do this by checking whether the time is the same or not
#-adding windsor for outliers
#For some reason the dates arent changing?

#Sets up the data
#1: file name/location (if the path doesn't exist, set your working directory to where the data file is (setwd("")))
#2: number of lines to skip before you reach where your headings are
setwd("/Users/lucychen/Documents/Lab stuff/Franzen Sarah lab/Code/Data/franzenlab/25")

files <- list.files(path = getwd(), pattern="*.csv", full.names=T,  recursive=FALSE)
ID_num <<- 1
final_data <<- data.frame(ID=character(),
                         IV=double(),
                         IV2=double(),
                         IS=double(), 
                         num_nan=integer(), 
                         Missing_data_percent = double(),
                         Missing_days = integer(),
                         Good_days = double(),
                         Windsor_num = integer(),
                         Windsor_percentage = double(),
                         stringsAsFactors=FALSE)


k <<-1
lapply(files, function(x) {
print(x)
data_location <- nparACT_data_load(x, 25)
ID_num <- substr(x, 1, 3)
print(ID_num)
final_data[k,]$ID <<- x
missing_days<<-0

#Gets the means for the interval and gets rid of data with too many missing points
#1: file variable
#2: Percentage threshold of missing data in an interval that is acceptable
#3: Percentage threshold of missing intervals in a day that is acceptable
data_interval <- nparACT_calculate_interval_mean(data_location, .5, .30, k)
timeOverall <- nrow(data_location)*timeRow_orig

#plot_ISIV(data_location, data_interval)

#Get mean of entire set using the interval means (8)
data_interval_entire_mean <- mean(data_interval$Activity, na.rm = TRUE)
print(data_interval_entire_mean)

#To look at data_interval
#write.csv(data_interval,'data_interval.csv')

num_nan <- sum(is.nan(data_interval$Activity))

#Running IV
final_IV <- nparACT_IV(nrow(data_interval), data_interval, data_interval_entire_mean)
final_IV2 <- nparACT_IV2(nrow(data_interval), data_interval, data_interval_entire_mean)

#Running IS
final_IS <- nparACT_IS(data_interval, data_interval_entire_mean)


#final_data<<-
print_summary(final_data, final_IS, final_IV, final_IV2, data_interval, k)
#final_data[1,]$IV<-1
k <<- k+1

#Make sure nothing is a result of workspace
rm(list = ls())
})

write.table(final_data, "final_data_good_days_not_windsorized.csv", row.names=F, sep=",")

