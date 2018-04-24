#A modificiation of the nparACT package to use with missing data for IS and IV
#Modified by Qianyi Chen, Carnegie Mellon University

#INCLUDE ALL LIBRARIES HERE
#library(nparACT)
setwd("/Users/lucychen/Documents/Lab stuff/Franzen Sarah lab/Code/Redone_Code")
source("ISIV_functions.R")
#source("RA_functions.R")
source("Data_functions.R")

#Additional things that can be added
#-currently IS cuts off after the last 24 hour period, modify to be able to take into account last day
#^ do this by checking whether the time is the same or not
#-adding windsor for outliers


#Sets up the data
#1: file name/location (if the path doesn't exist, set your working directory to where the data file is (setwd("")))
#2: number of lines to skip before you reach where your headings are
setwd("/Users/lucychen/Documents/Lab stuff/Franzen Sarah lab/Code/Data/test")
data_location <- nparACT_data_load("783_test.csv", 26)
timeOverall <- nrow(data_location)*timeRow_orig

#Gets the means for the interval and gets rid of data with too many missing points
#1: file variable
#2: Percentage threshold of missing data in an interval that is acceptable
#3: Percentage threshold of missing intervals in a day that is acceptable
data_interval <- nparACT_calculate_interval_mean(data_location, .5, .3)

print("this thing")
plot_ISIV(data_location, data_interval)

#Get mean of entire set using the interval means (8)
data_interval_entire_mean <- mean(data_interval$Activity, na.rm = TRUE)
print(data_interval_entire_mean)
#print(data_interval[1:3,]$Activity)
print("CHECKING")

#To look at data_interval
#write.csv(data_interval,'/data_interval.csv')

num_nan <- sum(is.nan(data_interval$Activity))
print("past here")
#Running IV
final_IV <- nparACT_IV(nrow(data_interval), data_interval, data_interval_entire_mean)

#Running IS
final_IS <- nparACT_IS(data_interval, data_interval_entire_mean)

#print_summary(final_IS, final_IV, data_interval)
 