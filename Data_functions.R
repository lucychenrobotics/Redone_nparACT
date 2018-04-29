library(randomcoloR)

############################################VARIABLES###################################################
#Need to create a function that sets the time variables

#Variables holding Time Information
interval_min<<- 30 #number of minutes in each interval
interval_day <<- 1440/interval_min #number of intervals in a day
timeRow_orig <<- 60 #seconds for each row originally
interval_rows <<- (60/timeRow_orig) * interval_min #number of rows from original data that make up one interval
timeRow <<- interval_min*60 #seconds for each row afterwards
timeDay <<- 24*60*60 #seconds in a day

#############################################DATA LOAD##################################################

nparACT_data_load = function(data_location, skip_num = 0, sep_char = ","){
  #reading in the file into a data frame
  data_location <-read.csv(file = data_location, skip = skip_num, header = TRUE, sep = sep_char)
  
  #check what the data looks like thus far (1)
  print("Data at input")
  print(head(data_location))
  
  #Combining date and time and making it a poxit class so R knows its time
  data_location$Date <- paste(data_location$Date, data_location$Time, sep=" ")
  data_location$Date <- strptime(data_location$Date,format='%m/%d/%Y %I:%M:%S %p')
  print(head(data_location))
  print("This is data_location")
  #Since $Date now includes the time too, this gets rid of useless variable: Time
  data_location$Time <- NULL
  
  #Getting rid of first part of NaN's
  found_first_num <- FALSE
  first_num <- 1
  while(!found_first_num){
    if(!is.na(data_location[first_num,]$Activity)){
      found_first_num <- TRUE
    }
    else{
      first_num <- first_num +1
    }
  }
  
  #If first_num is not 1 then that means there are NaN's at the beginning so delete them
  if(first_num != 1){
    data_location <- data_location[-seq(1, first_num-1, by=1),]
  }
  
  #Getting rid of last part of NaN's
  found_last_num <- FALSE
  last_num <- nrow(data_location)
  while(!found_last_num){
    if(!is.na(data_location[last_num,]$Activity)){
      found_last_num <- TRUE
    }
    else{
      last_num <- last_num-1
    }
  }
  
  
  #If last_num is not 1 then that means there are NaN's at the beginning so delete them
  if(last_num != nrow(data_location)){
    data_location <- data_location[-seq(last_num+1, nrow(data_location), by=1),]
  }
  
  print("first num")
  print(first_num)
  print(data_location[first_num,])
  print("last num")
  print(last_num)
  print(data_location[last_num,])
  
  #Getting rid of last day (because it is only a partial day)
  rows_in_day_orig <- timeDay/timeRow_orig
  
  num_days <- floor(nrow(data_location)/rows_in_day_orig)

  data_location <- data_location[-seq(floor(num_days*rows_in_day_orig)+1, nrow(data_location), by=1),]
  
  #Checking and making sure data looks cleaned up (2)
 
  
  #returns it so it can be put in a variable
  return(data_location)
}

############################################INTERVAL MEANS FUNCTION###################################################

#Calculates interval means
#If in the interval, interval_missing_thres of the data is missing, then NA is placed as the value for that bin
#If within a 24 hour period there is more than day_missing_thres of the data missing, then all of the values within that 24 hours are NA.
nparACT_calculate_interval_mean = function(data_location, interval_missing_thres, day_missing_thres, k){
  missing_days<<-0
  timeOverall <- nrow(data_location)*timeRow_orig
  
  #Setting up a data frame to put the mean within each interval
  #seq(start time, end time, by = step time in seconds)

  data_interval <- data.frame(DateTime = seq(data_location[1,]$Date,
                                             data_location[nrow(data_location),]$Date, 
                                             by=(timeRow)))


  #Creates the Activity column
  data_interval$Activity = NA

  #Checking and making sure set up of intervals is correct (3)
  print("Example of the new intervals")
  print(head(data_interval))
  print(nrow(data_interval))
  print("nrow data interval")
  
  #Total rows of the new interval
  #print("Total number of rows given new interval vs. original number of rows")
  #print(c(round(timeOverall/timeRow), nrow(data_location)))
  
  #Goes through each of the new rows individually and calculates the mean
  #Starts at 0 because of the first set of 48, need to add 1 though because rows start at 1
  #print("Rows of original data where too much is missing")
  for (i in 0:(floor(timeOverall/timeRow)-1)){
    
    #Gets the rows that will be used to calculate the mean of this new row (4)
    subset_h <- data_location$Activity[(i*interval_rows+1):((i+1)*interval_rows)]
    if(i == 0) print(subset_h)
    
    #Sets interval mean as NaN where more than interval_missing_thres of the data in that interval is missing (5)
    if(i == 1){
      print(i)
      print("checking")
      print("working?")
      print(sum(is.na(subset_h))/length(subset_h))
      print("checking percent missing")
      print(interval_missing_thres)
    }
    
    if(sum(is.na(subset_h))/length(subset_h) >= interval_missing_thres){
      #print(c((i*interval_rows+1), (i+1)*interval_rows))
      mean_subset_h <- NaN
    }
    
    #Otherwise it calculates the mean of that interval
    else{
      #Get mean of activity counts within that interval
      mean_subset_h <- mean(subset_h, na.rm = TRUE)
    }
    
    #Set that interval mean in the data frame that holds all the means
    data_interval[i+1,]$Activity <- mean_subset_h
  }
  print(nrow(data_interval))
  print("this again")
  
  #Windsorizer
  print(quantile(data_interval$Activity, na.rm = TRUE))
  print(unname(quantile(data_interval$Activity, na.rm = TRUE)[4]))
  activity_third_quantile <- unname(quantile(data_interval$Activity, na.rm = TRUE)[4])
  #print(activity_third_quantile)
  activity_IQR <- IQR(data_interval$Activity, na.rm = TRUE)
  print(activity_IQR)
  #print(activity_IQR)
  #print("THIS IS IQR?")
  #print(activity_third_quantile+1.5*activity_IQR)
  #print("UPPER THRES?")
  #print(quantile(data_interval$Activity, na.rm = TRUE))
  windsor_count <<- 0
  print(windsor_count)
  print("WINDSOR COUNT")
  for(i in 1:nrow(data_interval)){
    if(!is.na(data_interval$Activity[i]) &&
      data_interval$Activity[i] > (activity_third_quantile+1.5*activity_IQR)){
      print(data_interval$Activity[i])
      print("Getting rid of this")
      data_interval$Activity[i] <- activity_third_quantile+1.5*activity_IQR
      windsor_count <<- windsor_count+1
    }

  }
  
  
  
  if(FALSE){
    #Getting rid of 24 hour periods where more than day_missing_thres of data are missing
    #It is a rolling window, does not depend on when the data started
    
    #Parameter to make sure that if a 24 hour period is deleted, it doesn't go through that period
    already_nan <- 0
    
    #Going through every single row and calculating percentage missing 24 hrs after it, stops at last day
    #print("Rows where day is ommitted because too much data missing")
    for (k in 1:(nrow(data_interval)-interval_day)){
      
      #if already_nan is at 0, that means this row was not ommited already and so it should be checked
      if(already_nan == 0){
        
        #24 hr period
        subset_k <- data_interval$Activity[(k):(k+interval_day)]
        
        #checking what the percentage missing is
        if((sum(is.na(subset_k))/length(subset_k)) > day_missing_thres){
          
          #These 24 hour periods had too much missing data(6)
          print(c("Percentage Missing", sum(is.na(subset_k))/length(subset_k)))
          print(c("Day eliminated", k/interval_day+1, "to", (k+interval_day)/interval_day+1))
          data_interval$Activity[k:((k+interval_day))] <- NaN
          missing_days <<- missing_days+1
          already_nan <- interval_day-1
        }
      }
      
      #Times
      else{
        already_nan <- already_nan - 1
      }
      
    }
    
  }
  
  if(TRUE){
    
    
    
    
    #Parameter to make sure that if a 24 hour period is deleted, it doesn't go through that period
    already_nan <- 0
    
    #Going through every single row and calculating percentage missing 24 hrs after it, stops at last day
    print("Rows where day is ommitted because too much data missing")
    for (k in 0:(floor(nrow(data_interval)/interval_day)-1)){

     
      #24 hr period
      subset_k <- data_interval$Activity[(k*interval_day+1):((k+1)*interval_day)]

      #checking what the percentage missing is
      if((sum(is.na(subset_k))/length(subset_k)) > day_missing_thres){
        
        #These 24 hour periods had too much missing data(6)
        print(c("Percentage Missing", sum(is.na(subset_k))/length(subset_k)))
        print(c("Day eliminated", k/interval_day+1, "to", (k+interval_day)/interval_day+1))
        data_interval$Activity[(k*interval_day+1):((k+1)*interval_day)] <- NaN
        missing_days <<- missing_days+1
        #already_nan <- interval_day-1
      }
      
    }
  }  
  #Check and make sure interval data is right(7)
  
  print("New intervals with means of each interval")
  print(head(data_interval))
  
  return(data_interval)
  
}





##############################################plot function##########################################

plot_ISIV = function(data_location, data_interval){
  #Compare plots for both data
  plot_color <- randomColor()  
  if(FALSE){
    for(x in 0: floor((nrow(data_interval))/(interval_day))){
      
      plot_color <- randomColor()  
      
      #plotting data before getting rid of days and intervals
      x_label <- paste("Intervals (", timeRow_orig, " seconds) since", strftime(data_location[1,]$Date, format="%H:%M:%S %p"))
      main_label <- paste("Original Data Day: ", x+1, "\n", 
                          data_location[(x*interval_day*interval_min+1),]$Date, "to", data_location[((x+1)*interval_day*interval_min),]$Date)
      plot(data_location$Activity[(x*interval_day*interval_min+1):((x+1)*interval_day*interval_min)], col=plot_color, ylim=c(0,2500),
           xlab= x_label, ylab="Activity Count", main = main_label)
      
      #plotting data after getting means of each interval and cleaning up missing intervals
      x_label2 <- paste("Intervals (", interval_min, " minutes) since", strftime(data_location[1,]$Date, format="%H:%M:%S %p"))
      main_label2 <- paste("Cleaned Mean Interval Data Day: ", x+1, "\n", 
                           data_location[(x*interval_day*interval_min+1),]$Date, "to", data_location[((x+1)*interval_day*interval_min),]$Date)
      plot(data_interval$Activity[(x*interval_day+1):((x+1)*interval_day)],col=plot_color, ylim=c(0,2500),
           xlab= x_label2, ylab="Activity Count", main = main_label2)
      
    }
    
  }
  #ameplot <- paste(getwd(), "/plot/", k, "_subject25_mean", ".jpg", sep = "")
  #jpeg(nameplot)
  x_label2 <- paste("Intervals (", interval_min, " minutes) since", strftime(data_location[1,]$Date, format="%H:%M:%S %p"))
  main_label2 <- paste("All days mean interval data for a 24 hour span ")
  plot(data_interval$Activity[(0*interval_day+1):((0+1)*interval_day)],col=plot_color, ylim=c(0,2500),
       xlab= x_label2, ylab="Activity Count", main = main_label2, cex = .5)
  
  
  #Plotting mean data for all the days onto one graph
  for(x in 0: floor((nrow(data_interval))/(interval_day))){
    
    plot_color <- randomColor()  
    
    #plotting data after getting means of each interval and cleaning up missing intervals
    points(data_interval$Activity[(x*interval_day+1):((x+1)*interval_day)],col=plot_color, ylim=c(0,2500),
           cex = .5)
  }
  #dev.off()
  plot(data_interval$Activity[1:nrow(data_interval)])
  #axis(1, at=seq(1, nrow(data_interval), by=1), labels=strptime(data_interval$DateTime,format = "%H:%M:%S"), col.axis="red", las=1)
}



##############################################save function##########################################

print_summary = function(final_data, IS, IV, IV2, L5M10, data_interval, k){
  print("-------------------------------------SUMMARY-----------------------------------------------")
  print(c("IS:", IS))
  print(c("IV:", IV))
  num_nan <- sum(is.na(data_interval$Activity))
  print(c("Number of NaN intervals", sum(is.na(data_interval$Activity)), "vs Overall Number Of Intervals", nrow(data_interval) ))
  Missing_data_percent <- num_nan/nrow(data_interval)
  print(c("Percentage of intervals missing:", Missing_data_percent))
  
  
  final_data[k,]$IS <<- IS
  final_data[k,]$IV <<- IV
  final_data[k,]$IV2 <<- IV2
  final_data[k,]$num_nan <<- num_nan
  final_data[k,]$Missing_data_percent <<- Missing_data_percent
  final_data[k,]$Missing_days<<- missing_days
  final_data[k,]$Good_days<<-(nrow(data_interval)-num_nan)/interval_day
  final_data[k,]$Windsor_num <<- windsor_count
  final_data[k,]$Windsor_percentage <<- windsor_count/nrow(data_interval)
  final_data[k,]$L5 <<- as.numeric(L5M10[1])
  final_data[k,]$L5_start_time <<- as.numeric(L5M10[2])
  final_data[k,]$M10 <<- as.numeric(L5M10[3])
  final_data[k,]$M10_start_time <<- as.numeric(L5M10[4])
  final_data[k,]$RA <<- as.numeric(L5M10[5])
  
  
  return(final_data)
}



####END OF FUNCTIONS#######





