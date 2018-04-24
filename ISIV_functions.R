library(randomcoloR)
#source("RA_functions.R")
#source("Data_functions.R")

############################################IV FUNCTION###################################################



nparACT_IV = function(n, data_interval, mean_all){
  num_nan <- sum(is.na(data_interval$Activity))
  interval_num_rows<-nrow(data_interval)
  
  #Creates data frame for holding the differences^2
  result_IVnum <- matrix(NA, nrow =  n)
  
  most_recent_not_nan <- 1
  #Goes through every row in the data and calculates the first derivative^2
  for (k in 2:(n)){
    
    if(!(is.na(data_interval[k,]$Activity))){
      
      z <- ((data_interval[k,]$Activity-data_interval[(most_recent_not_nan),]$Activity)^2)  
      
      result_IVnum[k,] <- z 
      
      #Since we know this one is not nan, we update the most_recent_not_nan
      most_recent_not_nan <- k
    }
  }
  
  
  
  #Results for the first few derivates for the IV numerator(9)
  print("First few derivatives for IV numerator")
  print(head(result_IVnum))
  
  #Sums the IV and then multiplies it by the number of points (excluding nan)
  IVnum <- sum(result_IVnum, na.rm = T)
  

  print("This is the number of rows not nan")
  print(nrow(data_interval)-num_nan)
  IVnumerator <- (nrow(data_interval)-num_nan)*IVnum
  
  #This is the IV numerator(10)
  print("Final IV numerator which is a sum of the derivatives multiplied by the number of points")
  print(IVnum)
  
  ## ---- IV denominator calculation
  #Creating data frame to hold denominator calculations
  result_ISdenom <- matrix(NA, nrow =  n)  
  
  
  for (j in 1:n){
    
    #Calculating the difference between the interval mean activity and the overall mean activity for each point
    if(!(is.na(data_interval[j,]$Activity))){
      y <- ((mean_all-data_interval[j,]$Activity)^2)
      result_ISdenom[j,] <- y 
    }
  }
  
  #Results for the first few differences for the IV denominator (11)
  print("First few differences for IV denominator")
  print(head(result_ISdenom))
  
  #Summing all these differences and multiplying by number of points-1 (excluding nan)
  ISdenom <- sum(result_ISdenom, na.rm = T)
  print("this is is denom")
  print(ISdenom)
  
  print("this is nan-1")
  print(n-1-num_nan)
  IVdenominator <- (n-1-num_nan)*ISdenom ## ISdenom can be used!
  
  # ----------------------------
  IV <- round(IVnumerator/IVdenominator, digits = 2)
  #Final IV(12)
  print("This is the final IV")
  print(IV)
  return(IV)
}



############################################IV FUNCTION Part 2###################################################


nparACT_IV2 = function(n, data_interval, mean_all){
  num_nan <- sum(is.na(data_interval$Activity))
  interval_num_rows<-nrow(data_interval)
  
  #Creates data frame for holding the differences^2
  result_IVnum <- matrix(NA, nrow =  n)
  
  most_recent_not_nan <- 1
  last_was_nan <- FALSE
  #Goes through every row in the data and calculates the first derivative^2
  for (k in 2:(n)){
    if(!(is.na(data_interval[k,]$Activity)) && !last_was_nan){
      
      z <- ((data_interval[k,]$Activity-data_interval[k-1,]$Activity)^2)  
      
      result_IVnum[k,] <- z 
      
      #Since we know this one is not nan, we update the most_recent_not_nan
      most_recent_not_nan <- k
    }
    if(!(is.na(data_interval[k,]$Activity)) && last_was_nan){
      last_was_nan <- FALSE
    }
    if(is.na(data_interval[k,]$Activity)){
      last_was_nan <- TRUE
    }
  }
  
  
  
  #Results for the first few derivates for the IV numerator(9)
  print("First few derivatives for IV numerator")
  print(head(result_IVnum))
  
  #Sums the IV and then multiplies it by the number of points (excluding nan)
  IVnum <- sum(result_IVnum, na.rm = T)
  
  
  print("This is the number of rows not nan")
  print(nrow(data_interval)-num_nan)
  
  #This is the IV numerator(10)
  print("Final IV numerator which is a sum of the derivatives multiplied by the number of points")
  print(IVnum)
  
  ## ---- IV denominator calculation
  #Creating data frame to hold denominator calculations
  result_ISdenom <- matrix(NA, nrow =  n)  
  
  
  for (j in 1:n){
    
    #Calculating the difference between the interval mean activity and the overall mean activity for each point
    if(!(is.na(data_interval[j,]$Activity))){
      y <- ((mean_all-data_interval[j,]$Activity)^2)
      result_ISdenom[j,] <- y 
    }
  }
  
  #Results for the first few differences for the IV denominator (11)
  print("First few differences for IV denominator")
  print(head(result_ISdenom))
  
  #Summing all these differences and multiplying by number of points-1 (excluding nan)
  ISdenom <- sum(result_ISdenom, na.rm = T)
  print("this is is denom")
  print(ISdenom)
  
  print("this is new nan for denom")
  print(nrow(result_IVnum)-sum(is.na(result_IVnum)))
  
  IVnumerator <- (nrow(result_ISdenom)-sum(is.na(result_ISdenom)))*IVnum
  
  #-1 not necessary because alreqady accounted for as is.na
  IVdenominator <- (nrow(result_IVnum)-sum(is.na(result_IVnum)))*ISdenom ## ISdenom can be used!
  
  
  
  # ----------------------------
  IV <- round(IVnumerator/IVdenominator, digits = 2)
  #Final IV(12)
  print("This is the final IV")
  print(IV)
  return(IV)
}




############################################IS FUNCTION###################################################




nparACT_IS = function(data_interval, mean_all){
  num_nan <- sum(is.na(data_interval$Activity))
  ## ---- IS numerator calculation
  
  #Creates dataframe for holding the differences
  result_ISnum <- matrix(NA, nrow = interval_day) 
  
  #Creates datafram for holding the means for the hour across the data
  hourly_means <- matrix(NA, nrow = interval_day) 
  
  
  n <- nrow(data_interval)
  p <- interval_day
  
  #Going through each hour and finding the average for that hour across all the days
  for (h in 1:interval_day){ 
    
    #s is the number of days there are
    s <- floor(nrow(data_interval)/interval_day) 
    
    
    #Creating another variable so as not to mess up the original through aliasing and such
    data_interval3 <- data_interval
    
    
    #Getting the hrly data for the same time for each day
    hrlytime <- data_interval3$DateTime[c(seq(h,nrow(data_interval3),interval_day))]
    hrlydat <- data_interval3$Activity[c(seq(h,nrow(data_interval3),interval_day))]
    
    
    #Examining whether or not the times are right and whether the data aligns with the time
    if(h==1){
      print("Times and corresponding data")
      print(hrlytime)
      print((hrlydat))
    }
    
    
    
    #Finding the difference between that hour mean for all the days and the mean across all the days/all the times
    hrlymean <- mean(hrlydat, na.rm = T) 
    hourly_means[h,] <- hrlymean
    
    x <- (hrlymean-mean_all)^2 
    result_ISnum[h,] <- x
  }
  
  x_label <- paste("Intervals (", interval_min, " Min) since", strftime(data_interval[1,]$Date, format="%H:%M:%S %p"))

  #plot(hourly_means, main = "Overall means for each hour, showing 24 hours", xlab = x_label, ylab = "Activity Count")

  print("This is the individual portions of the IS numerator")
  print(head(result_ISnum))
  
  
  print("THIS IS THE NUM NAN")
  #print(num_nan)
  ISnum <- sum(result_ISnum)  
  ISnumerator <- (n-num_nan)*ISnum  
  
  
  ## ---- IS denominator calculation
  result_ISdenom <- matrix(NA, nrow = n)  
  for (j in 1:n){
    y <- ((data_interval[j,]$Activity-mean_all)^2)
    result_ISdenom[j,] <- y
  }
  ISdenom <- sum(result_ISdenom, na.rm = T)   
  ISdenominator <- p*ISdenom  
  ## -----------------------------
  IS <- round(ISnumerator/ISdenominator, digits = 2)
  print("Final IS")
  print(IS)
  #return_IS <- c(IS, ISdenom, n, p)
  return(IS)
}




