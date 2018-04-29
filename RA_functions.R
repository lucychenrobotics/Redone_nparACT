nparACT_L5M10 = function(interval_means, data_interval){
  ## ---- L5 values 
  
  ##Need to make sure at least 50% of data is there or something
  L5_matrix <- matrix(NA, interval_day)
  data_L5_M10 <- interval_means
  five_hr_in_min <- 5*60
  num_interval_in_five_hr <- five_hr_in_min/interval_min
  for (l in 1:(interval_day-num_interval_in_five_hr+1)){  
    for_L5 <- data_L5_M10[l:(num_interval_in_five_hr+l-1)]
    L5_matrix[l] <- mean(for_L5, na.rm = T)
  }
  
  for (l in (interval_day-num_interval_in_five_hr+2):interval_day){  
    if(!is.na(data_L5_M10[l])){
      for_L5 <- data_L5_M10[l:nrow(data_L5_M10)]
      for_L5_wraparound <- data_L5_M10[1:(num_interval_in_five_hr-(nrow(data_L5_M10)-l+1))]
    
      for_L5_combined <<- c(for_L5, for_L5_wraparound)
      ##Don't hardcode .3 later
      if(sum(is.na(for_L5_combined))/length(for_L5_combined) > .3){
        L5_matrix[l] <- NA
      }
      else{
        L5_matrix[l] <- mean(for_L5_combined, na.rm = T)
      }
    }
      
    else{
      L5_matrix[l] <- NA
    }
  }
  ## --------------------------------
  print(L5_matrix)

  
  # M10
  M10_matrix <- matrix(NA, interval_day)
  ten_hr_in_min <- 10*60
  num_interval_in_ten_hr <- ten_hr_in_min/interval_min
  for (l in 1:(interval_day-num_interval_in_ten_hr+1)){  
    for_M10 <- data_L5_M10[l:(num_interval_in_ten_hr+l-1)]
    M10_matrix[l] <- mean(for_M10, na.rm = T)
  }
  
  for (l in (interval_day-num_interval_in_ten_hr+2):interval_day){  
    for_M10 <- data_L5_M10[l:nrow(data_L5_M10)]
   
    for_M10_wraparound <- data_L5_M10[1:(num_interval_in_ten_hr-(nrow(data_L5_M10)-l+1))]
   
    for_M10_combined <<- c(for_M10, for_M10_wraparound)
    
    M10_matrix[l] <- mean(for_M10_combined, na.rm = T)
  }
  
  print(M10_matrix)
  ## --------------------------------
  
  if(FALSE){
  ## ---- M10 values (most active 10h period)
  M10_matrix <- matrix (NA, 1440)
  for (m in 1:1440){  
    for_M10 <- data_L5_M10[m:(599+m)]
    M10_matrix[m] <- mean(for_M10, na.rm = T)
  }
  ## --------------------------------
  }
  
  ## ---- Find min of L5 and max of M10
  L5 <- min(L5_matrix)
  L5_start <- which.min(L5_matrix)
  L5_start_time <- data_interval[L5_start,]$DateTime
  L5_start_time <- as.character(L5_start_time)
  if(length(L5_start_time) < 16){
    L5_start_time <- paste(L5_start_time, "00:00:00")
  }
  L5_start_time_hr <- as.numeric(substr(L5_start_time, 12, 13))
  L5_start_time_min <- as.numeric(substr(L5_start_time, 15, 16))
  #Fixing in case it crosses over 12 am
  if(L5_start_time_hr < 12){
    L5_start_time_hr <- L5_start_time_hr+24
  }
  
  L5_start_time <- L5_start_time_hr+L5_start_time_min/60
  
  
  
  #L5_start_time_hr <- as.numeric(format(L5_start_time), "%H")
  #print(L5_start_time_hr)
  #print("hr")
  #print(L5_start_time_min)
  #print("min")
  print(L5_start_time)
  print("final L5")
  #L5_start_time <- strftime((L5_start_time), format="%H:%M:%S")
  ##print(strftime(L5_start_time, format="%H"))
  #print("456")
  #L5_hour <- as.numeric(as.character(strftime(L5_start_time, format="%H")))
  #print(L5_hour)
  #print("123")
  
 
  M10 <- round(max(M10_matrix), digits = 2)
  M10_start <- which.max(M10_matrix)
  M10_start_time <- data_interval[M10_start,]$DateTime
  M10_start_time <- as.character(M10_start_time)
  M10_start_time_hr <- as.numeric(substr(M10_start_time, 12, 13))
  M10_start_time_min <- as.numeric(substr(M10_start_time, 15, 16))
  

  M10_start_time <- M10_start_time_hr+M10_start_time_min/60
  #daytime_minutes <- matrix(NA)
  #time <- hourly_means$DateTime
  #time <- as.character(DateTime)
  #for (v in seq(1,a,(SR*60))){  
  #  daytime_minutes[v] <- time[v]
  #}
  #daytime_minutes <- na.omit(daytime_minutes)
  #daytime_minutes <- as.character(daytime_minutes)
  if(FALSE){
  L5_starttime <- as.character(data_interval[L5_start,]$DateTime)
  temp = unlist(strsplit(L5_starttime, ' ') )
  L5_starttime <- temp[2] 
  print(L5_starttime)
  print("This better not work.")
  
  M10_starttime <- daytime_minutes[M10_start]  
  temp = unlist(str_split(M10_starttime, ' ') )
  M10_starttime <- temp[2] 
  }
  
  ## --------------------------------
  RA <- round((M10-L5)/(M10+L5), digits = 2)
  print(L5)
  print("L5")
  print(M10)
  print("M10")
  print(RA)
  print("RA")
  #result_RA <- data.frame(L5, L5_starttime, M10, M10_starttime, RA)
  result_RA <- data.frame(L5, L5_start_time, M10, M10_start_time, RA)
  return(result_RA)
}

nparACT_L5M10Lflex = function(data, minaverage, a, SR, minutes){
  ## ---- L5 values (least active 5h period)
  L5_matrix <- matrix(NA, 1440)
  data_L5_M10 <- rep(minaverage, 2) 
  for (l in 1:1440){  
    for_L5 <- data_L5_M10[l:(299+l)]
    L5_matrix[l] <- mean(for_L5, na.rm = T)
  }
  ## --------------------------------
  
  ## ---- M10 values (most active 10h period)
  M10_matrix <- matrix (NA, 1440)
  for (m in 1:1440){  
    for_M10 <- data_L5_M10[m:(599+m)]
    M10_matrix[m] <- mean(for_M10, na.rm = T)
  }
  ## --------------------------------
  
  ## ---- Lflex values (flexible length)
  Lflex_matrix <- matrix(NA, 1440)
  data_L5_M10 <- rep(minaverage, 2) 
  for (o in 1:1440){  
    for_Lflex <- data_L5_M10[o:((minutes-1)+o)]
    Lflex_matrix[o] <- mean(for_Lflex, na.rm = T)
  }
  ## --------------------------------
  
  ## ---- Find min of L5 and max of M10
  L5 <- round(min(L5_matrix), digits = 2)
  L5_start <- which.min(L5_matrix)
  
  M10 <- round(max(M10_matrix), digits = 2)
  M10_start <- which.max(M10_matrix)
  
  Lflex <- round(min(Lflex_matrix), digits = 2)
  Lflex_start <- which.min(Lflex_matrix)
  
  daytime_minutes <- matrix(NA)
  time <- data$time
  time <- as.character(time)
  for (v in seq(1,a,(SR*60))){  
    daytime_minutes[v] <- time[v]
  }
  daytime_minutes <- na.omit(daytime_minutes)
  daytime_minutes <- as.character(daytime_minutes)
  
  L5_starttime <- daytime_minutes[L5_start] 
  temp = unlist(str_split(L5_starttime, ' ') )
  L5_starttime <- temp[2] 
  
  M10_starttime <- daytime_minutes[M10_start]  
  temp = unlist(str_split(M10_starttime, ' ') )
  M10_starttime <- temp[2] 
  
  Lflex_starttime <- daytime_minutes[Lflex_start]  
  temp = unlist(str_split(Lflex_starttime, ' ') )
  Lflex_starttime <- temp[2]
  
  ## --------------------------------
  RA <- round((M10-L5)/(M10+L5), digits = 2)
  result_RA <- data.frame(L5, L5_starttime, M10, M10_starttime, Lflex, Lflex_starttime, RA)
  return(result_RA)
}
