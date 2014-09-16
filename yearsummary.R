# Function for creating 1 year load/generation characteristic summary
# Summary table will have the following variables:
#   1. Customer ID
#   2. Month
#   3. Boundary
#   4. Demand Peak-Coincidence 
#   5. Generation Peak-Coincidence
#   6. Demand maxtimestring
#   7. Generation maxtimestring
#   8. Demand Load factor 
#   9. Generation Load factor
#   10. Dem/Gen Correlation
# 
# yearsummary gets fed df which is processed interval data for one year
yearsummary <- function(df){
  
  
  
  library(lubridate)
  library(plyr)
  
  #create blank dataframe with (1) customer id
  summary <- data.frame(Customer=rep(1, 12))
  
  #creating (2) month and (3) dem/gen boundaries
  source("boundary.R")
  monthlist_df <- split(df, df$month)
  
  monthboundaries <- sapply(monthlist_df, FUN = boundary)
  boundadj <- (-.2)
#   boundadj <- 0
  monthboundaries <- monthboundaries + boundadj
  #putting months into dataframe
  summary$Month <- names(monthboundaries)
  #boundary <- monthboundaries[[1:12]]
  summary$Boundary <- round(monthboundaries, 2)
  
  
  #adding in 4-10 from profile analysis
  source("peakperiod2.R")
  #repeat this 1 time for each month, or find a better way with ddply/apply/logical vector
  #df$bounds <- monthboundaries[[which(names(monthboundaries) == df$month)]]
  
  df$bounds[df$month == "Jan"] <- monthboundaries[[1]]
  df$bounds[df$month == "Feb"] <- monthboundaries[[2]]
  df$bounds[df$month == "Mar"] <- monthboundaries[[3]]
  df$bounds[df$month == "Apr"] <- monthboundaries[[4]]
  df$bounds[df$month == "May"] <- monthboundaries[[5]]
  df$bounds[df$month == "Jun"] <- monthboundaries[[6]]
  df$bounds[df$month == "Jul"] <- monthboundaries[[7]]
  df$bounds[df$month == "Aug"] <- monthboundaries[[8]]
  df$bounds[df$month == "Sep"] <- monthboundaries[[9]]
  df$bounds[df$month == "Oct"] <- monthboundaries[[10]]
  df$bounds[df$month == "Nov"] <- monthboundaries[[11]]
  df$bounds[df$month == "Dec"] <- monthboundaries[[12]]
  
  #creating dem and gen year dataframes
  dem <- ddply(df, .(month), subset, netKW >= bounds)
  
  gen <- ddply(df, .(month), subset, netKW < bounds)
  gen$netKW <- gen$netKW * -1
  #since the bounds is created by month, can simply just add netKW to bounds
  gen$netKW <- gen$netKW + gen$bounds
  
  #now perform profile analysis on each profile by month via for loop
  mnth <- unique(df$month)
  
  for(i in 1:12){
    #creating month subsets
    monthdem <- dem[dem$month == mnth[i], ]
    monthgen <- gen[gen$month == mnth[i], ]
    
    #trimming dem so that the time intervals are equal between the two profles
    #create window of time intervals based on the generation period
    genwindow <- strftime(monthgen$datetime, "%H:%M")  
    monthdem <- monthdem[strftime(monthdem$datetime, "%H:%M") %in% genwindow, ]
    
    #now can send monthdem and monthgen to peakperiod2.R for analysis (4-9)
    demandresults <- peakperiod2(monthdem, 14, 20)
    genresults <- peakperiod2(monthgen, 14, 20)
    
    #correlation between demand and generation smoothed kfs profiles (10)
    #create dataframes for demand and generation that contain maxnetKW per time interval
    dmax <- ddply(monthdem, .(time), summarize, maxnetKW=max(netKW))
    gmax <- ddply(monthgen, .(time), summarize, maxnetKW=max(netKW))
    #cor(dmax$maxnetKW, gmax$maxnetKW)
    
    #add in kfs smoothed values
    dmax$kfs <- smoothkfs(dmax$maxnetKW)
    gmax$kfs <- smoothkfs(gmax$maxnetKW)
    
    #compute correlation on smoothed values
    if(length(dmax$kfs)==length(gmax$kfs)){
      fitscore <- cor(dmax$kfs, gmax$kfs)
    } else {
      fitscore <- NA
    }
    
    #now add all the results into summary df
    #results from peakperiod2.R
    summary$PeakCoinDem[i] <- demandresults[[3]]
    summary$PeakCoinGen[i] <- genresults[[3]]
    summary$LoadFactorDem[i] <- demandresults[[4]]
    summary$LoadFactorGen[i] <- genresults[[4]]
    summary$MaxDem[i] <- demandresults[[1]]
    summary$MaxDemTime[i] <- demandresults[[2]]
    summary$MaxGen[i] <- genresults[[1]]
    summary$MaxGenTime[i] <- genresults[[2]]
    
    #result from fitscore
    summary$FitScore[i] <- round(fitscore, 2)
    
    
  }
  
  #in loop above, max times are converted to numeric, so convert back to POSIXct
  summary$MaxDemTime <- structure(summary$MaxDemTime,class=c('POSIXt','POSIXct'))
  summary$MaxGenTime <- structure(summary$MaxGenTime,class=c('POSIXt','POSIXct'))
  
  #changing column names
  names(summary) <- c("Customer", "Month", "Boundary kW", "Demand PC",
                      "Generation PC", "Demand LF", "Generation LF", "Demand Max kW",
                      "Demand Max Time", "Generation Max kW", "Generation Max Time",
                      "Dem/Gen Correlation")
  
  
  return(summary)
  
}
