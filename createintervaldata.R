createintervaldata <- function(Hs=04, Hf=21, mInterval=15,
                           #min_netKW, minKWLabel, 
                           filename = '../rawdata/bfamdata1.csv', 
                           plotdata=FALSE,
                           showmonths=FALSE,
                           showParamList=FALSE){
  
  #source('DGD_PlotRawData.R')
  #Hs hour start
  #Hf hour end
  #mInterval sampling interval
  #if min_netKW is a value then discard rows having netKW lower 
  #   than this threshold
  
  setwd("C:/Users/Jonathan/Desktop/DGLoadMiner/functions")
  
  if (showParamList){
    print(paste('params in subsetdata:',as.list(match.call())))
  }
  
  print(paste('reading raw data filename=',filename))
  
  rawdf<-read.csv(filename, header = TRUE)
  str(rawdf)
  
  #keep cols 1 -3 
  rawdf<-subset(rawdf[,1:3])
  
  #rename columns
  colnames(rawdf) <- c("datetime", "useKW", "genKW")
  
  #POSIXct method makes possible slicing by time
  timeFormat='%m/%d/%Y %H:%M'
  oneTime<-as.character(rawdf[1,1])
  if (grepl('-',oneTime,fixed=TRUE)==TRUE) {
    timeFormat<-'%Y-%m-%d %H:%M'
  }
  #rawdf$datetime <- as.POSIXct(rawdf$datetime, format = '%m/%d/%Y %H:%M')
  rawdf$datetime <- as.POSIXct(rawdf$datetime, format = timeFormat)
  
  
  #order
  rawdf <- rawdf[order(rawdf$datetime),]
  
  # collect rows only from 'daytime' or Hour Start - Hour Finish
  resourcewindow <- sprintf('%02d',c(Hs:Hf))
  rawdf <- rawdf[strftime(rawdf$datetime, '%H') %in% resourcewindow, ]
  
  #('00','15','30','45') if mInterval=15
  #or by any interval
  intervals<-sprintf('%02d',seq(0,59,mInterval))
  rawdf <- rawdf[strftime(rawdf$datetime, "%M") %in% intervals, ]
  
  #add in %H:%M helper column for downstream use in computing maxes for each time interval within each month
  rawdf$time <- strftime(rawdf$datetime, "%H:%M")
  
  #add in month categorical variable and make factor to later use in ggplot
  rawdf$month <- months(rawdf$datetime, abbreviate=TRUE)
  monthlevels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  rawdf$month <- factor(rawdf$month, levels = monthlevels)
  
  #create netKW
  rawdf$netKW <- rawdf[, "useKW"]-rawdf[, "genKW"]
  
  #add an index for loess_fit trend line
  rawdf$index<-seq(1:nrow(rawdf))
  
  #create processed (comment if using SR/SS block below)
  processed <- rawdf
  
#   ##########SS/SR block########## (if using comment out lines 38-40 & lines 61-62)
#   source("sun.R")
#   sunshine <- sun()
#   
#   #output dataframe from loop should be called "processed"
#   processed <- data.frame("datetime"=as.POSIXct(NA), "useKW"=NA, "genKW"=NA, "time"=NA, "month"=NA)
#   for(i in 1:length(monthlevels)){
#     monthdf <- rawdf[rawdf$month == monthlevels[i], ]
#     window <- sprintf("%02d", sunshine[[1]][i]:sunshine[[2]][i])
#     monthdf <- monthdf[strftime(monthdf$datetime, "%H") %in% window, ]
#     processed <- rbind(processed, monthdf)
#   }
#   
#   #delete first column of processed
#   processed <- processed[-1, ]
#   
#   #set factor levels
#   processed$month <- factor(processed$month, levels = monthlevels)
# 
#   
#   #adding in 1 hour for DST. 2013 DST starts March 10th and ends Nov 3rd at 2am. 
#   DSTstart <- "03-10"
#   DSTend <- "11-03"
#   
#   #create sequence of days
#   DSTrange <- seq(as.Date(DSTstart, '%m-%d'), as.Date(DSTend, '%m-%d'), "day")
#   #strip out only the month and day and convert to character vector
#   DSTrange <- strftime(DSTrange, '%m-%d')
#     
#   #create row index of applicable observations within DSTrange
#   DSTindex <- which(strftime(processed$datetime, '%m-%d') %in% DSTrange)
#   #creating a copy for validation
# #   processed2 <- processed
#   processed[DSTindex, "datetime"] <- processed[DSTindex, "datetime"] + 60^2
# #   #validation
# #   processed[DSTindex, "datetime"]  <- processed[DSTindex, "datetime"] + 60^2
# #   all(processed2[DSTindex, "datetime"] + 60^2 == processed[DSTindex, "datetime"])
  
  
  
  
  # write a file for future use.
  processedfilename=paste('../processedintervaldata/',sprintf('%02d',Hs),'-',sprintf('%02d',Hf),'H_',mInterval, '.csv',sep='')
  print(paste('writing processed interval data file=', processedfilename))
  write.csv(processed, file=processedfilename)
  
  #return(processedfilename)
  
  
  #plotting
  library(ggplot2)
  
  if(showmonths) {
    points <- geom_point(aes(color=month))
  } else {
    points <- geom_point()
  }
  
  if (plotdata==TRUE) {
    print('parameter, plotInProcessRawData=TRUE. Loading ggplot2 and plotting.')
    
    rawplot <- ggplot(processed, aes(datetime, netKW)) + 
      points +  
      labs(title="2013 Daily Metered Net kW", 
           x=paste("Hours: ", Hs, ":00", "-", Hf, ":00,", " Interval= ", mInterval, " minutes", '\n© 2014 Stoke Informatics, LLC', sep=""),
           y="Net kW",
           color="Month") +
      theme_bw() +
      theme(plot.title=element_text(face="bold")) 
      
#       ggtitle("2013 Daily Metered Net kW") +
#       xlab(paste("Hours: ", Hs, ":00", "-", Hf, ":00,", " Interval= ", mInterval, " minutes", '\n© 2014 Stoke Informatics, LLC', sep="")) +
#       ylab("Net kW") + 
#       labs(color="Month") +
#       theme_bw()
    print(rawplot)    
  } 
  else {
    print('parameter, plotInProcessRawData=FALSE. Done')
  }
  # return the data.frame to the calling function
  return(processed)
}
