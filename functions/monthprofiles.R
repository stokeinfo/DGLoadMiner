monthprofiles <- function(df,
                          analyzedemand=FALSE,
                          analyzegeneration=FALSE,
                          smoothkfs=FALSE,
                          LDC=FALSE
                          ){
  
  #setwd("C:/Users/Jonathan/Desktop/DGLoadMiner/functions")
  
  library(lubridate)
  library(plyr)
  
  
  source("boundary.R")
  monthlist_df <- split(df, df$month)
  
  monthboundaries <- sapply(monthlist_df, FUN = boundary)
  
  #this is the "good enough" solution for now. Later on will need to the horizontal
  #and vertical intersection in boundary.R
  boundadj <- (-.2)
  
  #repeat this 1 time for each month, or find a better way with ddply/apply function
  df$bounds[df$month == "Jan"] <- monthboundaries[[1]]  + boundadj
  df$bounds[df$month == "Feb"] <- monthboundaries[[2]]  + boundadj
  df$bounds[df$month == "Mar"] <- monthboundaries[[3]]  + boundadj
  df$bounds[df$month == "Apr"] <- monthboundaries[[4]]  + boundadj
  df$bounds[df$month == "May"] <- monthboundaries[[5]]  + boundadj
  df$bounds[df$month == "Jun"] <- monthboundaries[[6]]  + boundadj
  df$bounds[df$month == "Jul"] <- monthboundaries[[7]]  + boundadj
  df$bounds[df$month == "Aug"] <- monthboundaries[[8]]  + boundadj
  df$bounds[df$month == "Sep"] <- monthboundaries[[9]]  + boundadj
  df$bounds[df$month == "Oct"] <- monthboundaries[[10]]  + boundadj
  df$bounds[df$month == "Nov"] <- monthboundaries[[11]]  + boundadj
  df$bounds[df$month == "Dec"] <- monthboundaries[[12]]  + boundadj
  
 
  if(analyzedemand){
    df <- ddply(df, .(month), subset, netKW >= bounds)
    KWtype <- "Demand"
  }

  if(analyzegeneration){
    df <- ddply(df, .(month), subset, netKW < bounds)
    df$netKW <- df$netKW * -1
    #since the bounds is created by month, can simply just add netKW to bounds
    df$netKW <- df$netKW + df$bounds
    KWtype <- "Generation"
  }
  
  #add in times formatted properly for ggplot xaxis.
  df$ggtime <- strptime(df$time, "%H:%M")
  df$ggtime <- as.POSIXct(df$ggtime)
  
  #create df2 for maxplot and smoothkfs
  df2 <- ddply(df, c("month", "time"), summarise,
              maxnetKW = max(netKW))
  
  #add in times formatted properly for ggplot xaxis in df2 as well.
  df2$ggtime <- strptime(df2$time, "%H:%M")
  df2$ggtime <- as.POSIXct(df2$ggtime)
  

#   #block for demand
#   maxbymonth <- ddply(df, c("month", "time"), summarise,
#                       maxnetKW = max(netKW))

#   #block for generation
#   maxbymonth <- ddply(df, c("month", "time"), summarise,
#                       maxnetKW = min(netKW)*-1)

  
  
  
  
  
  #now the plotting...
  maxplot <- ggplot(df2, aes(ggtime, maxnetKW)) +
    geom_point(aes(color=month)) +
    geom_line(aes(color=month)) +
#     facet_wrap(~month, ncol=6) +
    labs(title=paste(KWtype, " Monthly Profiles\n2013", sep=""), x="© 2014 Stoke Informatics", y="Net kW") +
    theme_bw() +  
    theme(plot.title=element_text(face="bold"))
  
  if(smoothkfs){
    #adding in KFS smoother
    source("smoothkfs.R")
    # create a list of smooth matrices by month.
    # kfstest <- tapply(maxbymonth$maxnetKW, maxbymonth$month, smoothkfs)
    #use the awesome ddply!
    df2 <- ddply(df2, .(month), transform,
                 kfs = smoothkfs(maxnetKW)[[1]],
                 low = smoothkfs(maxnetKW)[[2]],
                 hi = smoothkfs(maxnetKW)[[3]])
                        
    if(analyzedemand){
      profile <- list(geom_ribbon(aes(ymin=low, ymax=hi), alpha=0.4),
                      geom_line(aes(color=month), size=0))
                      
    }
    
    if(analyzegeneration){      
      profile <- list(geom_ribbon(aes(ymin=low, ymax=hi), alpha=0.4),
                      geom_line(aes(color=month), size=0))
      
    }
    
    maxplot <- ggplot(df2, aes(ggtime, kfs)) +
      #geom_point(aes(color=month)) +
      #geom_line(aes(color=month)) +
      #geom_smooth(aes(color=month)) +
      #coord_cartesian(ylim = c(0, max(df$kfs))) + 
      profile +
      facet_wrap(~month, ncol=6) +
      labs(title=paste("Monthly ",KWtype, " Profiles\n2013", sep=""), x="© 2014 Stoke Informatics", y="Net kW") +
      theme_bw() +  
      theme(plot.title=element_text(face="bold"), legend.position="none", axis.text.x = element_text(angle = 90, hjust = -1))
#       scale_colour_tableau("colorblind10") + 
#       theme_igray()
#       theme_bw()
#       scale_colour_tableau()
    }

  if(LDC){
    source("durationcurve.R")
    dc <- ldc(df, KWtype)
  }
  
  print(maxplot)
  
  return(df)
}