multiday <- function(df, m1=6, d1=1, m2=6, d2=30,
                     plotnet=TRUE,
                     plotuse=FALSE,
                     plotgen=FALSE,
                     setboundary=FALSE, 
                     analyzedemand=FALSE, 
                     analyzegeneration=FALSE,
                     profileanalysis=FALSE,
                     LDC=FALSE,
                     maxclusters=FALSE,
                     maxloess=FALSE,
                     netloess=FALSE,
                     smoothkfs=FALSE){
  
#Code block for   
#   filename=paste('../processedintervaldata/',sprintf('%02d',Hs),'-',sprintf('%02d',Hf),'H_',mInterval, '.csv',sep='')
#   
#   #return(filename)
#   #if data was a localized variable this would not work
#   if (file.exists(filename)){
#     #read it in, validate contents
#     print(paste('found pre-processed datafile=',filename, 'reading...'))
#     df <- read.csv(filename, header = TRUE, stringsAsFactors=FALSE)    
#   } else {
#     # need to be sourced
#     print(paste(filename, 'Does not exist'))
#     print('...sourcing and running createintervaldata.R to create processed interval data')
#     source('createintervaldata.R')
#     
#     df <- createintervaldata(Hs=Hs,Hf=Hf,mInterval=mInterval)
#     print(filename, 'created')
#   }
  
  start <- paste(sprintf('%02d', m1), "-", sprintf('%02d', d1), sep="")
  end <- paste(sprintf('%02d', m2), "-", sprintf('%02d', d2), sep="")
  
  #create sequence of days
  dayrange <- seq(as.Date(start, '%m-%d'), as.Date(end, '%m-%d'), "day")
  #strip out only the month and day and convert to character vector
  dayrange <- strftime(dayrange, '%m-%d')
  
  
  #now subset to only the desired days
  df <- df[strftime(df$datetime, '%m-%d') %in% dayrange, ]
  
  #create dayrange character string for plot title
  dayrange <- strftime(range(df$datetime), '%m-%d-%Y')
  dayrange <- paste(dayrange[1],"to", dayrange[2])
  
  #add in times formatted properly for ggplot xaxis.
  df$ggtime <- strptime(df$time, "%H:%M")
  df$ggtime <- as.POSIXct(df$ggtime)
  
  #switches for demand and generation
  source("boundary.R")
  if(analyzedemand|analyzegeneration){
    setboundary=FALSE
    source("boundary.R")
    library(lubridate)
#     Hs <- min(hour(df$datetime))
#     Hf <- max(hour(df$datetime))
    bounds <- boundary(df) - .2
#     bounds <- boundary(df)
    if(analyzedemand){
      df <- df[df$netKW >= bounds, ]
      KWtype <- "Demand"
    }
    if(analyzegeneration){
      df <- df[df$netKW <= bounds, ]
      df$netKW <- df$netKW * -1 + bounds
      #df$genKW <- df$genKW * -1
      KWtype= "Generation"
    }    
  }
  
  if(LDC){
    source("durationcurve.R")
    dc <- ldc(df, KWtype)
  }
    
  #plotting
  library(ggplot2)
  library(ggthemes)
  mdplot <- ggplot(df, aes(ggtime)) +
    labs(title=paste("DG LoadMiner\n", dayrange,  sep=""), x="Hour\n© 2014 Stoke Informatics", y="Kilowatts (kW)") +
    theme_bw() +
    theme(plot.title=element_text(face="bold"))
    
    
  if(plotuse){
    mdplot <- mdplot +
      geom_point(aes(y=useKW, color="useKW"))
  }
  
  if(plotgen){
    mdplot <- mdplot +
      geom_point(aes(y=genKW, color="genKW"))
  }

  if(plotnet){
    mdplot <- mdplot +
      geom_point(aes(y=netKW, color="netKW"))
    if(netloess){
      mdplot <- mdplot + 
        geom_smooth(aes(y=netKW), size=1.5)
    } 
    
    if(setboundary){
      library(lubridate)
#       Hs <- min(hour(df$datetime))
#       Hf <- max(hour(df$datetime))
      bounds <- boundary(df)
      mdplot <- mdplot + geom_hline(yintercept=bounds, color="black", show_guide=FALSE)
    }
    
    #sending to peakperiod.R for profile analysis
    if(profileanalysis){
      source("peakperiod.R")
      opdf <- peakperiod(df, 14, 20)
      
      #geoms to add to plot
      library(scales)
      peakstartline  <- min(opdf$ggtime)
      peakendline <- max(opdf$ggtime)
     
      mdplot <- mdplot + geom_vline(xintercept=as.numeric(peakstartline), linetype="longdash") +
        geom_vline(xintercept=as.numeric(peakendline), linetype="longdash") +
        geom_text(x=as.numeric(peakstartline), y=14, label=strftime(peakstartline, '%H:%M'), hjust=0, vjust=0) +
        geom_text(x=as.numeric(peakendline), y=14, label=strftime(peakendline, '%H:%M'), hjust=0, vjust=0)
    }
    
    
    if(maxclusters|maxloess|smoothkfs){
      library(plyr)
      maxpoints <- ddply(df, .(ggtime), summarize, maxnetKW=max(netKW))
      #maxpoints <- ddply(df, .(ggtime), subset, netKW > quantile(netKW, .9))
    }
    
    if(maxclusters){  
      mdplot <- mdplot + geom_point(data=maxpoints,aes(y=maxnetKW), color='red')
      #deepskyblue3
    }
    
    if(maxloess){
      mdplot <- mdplot + geom_smooth(data=maxpoints, aes(y=maxnetKW), size=1, alpha=0.5)                           
    }
    
    if(smoothkfs){
      source("smoothkfs.R")
      kalman <- smoothkfs(maxpoints$maxnetKW)
      maxpoints$kfs <- kalman$smooth
      maxpoints$low <- kalman$low
      maxpoints$hi <- kalman$hi
      
      if(analyzedemand){
        mdplot <- mdplot + 
          #         geom_point(data=maxpoints, aes(y=kfs), color="black", size=2) +
          geom_line(data=maxpoints, aes(y=kfs), color="black", size=.75) +
          geom_ribbon(data=maxpoints,aes(ymin=low,ymax=hi),alpha=0.4)        
      }
      
      if(analyzegeneration){
        mdplot <- mdplot + 
          geom_ribbon(data=maxpoints,aes(ymin=low,ymax=hi),alpha=0.4) +
          geom_line(data=maxpoints, aes(y=low), color="black", size=.7) +
          geom_line(data=maxpoints, aes(y=hi), color="black", size=.7) +     
          geom_line(data=maxpoints, aes(y=kfs), size=1) +
          coord_cartesian(ylim = c(0, max(maxpoints$maxnetKW) + 1))
      }
        
    }
  }
  
  #add legend
  if(plotnet){
    breaks <- "netKW" 
    labels <- "net-meter"
  }
  
  if(plotuse){
    breaks <- "useKW"
    labels <- "consumption"
  }
  
  if(plotgen){
    breaks <- "genKW"
    labels <- "generation"
  }
  
  if(plotnet & plotuse){
    breaks <- c("netKW", "useKW")
    labels <- c("net-meter", "consumption")
  }
  
  if(plotnet & plotgen){
    breaks <- c("netKW", "genKW")
    labels <- c("net-meter", "generation")
  }
  
  if(plotuse & plotgen){
    breaks <- c("useKW", "genKW")
    labels <- c("consumption", "generation")
  }
  
  if(plotuse & plotnet & plotgen){
    breaks <- c("netKW", "useKW", "genKW")
    labels <- c("net-meter", "consumption", "generation")
  }
  
  
  
  
  mdplot <- mdplot + 
#     theme_bw() + 
    scale_colour_tableau(name="Legend", breaks=breaks, labels=labels)
  #mdplot <- mdplot + scale_color_hue(name="kW Source", breaks=breaks, labels=labels, l=70, c=150)

  print(mdplot)
  
  
  
  return(df)
}