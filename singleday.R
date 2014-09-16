singleday <- function(df, month, day, plotnet=TRUE, 
                      plotuse=FALSE, plotgen=FALSE, 
                      analyzedemand=FALSE,
                      analyzegeneration=FALSE){
  
  #day character variable to send to subset below
  
  day <- paste(sprintf('%02d', month),"-", sprintf('%02d', day), sep="")
  
  daydf <- df[strftime(df$datetime, "%m-%d") %in% day, ]
  
  #add in times formatted properly for ggplot xaxis.
  daydf$ggtime <- strptime(daydf$time, "%H:%M")
  daydf$ggtime <- as.POSIXct(daydf$ggtime)
  
  #title
  title <- paste("DG LoadMiner\n",day, "-2013", sep="")
  
  if(analyzedemand){
    daydf <- daydf[daydf$netKW > 0, ]
    title <- paste("DG LoadMiner - Demand Net kW\n",day, "-2013", sep="")
  } 
  if(analyzegeneration){
    daydf <- daydf[daydf$netKW < 0, ]
    title <- paste("DG LoadMiner - Generation Net kW\n",day, "-2013", sep="")
  } 
  
  #plot
  library(ggplot2)
  plot <- ggplot(daydf, aes(ggtime)) +
    labs(title=title, x="© 2014 Stoke Informatics, LLC", y="Kilowatts (kW)") +
    theme_bw() +
    theme(plot.title=element_text(face="bold"))
  
  if(plotnet){
    plot <- plot + 
      geom_line(aes(y=netKW, color="netKW")) +
      geom_abline(intercept=0, slope=0, color="red") +
      geom_smooth(aes(y=netKW))
    if(analyzedemand|analyzegeneration){
      plot <- plot + 
        geom_point(aes(y=netKW))
    }
  }
    
  if(plotuse){
    plot <- plot + 
      geom_line(aes(y=useKW, color="useKW")) 
  }
  
  if(plotgen){
    plot <- plot + 
      geom_line(aes(y=genKW, color="genKW"))
  }
  
  #save for making title programmatic later...
#   #adding title
#   if(plotnet){
#     kwtype <- "Net-meter"
#   }
  
  
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

  plot <- plot + scale_color_hue(name="kW Source", breaks=breaks, labels=labels, l=70, c=150)

  print(plot)
  
  return(daydf)
}