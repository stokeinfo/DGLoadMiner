#ldc version 2
ldc <- function(df, KWtype){
  
  ####Code block for doing LDC with smoothed kfs values###
#   library(plyr)
#   maxpoints <- ddply(df, .(ggtime), summarize, maxnetKW=max(netKW))
#   maxpoints$kfs <- smoothkfs(maxpoints$maxnetKW)
#   maxpoints <- maxpoints[order(-maxpoints$kfs), ]
#   maxpoints$index <- 1:nrow(maxpoints)
#   maxpoints$utilization <- 100*maxpoints$index/nrow(maxpoints)
#   
#   dayrange <- strftime(range(df$datetime), '%m-%d-%Y')
#   dayrange <- paste(dayrange[1],"to", dayrange[2])
#   
#   ldplot <- ggplot(maxpoints, aes(utilization, kfs)) + 
#     geom_point(color="orange") + 
#     geom_line(color="orange") +
#     labs(title=paste("DG LoadMiner - ", KWtype, " Load Duration Curve", "\n", dayrange,  sep=""), x="Percent of Hours\n© 2014 Stoke Informatics", y="Kilowatts (kW)") +
#     theme(plot.title=element_text(face="bold")) +
#     scale_colour_tableau()
  
  
  df <- df[order(-df$netKW), ]
  df$index <- 1:nrow(df)
  df$utilization <- 100*df$index/nrow(df)
  
  dayrange <- strftime(range(df$datetime), '%m-%d-%Y')
  dayrange <- paste(dayrange[1],"to", dayrange[2])
  
  ldplot <- ggplot(df, aes(utilization, netKW)) + 
    #geom_point(color="orange") + 
    geom_line(color="orange", size=1.25) +
    theme_bw() +
    labs(title=paste("DG LoadMiner - ", KWtype, " Load Duration Curve", "\n", dayrange,  sep=""), x="Percent of Hours\n© 2014 Stoke Informatics", y="Kilowatts (kW)") +
    theme(plot.title=element_text(face="bold")) +
    scale_colour_tableau()

  
  print(ldplot)
  
  return(df)
  
}