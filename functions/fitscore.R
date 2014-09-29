#fitscore functon for computing correlation between demand and generation
fitscore <- function(df, m1=6, d1=1, m2=6, d2=30){

  dem <- multiday(df, m1, d1, m2, d2,
                  analyzedemand=T)
  gen <- multiday(df, m1, d1, m2, d2,
                  analyzegeneration=T)
  
  #create window of time intervals based on the generation period
  genwindow <- strftime(gen$datetime, "%H:%M")
  #subset demand by time intervals that are in the generation period
  dem2 <- dem[strftime(dem$datetime, "%H:%M") %in% genwindow, ]
  
#   #validation
#   all(strftime(dem2$datetime, "%H:%M") %in% strftime(gen$datetime, "%H:%M"))
  
  #create dataframes for demand and generation that contain maxnetKW per time interval
  dmax <- ddply(dem2, .(ggtime), summarize, maxnetKW=max(netKW))
  gmax <- ddply(gen, .(ggtime), summarize, maxnetKW=max(netKW))
  #cor(dmax$maxnetKW, gmax$maxnetKW)
  
  #add in kfs smoothed values
  dmax$kfs <- smoothkfs(dmax$maxnetKW)
  gmax$kfs <- smoothkfs(gmax$maxnetKW)
  
  #compute correlation on smoothed values
  fitscore <- cor(dmax$kfs, gmax$kfs)
  
  return(fitscore)

}