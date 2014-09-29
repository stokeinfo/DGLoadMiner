peakperiod <- function(df, peakstart, peakend, comparedemand=FALSE, comparegeneration=FALSE){
  
  #PeakStart (hh:mm) - PeakFinish (hh:mm)
  # PeakStart and PeakFinish should be chosen such that each fulfills 
  #    the following criteria:
  #    PeakStart and PeakFinish minutes must be 00 or a multiple of mInterval minutes
  #     i.e., if mInterval is 5 PeakStart minutes may be :00, :05, :10, etc
  #     if the minutes and mInterval are out of alignment, data collected will 
  #     be incomplete and functions below may squawk or die
  #data should be filtered/processed data (same data that multiday uses)
  #mInterval comes from DGDMultiday.R
  
  
  #subtract 1 from PeakFinish to grab data up to the finish hour, but not including...
  #replaced code below by subsequent code block
  #peakWindow <- sprintf("%02d", c(PeakStart:(PeakFinish-1)))
  
  #the 5 in the sprintf() should be replaced with mInterval when this gets integrated
  
  peakhours <- sprintf('%02d', c(peakstart:(peakend-1)))
  
  #create onpeak subset data
  opdf <- df[strftime(df$datetime, '%H') %in% peakhours, ]
    
    
  #total observatons within the data from multiday.R
  totalObs <- nrow(df)  
  
  #total observations from the onpeak dataframe
  opObs <- nrow(opdf)
  
  #percent of data from multiday.R that is within the peak period
  peakdataratio <- (opObs/totalObs)*100
  
  
  #coincidence factor. nonPeakSumKW is sum of all demands within the resource window, 
  #not within the fully 24 hour period, which should it be?
  opsum <- sum(opdf$netKW)
  npsum <- sum(df$netKW)
  peakcoin <- opsum/npsum
  

  
  #maximum netKW of all the data and opdf
  maxnet <- max(df$netKW)
  #when does maximum kW happen?
  maxtime <- df[df$netKW == max(df$netKW), "datetime"]
  
  maxtimestring <- paste("Maximum kW of ", round(maxnet, 2), " occurs on ", strftime(maxtime, '%A'), ", ", strftime(maxtime, '%b-%d'), " at ", strftime(maxtime, '%H:%M'), sep='')
  
  
  #switches for computing peak coincidence and load factor of use and gen for comparison
  if(comparedemand){  
    opsumuse <- sum(opdf$useKW)
    npsumuse <- sum(df$useKW)
    peakcoinuse <- opsumuse/npsumuse
  }
  
  if(comparegeneration){
    opsumgen <- sum(opdf$genKW)
    npsumgen <- sum(df$genKW)
    peakcoingen <- opsumgen/npsumgen
  }
  
  
  #load factor. Same issue as with coincidence factor above, nonPeakSumKW is sum of all demands within the resource window, 
  #not within the fully 24 hour period, which should it be? 
  theoreticalmax <- maxnet*totalObs
  loadfactor <- npsum/theoreticalmax
  
  #comparisons
  if(comparedemand){
    theoreticalmaxuse <- max(df$useKW)*totalObs
    loadfactoruse <- npsumuse/theoreticalmaxuse
  }
  if(comparegeneration){
    theoreticalmaxgen <- max(df$genKW)*totalObs
    loadfactorgen <- npsumgen/theoreticalmaxgen
  }
  
  print("PROFILE ANALYSIS")
  print(paste("Peak Period Data Ratio =", paste(round(peakdataratio, 1),"%", sep='')))
  print(maxtimestring)
  print(paste("Peak-Coincidence =", sprintf("%.2f", round(peakcoin,2))))
  print(paste("Load-Factor =", sprintf("%.2f", round(loadfactor,2))))

  
    
    
    
  return(opdf)   
}
