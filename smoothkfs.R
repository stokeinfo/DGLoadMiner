smoothkfs <- function(vector){
  library(KFAS) 
  
  fit_maxNET<-fitSSM(inits=c(0, 0), model = structSSM(y = vector))
  
  kfsmaxNET<-KFS(fit_maxNET$model,smoothing="state")
  
  smooth <- unlist(kfsmaxNET$alphahat)
  smooth <- c(t(smooth))
  
  low <- c(kfsmaxNET$alphahat - sqrt(c(kfsmaxNET$V))) 
  hi <- c(kfsmaxNET$alphahat + sqrt(c(kfsmaxNET$V))) 
  
  results <- list(smooth = smooth, low = low, hi = hi)
  
  return(results)
}


# # Confidence intervals 
# lows<-c(kfsmaxNET$alphahat- sqrt(c(kfsmaxNET $V))) 
# ups<-c(kfsmaxNET$alphahat+ sqrt(c(kfsmaxNET $V))) 
# lines(x,lows,col="green") 
# lines(x,ups,col="green")
# 
# #replicating in ggplot
# library(ggplot2)
# b <- ggplot(data, aes(hms, maxNetKW))
# b + geom_point() 
# 
# #add in kfs smoother. First convert kfsmaxNET to dataframe...
# smooth <- unlist(kfsmaxNET$alphahat)
# smooth <- t(smooth)
# dim(smooth)
# 
# smooth <- data.frame(smooth)
# colnames(smooth) <- "kfs"
# data <- cbind(data, smooth)
# 
# #with smoother
# c <- ggplot(data, aes(hms, maxNetKW))
# c + geom_point() + 
#   geom_line(aes(y=kfs), color="orange") + 
#   labs(title="Net kW Demand\nJuly 2013", x="© 2014 Stoke Informatics, LLC", y="Net kW Demand") +
#   theme(plot.title=element_text(face="bold"))
