boundary <- function(df){  
  #AM tail subset
  #amrange <- sprintf("%02d", c(Hs:(Hs+1)))
  #just use the first and last hour instead of a larger window.
  
  Hs <- min(hour(df$datetime))
  Hf <- max(hour(df$datetime))
  
  amrange <- sprintf("%02d", Hs)
  
  amdf <- df[strftime(df$datetime, "%H") %in% amrange, ]
  
  #now with DST, min is being set too low because in Boulder the sun comes out later due to the mountains. 
  #for time being only focus in on amdf to set the min. 
  #Some facilities' SR/SS will not always coincide with the true SR/SS due to system characteristics
  #tilt/azimuth, shading due to trees, other buildings, MOUNTAINS...
  #SO perhaps need to make the SR/SS specific to the facility by setting the boundaries both horizontally and vertically.
  
  #PM tail subset
  #pmrange <- sprintf("%02d", c((Hf-1):Hf))
  pmrange <- sprintf("%02d", Hf)
  
  pmdf <- df[strftime(df$datetime, "%H") %in% pmrange, ]
  
    
  boundary <- min(c(amdf$netKW, pmdf$netKW))
#   boundary <- min(amdf$netKW)
  
  #netKWrange <- range(sunrisetail$netKW, sunsettail$netKW)
  
  #print(min(netKWrange))
  
  return(boundary)
  
}
