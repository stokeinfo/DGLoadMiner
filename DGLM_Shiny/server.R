#dg loadminer shiny app server.R script

#load necessary libraries
library(ggplot2)
library(plyr)

#read in data
load("data/pr.Rdata")
attach(pr)

shinyServer(function(input, output) {
  
  output$yeardata <- renderPlot({ 
    
    rawplot <- ggplot(pr, aes(datetime, netKW)) +
      geom_point(aes(color=month)) +
      labs(title="2013 Daily Metered Net Kw",
           x=paste("Hours: 04:00-21:00, Interval=15 minutes \n 2014 Stoke Informatics"),
           y="Net kW",
           color="Month") +
      theme_bw() +
      theme(plot.title=element_text(face="bold"))
    
    print(rawplot)
  })

  #singleday plot for context
  output$singleday <- renderPlot({
    day <- substr(input$date, 6, 10)
#     day
#     paste("You selected", as.character(day))
    
    #subset pr to create df of single day's data
    sd <- pr[strftime(pr$datetime, "%m-%d") %in% day, ]
    #add in times formatted for ggplot xaxis
    sd$ggtime <- strptime(sd$time, "%H:%M")
    sd$ggtime <- as.POSIXct(sd$ggtime)

    #create title 
    title <- paste("DG LoadMiner\n", day, "-2013", sep="")
    
    #now plot...
    sdplot <- ggplot(sd, aes(ggtime)) +
      labs(title=title, x="Time (HH:MM)", y="Kilowatts (kW)") +
      theme_bw() +
      theme(plot.title=element_text(face="bold"))

    #switches for plotting different loads
    if(input$con){
      sdplot <- sdplot +
        geom_line(aes(y=useKW, color="useKW")) +
        geom_abline(intercept=0, slope=0, color="red")        
    }

    if(input$gen){
      sdplot <- sdplot +
        geom_line(aes(y=genKW, color="genKW"))
    }
      
    if(input$net){
      sdplot <- sdplot +
        geom_line(aes(y=netKW, color="netKW"))
    }

    #add legend
    if(input$net){
      breaks <- "netKW" 
      labels <- "Net-Meter"
    }
    
    if(input$con){
      breaks <- "useKW"
      labels <- "Consumption"
    }
    
    if(input$gen){
      breaks <- "genKW"
      labels <- "Generation"
    }
    
    if(input$net & input$con){
      breaks <- c("netKW", "useKW")
      labels <- c("Net-Meter", "Consumption")
    }
    
    if(input$net & input$gen){
      breaks <- c("netKW", "genKW")
      labels <- c("Net-Meter", "Generation")
    }
    
    if(input$con & input$gen){
      breaks <- c("useKW", "genKW")
      labels <- c("Consumption", "Generation")
    }
    
    if(input$con & input$net & input$gen){
      breaks <- c("netKW", "useKW", "genKW")
      labels <- c("Net-Meter", "Consumption", "Generation")
    }
    
    sdplot <- sdplot + scale_color_hue(name="kW Source", breaks=breaks, labels=labels, l=70, c=150)
  
    print(sdplot)
    
  })
  
}
)