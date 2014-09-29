#dgloadminer shiny app ui.R

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(list("DG LoadMiner",
                  br(),
                  h3("Mining demand and generation profiles of rooftop solar customers from net-metered data"))
             ),  
  
  tabsetPanel(
    tabPanel("Summary",                 
            p("DG LoadMiner is an attempt to use advanced analytics to help utilities 
              effectively manage large market penetrations of prosumers (grid-connected 
              solar electric customers) by mining out the behind-the-meter demand and 
              generation profiles from only net-meter interval data."),
            p("Electric utilities meter retail solar customers on a net basis, which means
              the energy meters just record the net difference between the customer's
              demand and generation. Then at the end of the month the customer either owes
              the utility money or gets a credit on their bill depending on whether they 
              were a net consumer or producer. It offers a simple arrangement for billing &
              compensating rooftop solar customers, however the utility cannot see the real 
              demand or generation profiles."),
            p("DG LoadMiner is able to reconstruct the behind-the-meter profiles by identifying
              the boundary between demand and generation in net-meter data, then aggregating
              the appropriate data points. The resulting profile data could then be used in 
              resource planning and forecasting, customer segmentation, and analysis of avoided
              and incremental costs."),
            p("This application demonstrates abilities in:"),
            strong("-working with time-series data"),br(),
            strong("-plotting in ggplot2"),br(),
            strong("-trend analysis"),br(),
            strong("-creating analytic data")),
    tabPanel("Raw Net-Meter Data",
             p("The plot below shows one year's worth of raw net-meter data. It's messy and 
               highly fragmented due to the combined variable and intermittent nature of both demand 
               and generation that's hidden within the net-meter data."),
             plotOutput("yeardata"),
             p("For the sake of context, we can look at data from just a single day to get
               a better feel for the interaction between demand and generation within the net-profile"),
             p("Use the interface below to inspect consumption, generation, and net data for a single day."),
             fluidRow(
               column(3,
                      dateInput("date", 
                                label = h3("Pick a date in 2013"), 
                                value = "2013-07-03", 
                                min = "2013-01-01",
                                max = "2013-12-31")),
               column(3,
                      h3("Select the desired load"),
                      checkboxInput("con", label = "Consumption", value=TRUE),
                      checkboxInput("gen", label = "Generation", value=FALSE),
                      checkboxInput("net", label = "Net-Meter", value=FALSE))
            ),
             br(),
             plotOutput("singleday")),    
    tabPanel("Profile Analysis")
  )      
))