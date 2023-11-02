#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readxl)
library(lattice)
library(tidyverse)
accident <- read_excel("accident.xlsx")
days <- read_excel("weekdays.xlsx")
Monday <- accident%>%filter(DAY_WEEKNAME == 'Monday')
Tuesday <- accident%>%filter(DAY_WEEKNAME == 'Tuesday')
Wednesday <- accident%>%filter(DAY_WEEKNAME == 'Wednesday')
Thursday <- accident%>%filter(DAY_WEEKNAME == 'Thursday')
Friday <- accident%>%filter(DAY_WEEKNAME == 'Friday')
Saturday <- accident%>%filter(DAY_WEEKNAME == 'Saturday')
Sunday <- accident%>%filter(DAY_WEEKNAME == 'Sunday')
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Current Time Output
  h2(textOutput("time")),

  #Output the Questionnaire for understanding time table
  radioButtons("radio", label = h3("Which Hour is it from 0-23?"), 
               choices = list("Hour 0" = 0, "Hour 1" = 1, "Hour 2" = 2, 
                              "Hour 3" = 3, "Hour 4" = 4,"Hour 5" = 5,
                              "Hour 6" = 6, "Hour 7" = 7, "Hour 8" = 8,
                              "Hour 9" = 9, "Hour 10" = 10, "Hour 11" = 11,
                              "Hour 12" = 12, "Hour 13" = 13, "Hour 14" = 14,
                              "Hour 15" = 15, "Hour 16" = 16, "Hour 17" = 17,
                              "Hour 18" = 18, "Hour 19" = 19, "Hour 20" = 20,
                              "Hour 21" = 21, "Hour 22" = 22, "Hour 23" = 23), selected = 0),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  # Application title
  titlePanel("Car Accidents Throughout the Day"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(selectInput("weekday", "Select a Day of the Week!", choices = colnames(days), selected = "Monday")),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("daytimePlot"),
    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
  currenthour3 <- Sys.time()
  currenthour2 <- as.POSIXct(currenthour3, "America/Chicago")
  currenthour1 <- format(currenthour2, "%k")
  currenthour <- as.numeric(unlist(currenthour1))
  # You can access the values of the widget (as a vector)
  # with input$radio, e.g.
  output$value <- renderPrint(
    if(input$radio == currenthour){print({ "Correct!" })
    }
    else{print("Almost! Try again.")
    })
  
  output$time <- renderText({
    invalidateLater(1000, session)
    s <- Sys.time()
    current <- as.POSIXct(s, "America/Chicago")
    time_string <- format(current, "%H:%M:%S")
    timetime <- format(strptime(time_string, "%H:%M:%S"), "%I:%M:%S %p") 
    paste("Today's weekday is ", format(current, "%A"), "and the time at South Dakota State University is", timetime, ".")
  })
  
  output$daytimePlot <- renderPlot({
    
  if(input$weekday == "Monday"){
    # generate bins based on input$bins from ui.R
    x    <- Monday$HOURNUMERIC
    
    # draw the histogram with the specified number of bins
    hist(x, col = rainbow(15), border = 'white',
         xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
         main = 'Histogram of Accident Times',
         ylab = 'How many accidents',
         breaks = 24)
  }
    
  else if(input$weekday == "Tuesday"){
      # generate bins based on input$bins from ui.R
      x    <- Tuesday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }

  else if(input$weekday == "Wednesday"){
      # generate bins based on input$bins from ui.R
      x    <- Wednesday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }    

  else if(input$weekday == "Thursday"){
      # generate bins based on input$bins from ui.R
      x    <- Thursday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }
        
  else if(input$weekday == "Friday"){
      # generate bins based on input$bins from ui.R
      x    <- Friday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }
        
  else if(input$weekday == "Saturday"){
      # generate bins based on input$bins from ui.R
      x    <- Saturday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }
  
  else if(input$weekday == "Sunday"){
      # generate bins based on input$bins from ui.R
      x    <- Sunday$HOURNUMERIC
      
      # draw the histogram with the specified number of bins
      hist(x, col = rainbow(15), border = 'white',
           xlab = 'Which Hour an Accident Occured from 0(12:00 am) to 23(11:00 pm)',
           main = 'Histogram of Accident Times',
           ylab = 'How many accidents',
           breaks = 24)
    }    
  })

}


# Run the application 
shinyApp(ui = ui, server = server)
