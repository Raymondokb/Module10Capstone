#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  h3("Raymond: Coursera Module 10 Capstone"),
  # Application title
  titlePanel("Barebones Word Prediction using Katz Backoff Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       # sliderInput("bins",
       #             "Number of bins:",
       #             min = 1,
       #             max = 50,
       #             value = 30),
      #actionButton("go", "Go"),
       textInput("caption", "Caption", "")
       #actionButton("button", "An action button")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
       #plotOutput("distPlot"),
      h6("Please wait for 0.5 minutes for the program to start"),
      h6("Wait 1-3 seconds for intermittance"),
      h4("Input with predicted next word"),
       verbatimTextOutput("value"),
       
       h2(" "),
       h3("Statistics of words probability"),
       dataTableOutput('mytable')
    )
  )
))
