

library(shiny)
#setwd("dev/summerschoolLabex/R-workshops/Modeling/syllabus/")
source("functionsOnly.R")


ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
        sliderInput(inputId = "steps",
                    label = "Number of steps:",
                    min = 1,
                    max = 500,
                    value = 100)
      
    ,
    sliderInput(inputId = "tolerance",
                label = "tolerance",
                min = 0,
                max = 1,
                value = 0.3)
    ,
    sliderInput(inputId = "pctOccupied",
                label = "Occupation rate",
                min = 0,
                max = 1,
                value = 0.8)
    ,
    actionButton(inputId = "setup", "setup")
    ,
    actionButton(inputId = "simButton", "simulate")
    )#sidebarpanel
    ,
    # Main panel for displaying outputs ----
     mainPanel(
    # Output: Histogram ----
      plotOutput(outputId = "plot2")
    )
  )#sidebarlayout 
)

server <- function(input, output, session) {
 s <- eventReactive(input$setup, {
   s <- createMeltedSchellingState(20, input$pctOccupied, 0.5, 0.5)
  
   s
 } ) 
 
 
  dsim <- eventReactive(input$simButton, {
   simulate(input$steps,s(), input$tolerance)
   
 })
 
 
 output$plot2<-renderPlot({
   print(displayStateUnhappy(dsim(), dotsize = 3))
 })
 
}

shinyApp(ui, server)
