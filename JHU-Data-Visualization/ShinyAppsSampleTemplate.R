#Load Package to run Shiny
library(shiny)

ui = fluidPage(

#Inputs - Each is separated with a comma!
  
  # Application title
  titlePanel("Hi :) - This is the Page Title"),
  textInput("name", "What's your name?"), #"name" is the label this input object will be given. Use it in the output below :)
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  
  #Each Input has to have a corresponding Output (so that it can display in the UI)
  textOutput("greeting"), #Goes with TextInput
  verbatimTextOutput("summary"), #Here we'll show the output verbatim as R would show when running Summary function
  tableOutput("table"), #Here we'll show the dataset...just the raw table itself
  
      #Example Input
      sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
      
      #Corresponding Output placed in the UI
      "then x times 5 is",
      textOutput("product"),
  
  # A plot 
    plotOutput("MyPlot")
  )

server <- function(input, output, session) {
 
#Specify the dataset to use reactively -- based on user's choice. Avoids duplication in code below :)
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })

#Outputs -- No need to separate by commas
  
  #Creates the actual output for greeting specified in UI
   output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
  
  #Creates actual output for summary specified in UI
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  #Creates actual output for table specified in UI
  output$table <- renderTable({
    dataset()
  })
  
  #Creates actual output for product specified in UI
  output$product <- renderText({
    input$x*5 #"Input$" specifies that the value needs to come from an Input specified above called "x"
    })
  
  #Creates actual output for plot specified in UI
  output$MyPlot <- renderPlot(
    #Insert code for plot here
      #Example 1
      #library(tidyverse)
      #ggplot...
    
     #Example 2 - Plot a line from 0 to whatever x value is selected in input
     plot(0:input$x), res = 96
     #FYI: res = 96 overrides Shiny defaults and makes plots match we we see in RStudio as closely as possible
  )
}

shinyApp(ui=ui, server = server) #Makes the App

#Deploy App to Shiny.io website -- Or use Publish icon
  #Load Package to connect with ShinyApp.io
  #library(rsconnect)
  
  #rsconnect::deployApp('/Users/ahmedkhan/Downloads/Week10_ShinyApp')
