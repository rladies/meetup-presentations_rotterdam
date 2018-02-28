#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("shiny")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("title panel"),
  sidebarLayout(
    sidebarPanel( "sidebar panel",
                  selectInput('element_id', label = 'Select one option', choices = LETTERS[1:10])),
    mainPanel("main panel", 
              h1('The title of some text'), 
              p('And here is some content that is put into the first paragraph'))
  )
   
)

# Run the application 
shinyApp(ui = ui, server = server)

###################################################################################################################


ui <- fluidPage(
  
  # Application title
  titlePanel("title panel"),
  sidebarLayout(
    sidebarPanel("sidebar panel",
                  selectInput("element_id", label = "Select one option", choices = LETTERS[1:10])),
    mainPanel("main panel", 
              h1("The title of some text"), 
              p("And here is some content that is put into the first paragraph"),
              p(textOutput("dynamicText")))
  )
  
)

server <- function(input, output) {
  
  output$dynamicText <- renderText({
    sprintf("You selected %s", input$element_id)
    #paste("You selected", input$element_id)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


###################################################################################################################


ui <- fluidPage(
  
  # Application title
  titlePanel("title panel"),
  sidebarLayout(
    sidebarPanel("sidebar panel",
                 selectInput('element_min', label = 'Select one option', choices = c(1:100)),
                 selectInput('element_max', label = 'Select one option', choices = c(100:500))),
    mainPanel("main panel", 
              h1('The title of some text'), 
              p('And here is some content that is put into the first paragraph'),
              plotOutput("modelPlot"))
  )
  
)

server <- function(input, output) {
  
  output$modelPlot <- renderPlot({
    plot(sample(input$element_min : input$element_max))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
