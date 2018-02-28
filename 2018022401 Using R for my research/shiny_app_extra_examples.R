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
   titlePanel("Testing simulated data set"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ,
    
       sliderInput("size",
                  "Number of observations:",
                  min = 1,
                  max = 300,
                  value = 10)
     ),
   
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("distPlot"), plotOutput("boxPlot")), 
          tabPanel("Summay", verbatimTextOutput("summary")), 
          tabPanel("Table", tableOutput("table")))

      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- sample(1:100, 100, replace = TRUE) 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$boxPlot <- renderPlot({
     # generate size based on input$size from ui.R
     x <- sample(1:100, input$size, replace = TRUE)
     
     # draw the boxplot with the specified size
     boxplot(x)
   })
   
   output$table <- renderTable({
     # generate size based on input$size from ui.R
     x <- sample(1:100, input$size, replace = TRUE)
     
     # present the numbers 
     cbind(1:input$size, x)
   })
   
   output$summary <- renderText({
     "We show an example of simulations."
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


######################################################################################
######################################################################################
install.packages("lattice")
install.packages("JM")
library(lattice)
library(JM)

# Define UI for prediction 
ui <- fluidPage(
  
  # Application title
  titlePanel("Prediction model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      
      sidebarPanel(
        selectInput("Model", "Model:", 
                    choices = c("All", "Spiders")),
        helpText("Data from R: pbc2.")
      ),
      
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Patient data", tableOutput("table")))
      
    )
  )
)



server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    fm <- lm(serBilir ~ drug * age + sex * age + spiders, data = pbc2.id)
    fmSign <- lm(serBilir ~ age + spiders, data = pbc2.id)
    
    newdata <- with(pbc2.id, data.frame(
      drug = gl(2,200,labels = c("placebo","D-penicil")),
      age = rep(seq(min(pbc2.id$age), max(pbc2.id$age), length = 50), times = 4),
      sex = gl(2,100,labels = c("male","female")),
      spiders = gl(2,50,labels = c("No spider","Yes spider"))
      
    ))
    
    newdataSign <- with(pbc2.id, data.frame(
      age = rep(seq(min(pbc2.id$age), max(pbc2.id$age), length = 50), times = 2),
      spiders = gl(2,50,labels = c("No spider","Yes spider"))
      
    ))
    
    X <- model.matrix(~ drug * age + sex * age + spiders, data = newdata)
    XSign <- model.matrix(~ age + spiders, data = newdataSign)
    
    betas <- summary(fm)$coefficients[, 1]
    newdata$pred <- c(X %*% betas)
    Vbetas <- vcov(fm)
    se.pred <- sqrt(diag(X %*% Vbetas %*% t(X)))
    newdata$lo <- newdata$pred - 1.96 * se.pred
    newdata$up <- newdata$pred + 1.96 * se.pred
    
    betas <- summary(fmSign)$coefficients[, 1]
    newdataSign$predSign <- c(XSign %*% betas)
    Vbetas <- vcov(fmSign)
    se.predSign <- sqrt(diag(XSign %*% Vbetas %*% t(XSign)))
    newdataSign$loSign <- newdataSign$predSign - 1.96 * se.predSign
    newdataSign$upSign <- newdataSign$predSign + 1.96 * se.predSign
    
      
    plots.names <- c("withoutInteraction", "withInteraction")
    plots <- as.list(rep(NA, length(plots.names)))
    plots[["All"]] <- xyplot(pred + lo + up ~ age | drug * sex * spiders, type = "l", lty = c(1, 2, 2), 
                                            cex = 1.8,  col = 1, lwd = 2, layout = c(4,2),
           main = "Effect plot", xlab = "age", ylab = "serBilir", data = newdata)
    plots[["Spiders"]] <- xyplot(predSign + loSign + upSign ~ age | spiders, type = "l", lty = c(1, 2, 2), cex = 1.8,  col = 1, lwd = 2, 
                                          main = "Effect plot", xlab = "age", ylab = "serBilir", data = newdataSign)

    plots[[input$Model]]
  })
  
  
  output$table <- renderTable({
    
    fm <- lm(serBilir ~ drug * age + sex * age + spiders, data = pbc2.id)
    fmSign <- lm(serBilir ~ age + spiders, data = pbc2.id)
    
    newdata <- with(pbc2.id, data.frame(
      drug = gl(2,200,labels = c("placebo","D-penicil")),
      age = rep(seq(min(pbc2.id$age), max(pbc2.id$age), length = 50), times = 4),
      sex = gl(2,100,labels = c("male","female")),
      spiders = gl(2,50,labels = c("No spider","Yes spider"))
      
    ))
    
    newdataSign <- with(pbc2.id, data.frame(
      age = rep(seq(min(pbc2.id$age), max(pbc2.id$age), length = 50), times = 2),
      spiders = gl(2,50,labels = c("No spider","Yes spider"))
      
    ))
    
    
     if (input$Model == "All") {
       return(newdata)
     } else if (input$Model == "Spiders") {
       return(newdataSign)
     }
  })
  
  output$summary <- renderPrint({
    
    fm <- lm(serBilir ~ drug * age + sex * age + spiders, data = pbc2.id)
    fmSign <- lm(serBilir ~ age + spiders, data = pbc2.id)
    
    
    if (input$Model == "All") {
      return(summary(fm))
    } else if (input$Model == "Spiders") {
      return(summary(fmSign))
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

######################################################################################
######################################################################################

# Define UI for prediction 
ui <- fluidPage(
  
  # Application title
  titlePanel("Prediction model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("BMI",
                  "BMI:",
                  min = 20,
                  max = 33,
                  value = 20, animate=TRUE)
    
 
    ,
      
    radioButtons("gender", "Gender:",
                 c("Male" = "male",
                   "Female" = "female")
    )),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("dataPlot"), plotOutput("modelPlot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Patient data", tableOutput("table")))
      
    )
  )
)



server <- function(input, output) {
  
  
  output$dataPlot <- renderPlot({
    
    set.seed(2015+5)
    patient <- c(1:200)
    height <- rnorm(200, 1.70, 0.1)
    weight <- height + rnorm(200, 60, 10)
    age <- rnorm(200, 60, 10)
    sex <- sample(1:2, 200, replace = T)
    sex <- factor(sex, levels = 1:2, labels = c("male", "female"))
    
    dat <- data.frame(patient, height, weight, sex, age)
    dat$BMI <- dat$weight/(dat$height^2)
    
    dat_subset <- reactive({
      a <- subset(dat, sex == input$gender)
      return(a)
    })
    
    return(xyplot(weight ~ age, type = "smooth", data = dat_subset(), ylim = c(30, 90), xlim = c(30, 90),
                  main = list(label = "Average evolution of weight", cex = 1.5), 
                  col = 2, lwd = 2, cex.lab = 2, ylab = list(label = "Weight", cex = 1.5), 
                  xlab = list(label = "Age", cex = 1.5)))
  })
  
  
  output$modelPlot <- renderPlot({
    
    set.seed(2015+5)
    patient <- c(1:200)
    height <- rnorm(200, 1.70, 0.1)
    weight <- height + rnorm(200, 60, 10)
    age <- rnorm(200, 60, 10)
    sex <- sample(1:2, 200, replace = T)
    sex <- factor(sex, levels = 1:2, labels = c("male", "female"))
    
    dat <- data.frame(patient, height, weight, sex, age)
    dat$BMI <- dat$weight/(dat$height^2)
    
    fm1 <- lm(weight ~ sex + BMI + age, data = dat)
    

      newdata <- with(dat, data.frame(
        sex = rep(1*I(input$gender == "male") + 2*I(input$gender == "female"), each = 40),
        age = rep(seq(min(dat$age), max(dat$age), length = 20), each = 2),
        BMI = rep(input$BMI, 20)
      ))
      
      X <- model.matrix(~ sex + BMI + age, data = newdata)
      
      betas <- summary(fm1)$coefficients[, 1]
      newdata$pred <- c(X %*% betas)
      Vbetas <- vcov(fm1)
      se.pred <- sqrt(diag(X %*% Vbetas %*% t(X)))
      newdata$lo <- newdata$pred - 1.96 * se.pred
      newdata$up <- newdata$pred + 1.96 * se.pred
      
      newdata$sex <- factor(newdata$sex, levels = c(1,2), labels = c("male","female"))
      return(xyplot(pred + lo + up ~ age, type = "l", lty = c(1, 2, 2), cex = 1.8, ylim = c(50, 88), xlim = c(30, 90),
                               col = 1, lwd = 2, main = list(label = "Effect plot", cex = 1.5), xlab = list("Age", cex = 1.5), 
                               ylab = list(label = "Weight", cex = 1.5), data = newdata, cex.lab = 2))
      
  
  })
  

  
  output$table <- renderTable({
      set.seed(2015+5)
      patient <- c(1:200)
      height <- rnorm(200, 1.70, 0.1)
      weight <- height + rnorm(200, 60, 10)
      age <- rnorm(200, 60, 10)
      sex <- sample(1:2, 200, replace = T)
      sex <- factor(sex, levels = 1:2, labels = c("male", "female"))
    
      dat <- data.frame(patient, height, weight, sex, age)
      dat$BMI <- dat$weight/(dat$height^2)
    
      fm1 <- lm(weight ~ sex + BMI + age, data = dat)
    
      newdata <- with(dat, data.frame(
        sex = rep(1*I(input$gender == "male") + 2*I(input$gender == "female"), each = 40),
        age = rep(seq(min(dat$age), max(dat$age), length = 20), each = 2),
        BMI = rep(input$BMI, 20)
        
      ))
      
      X <- model.matrix(~ sex + BMI + age, data = newdata)
      
      betas <- summary(fm1)$coefficients[, 1]
      newdata$pred <- c(X %*% betas)
      Vbetas <- vcov(fm1)
      se.pred <- sqrt(diag(X %*% Vbetas %*% t(X)))
      newdata$lo <- newdata$pred - 1.96 * se.pred
      newdata$up <- newdata$pred + 1.96 * se.pred
      
      newdata$sex <- factor(newdata$sex, levels = c(1,2), labels = c("male","female"))
      
      
      return(newdata)
    
  })
  
  output$summary <- renderPrint({
      set.seed(2015+5)
      patient <- c(1:200)
      height <- rnorm(200, 1.70, 0.1)
      weight <- height + rnorm(200, 60, 10)
      age <- rnorm(200, 60, 10)
      sex <- sample(1:2, 200, replace = T)
      sex <- factor(sex, levels = 1:2, labels = c("male", "female"))
    
      dat <- data.frame(patient, height, weight, sex, age)
      dat$BMI <- dat$weight/(dat$height^2)
    
      fm1 <- lm(weight ~ sex + BMI + age, data = dat)
    
      return(summary(fm1))
  
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

