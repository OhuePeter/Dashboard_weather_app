## This application will help you decide 
## whether to play golf or not

# Import libraries
library(shiny)
library(shinythemes)

#install.packages('data.table')
#install.packages('RCurl')
#install.packages('randomForest')

library(data.table)
library(RCurl)
library(randomForest)

# Read Data
weather <- read.csv("C:/Users/Peter Oseghale Ohue/Downloads/gulf_or_not.csv")
print(head(weather))

str(weather)

# Check the unique values in the 'play' column
print(unique(weather$play))

# Convert 'play' to a factor if it isn't already
weather$play <- as.factor(weather$play)
str(weather)  # Check the structure again to ensure 'play' is now a factor

# Convert 'outlook' to a factor if it isn't already
weather$outlook <- as.factor(weather$outlook)
str(weather)  # Check the structure again to ensure 'play' is now a factor

# Build Model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)
print(model)  # Print the model summary


## Build the user interface
ui <- fluidPage(theme = shinytheme("united"),
  # Page header
  headerPanel("Can I Play Golf today?"),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input the parameters</h3>"),
    selectInput('outlook', label = "Outlook:",
                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"),
                selected = "Rainy"),
  sliderInput("temperature", label = "Temperature:",
              min = 12, max = 55,
              value = 32),
  sliderInput("humidity", label = "Humidity:",
              min = 48, max = 102,
              value = 75),
  selectInput("windy", label = "Windy:",
              choices = list("yes" = "TRUE", "no" = "FALSE"),
              selected = "TRUE"),
  
  actionButton("submitbutton", "Submit", class = "btn btn-primary")
),

mainPanel(
  tags$label(h2('Status/Output')),  # Status/Output Text Box
  verbatimTextOutput("contents"),
  tableOutput("tabledata")  # Prediction result table
  )
)

### Build the sever side
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({
    # outlook, temperature, humidity, wind, play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep = ""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels =  c("overcast", "rainy", "sunny"))
    
    output <- data.frame(Prediction=predict(model, test), round(predict(model,test,type="prob"), 3))
    print(output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
  
}

################ Create the Shiny app  #

################
shinyApp(ui = ui, server = server)
