# This is a Shiny web application written by Andrea Kropp (andrea.kropp@datarobot.com)
# It can be used as a starting point for use cases where head-to-head predictions are made

library(shiny)
library(shinyWidgets)
library(DT)
library(stringr)
library(tidyverse)

### Load the predictions and explanations
### This CSV file is downloaded directly from the DataRobot UI
predictions <- read.csv("datasets/model263_withExplanations.csv", stringsAsFactors = FALSE)

### The names of the unique teams will be used to populate the drop down menus
unique_teams <- sort(unique(c(predictions$team1_name, predictions$team2_name)))

  
# This section defines the UI for application
ui <- fluidPage(

  # Sets a background image
  setBackgroundImage(src = "https://images.rawpixel.com/image_1000/czNmcy1wcml2YXRlL3Jhd3BpeGVsX2ltYWdlcy93ZWJzaXRlX2NvbnRlbnQvbHIvay1wLTg0LWJhc2tldGJhbGwuanBn.jpg"),
  
  titlePanel("Men's NCAA March Madness 2022 - Head-to-Head"),
  
  mainPanel(
    HTML("<br>"),
    
    fluidRow(
      column(6,
             selectInput("select1", label = h3("Select a team"), 
                         choices = unique_teams,
                         selected = "Kansas")
      ),
      column(6,
             selectInput("select2", label = h3("Select a team"), 
                         choices = unique_teams,
                         selected = "North Carolina")
      )
    ),
    HTML("<br>"),
    h3(uiOutput("prediction_sentance")),
    h4(uiOutput("prediction_value")),
    HTML("<br>"),
    HTML("<br>"),
    fluidRow(
      column(6,
             h3(uiOutput("positive_header")),
             DTOutput("positive_table")
             ),
      column(6,
             h3(uiOutput("negative_header")),
             DTOutput("negative_table"))
    )
  )
    
)

# Define server logic required
server <- function(input, output) {

  ### IMPORTANT NOTE: When this machine learning model was trained the data set had a column for team1 and a column for team2.
  ### When asking for predictions from the model, you get 'slightly' different predictions depending upon which team you list first and second.
  ### This is largely unavoidable due to the random processes that are involved in training tree-based models
  ### Therefore, this application actually combines two different predictions produced when the team names are reversed.
  ### This application could be much simpler if we were not trying to combine these two predictions which also requires reversing the explanations
  
  #prediction when team names are given in the same order as entered by user
  pred_original <- reactive({
    temp <- which(predictions$team1_name == input$select1 & predictions$team2_name == input$select2)
    predictions$Prediction[temp]
  })
    
  #prediction when team names are given in the reverse order as entered by user
  pred_reversed <- reactive({
    temp <- which(predictions$team1_name == input$select2 & predictions$team2_name == input$select1)
    1-predictions$Prediction[temp]
  })
  
  #average of two predictions
  pred <- reactive({
    round((pred_original() + pred_reversed())/2,3)
  })
  
  #Creates a dynamic word based on the prediction value
  win_loss_text <- reactive({
    ifelse(pred() < 0.5, "lose","win")
  })
  
  #Creates a dynamic sentence
  output$prediction_sentance <- renderUI({HTML(paste0(input$select1," will ",win_loss_text()," versus ",input$select2))})
  
  #Creates another dynamic sentence
  output$prediction_value <- renderUI({HTML(paste0("Win probability: ",pred()))})
  
  #Creates dynamic headers
  output$positive_header <- renderUI({HTML(paste0("Factors in favor of ",input$select1))})
  output$negative_header <- renderUI({HTML(paste0("Factors against ",input$select1))})
  
  #This section gathers and transfroms the explanation text to be displayed
  #It is fairly long to accomplish the 'reversing' of explanations when team1 and team2 are reversed
  explanations_df <- reactive({
    temp_original <- which(predictions$team1_name == input$select1 & predictions$team2_name == input$select2)
    temp_reversed <- which(predictions$team1_name == input$select2 & predictions$team2_name == input$select1)
    
    df <- data.frame("team1" = rep(input$select1,20),
                     "team2" = rep(input$select2,20),
                     "explanation_num" = c(seq(1:10),seq(1:10)),
                     "explanation_strength" = c(predictions$Explanation.1.Strength[temp_original],
                                                predictions$Explanation.2.Strength[temp_original],
                                                predictions$Explanation.3.Strength[temp_original],
                                                predictions$Explanation.4.Strength[temp_original],
                                                predictions$Explanation.5.Strength[temp_original],
                                                predictions$Explanation.6.Strength[temp_original],
                                                predictions$Explanation.7.Strength[temp_original],
                                                predictions$Explanation.8.Strength[temp_original],
                                                predictions$Explanation.9.Strength[temp_original],
                                                predictions$Explanation.10.Strength[temp_original],
                                                predictions$Explanation.1.Strength[temp_reversed],
                                                predictions$Explanation.2.Strength[temp_reversed],
                                                predictions$Explanation.3.Strength[temp_reversed],
                                                predictions$Explanation.4.Strength[temp_reversed],
                                                predictions$Explanation.5.Strength[temp_reversed],
                                                predictions$Explanation.6.Strength[temp_reversed],
                                                predictions$Explanation.7.Strength[temp_reversed],
                                                predictions$Explanation.8.Strength[temp_reversed],
                                                predictions$Explanation.9.Strength[temp_reversed],
                                                predictions$Explanation.10.Strength[temp_reversed]
                                                ),
                     "explanation_feature" = c(predictions$Explanation.1.Feature[temp_original],
                                               predictions$Explanation.2.Feature[temp_original],
                                               predictions$Explanation.3.Feature[temp_original],
                                               predictions$Explanation.4.Feature[temp_original],
                                               predictions$Explanation.5.Feature[temp_original],
                                               predictions$Explanation.6.Feature[temp_original],
                                               predictions$Explanation.7.Feature[temp_original],
                                               predictions$Explanation.8.Feature[temp_original],
                                               predictions$Explanation.9.Feature[temp_original],
                                               predictions$Explanation.10.Feature[temp_original],
                                               predictions$Explanation.1.Feature[temp_reversed],
                                               predictions$Explanation.2.Feature[temp_reversed],
                                               predictions$Explanation.3.Feature[temp_reversed],
                                               predictions$Explanation.4.Feature[temp_reversed],
                                               predictions$Explanation.5.Feature[temp_reversed],
                                               predictions$Explanation.6.Feature[temp_reversed],
                                               predictions$Explanation.7.Feature[temp_reversed],
                                               predictions$Explanation.8.Feature[temp_reversed],
                                               predictions$Explanation.9.Feature[temp_reversed],
                                               predictions$Explanation.10.Feature[temp_reversed]
                                              ),
                     "explanation_value" = c(predictions$Explanation.1.Value[temp_original],
                                               predictions$Explanation.2.Value[temp_original],
                                               predictions$Explanation.3.Value[temp_original],
                                               predictions$Explanation.4.Value[temp_original],
                                               predictions$Explanation.5.Value[temp_original],
                                               predictions$Explanation.6.Value[temp_original],
                                               predictions$Explanation.7.Value[temp_original],
                                               predictions$Explanation.8.Value[temp_original],
                                               predictions$Explanation.9.Value[temp_original],
                                               predictions$Explanation.10.Value[temp_original],
                                               predictions$Explanation.1.Value[temp_reversed],
                                               predictions$Explanation.2.Value[temp_reversed],
                                               predictions$Explanation.3.Value[temp_reversed],
                                               predictions$Explanation.4.Value[temp_reversed],
                                               predictions$Explanation.5.Value[temp_reversed],
                                               predictions$Explanation.6.Value[temp_reversed],
                                               predictions$Explanation.7.Value[temp_reversed],
                                               predictions$Explanation.8.Value[temp_reversed],
                                               predictions$Explanation.9.Value[temp_reversed],
                                               predictions$Explanation.10.Value[temp_reversed]
                     )
              )
    
    df$explanation <- df$explanation_feature
    df$explanation[1:10] <- gsub("team1", paste0(input$select1," "), df$explanation[1:10])
    df$explanation[1:10] <- gsub("team2", paste0(input$select2," "), df$explanation[1:10])
    df$explanation[11:20] <- gsub("team2", paste0(input$select1," "), df$explanation[11:20])
    df$explanation[11:20] <- gsub("team1", paste0(input$select2," "), df$explanation[11:20])
    
    df$explanation_strength[11:20] <- gsub("\\+\\+\\+", "positive3", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("\\+\\+", "positive2", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("\\+", "positive1", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("---", "negative3", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("--", "negative2", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("-", "negative1", df$explanation_strength[11:20])
    
    df$explanation_strength[11:20] <- gsub("positive3", "---", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("positive2", "--", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("positive1", "-", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("negative3", "+++", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("negative2", "++", df$explanation_strength[11:20])
    df$explanation_strength[11:20] <- gsub("negative1", "+", df$explanation_strength[11:20])
    
    #Tag the positive and negative factors from the perspective of team1
    df$positive <- grepl("\\+", df$explanation_strength)
    df$negative <- grepl("\\-", df$explanation_strength)
    
    #Remove single quotes
    df$explanation_value <- gsub("'","",df$explanation_value)
    
    #Truncated numeric values at 5 characters
    for (i in 1:length(df$explanation_value)) {
      if (grepl("[a-zA-Z]",df$explanation_value[i][1]) == FALSE) {
        df$explanation_value[i] <- str_extract(df$explanation_value[i],"^..?.?.?.?")
      }
    }
    
    df
  })
  
  #Subset of explanations in favor of team1
  positive_explanations <- reactive({
    temp <- subset(explanations_df(), positive == TRUE)
    temp[order(temp$explanation_num),]
  })
  
  output$positive_table <- renderDT({
    data <- positive_explanations()
    data <- data[,c(7,6)]
    data <- unique(data)
    
    datatable(data, 
              colnames = c("Feature","Value"),
              rownames= FALSE, 
              options = list(
                dom = 't',
                autoWidth = FALSE)
    )
    }) 
  
  #Subset of explanations against team1
  negative_explanations <- reactive({
    temp <- subset(explanations_df(), negative == TRUE)
    temp[order(temp$explanation_num),]
  })
  
  output$negative_table <- renderDT({
    data <- negative_explanations()
    data <- data[,c(7,6)]
    data <- unique(data)
    
    datatable(data, 
              colnames = c("Feature","Value"),
              rownames= FALSE, 
              options = list(
                dom = 't',
                autoWidth = FALSE)
    )
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
