#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sqldf)

nfl_matchups <- read_rds("matchups.rds")
nba_matchups <- read_rds("nba_matchups.rds")

### Accuracy
nfl_acc <- read_rds("nfl_acc.rds")
nba_acc <- read_rds("nba_acc.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Predictors",

    tabPanel("NFL Model",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "home",
                                 label = "Select Home Team:",
                                 choices = sort(unique(nfl_matchups$home))),
                     selectInput(inputId = "away",
                                 label = "Select Visiting Team:",
                                 choices = sort(unique(nfl_matchups$visitor))),
                     textInput(inputId = "odds_ff",
                               label = "Odds for Home Team:",
                               value = "0")),
                 mainPanel(
                     textOutput("nfl_match"),
                     tableOutput("nfl_matches"),
                     textOutput("nfl_setup")
                 ))),
    tabPanel("NBA Model",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "nba_home",
                                 label = "Select Home Team:",
                                 choices = sort(unique(nba_matchups$home))),
                     selectInput(inputId = "nba_away",
                                 label = "Select Visiting Team:",
                                 choices = sort(unique(nba_matchups$visitor))),
                     textInput(inputId = "nba_odds_ff",
                               label = "Odds for Home Team:",
                               value = "0")),
                 mainPanel(
                     textOutput("nba_match"),
                     tableOutput("nba_matches"),
                     textOutput("nba_setup")
                 ))),
    tabPanel("About",
             mainPanel(h1("About the Model"),
                       "This app contains models to predict outcomes for various sport matches. The accuracies for each model can be seen below:
                       ",
                       textOutput("nfl_acc"),
                       textOutput("nba_acc"))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$nfl_match <- renderText({
        paste("You have selected the", input$home, "versus the", input$away, sep = " ")
    })
    output$nfl_setup <- renderText({
        odd <- as.numeric(input$odds_ff)
        odd2 <- ifelse(odd < 0, -1*odd/(-1*odd + 100), 100/(100+odd))
        imp_prob <- round(odd2*100, digits = 2)
        prediction <- sqldf(paste("SELECT avg_estimate from nfl_matchups WHERE home = '",
                    input$home, "' AND visitor = '", input$away, "'", sep = ""))
        prediction <- round(prediction, digits = 3)*100
        paste("The ", input$home, " have a ", prediction, " percent chance of beating the ", input$away,
              ". With odds of ", odd, ", the implied probability of ", input$home, " winning is ",
              imp_prob, " percent.", sep = "")
    })
    
    ## NBA
    output$nba_match <- renderText({
        paste("You have selected the", input$nba_home, "versus the", 
              input$nba_away, sep = " ")
    })
    output$nba_setup <- renderText({
        odd <- as.numeric(input$nba_odds_ff)
        odd2 <- ifelse(odd < 0, -1*odd/(-1*odd + 100), 100/(100+odd))
        imp_prob <- round(odd2*100, digits = 2)
        prediction <- sqldf(paste("SELECT avg_estimate from nba_matchups WHERE home = '",
                                  input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        prediction <- round(prediction, digits = 3)*100
        paste("The ", input$nba_home, " have a ", prediction, " percent chance of beating the ", input$nba_away,
              ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
              imp_prob, " percent.", sep = "")
    })
    output$nfl_acc <- renderText({
        paste("Currently, the NFL model is about", round(nfl_acc*100),
              "percent accurate, including last season.")
    })
    output$nba_acc <- renderText({
        paste("The NBA model's accuracy is about", 
              round(nba_acc*100), "percent accurate.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
