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
library(DT)

nfl_matchups <- read_rds("matchups.rds")
nba_matchups <- read_rds("nba_matchups.rds")
nba_matchups_hb2b <- read_rds("nba_matchups_homeb2b.rds")
nba_matchups_vb2b <- read_rds("nba_matchups_visitorb2b.rds")
nba_matchups_bothb2b <- read_rds("nba_matchups_bothb2b.rds")
home_stats <- read_rds("home_stats.rds")
visitor_stats <- read_rds("visitor_stats.rds")
advanced_data_home <- read_rds("advanced_data_home.rds")
advanced_data_visitor <- read_rds("advanced_data_visitor.rds")
possessions <- read_rds("possessions.rds")
model_stats <- read_rds("model_stats.rds")
gamelogs <- read_rds("gamelogs.rds")
sf_stats <- read_rds("sf_stats.rds")
pg_stats <- read_rds("pg_stats.rds")
pf_stats <- read_rds("pf_stats.rds")
sg_stats <- read_rds("sg_stats.rds")
c_stats <- read_rds("c_stats.rds")
qb_rushing <- read_rds("qb_rushing.rds")
qb_passing <- read_rds("qb_passing.rds")
rb_rushing <- read_rds("rb_rushing.rds")
rb_passing <- read_rds("rb_passing.rds")
wr_passing <- read_rds("WR_passing.rds")
te_passing <- read_rds("TE_passing.rds")
player_stats <- read_rds("nfl_player_stats.rds")




gamelogs <- gamelogs %>% 
    drop_na(PTS)

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
    tabPanel("NBA Player Sim",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "player",
                                 label = "Select Player:",
                                 choices = sort(unique(gamelogs$PLAYER_NAME))),
                     selectInput(inputId = "team",
                                 label = "Select Team:",
                                 choices = sort(unique(pg_stats$TEAM))),
                     selectInput(inputId = "stats",
                                 label = "Select a Stat:",
                                 choices = c("REB", "PTS", "AST", "FG3M", 
                                                  "TO", "STL", "PTS+REB+AST",
                                                  "PTS+AST", "PTS+REB", "REB+AST",
                                                  "BLK", "F_PTS")),
                     selectInput(inputId = "pos",
                                 label = "Select player's position:",
                                 choices = c("Point Guard", "Shooting Guard",
                                             "Small Forward", "Power Forward",
                                             "Centre")),
                     textInput(inputId = "prop",
                               label = "Player Prop:",
                               value = "0"),
                     textInput(inputId = "over_odds",
                               label = "Odds for Over:",
                               value = "0"),
                     textInput(inputId = "under_odds",
                               label = "Odds for Under:",
                               value = "0")),
                 mainPanel(
                     textOutput("player_match"),
                     plotOutput('hist'),
                     plotOutput('MA'),
                     dataTableOutput('last_10')
                 ))),
    tabPanel("NFL Player Sim",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "nfl_player",
                                 label = "Select Player:",
                                 choices = sort(unique(player_stats$player_name))),
                     selectInput(inputId = "nfl_team",
                                 label = "Select Team:",
                                 choices = sort(unique(qb_rushing$Team))),
                     selectInput(inputId = "nfl_stats",
                                 label = "Select a Stat:",
                                 choices = c("QB - passing yards", "QB - rushing yards",
                                             "QB - pass attempts", "QB - pass completions",
                                             "QB - passing TDs", "QB - rushing TDs",
                                             "RB - rushing yards", "RB - rushes",
                                             "RB - receptions", "RB - rushing TDs",
                                             "RB - receiving yards", "RB - receiving TDs",
                                             "WR - receptions", "WR - receiving yards",
                                             "WR - receiving TDs", "TE - receptions",
                                             "TE - receiving yards", "TE - receiving TDs")),
                     textInput(inputId = "nfl_prop",
                               label = "Player Prop:",
                               value = "0"),
                     textInput(inputId = "nfl_over_odds",
                               label = "Odds for Over:",
                               value = "0"),
                     textInput(inputId = "nfl_under_odds",
                               label = "Odds for Under:",
                               value = "0")),
                 mainPanel(
                     textOutput("nfl_player_match"),
                     dataTableOutput('nfl_dt'),
                 ))
    ),
    
    tabPanel("NBA Stats",
                selectInput(inputId = "team_pos",
                            label = "Select Position:",
                            choices = c("Point Guard", "Shooting Guard", "Small Forward",
                                             "Power Forward", "Centre")),
             mainPanel(
                 dataTableOutput("pos_dt")
             )),
    
    tabPanel("About",
             mainPanel(h1("About the Model"),
                       "This app contains models to predict outcomes for various sport matches. NBA data has been collected from NBA.com, CleaningtheGlass and Hashtag Basketball. The accuracies for each model can be seen below:
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
        spread_pred <- round(sqldf(paste("SELECT model_spread from nfl_matchups WHERE home = '",
                                   input$home, "' AND visitor = '", input$away, "'", sep = "")))
        total_pred <- round(sqldf(paste("SELECT total from nfl_matchups WHERE home = '",
                                        input$home, "' AND visitor = '", input$away, "'", sep = "")))
        paste("The ", input$home, " have a ", prediction, " percent chance of beating the ", input$away,
              ". With odds of ", odd, ", the implied probability of ", input$home, " winning is ",
              imp_prob, " percent. The difference between the home and away team's scores is projected to be: ",
              spread_pred, ". The projected total score is: ", total_pred, sep = "")
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
        prediction <- sqldf(paste("SELECT estimate from nba_matchups WHERE home = '",
                                      input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        prediction <- round(prediction, digits = 3)*100
        spread_pred <- sqldf(paste("SELECT estimate_spread from nba_matchups WHERE home = '",
                                   input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        total_pred <- sqldf(paste("SELECT estimate_total from nba_matchups WHERE home = '",
                                   input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        if(input$nba_home == input$nba_away){
            paste("The ", input$nba_home, " have no chance of beating the ", input$nba_away,
                  ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
                  imp_prob, " percent.", sep = "")
        }
        else{paste("The ", input$nba_home, " have a ", prediction, " percent chance of beating the ", input$nba_away,
              ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
              imp_prob, " percent. The difference between the home and away team's scores is projected to be: ",
              spread_pred, ". The total score is projected to be: ", total_pred, sep = "")}
    })
    output$nfl_acc <- renderText({
        paste("Currently, the NFL model is about", round(nfl_acc*100),
              "percent accurate, including last season.")
    })
    output$nba_acc <- renderText({
        paste("The NBA model's accuracy is about", 
              round(nba_acc*100), "percent accurate.")
    })
    output$player_match <- renderText({
        if(input$pos == "Point Guard"){
            team_stats <- pg_stats
        }
        else if(input$pos == "Shooting Guard"){
            team_stats <- sg_stats
        }
        else if(input$pos == "Small Forward"){
            team_stats <- sf_stats
        }
        else if(input$pos == "Power Forward"){
            team_stats <- pf_stats
        }
        else{
            team_stats <- c_stats
        }
        x <- input$stats
        if(x == "AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "BLK"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "PTS+REB+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "PTS+REB"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "PTS+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "REB+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "F_PTS"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else{
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
        }

        player_average = as.numeric(avgs[1,1])
        player_sd = as.numeric(avgs[1,2])
        

        player_average1 = as.numeric(avgs1[1,1])
        player_sd1 = as.numeric(avgs1[1,2])
        

        player_average2 = as.numeric(avgs2[1,1])
        player_sd2 = as.numeric(avgs2[1,2])

        
        if(x == "AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_AST))
            stat_col <- team_stats %>% select(OPP_AST)
        }
        else if(x == "REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_REB))
            stat_col <- team_stats %>% select(OPP_REB)
        }
        
        else if(x == "FG3M"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_FG3M))
            stat_col <- team_stats %>% select(OPP_FG3M)
        }
        
        else if(x == "TO"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_TO))
            formatted = "TOV"
            stat_col <- team_stats %>% select(OPP_TO)
        }
        
        else if(x == "BLK"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_BLK))
            stat_col <- team_stats %>% select(OPP_BLK)
        }
        
        else if(x == "STL"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_STL))
            stat_col <- team_stats %>% select(OPP_STL)
        }
        
        else if(x == "PTS+REB+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_REB_AST))
            stat_col <- team_stats %>% select(PTS_REB_AST)
        }
        
        else if(x == "PTS+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_AST))
            stat_col <- team_stats %>% select(PTS_AST)
        }
        
        else if(x == "PTS+REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_REB))
            stat_col <- team_stats %>% select(PTS_REB)
        }
        
        else if(x == "REB+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(REB_AST))
            stat_col <- team_stats %>% select(REB_AST)
        }
        
        else if(x == "F_PTS"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(F_PTS))
            stat_col <- team_stats %>% select(F_PTS)
        }
        
        else{
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_PTS))
            stat_col <- team_stats %>% select(OPP_PTS)
        }
        x <- mean(scale(teams$avg.stat, center = 0))
        teams <- teams %>%
            mutate(strength = round(scale(avg.stat, center = 0) - 1, digits = 3))
        
        team_adj <- sqldf(paste("select strength[,1] from teams where TEAM is '",
                                input$team, "'", sep = ""))[1,1]

        simmed <- c()
        simmed2 <- c()
        simmed3 <- c()
        set.seed(21)
        for(i in 1:10000){
            simmed[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed2[i] <- qnorm(runif(1), mean = player_average1, sd = player_sd1) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed3[i] <- qnorm(runif(1), mean = player_average2, sd = player_sd2) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
        }
        combined <-  0.3*simmed + 0.6*simmed2 + 0.1*simmed3
        num = as.numeric(input$prop)
        o_odd <- as.numeric(input$over_odds)
        o_odd2 <- ifelse(o_odd < 0, -1*o_odd/(-1*o_odd + 100), 100/(100+o_odd))
        u_odd <- as.numeric(input$under_odds)
        u_odd2 <- ifelse(u_odd < 0, -1*u_odd/(-1*u_odd + 100), 100/(100+u_odd))
        book_odds <- round(o_odd2/(o_odd2 + u_odd2), 4)
        print(paste("Estimate:", round(mean(combined), digits = 1), "| Prob (%):", (sum(combined > num))/10000,
              "| Sportsbook's Prob for Over (%):", book_odds*100, "| Last 10 Average:", 
              player_average2, "| Last 10 SD:", round(player_sd2, digits = 3), "| Last 7 Average:", 
              round(player_average1, digits = 1), "| Last 7 SD:", round(player_sd1, digits = 3)))
    })
    output$hist <- renderPlot({
        if(input$pos == "Point Guard"){
            team_stats <- pg_stats
        }
        else if(input$pos == "Shooting Guard"){
            team_stats <- sg_stats
        }
        else if(input$pos == "Small Forward"){
            team_stats <- sf_stats
        }
        else if(input$pos == "Power Forward"){
            team_stats <- pf_stats
        }
        else{
            team_stats <- c_stats
        }
        x <- input$stats
        if(x == "AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "BLK"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(BLK, na.rm = T),
                          sd1 = sd(BLK, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "PTS+REB+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_REB_AST, na.rm = T),
                          sd1 = sd(PTS_REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "PTS+REB"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_REB, na.rm = T),
                          sd1 = sd(PTS_REB, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "PTS+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS_AST, na.rm = T),
                          sd1 = sd(PTS_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "REB+AST"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB_AST, na.rm = T),
                          sd1 = sd(REB_AST, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else if(x == "F_PTS"){
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(F_PTS, na.rm = T),
                          sd1 = sd(F_PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            
            
        }
        
        else{
            avgs <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                filter(PLAYER_NAME == input$player) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        

        player_average = as.numeric(avgs[1,1])
        player_sd = as.numeric(avgs[1,2])
        
        player_average1 = as.numeric(avgs1[1,1])
        player_sd1 = as.numeric(avgs1[1,2])
        

        player_average2 = as.numeric(avgs2[1,1])
        player_sd2 = as.numeric(avgs2[1,2])
        
        
        if(x == "AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_AST))
            stat_col <- team_stats %>% select(OPP_AST)
        }
        else if(x == "REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_REB))
            stat_col <- team_stats %>% select(OPP_REB)
        }
        
        else if(x == "FG3M"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_FG3M))
            stat_col <- team_stats %>% select(OPP_FG3M)
        }
        
        else if(x == "STL"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_STL))
            stat_col <- team_stats %>% select(OPP_STL)
        }
        
        else if(x == "TO"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_TO))
            stat_col <- team_stats %>% select(OPP_TO)
        }
        
        else if(x == "BLK"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_BLK))
            stat_col <- team_stats %>% select(OPP_BLK)
        }

        else if(x == "PTS+REB+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_REB_AST))
            stat_col <- team_stats %>% select(PTS_REB_AST)
        }
        
        else if(x == "PTS+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_AST))
            stat_col <- team_stats %>% select(PTS_AST)
        }
        
        else if(x == "PTS+REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(PTS_REB))
            stat_col <- team_stats %>% select(PTS_REB)
        }
        
        else if(x == "REB+AST"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(REB_AST))
            stat_col <- team_stats %>% select(REB_AST)
        }
        
        else if(x == "F_PTS"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(F_PTS))
            stat_col <- team_stats %>% select(F_PTS)
        }
        
        else{
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_PTS))
            stat_col <- team_stats %>% select(OPP_PTS)
        }
        teams <- teams %>%
            mutate(strength = round(scale(avg.stat, center = 0) - 1, digits = 3))
        
        team_adj <- sqldf(paste("select strength[,1] from teams where TEAM is '",
                                input$team, "'", sep = ""))[1,1]
        
        simmed <- c()
        simmed2 <- c()
        simmed3 <- c()
        set.seed(21)
        for(i in 1:10000){
            simmed[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed2[i] <- qnorm(runif(1), mean = player_average1, sd = player_sd1) +
                qnorm(runif(1), mean = player_average1, sd = player_sd1)*team_adj
            simmed3[i] <- qnorm(runif(1), mean = player_average2, sd = player_sd2) +
                qnorm(runif(1), mean = player_average2, sd = player_sd2)*team_adj
        }
        combined <- 0.3*simmed + 0.6*simmed2 + 0.1*simmed3
        hist(combined)
        abline(v = input$prop, col = "green")
    })
    output$last_10 <- renderDataTable({
        logs <- gamelogs %>% 
            filter(PLAYER_NAME == input$player) %>% 
            select(PTS, REB, AST, FG3M, FG3A, TO, STL, PTS_REB_AST,
                   PTS_REB, PTS_AST, REB_AST, BLK, F_PTS) %>% 
            slice(1:10)
        prop = as.numeric(input$prop)
        stat = input$stats
        if(stat == "AST"){
        x <- DT::datatable(logs) %>% formatStyle("AST",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "REB"){
            DT::datatable(logs) %>% formatStyle("REB",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "FG3M"){
            DT::datatable(logs) %>% formatStyle("FG3M",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "TO"){
            DT::datatable(logs) %>% formatStyle("TO",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "BLK"){
            DT::datatable(logs) %>% formatStyle("BLK",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "STL"){
            DT::datatable(logs) %>% formatStyle("STL",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "PTS+REB+AST"){
            DT::datatable(logs) %>% formatStyle("PTS_REB_AST",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "PTS+REB"){
            DT::datatable(logs) %>% formatStyle("PTS_REB",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "PTS+AST"){
            DT::datatable(logs) %>% formatStyle("PTS_AST",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "REB+AST"){
            DT::datatable(logs) %>% formatStyle("REB_AST",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else if(stat == "F_PTS"){
            DT::datatable(logs) %>% formatStyle("F_PTS",
                                                backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else{
            DT::datatable(logs) %>% formatStyle("PTS",
            backgroundColor = styleInterval(prop, c('red','lime')))
        }
    })
    output$MA <- renderPlot({
        if(input$stats == "PTS"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(PTS), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(PTS), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(PTS), k = 7, fill = NA),
                       season_avg = cummean(rev(PTS)),
                       game_num = seq_along(PTS)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average") +
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "REB"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(REB), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(REB), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(REB), k = 7, fill = NA),
                       season_avg = cummean(rev(REB)),
                       game_num = seq_along(REB)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "AST"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(AST), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(AST), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(AST), k = 7, fill = NA),
                       season_avg = cummean(rev(AST)),
                       game_num = seq_along(AST)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "BLK"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(BLK), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(BLK), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(BLK), k = 7, fill = NA),
                       season_avg = cummean(rev(BLK)),
                       game_num = seq_along(BLK)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "FG3M"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(FG3M), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(FG3M), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(FG3M), k = 7, fill = NA),
                       season_avg = cummean(rev(FG3M)),
                       game_num = seq_along(FG3M)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "STL"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(STL), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(STL), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(STL), k = 7, fill = NA),
                       season_avg = cummean(rev(STL)),
                       game_num = seq_along(STL)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "TO"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(TO), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(TO), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(TO), k = 7, fill = NA),
                       season_avg = cummean(rev(TO)),
                       game_num = seq_along(TO)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "PTS+REB+AST"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(PTS_REB_AST), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(PTS_REB_AST), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(PTS_REB_AST), k = 7, fill = NA),
                       season_avg = cummean(rev(PTS_REB_AST)),
                       game_num = seq_along(PTS_REB_AST)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "PTS+REB"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(PTS_REB), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(PTS_REB), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(PTS_REB), k = 7, fill = NA),
                       season_avg = cummean(rev(PTS_REB)),
                       game_num = seq_along(PTS_REB)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "PTS+AST"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(PTS_AST), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(PTS_AST), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(PTS_AST), k = 7, fill = NA),
                       season_avg = cummean(rev(PTS_AST)),
                       game_num = seq_along(PTS_AST)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        else if(input$stats == "REB+AST"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(REB_AST), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(REB_AST), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(REB_AST), k = 7, fill = NA),
                       season_avg = cummean(rev(REB_AST)),
                       game_num = seq_along(REB_AST)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
        
        else if(input$stats == "F_PTS"){
            prop <- as.numeric(input$prop)
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset %>% 
                mutate(three_game = zoo::rollmean(rev(F_PTS), k = 3, fill = NA),
                       five_game = zoo::rollmean(rev(F_PTS), k = 5, fill = NA),
                       seven_game = zoo::rollmean(rev(F_PTS), k = 7, fill = NA),
                       season_avg = cummean(rev(F_PTS)),
                       game_num = seq_along(F_PTS)) %>% 
                pivot_longer(names_to = "rolling_mean_key", 
                             values_to = "rolling_mean_value", 
                             cols = c(three_game, five_game, 
                                      seven_game, season_avg)) %>% 
                ggplot(aes(game_num, rolling_mean_value, 
                           color = rolling_mean_key)) +
                geom_line() + 
                theme_classic() +
                labs(color = "Average")+
                geom_hline(yintercept = prop)
        }
    })
    
    output$nfl_player_match <- renderText({
        if(input$nfl_stats %in% c("QB - passing yards", "QB - pass attempts", "QB - pass completions",
                                  "QB - passing TDs")){
            team_stats <- qb_passing
        }
        else if(input$nfl_stats %in% c("QB - rushing yards", "QB - rushing TDs")){
            team_stats <- qb_rushing
        }
        else if(input$nfl_stats %in% c("RB - rushing yards", "RB - rushes", "RB - rushing TDs")){
            team_stats <- rb_rushing
        }
        else if(input$nfl_stats %in% c("RB - receptions", "RB - receiving TDs", "RB - receiving yards")){
            team_stats <- rb_passing
        }
        else if(input$nfl_stats %in% c("WR - receptions", "WR - receiving TDs", "WR - receiving yards")){
            team_stats <- wr_passing
        }
        else if(input$nfl_stats %in% c("TE - receiving TDs", "TE - receiving yards", "TE - receptions")){
            team_stats <- te_passing
        }
        else{}
        x <- input$nfl_stats
        if(x == "QB - passing yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(passing_yards, na.rm = T),
                          sd1 = sd(passing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(passing_yards, na.rm = T),
                          sd1 = sd(passing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(passing_yards, na.rm = T),
                          sd1 = sd(passing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "QB - rushing yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "QB - pass attempts"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(attempts, na.rm = T),
                          sd1 = sd(attempts, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(attempts, na.rm = T),
                          sd1 = sd(attempts, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(attempts, na.rm = T),
                          sd1 = sd(attempts, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "QB - pass completions"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(completions, na.rm = T),
                          sd1 = sd(completions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(completions, na.rm = T),
                          sd1 = sd(completions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(completions, na.rm = T),
                          sd1 = sd(completions, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "QB - passing TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(passing_tds, na.rm = T),
                          sd1 = sd(passing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(passing_tds, na.rm = T),
                          sd1 = sd(passing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(passing_tds, na.rm = T),
                          sd1 = sd(passing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "QB - rushing TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - rushes"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(carries, na.rm = T),
                          sd1 = sd(carries, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(carries, na.rm = T),
                          sd1 = sd(carries, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(carries, na.rm = T),
                          sd1 = sd(carries, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - rushing yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(rushing_yards, na.rm = T),
                          sd1 = sd(rushing_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - rushing TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(rushing_tds, na.rm = T),
                          sd1 = sd(rushing_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - receptions"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - receiving yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "RB - receiving TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "WR - receptions"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "WR - receiving yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "WR - receiving TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TE - receptions"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receptions, na.rm = T),
                          sd1 = sd(receptions, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TE - receiving yards"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_yards, na.rm = T),
                          sd1 = sd(receiving_yards, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TE - receiving TDs"){
            avgs <- player_stats %>%
                group_by(player_name) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- player_stats %>%
                group_by(player_name) %>%
                slice(1:8) %>% 
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- player_stats %>%
                group_by(player_name) %>%
                summarise(avg1 = median(receiving_tds, na.rm = T),
                          sd1 = sd(receiving_tds, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else{}
        
        player.df <- avgs %>%
            filter(player_name == input$nfl_player)
        player_average = as.numeric(player.df[1,1])
        player_sd = as.numeric(player.df[1,2])
        
        player.df1 <- avgs1 %>%
            filter(player_name == input$nfl_player)
        player_average1 = as.numeric(player.df[1,1])
        player_sd1 = as.numeric(player.df[1,2])
        
        player.df2 <- avgs2 %>%
            filter(player_name == input$nfl_player)
        player_average2 = as.numeric(player.df[1,1])
        player_sd2 = as.numeric(player.df[1,2])
        
        
        if(x == "QB - passing yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        else if(x == "QB - rushing yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        
        else if(x == "QB - pass attempts"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Att))
            stat_col <- team_stats %>% select(Att)
        }
        
        else if(x == "QB - pass completions"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Cmp))
            stat_col <- team_stats %>% select(Cmp)
        }
        
        else if(x == "QB - passing TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else if(x == "QB - rushing TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else if(x == "RB - rushing yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        
        else if(x == "RB - rushes"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Att))
            stat_col <- team_stats %>% select(Att)
        }
        
        else if(x == "RB - receptions"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Recpt))
            stat_col <- team_stats %>% select(Recpt)
        }
        
        else if(x == "RB - rushing TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else if(x == "RB - receiving TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else if(x == "RB - receiving yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        
        else if(x == "WR - receiving yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        
        else if(x == "WR - receptions"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Recpt))
            stat_col <- team_stats %>% select(Recpt)
        }
        
        else if(x == "WR - receiving TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else if(x == "TE - receiving yards"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Yd))
            stat_col <- team_stats %>% select(Yd)
        }
        
        else if(x == "TE - receptions"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(Recpt))
            stat_col <- team_stats %>% select(Recpt)
        }
        
        else if(x == "TE - receiving TDs"){
            teams <- team_stats %>%
                group_by(Team) %>%
                summarise(avg.stat = median(TD))
            stat_col <- team_stats %>% select(TD)
        }
        
        else{}
        
        x <- mean(scale(teams$avg.stat, center = 0))
        teams <- teams %>%
            mutate(strength = round(scale(avg.stat, center = 0) - 1, digits = 3))
        
        team_adj <- sqldf(paste("select strength[,1] from teams where Team is '",
                                input$nfl_team, "'", sep = ""))[1,1]
        
        simmed <- c()
        simmed2 <- c()
        simmed3 <- c()
        set.seed(21)
        for(i in 1:10000){
            simmed[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed2[i] <- qnorm(runif(1), mean = player_average1, sd = player_sd1) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed3[i] <- qnorm(runif(1), mean = player_average2, sd = player_sd2) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
        }
        combined <- 0.55*simmed + 0.25*simmed2 + .2*simmed3
        num = as.numeric(input$nfl_prop)
        o_odd <- as.numeric(input$nfl_over_odds)
        o_odd2 <- ifelse(o_odd < 0, -1*o_odd/(-1*o_odd + 100), 100/(100+o_odd))
        u_odd <- as.numeric(input$nfl_under_odds)
        u_odd2 <- ifelse(u_odd < 0, -1*u_odd/(-1*u_odd + 100), 100/(100+u_odd))
        book_odds <- round(o_odd2/(o_odd2 + u_odd2), 4)
        print(paste("Estimate:", round(mean(combined), digits = 1), "| Prob (%):", (sum(combined > num))/10000,
                    "| Sportsbook's Prob for Over (%):", book_odds*100))
    })
    
    output$nfl_dt <- renderDataTable({
        logs <- player_stats %>% 
            filter(player_name == input$nfl_player) %>% 
            select(passing_yards, rushing_yards, attempts, completions, passing_tds,
                   rushing_tds, carries, receptions, targets, receiving_yards, receiving_tds) %>% 
            slice(1:10)
    })
    
    output$pos_dt <- renderDataTable({
        if(input$team_pos == "Point Guard"){
            pg_stats
        }
        
        else if(input$team_pos == "Shooting Guard"){
            sg_stats
        }
        
        else if(input$team_pos == "Small Forward"){
            sf_stats
        }
        else if(input$team_pos == "Power Forward"){
            pf_stats
        }
        else{
            c_stats
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
