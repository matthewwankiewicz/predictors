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
                                              choices = sort(c("REB", "PTS", "AST", "FG3M", 
                                                               "TO", "STL"))),
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
        spread_pred <- round(sqldf(paste("SELECT model_spread from nfl_matchups WHERE home = '",
                                         input$home, "' AND visitor = '", input$away, "'", sep = "")))
        paste("The ", input$home, " have a ", prediction, " percent chance of beating the ", input$away,
              ". With odds of ", odd, ", the implied probability of ", input$home, " winning is ",
              imp_prob, " percent. The difference between the home and away team's scores is projected to be: ",
              spread_pred, sep = "")
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
        if(input$nba_home == input$nba_away){
            paste("The ", input$nba_home, " have no chance of beating the ", input$nba_away,
                  ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
                  imp_prob, " percent.", sep = "")
        }
        else{paste("The ", input$nba_home, " have a ", prediction, " percent chance of beating the ", input$nba_away,
                   ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
                   imp_prob, " percent. The difference between the home and away team's scores is projected to be: ",
                   spread_pred, sep = "")}
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
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else{
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        player.df <- avgs %>%
            filter(PLAYER_NAME == input$player)
        player_average = as.numeric(player.df[1,2])
        player_sd = as.numeric(player.df[1,3])
        
        player.df1 <- avgs1 %>%
            filter(PLAYER_NAME == input$player)
        player_average1 = as.numeric(player.df[1,2])
        player_sd1 = as.numeric(player.df[1,3])
        
        player.df2 <- avgs2 %>%
            filter(PLAYER_NAME == input$player)
        player_average2 = as.numeric(player.df[1,2])
        player_sd2 = as.numeric(player.df[1,3])
        
        
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
        
        else if(x == "STL"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = median(OPP_STL))
            stat_col <- team_stats %>% select(OPP_STL)
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
        combined <-  0.55*simmed + 0.25*simmed2 + .2*simmed3
        num = as.numeric(input$prop)
        o_odd <- as.numeric(input$over_odds)
        o_odd2 <- ifelse(o_odd < 0, -1*o_odd/(-1*o_odd + 100), 100/(100+o_odd))
        u_odd <- as.numeric(input$under_odds)
        u_odd2 <- ifelse(u_odd < 0, -1*u_odd/(-1*u_odd + 100), 100/(100+u_odd))
        book_odds <- round(o_odd2/(o_odd2 + u_odd2), 4)
        print(paste("Estimate:", round(mean(combined), digits = 1), "| Prob (%):", (sum(combined > num))/10000,
                    "| Sportsbook's Prob for Over (%):", book_odds*100, "| Last 10 Average:", 
                    player_average2, "| Last 10 SD:", round(player_sd2, digits = 3)))
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
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else{
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:3) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = median(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        player.df <- avgs %>%
            filter(PLAYER_NAME == input$player)
        player_average = as.numeric(player.df[1,2])
        player_sd = as.numeric(player.df[1,3])
        
        player.df1 <- avgs1 %>%
            filter(PLAYER_NAME == input$player)
        player_average1 = as.numeric(player.df[1,2])
        player_sd1 = as.numeric(player.df[1,3])
        
        player.df2 <- avgs2 %>%
            filter(PLAYER_NAME == input$player)
        player_average2 = as.numeric(player.df[1,2])
        player_sd2 = as.numeric(player.df[1,3])
        
        
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
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
            simmed3[i] <- qnorm(runif(1), mean = player_average2, sd = player_sd2) +
                qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
        }
        combined <- 0.55*simmed + 0.25*simmed2 + .2*simmed3
        hist(combined)
        abline(v = input$prop, col = "green")
    })
    output$last_10 <- renderDataTable({
        logs <- gamelogs %>% 
            filter(PLAYER_NAME == input$player) %>% 
            select(PTS, REB, AST, FG3M, FG3A, TO, STL) %>% 
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
        else if(stat == "STL"){
            DT::datatable(logs) %>% formatStyle("STL",
                                                backgroundColor = styleInterval(prop, c('red','lime')))
        }
        else{
            DT::datatable(logs) %>% formatStyle("PTS",
                                                backgroundColor = styleInterval(prop, c('red','lime')))
        }
    })
    output$MA <- renderPlot({
        if(input$stats == "PTS"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(PTS, k = 3, fill = NA),
                       five_game = zoo::rollmean(PTS, k = 5, fill = NA),
                       seven_game = zoo::rollmean(PTS, k = 7, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
        else if(input$stats == "REB"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(REB, k = 3, fill = NA),
                       five_game = zoo::rollmean(REB, k = 5, fill = NA),
                       seven_game = zoo::rollmean(REB, k = 7, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
        else if(input$stats == "AST"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(AST, k = 3, fill = NA),
                       five_game = zoo::rollmean(AST, k = 5, fill = NA),
                       seven_game = zoo::rollmean(AST, k = 7, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
        else if(input$stats == "FG3M"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(FG3M, k = 3, fill = NA),
                       five_game = zoo::rollmean(FG3M, k = 5, fill = NA),
                       seven_game = zoo::rollmean(FG3M, k = 7, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
        else if(input$stats == "STL"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(STL, k = 3, fill = NA),
                       five_game = zoo::rollmean(STL, k = 5, fill = NA),
                       seven_game = zoo::rollmean(STL, k = 7, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
        else if(input$stats == "TO"){
            dataset <- gamelogs %>% filter(PLAYER_NAME == input$player)
            dataset <- dataset %>% 
                mutate(three_game = zoo::rollmean(TO, k = 3, fill = NA),
                       five_game = zoo::rollmean(TO, k = 5, fill = NA),
                       seven_game = zoo::rollmean(TO, k = 7, fill = NA),
                       fifteen = zoo::rollmean(TO, k = 15, fill = NA))
            plot(rev(dataset$three_game), type = "l", col = "orange")  
            lines(rev(dataset$five_game), col = "green")
            lines(rev(dataset$seven_game), col = "blue")
            abline(h = input$prop, col = "red")
            legend("topleft", legend=c("Three Game", "Five Game", "Seven Game"),
                   col=c("orange", "green", "blue"), lty = c(1, 1, 1), cex=0.8)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
