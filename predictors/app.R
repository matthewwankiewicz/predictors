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

simulate_score <- function(probs, p_2pt, p_3pt, p_reb, p_ft, pace, sims){
    
    a <- 1
    pts <- 0
    
    while(a <= sims){
        
        outcome <- sample(c(1:4), 1, prob = probs)
        
        if(outcome == 2){
            end <- 0
            while(end < 1){
                
                shot_prob <- runif(1)
                
                if(shot_prob <= p_2pt){
                    
                    pts <- pts + 2
                    end = 1
                    
                }
                
                else{
                    
                    reb <- runif(1)
                    if(reb >= p_reb){
                        
                        end = 1
                        
                    }
                    
                }
                
            }
        }
        
        if(outcome == 3){
            end <- 0
            while(end < 1){
                
                shot_prob <- runif(1)
                
                if(shot_prob <= p_3pt){
                    
                    pts <- pts + 3
                    end = 1
                    
                }
                
                else{
                    
                    reb <- runif(1)
                    if(reb >= p_reb){
                        
                        end = 1
                        
                    }
                    
                }
                
            }
            
        }
        
        if(outcome == 4){
            end <- 0
            while(end < 1){
                
                shot_prob <- runif(1)
                
                if(shot_prob <= p_ft){
                    
                    pts <- pts + 1
                    end = 1
                    
                }
                shot_prob <- runif(1)
                if(shot_prob <= p_ft){
                    
                    pts <- pts + 1
                    end = 1
                    
                }
                
                else{
                    
                    reb <- runif(1)
                    if(reb >= p_reb){
                        
                        end = 1
                        
                    }
                    
                }
                
            }
        }
        a = a+1
    }
    print((pts/sims)*pace)
}

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
                               value = "0"),
                     radioButtons(inputId = "homeb2b",
                                  label = "Home on back to back?",
                                  choices = list("Yes", "No")),
                     radioButtons(inputId = "roadb2b",
                                  label = "Visitor on back to back?",
                                  choices = list("Yes", "No"))),
                 mainPanel(
                     textOutput("nba_match"),
                     tableOutput("nba_matches"),
                     textOutput("nba_setup")
                 ))),
    tabPanel("Monte Carlo",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "mc_home",
                                 label = "Select Home Team:",
                                 choices = sort(unique(home_stats$TEAM_NAME))),
                     selectInput(inputId = "mc_away",
                                 label = "Select Visiting Team:",
                                 choices = sort(unique(home_stats$TEAM_NAME)))),
                 mainPanel(
                     textOutput("mc_match"),
                     textOutput("mc_setup"),
                     DTOutput('mc_breakdown')
                 ))),
    tabPanel("Player Sim",
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
        if(input$homeb2b == "Yes" & input$roadb2b == "Yes"){
            prediction <- sqldf(paste("SELECT avg_estimate from nba_matchups_bothb2b WHERE home = '",
                                      input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        }
        else if(input$homeb2b == "Yes" & input$roadb2b == "No"){
            prediction <- sqldf(paste("SELECT avg_estimate from nba_matchups_hb2b WHERE home = '",
                                      input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        }
        else if(input$homeb2b == "No" & input$roadb2b == "Yes"){
            prediction <- sqldf(paste("SELECT avg_estimate from nba_matchups_vb2b WHERE home = '",
                                      input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        }
        else{
            prediction <- sqldf(paste("SELECT avg_estimate from nba_matchups WHERE home = '",
                                      input$nba_home, "' AND visitor = '", input$nba_away, "'", sep = ""))
        }
        prediction <- round(prediction, digits = 3)*100
        if(input$nba_home == input$nba_away){
            paste("The ", input$nba_home, " have no chance of beating the ", input$nba_away,
                  ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
                  imp_prob, " percent.", sep = "")
        }
        else{paste("The ", input$nba_home, " have a ", prediction, " percent chance of beating the ", input$nba_away,
              ". With odds of ", odd, ", the implied probability of ", input$nba_home, " winning is ",
              imp_prob, " percent.", sep = "")}
    })
    output$nfl_acc <- renderText({
        paste("Currently, the NFL model is about", round(nfl_acc*100),
              "percent accurate, including last season.")
    })
    output$nba_acc <- renderText({
        paste("The NBA model's accuracy is about", 
              round(nba_acc*100), "percent accurate.")
    })
    output$mc_setup <- renderText({
        home_team <- input$mc_home
        vis_team <- input$mc_away
        
        avg_3pt_rate_off <- mean(home_stats$FG3_rate)/100
        avg_2pt_rate_off <- mean(home_stats$FG2_rate)/100
        avg_2pt_pct_off <- mean(home_stats$FG2_PCT)/100
        avg_3pt_pct_off <- mean(home_stats$FG3_PCT)
        avg_ft_pct_off <- mean(home_stats$FT_PCT)
        
        ### home ------------
        # get offence numbers
        home_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from home_stats where TEAM_NAME is '", 
                                         home_team, "'", sep = ""))[1, 1]/100
        home_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from home_stats where TEAM_NAME is '", 
                                         home_team, "'", sep = ""))[1, 1]/100
        home_2pt_pct_off <- sqldf(paste("SELECT FG2_PCT from home_stats where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1, 1]/100
        home_3pt_pct_off <- sqldf(paste("SELECT FG3_PCT from home_stats where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1, 1]
        home_ft_pct_off <- sqldf(paste("SELECT FT_PCT from home_stats where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1, 1]
        
        ### rebound, tov, free throw numbers
        
        home_orb_pct_off <- sqldf(paste("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        home_orb_pct_def <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        
        home_ft_rate_off <- sqldf(paste("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        home_ft_rate_def <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        
        home_tov_pct_off <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        home_tov_pct_def <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        
        ## get defense numbers
        home_3pt_rate_def <- sqldf(paste("SELECT OPP_FG3_rate from home_stats where TEAM_NAME is '", 
                                         home_team, "'", sep = ""))[1, 1]/100
        home_2pt_rate_def <- sqldf(paste("SELECT OPP_FG2_rate from home_stats where TEAM_NAME is '", 
                                         home_team, "'", sep = ""))[1, 1]/100
        home_2pt_pct_def <- sqldf(paste("SELECT OPP_FG2_PCT from home_stats where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1, 1]/100
        home_3pt_pct_def <- sqldf(paste("SELECT OPP_FG3_PCT from home_stats where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1, 1]
        home_ft_pct_def <- sqldf(paste("SELECT OPP_FT_PCT from home_stats where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1, 1]
        
        ### visitor -------
        # get offence numbers
        
        vis_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from visitor_stats where TEAM_NAME is '", 
                                        vis_team, "'", sep = ""))[1, 1]/100
        vis_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from visitor_stats where TEAM_NAME is '", 
                                        vis_team, "'", sep = ""))[1, 1]/100
        vis_2pt_pct_off <- sqldf(paste("SELECT FG2_PCT from visitor_stats where TEAM_NAME is '", 
                                       vis_team, "'", sep = ""))[1, 1]/100
        vis_3pt_pct_off <- sqldf(paste("SELECT FG3_PCT from visitor_stats where TEAM_NAME is '", 
                                       vis_team, "'", sep = ""))[1, 1]
        vis_ft_pct_off <- sqldf(paste("SELECT FT_PCT from visitor_stats where TEAM_NAME is '", 
                                      vis_team, "'", sep = ""))[1, 1]
        
        ### rebound numbers
        vis_orb_rate_off <- sqldf(paste("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        vis_orb_rate_def <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                        home_team, "'", sep = ""))[1,1]
        
        vis_ft_rate_off <- sqldf(paste("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1,1]
        vis_ft_rate_def <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1,1]
        
        vis_tov_pct_off <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1,1]
        vis_tov_pct_def <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                       home_team, "'", sep = ""))[1,1]
        
        
        ## get defense numbers
        vis_3pt_rate_def <- sqldf(paste("SELECT OPP_FG3_rate from visitor_stats where TEAM_NAME is '", 
                                        vis_team, "'", sep = ""))[1, 1]/100
        vis_2pt_rate_def <- sqldf(paste("SELECT OPP_FG2_rate from visitor_stats where TEAM_NAME is '", 
                                        vis_team, "'", sep = ""))[1, 1]/100
        vis_2pt_pct_def <- sqldf(paste("SELECT OPP_FG2_PCT from visitor_stats where TEAM_NAME is '", 
                                       vis_team, "'", sep = ""))[1, 1]/100
        vis_3pt_pct_def <- sqldf(paste("SELECT OPP_FG3_PCT from visitor_stats where TEAM_NAME is '", 
                                       vis_team, "'", sep = ""))[1, 1]
        vis_ft_pct_def <- sqldf(paste("SELECT OPP_FT_PCT from visitor_stats where TEAM_NAME is '", 
                                      vis_team, "'", sep = ""))[1, 1]
        
        
        ## get home difference vs average --------
        
        #offence
        home_3pt_rate_off.dif <- home_3pt_rate_off - avg_3pt_rate_off
        home_2pt_rate_off.dif <- home_2pt_rate_off - avg_2pt_rate_off
        home_2pt_pct_off.dif <- home_2pt_pct_off - avg_2pt_pct_off
        home_3pt_pct_off.dif <- home_3pt_pct_off - avg_3pt_pct_off
        home_ft_pct_off.dif <- home_ft_pct_off - avg_ft_pct_off
        
        #defence
        home_3pt_rate_def.dif <- home_3pt_rate_def - avg_3pt_rate_off
        home_2pt_rate_def.dif <- home_2pt_rate_def - avg_2pt_rate_off
        home_2pt_pct_def.dif <- home_2pt_pct_def - avg_2pt_pct_off
        home_3pt_pct_def.dif <- home_3pt_pct_def - avg_3pt_pct_off
        home_ft_pct_def.dif <- home_ft_pct_def - avg_ft_pct_off
        
        #reb
        home_orb_pct_off.dif <- sqldf(paste("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        home_orb_pct_def.dif <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        
        #ft
        home_ft_rate_off.dif <- sqldf(paste("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        home_ft_rate_def.dif <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        
        #tov
        home_tov_pct_off.dif <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        home_tov_pct_def.dif <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                            home_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
        
        ## get visitor difference vs average ------
        
        #offence
        vis_3pt_rate_off.dif <- vis_3pt_rate_off - avg_3pt_rate_off
        vis_2pt_rate_off.dif <- vis_2pt_rate_off - avg_2pt_rate_off
        vis_2pt_pct_off.dif <- vis_2pt_pct_off - avg_2pt_pct_off
        vis_3pt_pct_off.dif <- vis_3pt_pct_off - avg_3pt_pct_off
        vis_ft_pct_off.dif <- vis_ft_pct_off - avg_ft_pct_off
        
        #defence
        vis_3pt_rate_def.dif <- vis_3pt_rate_def - avg_3pt_rate_off
        vis_2pt_rate_def.dif <- vis_2pt_rate_def - avg_2pt_rate_off
        vis_2pt_pct_def.dif <- vis_2pt_pct_def - avg_2pt_pct_off
        vis_3pt_pct_def.dif <- vis_3pt_pct_def - avg_3pt_pct_off
        vis_ft_pct_def.dif <- vis_ft_pct_def - avg_ft_pct_off
        
        #reb
        vis_orb_pct_off.dif <- sqldf(paste("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        vis_orb_pct_def.dif <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        
        #ft
        vis_ft_rate_off.dif <- sqldf(paste("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        vis_ft_rate_def.dif <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        
        #tov
        vis_tov_pct_off.dif <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        vis_tov_pct_def.dif <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                           vis_team, "'", sep = ""))[1,1] - 
            sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        
        
        ### get difference sums
        
        #home off
        diff_sum3pt_rate <- home_3pt_rate_off.dif + vis_3pt_rate_def.dif
        diff_sum2pt_rate<- home_2pt_rate_off.dif + vis_2pt_rate_def.dif
        diff_sum2pt_pct <- home_2pt_pct_off.dif + vis_2pt_pct_def.dif
        diff_sum3pt_pct <- home_3pt_pct_off.dif + vis_3pt_pct_def.dif
        diff_sumft_pct <- home_ft_pct_off.dif + vis_ft_pct_def.dif
        diff_sumorb_pct <- home_orb_pct_off.dif + vis_orb_pct_def.dif
        diff_sumft_rate <- home_ft_rate_off.dif + vis_ft_rate_def.dif
        diff_sumtov_rate <- home_tov_pct_off.dif + vis_tov_pct_def.dif
        
        ### get adjusted values
        
        adj_3pt_rate <- diff_sum3pt_rate + avg_3pt_rate_off
        adj_2pt_rate <- diff_sum2pt_rate + avg_2pt_rate_off
        adj_2pt_pct <- diff_sum2pt_pct + avg_2pt_pct_off
        adj_3pt_pct <- diff_sum3pt_pct + avg_3pt_pct_off
        adj_ft_pct <- diff_sumft_pct + avg_ft_pct_off
        adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        adj_ft_rate <- diff_sumft_rate + sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        
        fga_pct <- 1 - adj_ft_rate - adj_tov_rate
        prob_2pt <- adj_2pt_rate * fga_pct
        prob_3pt <- adj_3pt_rate * fga_pct
        
        
        probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
        p_2pt <- adj_2pt_pct
        p_3pt <- adj_3pt_pct
        p_reb <- adj_orb_pct
        p_ft <- adj_ft_pct
        
        pace <- sqldf(paste("SELECT last_3 from possessions where Team_name is '",
                            home_team, "'", sep = ""))[1,1]
        #home score
        home <- round(simulate_score(probs, p_2pt, p_3pt, 
                                           p_reb, p_ft, pace, 10000), digits = 0)
        
        diff_sum3pt_rate <- vis_3pt_rate_off.dif + home_3pt_rate_def.dif
        diff_sum2pt_rate<- vis_2pt_rate_off.dif + home_2pt_rate_def.dif
        diff_sum2pt_pct <- vis_2pt_pct_off.dif + home_2pt_pct_def.dif
        diff_sum3pt_pct <- vis_3pt_pct_off.dif + home_3pt_pct_def.dif
        diff_sumft_pct <- vis_ft_pct_off.dif + home_ft_pct_def.dif
        diff_sumorb_pct <- vis_orb_pct_off.dif + home_orb_pct_def.dif
        diff_sumft_rate <- vis_ft_rate_off.dif + home_ft_rate_def.dif
        diff_sumtov_rate <- vis_tov_pct_off.dif + home_tov_pct_def.dif
        
        adj_3pt_rate <- diff_sum3pt_rate + avg_3pt_rate_off
        adj_2pt_rate <- diff_sum2pt_rate + avg_2pt_rate_off
        adj_2pt_pct <- diff_sum2pt_pct + avg_2pt_pct_off
        adj_3pt_pct <- diff_sum3pt_pct + avg_3pt_pct_off
        adj_ft_pct <- diff_sumft_pct + avg_ft_pct_off
        adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        adj_ft_rate <- diff_sumft_rate + sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
        
        fga_pct <- 1 - adj_ft_rate - adj_tov_rate
        prob_2pt <- adj_2pt_rate * fga_pct
        prob_3pt <- adj_3pt_rate * fga_pct
        
        
        probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
        p_2pt <- adj_2pt_pct
        p_3pt <- adj_3pt_pct
        p_reb <- adj_orb_pct
        p_ft <- adj_ft_pct
        
        pace <- sqldf(paste("SELECT last_3 from possessions where Team_name is '",
                            vis_team, "'", sep = ""))[1,1]
        #visitor score
        visitor <- round(simulate_score(probs, p_2pt, p_3pt,
                                              p_reb, p_ft, sqrt(105*pace), 10000), digits = 0)
        
        print(paste("Based on 10000 simulations, the predicted score is ", home_team, 
                    ": ", home, ", ", vis_team, ": ", visitor, sep = ""))
    })
    output$mc_breakdown <- renderDataTable({
        model_stats %>% 
            select("team" = `home`,
                   home_accuracy, visitor_accuracy, 
                   accuracy)
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
                slice(1:4) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
        }

        else{
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
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
                summarise(avg.stat = mean(OPP_AST))
            stat_col <- team_stats %>% select(OPP_AST)
        }
        else if(x == "REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_REB))
            stat_col <- team_stats %>% select(OPP_REB)
        }
        
        else if(x == "FG3M"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_FG3M))
            stat_col <- team_stats %>% select(OPP_FG3M)
        }
        
        else if(x == "TO"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_TO))
            formatted = "TOV"
            stat_col <- team_stats %>% select(OPP_TO)
        }
        
        else if(x == "STL"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_STL))
            stat_col <- team_stats %>% select(OPP_STL)
        }
        
        else{
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_PTS))
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
        combined <- 0.6*simmed + 0.3*simmed2 + .1*simmed3
        num = as.numeric(input$prop)
        o_odd <- as.numeric(input$over_odds)
        o_odd2 <- ifelse(o_odd < 0, -1*o_odd/(-1*o_odd + 100), 100/(100+o_odd))
        u_odd <- as.numeric(input$under_odds)
        u_odd2 <- ifelse(u_odd < 0, -1*u_odd/(-1*u_odd + 100), 100/(100+u_odd))
        book_odds <- round(o_odd2/(o_odd2 + u_odd2), 4)
        print(paste("Estimate:", round(mean(combined), digits = 1), "| Prob (%):", (sum(combined > num))/10000,
              "| Sportsbook's Prob for Over (%):", book_odds*100))
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
                slice(1:4) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(AST, na.rm = T),
                          sd1 = sd(AST, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        else if(x == "REB"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(REB, na.rm = T),
                          sd1 = sd(REB, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "FG3M"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(FG3M, na.rm = T),
                          sd1 = sd(FG3M, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "TO"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(TO, na.rm = T),
                          sd1 = sd(TO, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else if(x == "STL"){
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(STL, na.rm = T),
                          sd1 = sd(STL, na.rm = T)) %>%
                filter(avg1 != 0)
        }
        
        else{
            avgs <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:4) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs1 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:7) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
                          sd1 = sd(PTS, na.rm = T)) %>%
                filter(avg1 != 0)
            avgs2 <- gamelogs %>%
                group_by(PLAYER_NAME) %>%
                slice(1:10) %>% 
                summarise(avg1 = mean(PTS, na.rm = T),
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
                summarise(avg.stat = mean(OPP_AST))
            stat_col <- team_stats %>% select(OPP_AST)
        }
        else if(x == "REB"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_REB))
            stat_col <- team_stats %>% select(OPP_REB)
        }
        
        else if(x == "FG3M"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_FG3M))
            stat_col <- team_stats %>% select(OPP_FG3M)
        }
        
        else if(x == "STL"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_STL))
            stat_col <- team_stats %>% select(OPP_STL)
        }
        
        else if(x == "TO"){
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_TO))
            stat_col <- team_stats %>% select(OPP_TO)
        }
        
        else{
            teams <- team_stats %>%
                group_by(TEAM) %>%
                summarise(avg.stat = mean(OPP_PTS))
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
        combined <- 0.6*simmed + 0.3*simmed2 + .1*simmed3
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
}

# Run the application 
shinyApp(ui = ui, server = server)
