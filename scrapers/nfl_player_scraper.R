library(tidyverse)
library(rvest)
library(janitor)
library(sqldf)
library(nflfastR)

format_name <- function(name){
  substr(name, start = gregexpr(" ", name)[[1]][2] + 1, stop = str_length(name))
}

link <- "https://www.cbssports.com/fantasy/football/stats/posvsdef/QB/ALL/avg/standard"
page <- read_html(link)
table <- page %>% html_table()
table <- table[[1]]
table <- table[3:nrow(table),]

### RUSHING --------------

QB_rushing_table <- table[, c(2, 9,10, 11, 12)]

QB_rushing_table <- row_to_names(QB_rushing_table, 1)
num_cols <- colnames(QB_rushing_table)[2:5]
QB_rushing_table[num_cols] <- sapply(QB_rushing_table[num_cols],as.numeric)

teams <- QB_rushing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

QB_rushing_table$Team <- names

### PASSING ----------

QB_passing_table <- table[, c(2:7)]

QB_passing_table <- row_to_names(QB_passing_table, 1)
num_cols <- colnames(QB_passing_table)[2:5]
QB_passing_table[num_cols] <- sapply(QB_passing_table[num_cols],as.numeric)

teams <- QB_passing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

QB_passing_table$Team <- names

##### RUNNING BACKS -----------------

link <- "https://www.cbssports.com/fantasy/football/stats/posvsdef/RB/ALL/avg/standard"
page <- read_html(link)
table <- page %>% html_table()
table <- table[[1]]
table <- table[3:nrow(table),]

### PASSING --------------

RB_passing_table <- table[, c(2, 8:11)]

RB_passing_table <- row_to_names(RB_passing_table, 1)
num_cols <- colnames(RB_passing_table)[2:5]
RB_passing_table[num_cols] <- sapply(RB_passing_table[num_cols],as.numeric)

teams <- RB_passing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

RB_passing_table$Team <- names

### RUSHING ----------

RB_rushing_table <- table[, c(2:6)]

RB_rushing_table <- row_to_names(RB_rushing_table, 1)
num_cols <- colnames(RB_rushing_table)[2:5]
RB_rushing_table[num_cols] <- sapply(RB_rushing_table[num_cols],as.numeric)

teams <- RB_rushing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

RB_rushing_table$Team <- names


#### WIDE RECEIVER -----------

link <- "https://www.cbssports.com/fantasy/football/stats/posvsdef/WR/ALL/avg/standard"
page <- read_html(link)
table <- page %>% html_table()
table <- table[[1]]
table <- table[3:nrow(table),]

### PASSING ----------

WR_passing_table <- table[, c(2, 7:11)]

WR_passing_table <- row_to_names(WR_passing_table, 1)
num_cols <- colnames(WR_passing_table)[2:6]
WR_passing_table[num_cols] <- sapply(WR_passing_table[num_cols],as.numeric)

teams <- WR_passing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

WR_passing_table$Team <- names


##### TIGHT END --------

link <- "https://www.cbssports.com/fantasy/football/stats/posvsdef/TE/ALL/avg/standard"
page <- read_html(link)
table <- page %>% html_table()
table <- table[[1]]
table <- table[3:nrow(table),]

### PASSING ----------

TE_passing_table <- table[, c(2, 7:11)]

TE_passing_table <- row_to_names(TE_passing_table, 1)
num_cols <- colnames(TE_passing_table)[2:5]
TE_passing_table[num_cols] <- sapply(TE_passing_table[num_cols],as.numeric)

teams <- TE_passing_table$Team
names <- c()
for(i in 1:32){
  names <- c(names, format_name(teams[i]))
}

TE_passing_table$Team <- names


### Player stats -------

stats <- load_player_stats(2021)
stats <- stats %>% 
  filter(season == 2021)
stats <- stats %>% 
  arrange(desc(week))

### write files ------

write_rds(QB_rushing_table, "predictors/qb_rushing.rds")
write_rds(QB_passing_table, "predictors/qb_passing.rds")
write_rds(RB_rushing_table, "predictors/rb_rushing.rds")
write_rds(RB_passing_table, "predictors/rb_passing.rds")
write_rds(WR_passing_table, "predictors/WR_passing.rds")
write_rds(TE_passing_table, "predictors/TE_passing.rds")
write_rds(stats, "predictors/nfl_player_stats.rds")

