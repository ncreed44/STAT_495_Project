library(tidyverse)
player_dfs = readRDS("player_df_list.RData")
all_players <- as.data.frame(player_dfs[[1]])
for (i in c(2:21)) {
  temp <- as.data.frame(player_dfs[[i]])
  #all_players<- rbind(all_players,temp)
  all_players <- all_players %>% 
    bind_rows(temp)
}
all_players
# get rid of column header names that turned into rows
game_clean1 = all_players[!grepl('Rk', all_players$Rk), ]

-grep('Rk', all_players$Rk)
!grepl('Rk',all_players$Rk)
all_players$Rk!='Rk'

patterns <- c("QB", "qb")
game_clean1
game_clean2 <- filter(game_clean1, grepl(paste(patterns, collapse="|"), Pos))
game_clean2
game_clean2[game_clean2==""]<-0
game_clean2
game_clean2$Att <- as.integer(game_clean2$Att)
game_clean2$Cmp <- as.integer(game_clean2$Cmp)
game_clean2$Yds <- as.integer(game_clean2$Yds)
game_clean2$TD <- as.integer(game_clean2$TD)
game_clean2$Int <- as.integer(game_clean2$Int)
game_clean2$QBR <- ((((game_clean2$Cmp/game_clean2$Att)-0.3)*5+
                      (game_clean2$Yds/game_clean2$Att)*.25+
                      (game_clean2$TD/game_clean2$Att)*20+2.375-
                      (game_clean2$Int/game_clean2$Att*25))/6)*100
num_cols = c(9:31)
game_clean2[, num_cols] <- apply(game_clean2[, num_cols], 2, as.numeric)
saveRDS(game_clean2, file='~/game_clean.RData')
