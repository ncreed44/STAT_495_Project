library(tidyverse)
library(janitor)
cfb_players_df = readRDS("cfb_df_list.RData")
cfb_players <- as.data.frame(cfb_players_df[[1]])
cfb_players

names(cfb_players) <- cfb_players[1,]
cfb_players <- cfb_players[-1,]
clean1 <- clean_names(cfb_players)

clean2 = clean1[!grepl('Rk', clean1$rk), ]
-grep('Rk', clean1$rk)
!grepl('Rk',clean1$rk)
clean1$rk!='Rk'
num_cols = c(5:18)
clean2[, num_cols] <- apply(clean2[, num_cols], 2, as.numeric)
clean2$QBR <- ((((clean2$cmp/clean2$att)-0.3)*5+
                       (clean2$yds/clean2$att)*.25+
                       (clean2$td/clean2$att)*20+2.375-
                       (clean2$int/clean2$att*25))/6)*100
clean2$yc <- clean2$yds/clean2$cmp
clean2$yg <- clean2$yds/clean2$g
saveRDS(clean2, file='~/cfb_clean.RData')
