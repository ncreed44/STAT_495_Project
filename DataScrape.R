# R Scraping

library(xml2)
library(rvest)
library(tidyverse)
library(rjson)
library(dplyr)

# NFL QB data

nfl_html = "https://www.pro-football-reference.com/years/2021/passing.htm"

nfl_passer_list = rvest::read_html(nfl_html)

nfl_passer_df = nfl_passer_list %>%
  rvest::html_table()

nfl_passer_href = nfl_passer_list %>% 
  rvest::html_nodes("td") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")

sportref_url = "https://www.pro-football-reference.com"

player_page = sportref_url %>%
  paste0(nfl_passer_href[11]) %>%
  rvest::read_html()

player_season_summary = player_page %>%
  rvest::html_table()

Joe_Burrow_per_game = player_season_summary[[1]] %>%
  data.frame()

player_seasons = player_page %>%
  rvest::html_nodes("tr") %>%
  rvest::html_nodes("td") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")

season_games = sportref_url %>%
  paste0(player_seasons[1]) %>%
  rvest::read_html()

season_df = season_games %>%
  rvest::html_table(fill=T) %>%
  .[[which(sapply(., ncol) > 3)]]

# All passers in Last 20 years code
season <- c(2001:2021)
passer_dfs = list()
season_counter = game_counter =  1
for (i in 1:length(season)) {
  passer_url = glue::glue("https://www.pro-football-reference.com/years/{season[i]}/passing.htm")
  passer_df = passer_url %>%
    read_html()%>%
    html_table()
  passer_dfs[[i]] = passer_df
}
passer_dfs
saveRDS(passer_dfs, file='~/player_df_list.RData')



