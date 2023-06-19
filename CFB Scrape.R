library(xml2)
library(rvest)
library(rjson)
library(dplyr)
library(tidyverse)

cfb_passer_dfs = list()
passer_url = "https://www.sports-reference.com/cfb/years/2021-passing.html"
passer_df = passer_url %>%
  read_html()%>%
  html_table()
cfb_passer_dfs[[1]] = passer_df
saveRDS(cfb_passer_dfs, file='~/cfb_df_list.RData')
