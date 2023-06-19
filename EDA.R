library(tidyverse)
library(corrplot)
game_clean = readRDS("game_clean.RData")

num_cols = c(9:31)
game_cor = cor(game_clean[, num_cols], use="complete.obs")
corrplot::corrplot(game_cor)

# what should be response variable
plot(density(game_clean$QBR, na.rm=T))
# does ___ affect QBR
plot(game_clean[,c("Age", "QBR")], col = "firebrick")
plot(game_clean[,c("Yds", "QBR")], col = " dark green")
plot(game_clean[,c("TD", "QBR")], col = "royal blue")
plot(game_clean[,c("Int.", "QBR")], col = "dim gray")
game_clean.lm <- lm(QBR~., data = game_clean)
plot(game_clean.lm$residuals)
hist(game_clean.lm$residuals)
