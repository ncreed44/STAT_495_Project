library(tidyverse)
library(corrplot)
library(randomForest)
library(car)

model_df = readRDS("game_clean.RData")
cfb_model_df = readRDS("cfb_clean.RData")

# random forest
rf_mod = randomForest(model_df$QBR ~ ., data=data.frame(model_df))
rfvi <- varImpPlot(rf_mod)
rfvi
# ANY.A, NY.A, Y.G, AY.A, Y.A, Int., TD, TD., Cmp important
plot(rf_mod)
# other types
library(gbm)
nummodel_df <- model_df[9:31]
gbm_mod = gbm(model_df$QBR ~ ., data=data.frame(nummodel_df))

summary(gbm_mod)

# PDP Plot
plot(gbm_mod,i.var='AY.A')

# fixing variables for my model
cfb_model_df$Cmp <- cfb_model_df$cmp
cfb_model_df$Int <- cfb_model_df$int
cfb_model_df$Y.G <- cfb_model_df$yg
cfb_model_df$AY.A <- cfb_model_df$ay_a
cfb_model_df$Y.A <- cfb_model_df$y_a
cfb_model_df$Int. <- cfb_model_df$Int/cfb_model_df$Cmp
cfb_model_df$TD <- cfb_model_df$td
cfb_model_df$TD. <- cfb_model_df$TD/cfb_model_df$Cmp
cfb_model_df = cfb_model_df[ -c(6,10,11,12,13,20,21) ]

# RMSE calculations
train_index = sample(nrow(model_df), 0.90*nrow(model_df))
nfl_train = model_df[train_index, ]
nfl_test = model_df[-train_index, ]
trainX = model.matrix(QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. +Cmp, 
                      data=nfl_train)[,-c(1)]
testX = model.matrix(QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. +Cmp, 
                     data=nfl_test)[,-c(1)]
rf_train = randomForest::randomForest(nfl_train$QBR ~ Y.G +AY.A +Y.A +Int. +TD 
                                      +TD. +Cmp, data=data.frame(trainX), 
                                      ntree=100, na.action=na.omit)
test_preds = predict(rf_train, data.frame(testX), type="response")
set.seed(21)
validation_sample = sample(nrow(model_df), .05*nrow(model_df))
nfl_validation = model_df[validation_sample,]
nfl_nonval = model_df[-validation_sample,]
k = 10
nonval_sample = sample(nrow(nfl_nonval))
nonval_deciles = quantile(1:nrow(nfl_nonval), seq(0, 1, by=1/k))
cv_list = list()
for(i in 1:k){
  randomized_dec = nonval_sample[ceiling(nonval_deciles[i]):floor(nonval_deciles[i+1])]
  cv_list[[i]] = nfl_nonval[randomized_dec, ]
}
pred_list = list()
for(i in 1:k) {
  cv_dat = do.call(rbind, cv_list[-i])
  cvX = model.matrix(QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. +Cmp, 
                     data=cv_dat)[,-c(1)]
  rf_cv = randomForest::randomForest(cv_dat$QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. 
                                     +Cmp, data=data.frame(cvX),ntree=5)
  test_dat = cv_list[[i]]
  test_datX = model.matrix(QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. +Cmp, 
                           data=test_dat)[,-c(1)]
  pred_list[[i]] = predict(rf_cv, data.frame(test_datX), type="response")
  print(i)
}
cv_preds = do.call(c, pred_list)
plot(density(cv_preds))
lines(density(nfl_nonval$QBR),col="blue")
# RMSE Check
sqrt(mean((cv_preds-nfl_nonval$QBR[nonval_sample])^2))

# my model
NFL_QB_Model <- lm(QBR ~ Y.G +AY.A +Y.A +Int. +TD +TD. +Cmp, data = model_df)
summary(NFL_QB_Model)
avPlots(NFL_QB_Model)
ggplot() + geom_histogram(mapping = aes(x=NFL_QB_Model$residuals))
plot(NFL_QB_Model$residuals)
predict(NFL_QB_Model, newdata = cfb_model_df)
plot(predict(NFL_QB_Model, newdata = cfb_model_df))
# from the model we can tell that the top 5 quarterbacks are still the top 5 
# according to my model the top 5 Quarterbacks to be drafted are Skylar Thompson
# Sam Howell Bailey Zappe Kenny Pickett and Matt Corral.