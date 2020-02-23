library("tidyverse")
library("olsrr")
library("caret")
library("glmnet")
library("glmnetUtils")
library('randomForest')

setwd("C:/Users/elmsc/Documents/gis/gis_r")

set.seed(1994)
#94 .41 + .45


sla <- read.csv("output_all/csv/sla_sf_2018.csv")

out <- capture.output(
str(sla),
summary(sla),
colSums(is.na(sla)),

sla <- sla %>% select(-c(NAMELSAD,MTFCC,FUNCSTAT)),

num_row <- nrow(sla),
train_index <- sample(1:num_row, size = 0.60*num_row),

sla_training <- sla %>% slice(train_index),
sla_testing <- sla %>%  slice(-train_index),

sla_mod_18 <- lm(CRIME_COUNT ~ p_moved + p_pov + p_renter + heterogeneity + pop_density 
                 + MJ_COUNT + LS_COUNT + TS_COUNT,
                  data = sla_training),

summary(sla_mod_18),

# out <- capture.output(summary(sla_mod_18))
# cat("ols_18", out, file = "output_all/summary/ols_18_summary.txt", sep = "\n", append = TRUE)

ols_vif_tol(sla_mod_18),

preds_train <- data.frame(preds = predict(sla_mod_18, newdata = sla_training),
                          true = sla_training$CRIME_COUNT,
                          resids = sla_training$CRIME_COUNT - predict(sla_mod_18, newdata = sla_training)),

preds_test <- data.frame(preds = predict(sla_mod_18, newdata = sla_testing),
                         true = sla_testing$CRIME_COUNT,
                         resids = sla_testing$CRIME_COUNT - predict(sla_mod_18, newdata = sla_testing)),

print("OLS TRAIN R2"),
caret::R2(preds_train$preds, preds_train$true),
print("OLS TRAIN RMSE"),
caret::RMSE(preds_train$preds, preds_train$true),
print("OLS TEST R2"),
caret::R2(preds_test$preds, preds_test$true),
print("OLS TEST RMSE"),
caret::RMSE(preds_test$preds, preds_test$true),

alpha_list <- seq(0,1,len = 5),
alpha_list,

enet_fit <- cva.glmnet(CRIME_COUNT ~ p_moved + p_pov + p_renter + heterogeneity + pop_density 
                       + MJ_COUNT + LS_COUNT + TS_COUNT, 
                       data = sla_training,
                       alpha = alpha_list),


minlossplot(enet_fit),
plot(enet_fit$modlist[[5]]),
coef(enet_fit, alpha = 1),
enet_lam <- enet_fit$modlist[[5]]$lambda.min,

#LASSO regression
enet_mod <- glmnet(CRIME_COUNT ~ p_moved + p_pov + p_renter + heterogeneity + pop_density 
                   + MJ_COUNT + LS_COUNT + TS_COUNT,
                   data = sla_training,
                   alpha = 1,
                   lambda = enet_lam),

enet_train_preds <- data.frame(
  preds = predict(enet_mod,s = enet_lam, newdata =sla_training),
  resids = sla_training$CRIME_COUNT - predict(enet_mod,s = enet_lam, newdata =sla_training),
  sla_training$CRIME_COUNT),

enet_train_preds <- rename(enet_train_preds, preds = X1, resids = X1.1, true = "sla_training.CRIME_COUNT"),

enet_test_preds <- data.frame(
  preds = predict(enet_mod,s = enet_lam, newdata =sla_testing),
  resids = sla_testing$CRIME_COUNT - predict(enet_mod,s = enet_lam, newdata =sla_testing),
  sla_testing$CRIME_COUNT),

enet_test_preds <- rename(enet_test_preds, preds = X1, resids = X1.1, true = "sla_testing.CRIME_COUNT"),

print("LASSO TRAIN R2"),
caret::R2(enet_train_preds$preds, enet_train_preds$true),
print("LASSO TRAIN RMSE"),
caret::RMSE(enet_train_preds$preds, enet_train_preds$true),

print("LASSO TEST R2"),
caret::R2(enet_test_preds$preds, enet_test_preds$true),
print("LASSO TEST RMSE"),
caret::RMSE(enet_test_preds$preds, enet_test_preds$true),


rf_ols <- randomForest(CRIME_COUNT~p_moved + p_pov + p_renter + heterogeneity + pop_density 
                       + MJ_COUNT + LS_COUNT + TS_COUNT,
                       data=sla_training,
                       ntree=1000),

preds_crime_train_2 <- data.frame(predicted = predict(rf_ols, newdata = sla_training, type = "response"),sla_training$CRIME_COUNT),
preds_crime_test_2 <- data.frame(predicted = predict(rf_ols, newdata = sla_testing, type = "response"),sla_testing$CRIME_COUNT),

print("RF TRAIN R2"),
caret::R2(preds_crime_train_2$predicted,preds_crime_train_2$sla_training.CRIME_COUNT),
print("RF TEST R2"),
caret::R2(preds_crime_test_2$predicted,preds_crime_test_2$sla_testing.CRIME_COUNT))

cat("rfols_18_all", out, file = "output_all/summary/rfols_18_summary_all.txt", sep = "\n", append = TRUE)
rm(list = ls())
