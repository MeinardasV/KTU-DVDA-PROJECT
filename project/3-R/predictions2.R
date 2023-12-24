#Meinardas Vilkas ir Agnė Pečiulytė, predictions2_model

#3 spėjimas skyrėsi tik tuo, kad paskolos, kurių suma buvo nurodyta 99999..., buvo pakeistos į 800000 (arti antro maksimumo)

library(tidyverse)

rm(list = ls()) 
setwd("path")

data <- read.csv("1-sample_data.csv")
data_rows <- read.csv("2-additional_data.csv")
data_col <- read.csv("3-additional_features.csv")

fullData = rbind(data,data_rows)
fullData = fullData[order(fullData$id),]
addData = data_col[order(data_col$id),]

fullData = cbind(fullData,addData[,-1])

test_data <- read.csv("test_data.csv")

dat <- data.frame(y=as.factor(fullData$y),term=as.factor(fullData$term),
                  credit_score=as.factor(fullData$credit_score),
                  home_ownership=as.factor(fullData$home_ownership),
                  loan_purpose=as.factor(fullData$loan_purpose),fullData[,-c(1,2,4,5,6,8)])

TESTdat <- data.frame(term=as.factor(test_data$term),
                      credit_score=as.factor(test_data$credit_score),
                      home_ownership=as.factor(test_data$home_ownership),
                      loan_purpose=as.factor(test_data$loan_purpose),test_data[,-c(1,3,4,5,7)])

#Panaikinami nenaudojami kintamieji vietos taupymui

rm(list=c("addData","data","data_rows","data_col","fullData","test_data"))

library(h2o)
h2o.init(max_mem_size = "8g")

quickH2O = as.h2o(dat)
TEST = as.h2o(TESTdat)

splits <- h2o.splitFrame(quickH2O, c(0.85), seed=456)
train  <- h2o.assign(splits[[1]], "train") # 85%
valid  <- h2o.assign(splits[[2]], "valid") # 15%

quickfit <- h2o.gbm(x = 2:16, y = 1, training_frame = train,
                    nfolds=0,seed=456,ntrees = 600,learn_rate=0.05,
                    stopping_rounds = 5,stopping_tolerance = 1e-4,
                    stopping_metric = "AUC",sample_rate = 0.9,
                    col_sample_rate = 1,score_tree_interval = 10,
                    learn_rate_annealing = 0.99,max_depth = 22,
                    min_split_improvement = 1e-5,validation_frame = valid)

predictions <- h2o.predict(quickfit, TEST)

predictions

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = predict, p0 = p0, p1 = p1) %>%
  select(id, y) %>%
  write_csv("path/predictions2.csv")

h2o.saveModel(quickfit, "path", filename = "predictions2_model")

h2o.shutdown(prompt=FALSE)