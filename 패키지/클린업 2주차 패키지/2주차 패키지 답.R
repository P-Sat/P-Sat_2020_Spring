
setwd(paste0("D:/대학교/[노트북] P-SAT/패키지/package 2/2주차 패키지"))
# 바탕화면에 놔두고 실행해주세요

# ---------------------- < START > ----------------------

####

options(scipen = 999)

rm(list = ls())

####

library(tidyverse)
library(plyr)
library(magrittr)
library(data.table)
library(stringr)

library(MLmetrics)
library(rpart)
library(tree)
library(randomForest)

library(caret)

# ---------------------- < 준비 > ----------------------

#### 1, 2

data <- fread("데이터1.csv")

data %<>% mutate(date = str_sub(event_datetime, 9, 10) %>% as.numeric()) %>% select(-event_datetime)

data_train <- data %>% filter(date < 10) %>% select(-date)
data_test <- data %>% filter(date >= 10) %>% select(-date)

rm(data)

#### 3

data_train %<>% mutate_if(is.character, as.factor)
data_test %<>% mutate_if(is.character, as.factor)

# ---------------------- < 1 > ----------------------

#### 1-1

model_glm_1_1 <- glm(click ~ ., family = binomial(link = "logit"), data_train %>% select(-c(device_model, predicted_house_price)))

#### 1-2

model_glm_1_2 <- glm(click ~ ., family = binomial(link = "logit"), data_train %>% select(-c(device_model)))

#### 1-3

model_glm_1_3 <- glm(click ~ ., family = binomial(link = "logit"), data_train %>% select(-c(predicted_house_price)))

#### 1-4

LogLoss(predict(model_glm_1_1, data_test, type = "response"), data_test$click)
LogLoss(predict(model_glm_1_2, data_test, type = "response"), data_test$click)
LogLoss(predict(model_glm_1_3, data_test, type = "response"), data_test$click)

#### 1-5 

model_glm_2 <- glm(click ~ ., family = binomial(link = "cloglog"), data_train %>% select(-c(device_model, predicted_house_price)))

#### 2

data_train %<>% select(-c(device_model, predicted_house_price))
data_test %<>% select(-c(device_model, predicted_house_price))

#### 3-1, 3-2

model_tree_1 <- tree(click ~ ., data_train)
model_tree_2 <- rpart(click ~ ., data_train)

#### 3-3

plot(model_tree_1); text(model_tree_1)
plot(model_tree_2); text(model_tree_2)

#### 4-1, 4-2

data_train$click %<>% as.character %>% as.factor
data_test$click %<>% as.character %>% as.factor

data_test <- rbind(data_train[1, ], data_test)
data_test <- data_test[-1, ]

data_train <- rbind(data_test[1, ], data_train)
data_train <- data_train[-1, ]

#### 4-3

set.seed(1)
model_rf_1 <- randomForest(click ~ ., data = data_train)

#### 5

LogLoss(predict(model_glm_1_1, data_test, type = "response"), data_test$click %>% as.character() %>% as.numeric())
LogLoss(predict(model_glm_2, data_test, type = "response"), data_test$click %>% as.character() %>% as.numeric())

LogLoss(predict(model_tree_1, data_test), data_test$click %>% as.character() %>% as.numeric())
LogLoss(predict(model_tree_2, data_test), data_test$click %>% as.character() %>% as.numeric())

LogLoss(predict(model_rf_1, data_test, type = "prob")[, 2], data_test$click %>% as.character() %>% as.numeric())

# ---------------------- < 2 > ----------------------

#### 1

data_train$click <- paste0("X", data_train$click)
data_test$click <- paste0("X", data_test$click)

fit_control <- trainControl(
    
    method = "cv",
    number = 5,
    
    classProbs = TRUE, 
    summaryFunction = mnLogLoss,
    
    verboseIter = TRUE
    
)

grid <- expand.grid(
    
    mtry = 3
    
)

model <- train(
    
    data_train[, -1], paste0("X", data_train$click),
    method = "rf",
    
    tuneGrid = grid, 
    trControl = fit_control,
    
    random_seed = 1
    
)

LogLoss(predict(model, data_test, type = "prob")[, 2], data_test$click %>% str_sub(2) %>% as.numeric())
