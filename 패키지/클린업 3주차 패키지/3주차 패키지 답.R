
setwd("D:/대학교/[노트북] P-SAT/패키지/package 3/3주차 패키지")
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

library(dummies)
library(cattonum)

library(tictoc)
library(foreach)
library(parallel)
library(doSNOW)
library(randomForest)
library(MLmetrics)

library(catboost)

# ---------------------- < 1 > ----------------------

#### 데이터 준비

data_train <- fread("데이터 0-1.csv")
data_test <- fread("데이터 0-2.csv")

#### 1

model_1 <- lm(Sepal.Length ~ ., data_train)

#### 2 : dummy

data_train_dummy <- catto_dummy(data_train, test = data_test)$train
data_test_dummy <- catto_dummy(data_train, test = data_test)$test

model_2 <- lm(Sepal.Length ~ ., data_train_dummy)

#### 3 : freq

data_train_freq <- catto_freq(data_train, test = data_test)$train
data_test_freq <- catto_freq(data_train, test = data_test)$test

model_3 <- lm(Sepal.Length ~ ., data_train_freq)

#### 4 : target

data_train_mean <- catto_mean(data_train, test = data_test, response = Sepal.Length)$train
data_test_mean <- catto_mean(data_train, test = data_test, response = Sepal.Length)$test

model_4 <- lm(Sepal.Length ~ ., data_train_mean)

#### 5 : loo

data_train_loo <- catto_loo(data_train, test = data_test, response = Sepal.Length)$train
data_test_loo <- catto_loo(data_train, test = data_test, response = Sepal.Length)$test

model_5 <- lm(Sepal.Length ~ ., data_train_loo)

#### 6 : order

do_cat_order_slow <- function(data, x, y, seed, replace) {
    
    set.seed(seed)
    
    data_save <- data[sample(nrow(data)), ] %>% as.data.frame()
    
    data_save[, x] %<>% as.character()
    
    start1 <- NULL
    
    for (i in data_save[, x] %>% unique()) {
        
        start1[i] <- which(data_save[, x] == i) %>% min()
        
    }; rm(i)
    
    start2 <- data_save %>%
        group_by(get(x)) %>%
        dplyr::summarize(encoding = mean(get(y))) %>%
        ungroup; colnames(start2)[1] <- x
    
    for (i in 1:nrow(data_save)) {
        
        if (i %in% start1) {
            
            encoding1 <- start2[start2[, x] == data_save[i, x], ]
            
            data_save$encoding[i] <- encoding1$encoding
            
        } else {
            
            encoding2 <- data_save[1:(i-1), ] %>%
                group_by(get(x)) %>%
                dplyr::summarize(encoding = mean(get(y))) %>%
                ungroup; colnames(encoding2)[1] <- x
                
                encoding2 <- encoding2[encoding2[, x] == data_save[i, x], ]
                
                data_save$encoding[i] <- encoding2$encoding
                
        }
        
        print(i)
        
    }; rm(encoding1, encoding2, start1, start2)
    
    if (replace == T) {
        
        data_save[x] <- NULL
        colnames(data_save)[ncol(data_save)] <- x
        
    } else {
        
        colnames(data_save)[ncol(data_save)] <- paste0(x, "_order")
        
    }
    
    data_save$row <- rownames(data_save) %>% as.numeric
    
    data_save %<>% arrange(row) %>% select(-row)
    
    return(data_save)
    
}

data_train_order <- do_cat_order_slow(data_train, "Species", "Sepal.Length", 1, T)
data_test_order <- data_test_mean

model_6 <- lm(Sepal.Length ~ ., data_train_order)

#### 7 : 비교

RMSE(data_test$Sepal.Length, predict(model_1, data_test))
RMSE(data_test_dummy$Sepal.Length, predict(model_2, data_test_dummy))
RMSE(data_train_freq$Sepal.Length, predict(model_3, data_test_freq))
RMSE(data_train_mean$Sepal.Length, predict(model_4, data_test_mean))
RMSE(data_train_loo$Sepal.Length, predict(model_5, data_test_loo))
RMSE(data_train_order$Sepal.Length, predict(model_6, data_test_order))

rm(list = ls())

# ---------------------- < 2 > ----------------------

#### 1

tic()
Sys.sleep(3)
toc()

#### 2

#

tic()

data1 <- NULL

for (i in 1:10) {
    
    data1 <- cbind(data1, rnorm(5000 * 2000))
    print(i)
    
}; rm(i)

toc()

#

tic()

data2 <- foreach(i = 1:10, .combine = cbind) %do% {
    
    return(rnorm(5000 * 2000))
    
}

toc()

#### 3-1, 3-2

tic()

cl <- makeCluster(detectCores() - 1); registerDoSNOW(cl)

pb <- txtProgressBar(max = 10, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

data3 <- foreach(i = 1:10, .combine = c, .options.snow = opts) %dopar% {
    
    return(rnorm(5000 * 2000))
    
}

stopCluster(cl); rm(cl, pb, progress, opts)

toc()

rm(list = ls())

#### 4

cl <- makeCluster(detectCores() - 1); registerDoSNOW(cl)

pb <- txtProgressBar(max = 99, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

data_train <- foreach(i = 1:99, .combine = rbind, .packages = "data.table", .options.snow = opts) %dopar% {
    
    return(fread(paste0("데이터 2/데이터 2 ", i, ".csv"), stringsAsFactors = F))
    
}

stopCluster(cl); rm(cl, pb, progress, opts)

data_test <- fread("데이터 1.csv", stringsAsFactors = F)

data_train %<>% mutate_if(is.character, as.factor)
data_test %<>% mutate_if(is.character, as.factor)

data_test <- rbind(data_train[1, ], data_test)
data_test <- data_test[-1, ]

data_train <- rbind(data_test[1, ], data_train)
data_train <- data_train[-1, ]

data_train$click %<>% as.character %>% as.factor
data_test$click %<>% as.character %>% as.factor

#### 5-1, 5-2

tic()
set.seed(1)
model_rf_1 <- randomForest(click ~ ., data = data_train)
toc()

tic()
cores <- detectCores() - 1

cl <- makeCluster(detectCores() - 1); registerDoSNOW(cl)

pb <- txtProgressBar(max = cores, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

model_rf_2 <- foreach(ntree = rep(floor(500/cores), cores),
                    seeds = 1:cores,
                    .combine = randomForest::combine,
                    .multicombine = T,
                    .packages = "randomForest", 
                    .options.snow = opts) %dopar% {
                        
                        set.seed(seeds)
                        randomForest(click ~ ., data = data_train, do.trace = T, ntree = ntree)
                        
                    }

stopCluster(cl); rm(cl, pb, progress, opts)

toc()

LogLoss(predict(model_rf_1, data_test, type = "prob")[, 2], data_test$click %>% as.character() %>% as.numeric())
LogLoss(predict(model_rf_2, data_test, type = "prob")[, 2], data_test$click %>% as.character() %>% as.numeric())

# ---------------------- < 3 > ----------------------

#### 1

data_train$click %<>% as.character %>% as.numeric()
data_test$click %<>% as.character %>% as.numeric()

#### 2

train_pool <- catboost.load_pool(
    data = data_train[, -1], # X 값들
    label = data_train[, 1] # Y 값들
)

test_pool <- catboost.load_pool(
    data = data_test[, -1],
    label = data_test[, 1]# Y값이 없다
)

#### 3

params <- list(
    
    random_seed = 1,
    
    loss_function = "Logloss",
    logging_level = "Verbose",
    
    iterations = 200,
    learning_rate = 0.1,
    
    task_type = "CPU"
    
)

model_cat_cpu_1 <- catboost.train(learn_pool = train_pool,
                                params = params)

LogLoss(catboost.predict(model_cat_cpu_1, test_pool, prediction_type = "Probability"), 
        data_test$click)

#### 4

CV <- catboost.cv(pool = train_pool,
                  params = params)

params$iterations <- 92

#### 5

model_cat_cpu_2 <- catboost.train(learn_pool = train_pool,
                                params = params)

LogLoss(catboost.predict(model_cat_cpu_2, test_pool, prediction_type = "Probability"), 
        data_test$click)

#### 6

params <- list(
    
    random_seed = 1,
    
    loss_function = "Logloss",
    logging_level = "Verbose",
    
    iterations = 200,
    learning_rate = 0.1,
    
    task_type = "GPU"
    
)

model_cat_gpu_1 <- catboost.train(learn_pool = train_pool,
                                  params = params)

LogLoss(catboost.predict(model_cat_gpu_1, test_pool, prediction_type = "Probability"), 
        data_test$click)

#### 7

object.size(model_cat_cpu_1)
object.size(model_cat_gpu_1)
