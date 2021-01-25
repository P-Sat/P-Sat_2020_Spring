
setwd(paste0("C:/Users/", Sys.info()[8], "/Desktop/오호 코딩 실력이 올라가는 군요"))
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
library(lubridate)

# ---------------------- < 0 > ----------------------

#### 답

data0 <- data.table::fread("데이터0.csv", stringsAsFactors = T) 

data0 %<>% dplyr::select(-Petal.Length) 

data0 %<>% dplyr::filter(Petal.Width <= 1.800) 

data0 %<>% dplyr::mutate(Sepal.Length_mean = Sepal.Length %>% mean(),
                        Sepal.Length_sd = Sepal.Length %>% var() %>% sqrt()) 

data0 %<>% dplyr::left_join(data0 %>% 
                               dplyr::group_by(Species) %>% 
                               dplyr::summarise(Sepal.Width_mean_each = mean(Sepal.Width),
                                                num = dplyr::n()) %>% 
                               dplyr::ungroup())

data0 %<>% dplyr::arrange(dplyr::desc(Sepal.Length))

data0$Species %<>% revalue(c("virginica" = "VI",
                            "versicolor" = "VE",
                            "setosa" = "SE"))

data0 %>% dplyr::rename(name = Species)

# ---------------------- < 1 > ----------------------

#### 1

data1 <- fread("데이터1.csv")

data1 %>% str()
data1 %>% summary()

#### 각 열마다 unique 한 값들의 숫자를 얻자

data1 %>% sapply(unique) %>% lapply(length)

#### str과 유니크 갯수로 봤을때 지워야 할 열을 지우자

data1 %<>% select(-c(bid_id, device_os, device_country, asset_index))

#### 나이변수는 명목형 변수 처리 해주어라

data1 %<>% mutate(age = paste0("X", age))

#### 파생변수 :평일, 주말 + 시간

data1 %<>% mutate(day = wday(str_sub(event_datetime, 1, 10) %>% ymd()) %>% as.character(),
                  hour = paste0("X", str_sub(event_datetime, 12, 13)) %>% as.factor())

data1$weekend <- data1$day %>% plyr::revalue(c("1" = "we", "2" = "wd", "3" = "wd", "4" = "wd", "5" = "wd", "6" = "wd", "7" = "we")) %>% as.factor()

data1$day %<>% plyr::revalue(c("1" = "sun", "2" = "mon", "3" = "tue", "4" = "wed", "5" = "thi", "6" = "fri", "7" = "sat")) %>% as.factor()

#### 파생변수 : 시간당 클릭율

data1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 13), ":00:00"))

data1 %<>% left_join(data1 %>% select(time, click) %>% group_by(time) %>% dplyr::summarise(ratio = mean(click)) %>% ungroup())

#### 파생변수 : 앱의 갯수

data1 %<>% mutate(install_num = data1$install_pack %>% str_split(",") %>% sapply(length))

#### event_datetime 제외한 모든 캐릭터 팩터화

data1 %<>% mutate_if(is.character, as.factor)

data1$event_datetime %<>% as.character()
data1$time %<>% as.character()

# ---------------------- < 2 > ----------------------

#### 클릭율 시게열 플롯

data2_1 <- data1 %>% select(time, ratio) %>% unique() %>% mutate(date = str_sub(time, 9, 10),
                                                                 time = str_sub(time, 12, 13))

a <- data2_1 %>% ggplot(aes(x = time, y = ratio, group = date, col = date), alpha = 0.1) +
    geom_line() +
    theme_bw()

b <- data2_1 %>% ggplot(aes(x = time, y = ratio, group = date, col = date)) + 
    geom_line() +
    facet_wrap( ~ date) +
    theme_bw()

gridExtra::grid.arrange(a, b, nrow = 2)

#### 두 범주 변수에 대한 벌룬 플롯

data2_2 <- data1 %>% group_by(age, placement_type) %>% dplyr::summarise(num = n())

a <- data2_2 %>% ggplot() + geom_point(aes(x = age, y = placement_type, size = num), shape = 21, colour = "black", fill = "skyblue") +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    scale_size_area(max_size = 15)

data2_3 <- data1 %>% group_by(age, placement_type) %>% dplyr::summarise(num = n())

b <- data2_2 %>% ggplot() + geom_bar(aes(x = age, y = num, fill = placement_type), stat = "identity", position = "fill")

gridExtra::grid.arrange(b, a, nrow = 2)

#### 여러개가 있는경우

data2_4 <- data1 %>% select(weekend, predicted_house_price, install_num, ratio) %>% gather(key, value, predicted_house_price:ratio)

a <- data2_4 %>% ggplot(aes(sample = value, col = factor(weekend))) +
    geom_qq() +
    facet_wrap( ~ key, scales = "free")

b <- data2_4 %>% ggplot(aes(x = factor(weekend), y = value)) +
    geom_violin(aes(fill = factor(weekend)))  + 
    geom_boxplot(width = 0.2) +
    facet_wrap( ~ key, scales = "free") +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2)

gridExtra::grid.arrange(a, b, nrow = 2)
