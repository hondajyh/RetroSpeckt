library(readr)
train_RM <- read_csv("Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/train.csv")
test_RM <- read_csv("Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/test.csv")

library(tidyverse)
library(lubridate)
library(ggplot2)

# Work on Train data set first
train_RM$UK <- ifelse(train_RM$country == 'united kingdom', 1, 0)

# For model; won't need this for Test data
train_RM$log_quantity <- log1p(train_RM$quantity)

# Transform date information for Train
train_RM$date2 <- mdy(train_RM$date)
train_RM$time2 <- hms(train_RM$time) 
train_RM$hour <- hour(train_RM$time)

# Word cloud one-hot encoding for Train
train_RM$desc_set <- ifelse(grepl("set",train_RM$description), 1, 0)
train_RM$desc_bag <- ifelse(grepl("bag",train_RM$description), 1, 0)
train_RM$desc_red <- ifelse(grepl("red",train_RM$description), 1, 0)
train_RM$desc_heart <- ifelse(grepl("heart",train_RM$description), 1, 0)
train_RM$desc_retrospot <- ifelse(grepl("retrospot",train_RM$description), 1, 0)
train_RM$desc_vintage <- ifelse(grepl("vintage",train_RM$description), 1, 0)
train_RM$desc_design <- ifelse(grepl("design",train_RM$description), 1, 0)
train_RM$desc_pink <- ifelse(grepl("pink",train_RM$description), 1, 0)
train_RM$desc_christmas <- ifelse(grepl("christmas",train_RM$description), 1, 0)
train_RM$desc_box <- ifelse(grepl("box",train_RM$description), 1, 0)

# Assign some meaning to time of day for Train
train_RM$morning <- ifelse(train_RM$hour<12 & train_RM$hour>4,1,0)
train_RM$afternoon <- ifelse(train_RM$hour>=12 & train_RM$hour<=17,1,0)
train_RM$evening <- ifelse(train_RM$hour>17 | train_RM$hour<=4,1,0)


# Work on Test data set next
test_RM$UK <- ifelse(test_RM$country == 'united kingdom', 1, 0)

# Transform date information for Test
test_RM$date2 <- mdy(test_RM$date)
test_RM$time2 <- hms(test_RM$time) 
test_RM$hour <- hour(test_RM$time)

# Word cloud one-hot encoding for Test
test_RM$desc_set <- ifelse(grepl("set",test_RM$description), 1, 0)
test_RM$desc_bag <- ifelse(grepl("bag",test_RM$description), 1, 0)
test_RM$desc_red <- ifelse(grepl("red",test_RM$description), 1, 0)
test_RM$desc_heart <- ifelse(grepl("heart",test_RM$description), 1, 0)
test_RM$desc_retrospot <- ifelse(grepl("retrospot",test_RM$description), 1, 0)
test_RM$desc_vintage <- ifelse(grepl("vintage",test_RM$description), 1, 0)
test_RM$desc_design <- ifelse(grepl("design",test_RM$description), 1, 0)
test_RM$desc_pink <- ifelse(grepl("pink",test_RM$description), 1, 0)
test_RM$desc_christmas <- ifelse(grepl("christmas",test_RM$description), 1, 0)
test_RM$desc_box <- ifelse(grepl("box",test_RM$description), 1, 0)


# Assign some meaning to time of day for Test
test_RM$morning <- ifelse(test_RM$hour<12 & test_RM$hour>4,1,0)
test_RM$afternoon <- ifelse(test_RM$hour>=12 & test_RM$hour<=17,1,0)
test_RM$evening <- ifelse(test_RM$hour>17 | test_RM$hour<=4,1,0)

# Write both files
write_csv(train_RM, 'Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/train_RM.csv')
write_csv(test_RM, 'Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/test_RM.csv')
