library(tidyverse)
library(lubridate)
options(scipen=999)

setwd("/Users/bentrevino/Dropbox/Big\ Data\ Analyst\ -\ Dev\ League/repos/RetroSpeckt/")
train <- read_csv("train_RM.csv")
test <- read_csv("test_RM.csv")

#subm <- read_csv("../input/hawaiiml-data/sample_submission.csv") 

# combine test and train data
full <- bind_rows(train, test)

# format dates
full$date <- mdy(full$date) 
full$dayofweek <- weekdays(full$date)
full$week  <- week(full$date)
full$month <- month(full$date)
full$year  <- year(full$date)
full$time <- as.numeric(full$time)

# Convert to factors 
shouldBeCategorical <- c('country','dayofweek')
for(v in shouldBeCategorical) {full[[v]] <- as.factor(full[[v]])}

# log transform target
full$quantity <- log1p(full$quantity)

# split back into train and test, remove id and date column
train <- full[1:nrow(train),-(1:2)]
test <- full[-(1:nrow(train)),-(1:2)]

# Run h2o's AutoML --------------------------------------------------------------

library(h2o)
h2o.init(nthreads = -1)

train <- as.h2o(train)

aml <- h2o.automl(y = "quantity",
                  training_frame = train,
                  stopping_metric = "RMSE",  # tried using RMSLE but h2o's metrics weren't good
                  max_runtime_secs = 3600)  # 1 hour

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

# The winning model
aml@leader

# Create predictions
test <- as.h2o(test)
pred <- h2o.predict(aml@leader, test)

subm <- read_csv("submission.csv") 
subm$quantity <- as.vector(expm1(pred))
write.csv(subm, "h2o_autoML.csv", row.names=FALSE)
