library(tidyverse)
library(lubridate)
options(scipen=999)

train_RM2 <- read_csv("Desktop/GitHub/RetroSpeckt/train_RM2.csv")
test_RM2 <- read_csv("Desktop/GitHub/RetroSpeckt/test_RM2.csv")
subm <- read_csv("Desktop/GitHub/RetroSeckt/sample_submission_RM.csv") 

# combine test and train data
full <- bind_rows(train_RM2, test_RM2)

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
train_RM2 <- full[1:nrow(train_RM2),-(1:2)]
test_RM2 <- full[-(1:nrow(train_RM2)),-(1:2)]

# Run h2o's AutoML --------------------------------------------------------------
install.packages("RCurl")
install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)

train_RM2 <- as.h2o(train_RM2)

aml <- h2o.automl(y = "quantity",
                  training_frame = train_RM2,
                  stopping_metric = "RMSE",  # tried using RMSLE but h2o's metrics weren't good
                  max_runtime_secs = 3600)  # 1 hour

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

# The winning model
aml@leader

# Create predictions
test_RM2 <- as.h2o(test)
pred <- h2o.predict(aml@leader, test)
subm$quantity <- as.vector(expm1(pred))
write.csv(subm, "h2o_autoML_RM.csv", row.names=FALSE)