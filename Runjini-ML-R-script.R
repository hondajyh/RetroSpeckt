library(readr)
train <- read_csv("Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/train.csv")
test <- read_csv("Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/test.csv")

library(tidyverse)
library(lubridate)
library(ggplot2)

summary(train)

train$UK <- ifelse(train$country == 'united kingdom', 1, 0)
head(train)

test$UK <- ifelse(test$country == 'united kingdom', 1, 0)
head(test)

train2 <- filter(train, quantity < 60000)
head(train2)
max(train2$quantity)
plot(train2$UK, train2$quantity)

train2$log_quantity <- log1p(train2$quantity)
head(train2)

plot(train2$customer_id, train2$log_quantity)

train2$date2 <- mdy(train2$date)
train2$time2 <- hms(train2$time) #This doesn't look very useful.  Although maybe the hour could be parsed as it's own column and histogrammed?
train2$hour <- hour(train2$time)
head(train2)
summary(train2)
hist(train2$hour)

train2$desc_set <- ifelse(grepl("set",train2$description), 1, 0)
train2$desc_bag <- ifelse(grepl("bag",train2$description), 1, 0)
train2$desc_red <- ifelse(grepl("red",train2$description), 1, 0)
View(train2)

plot(train2$desc_set, train2$quantity)

test$date2 <- mdy(test$date)
test$time2 <- hms(test$time) 
test$hour <- hour(test$time)
head(test)
summary(test)

test$desc_set <- ifelse(grepl("set",test$description), 1, 0)
test$desc_bag <- ifelse(grepl("bag",test$description), 1, 0)
test$desc_red <- ifelse(grepl("red",test$description), 1, 0)
View(test)

# Assign some meaning to time of day
train2$morning <- ifelse(train2$hour<12 & train2$hour>4,1,0)
train2$afternoon <- ifelse(train2$hour>=12 & train2$hour<=17,1,0)
train2$evening <- ifelse(train2$hour>17 | train2$hour<=4,1,0)

library(rpart) # for regression trees
library(randomForest) # for random forests
print('Libraries loaded.')

# train a decision tree based on our dataset 
fit <- rpart(log_quantity ~ customer_id + invoice_id + UK + date2 + hour + desc_set + desc_bag + desc_red, data =  train2)
print('Fit complete.')

# plot our regression tree 
plot(fit, uniform=TRUE)
# add text labels & make them 60% as big as they are by default
text(fit, cex=.6)

print("Making predictions for the following 5 IDs:")
print(head(train2))

print("The predictions are")
print(predict(fit, head(train2)))

print("Actual log quantities")
print(head(train2$log_quantity))

print("Converted quantities")
train2$converted_qty <- expm1(train2$log_quantity)
print(head(train2$converted_qty))

# Now let's split our data so that 30% is in the test set and 70% is in the training set.  Load modelr library.
library(modelr)
splitData <- resample_partition(train2, c(test = 0.3, train = 0.7))

# how many cases are in test & training set? 
lapply(splitData, dim)

# fit a new model to our training set
fit2 <- rpart(log_quantity ~ customer_id + invoice_id + UK + date2 + hour + desc_set + desc_bag + desc_red, data = splitData$train2)
print('Fit 2 complete.')

print("Making predictions for the following 5 IDs:")
print(head(train2))

print("The predictions are")
print(predict(fit2, head(train2)))

print("Actual log quantities")
print(head(train2$log_quantity))

print("Converted quantities")
train2$converted_qty <- expm1(train2$log_quantity)
print(head(train2$converted_qty))

# fit a random forest model to our training set
#require(randomForest)
#fitRandomForest <- randomForest(log_quantity ~ customer_id + invoice_id + UK + date2 + hour + desc_set + desc_bag + desc_red, data = splitData$train)
#print('Random Forest fit complete.')

predict_log_quantity <- predict(fit, test)
predict_quantity <- expm1(predict_log_quantity)


predict2_log_quantity <- predict(fit2, test)
predict2_quantity <- expm1(predict2_log_quantity)

# create a dataframe with our results
my_submission <- data_frame('Id' = test$id, 'Quantity' = predict_quantity)

# save our file
write_csv(my_submission, 'Desktop/GitHub/BigDataAnalyst_ProjectDocumentation/Sprint09alt_Machine_Learning_Hawaii_Kaggle_Competition/RM2_submission.csv')
