# Tori's code for Train
#created mean for unit price - going to do min and max as well.
train_RM %>% group_by(stock_id) %>% summarise(min(unit_price))

# mutated the data to add mean  of unit price to the dataset. 
train_RM = train_RM %>%
  group_by(stock_id) %>% mutate(mean_price = mean(unit_price))

# mutated the data to add max of unit price to the dataset. 
train_RM = train_RM %>%
  group_by(stock_id) %>% mutate(max_price = max(unit_price))

# mutated the data to add min of unit price to the dataset. 
train_RM = train_RM %>%
  group_by(stock_id) %>% mutate(min_price = min(unit_price))

# mutated the data to add median of unit price to the dataset. 
train_RM = train_RM %>%
  group_by(stock_id) %>% mutate(median_price = median(unit_price))

# subtracted unit price from median price to get the difference. 
train_RM = train_RM %>% mutate(median_diff = unit_price - median_price)

# subtracted unit price from mean price to get the difference. 
train_RM = train_RM %>% mutate(mean_diff = unit_price - mean_price)

# subtracted unit price from min price to get the difference. 
train_RM = train_RM %>% mutate(min_diff = unit_price - min_price)

# subtracted max price from unit price to get the difference. 
train_RM = train_RM %>% mutate(max_diff = max_price - unit_price)



# Tori's code for Test
#created mean for unit price - going to do min and max as well.
test_RM %>% group_by(stock_id) %>% summarise(min(unit_price))

# mutated the data to add mean  of unit price to the dataset. 
test_RM = test_RM %>%
  group_by(stock_id) %>% mutate(mean_price = mean(unit_price))

# mutated the data to add max of unit price to the dataset. 
test_RM = test_RM %>%
  group_by(stock_id) %>% mutate(max_price = max(unit_price))

# mutated the data to add min of unit price to the dataset. 
test_RM = test_RM %>%
  group_by(stock_id) %>% mutate(min_price = min(unit_price))

# mutated the data to add median of unit price to the dataset. 
test_RM = test_RM %>%
  group_by(stock_id) %>% mutate(median_price = median(unit_price))

# subtracted unit price from median price to get the difference. 
test_RM = test_RM %>% mutate(median_diff = unit_price - median_price)

# subtracted unit price from mean price to get the difference. 
test_RM = test_RM %>% mutate(mean_diff = unit_price - mean_price)

# subtracted unit price from min price to get the difference. 
test_RM = test_RM %>% mutate(min_diff = unit_price - min_price)

# subtracted max price from unit price to get the difference. 
test_RM = test_RM %>% mutate(max_diff = max_price - unit_price)