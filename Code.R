##########################################################
#' Code.R 
#' Author: Yuanhang Zhang (Charles)

#' Please refer to the Report.pdf for further explanation
##########################################################

##########################################################
# Load packages and import data
##########################################################

# Download and install the packages if not directly available
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("rlang", repos = "http://cran.us.r-project.org")

# Load the packages
library(tidyverse)
library(dplyr)
library(caret)
library(broom)
library(readxl)

# Read the data from Kaggle

dat <- read_csv('./data/train.csv')
final_holdout_test <- read_csv('./data/test.csv')

##########################################################
# Exploratory Analysis and Pre-processing
##########################################################

# Explore the size of data

tibble("# Rows" = nrow(dat), "# Columns" = ncol(dat))

names(dat) # Print out the column names

# Explore the size of final_holdout_test

tibble("# Rows" = nrow(final_holdout_test), 
       "# Columns" = ncol(final_holdout_test))

### Date

# List all the columns that in dat but not in final_holdout_test

tibble('Col_names' = names(dat)) %>% 
  filter(!Col_names %in% names(final_holdout_test))

### Orders

# Derive a plot of order number distribution

dat %>% ggplot(aes(x = orders)) +
  geom_histogram(fill = "grey", col = "black") +
  geom_vline(aes(xintercept = mean(orders)), col = "firebrick") + # Add a line of average orders
  annotate(
    geom = "text", x = 8000, y = 1200, 
    label = paste("Mean = ", round(mean(dat$orders), 2)), # Add text annotations
    col = "firebrick") +
  theme_linedraw()

summary(dat$orders) # Summarize the characteristics

### Indicators in both sets
# List all the columns that in both sets

tibble('Col_names' = names(dat)) %>% 
  filter(Col_names %in% names(final_holdout_test))

## Warehouses

# Derive the average orders for every warehouse

warehouse_tab <- dat %>% group_by(warehouse) %>% 
  summarise(total_orders = sum(orders), average_orders = mean(orders), entries = n()) %>% 
  arrange(desc(total_orders))

warehouse_tab

# Check if the name of warehouses in both sets are the same

sum(!unique(final_holdout_test$warehouse) %in% warehouse_tab$warehouse)

# Derive a plot to depict distributions in different warehouses

dat %>% ggplot(aes(
  x = reorder(warehouse, orders, FUN = mean), 
  y = orders)) +
  geom_boxplot() +
  theme_linedraw()

## Date

class(dat$date)

# Compare the time range of dat and final_holdout_test

tibble("data_set" = c("dat", "final_holdout_test"),
       "Min_date" = c(min(dat$date), min(final_holdout_test$date)),
       "Max_date" = c(max(dat$date), max(final_holdout_test$date))
)

# Derive a plot to depict distributions in different week days

dat %>% mutate(wday = factor(wday(date))) %>% 
  ggplot(aes(x = wday, y = orders, group = wday)) +
  geom_boxplot()

# Create a factorized week day parameter for both sets

dat_n <- dat %>% mutate(wday = factor(wday(date)))
final_holdout_test_n <- final_holdout_test %>% 
  mutate(wday = factor(wday(date)))

# Derive a plot to depict distributions in different months

dat %>% mutate(month = factor(month(date))) %>% 
  ggplot(aes(x = month, y = orders, group = month)) +
  geom_boxplot()

# Create a factorized month parameter for both sets

dat_n <- dat_n %>% mutate(month = factor(month(date)))
final_holdout_test_n <- final_holdout_test_n %>% 
  mutate(month = factor(month(date)))

# Derive a plot to depict distributions in different years

dat %>% mutate(year = factor(year(date))) %>% 
  ggplot(aes(x = year, y = orders, group = year)) +
  geom_boxplot()

# Create a year parameter for both sets

dat_n <- dat_n %>% mutate(year = year(date))
final_holdout_test_n <- final_holdout_test_n %>% 
  mutate(year = year(date))

## Holidays

# Derive a plot to depict distributions in normal days and holidays

dat %>% mutate(holiday = factor(holiday), 
               orders_log = log(orders)) %>% 
  ggplot(aes(x = holiday, y = orders_log, group = holiday)) +
  geom_boxplot()

# List all existent holiday names

unique(dat$holiday_name)

# List all named holidays without actual days off

dat %>% filter(holiday == 0 & !is.na(holiday_name)) %>% 
  distinct(holiday_name)

# List all holidays without names

dat %>% filter(holiday == 1 & is.na(holiday_name)) %>% 
  distinct(date)

# Create an 'nominal holiday' identifier

dat_nom <- dat %>% filter(holiday == 0) %>% 
  mutate(nominal_holiday = factor(ifelse(is.na(holiday_name), 1, 0)))

# Use ANOVA to test the relevance of 'nominal holiday'

anova_result <- aov(dat_nom$orders ~ dat_nom$nominal_holiday)
summary(anova_result)

# Name the 'unamed holidays'

dat_n <- dat_n %>% mutate(holiday_name = 
                            ifelse(holiday == 1 & is.na(holiday_name),
                                   "Unamed holiday",
                                   holiday_name
                            )
)

final_holdout_test_n <- final_holdout_test_n %>% mutate(holiday_name = 
                                                          ifelse(holiday == 1 & is.na(holiday_name),
                                                                 "Unamed holiday",
                                                                 holiday_name
                                                          )
)

# first extract the list of nominal holidays
nominal_holidays <- dat %>% filter(holiday == 0 & 
                                     !is.na(holiday_name)) %>% 
  distinct(holiday_name) %>% 
  .$holiday_name

# Also calculate the average order number
avg_orders <- mean(dat$orders)

# # Derive a plot to depict distributions in different holidays
dat_n %>% filter(!is.na(holiday_name) &
                   !holiday_name %in% nominal_holidays) %>% # filter out nominal holidays
  mutate(holiday_abb = paste(str_sub(holiday_name, 1, 5), '...')) %>%  
  # Create abbreviations for holidays for better labeling
  ggplot() +
  geom_boxplot(aes(x = reorder(holiday_abb, orders, FUN = mean), # reorder the holidays
                   y = orders)) +
  geom_hline(aes(yintercept = avg_orders), col = "firebrick") + 
  # add a line to highlight the average orders
  annotate(
    geom = "text", x = 3, y = 6000, 
    label = paste("Mean = ", round(mean(avg_orders), 2)),
    col = "firebrick") + # give text annotations
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = "Holidays",
    y = "Orders"
  )

# Create a table to show the average in different holidays

holiday_avg <- dat_n %>% filter(!is.na(holiday_name) &
                                  !holiday_name %in% nominal_holidays) %>% 
  group_by(holiday_name) %>% 
  summarise(avg_orders = mean(orders)) %>% 
  arrange(desc(avg_orders))

holiday_high_list <- holiday_avg$holiday_name[1:5] # Extract the list of high demand holidays
print(holiday_high_list)

l <- length(holiday_avg$holiday_name)
holiday_low_list <- holiday_avg$holiday_name[(l-3):l] # Extract the list of low demand holidays
print(holiday_low_list)

# Create high(low) demand holiday identifier for both sets

dat_n <- dat_n %>% mutate(
  holiday_high = factor(ifelse(
    holiday_name %in% holiday_high_list, 1, 0
  )),
  holiday_low = factor(ifelse(
    holiday_name %in% holiday_low_list, 1, 0
  ))
)

final_holdout_test_n <- final_holdout_test_n %>% mutate(
  holiday_high = factor(ifelse(
    holiday_name %in% holiday_high_list, 1, 0
  )),
  holiday_low = factor(ifelse(
    holiday_name %in% holiday_low_list, 1, 0
  ))
)

## Shops Closed

unique(dat$shops_closed) # see the unique values of shops_closed

# Derive a plot to depict distributions in shops_closed days or not

dat %>% mutate(shops_closed = factor(shops_closed)) %>% 
  ggplot(aes(x = shops_closed, y = orders)) +
  geom_boxplot() + 
  labs(
    x = "Shops closed",
    y = "Orders"
  )

# Use ANOVA to test the relevance of shops_closed

anova_result <- aov(dat$orders ~ factor(dat$shops_closed))
summary(anova_result)

# Factorize shops_closed in both sets

dat_n <- dat_n %>% mutate(shops_closed = factor(shops_closed))
final_holdout_test_n <- final_holdout_test_n %>% 
  mutate(shops_closed = factor(shops_closed))

## School holidays
# List all the unique values of (winter_)school_holidays

print(unique(dat$winter_school_holidays))
print(unique(dat$school_holidays))

# Derive a plot to depict distributions in (winter_)school_holidays or not

dat %>% mutate(winter_school_holidays = factor(winter_school_holidays)) %>% 
  ggplot(aes(x = winter_school_holidays, y = orders)) +
  geom_boxplot() + 
  labs(
    x = "Winter School Holidays",
    y = "Orders"
  )

dat %>% mutate(school_holidays = factor(school_holidays)) %>% 
  ggplot(aes(x = school_holidays, y = orders)) +
  geom_boxplot() + 
  labs(
    x = "School Holidays",
    y = "Orders"
  )

# Factorize (winter_)school_holidays in both sets

dat_n <- dat_n %>% mutate(school_holidays = factor(school_holidays))
final_holdout_test_n <- final_holdout_test_n %>% 
  mutate(school_holidays = factor(school_holidays))

dat_n <- dat_n %>% mutate(winter_school_holidays = factor(winter_school_holidays))
final_holdout_test_n <- final_holdout_test_n %>% 
  mutate(winter_school_holidays = factor(winter_school_holidays))

### Indicators only in dat

# List all the columns that in dat but not in final_holdout_test

tibble('Col_names' = names(dat)) %>% 
  filter(!Col_names %in% names(final_holdout_test))

# Explore the occurance of specific variables

dat %>% filter(shutdown == 1) %>% nrow()

dat %>% filter(mini_shutdown == 1) %>% nrow()

dat %>% filter(blackout == 1) %>% nrow()

dat %>% filter(frankfurt_shutdown == 1) %>% nrow()

# Distribution of mov_change

dat %>% ggplot(aes(x = date, y = mov_change, colour = warehouse)) +
  geom_point()

# Distribution of user_activity_1

dat %>% ggplot(aes(x = date, y = user_activity_1, colour = warehouse)) +
  geom_point()

# Distribution of user_activity_2

dat %>% ggplot(aes(x = date, y = user_activity_2, colour = warehouse)) +
  geom_point()

# Derive a plot to depict relevance between user_activity_2 and orders

dat %>% ggplot(aes(x = user_activity_2, y = orders)) +
  geom_point()

### Abnormality

# Extract the abnormal entries

dat_n %>% filter(warehouse == 'Prague_1') %>% 
  arrange(desc(orders)) %>% 
  slice(1:2)

# Drop the abnormal entries

dat_n <- dat_n %>% filter(id != "Prague_1_2023-12-23" &
                            id != "Prague_1_2023-12-22")

### Prepare the data sets
# Keep the indicators we need in the final set

dat_f <- dat_n %>% select(warehouse, wday, month, year, holiday_high, holiday_low, 
                          shops_closed, school_holidays, winter_school_holidays, orders)

final_holdout_test_f <- final_holdout_test_n %>% select(warehouse, wday, month, year, 
                                                        holiday_high, holiday_low, shops_closed, school_holidays, winter_school_holidays)

# remove NA lines

dat_f <- na.omit(dat_f)

### Creating training set and data set
# Randomly create training set and test set

set.seed(1) # To ensure the code and results are reproducible
test_indices <- createDataPartition(y = dat_f$orders, times = 1, p = 0.1, list = FALSE)

test <- dat_f[test_indices,]
train <- dat_f[-test_indices,]

##########################################################
# Modelling
##########################################################

# Model 1: kNN without predicted user_activity_2

#set cross validation methods for all ML algorithms
control <- trainControl(method = "cv", number = 10, p = .9) 
# train kNN algorithm
train_knn <- train(orders ~ ., method = "knn",
                   data = train,
                   tuneGrid = data.frame(k = seq(5, 20)),
                   trControl = control)
train_knn

y_hat_knn <- predict(train_knn, test) # predict order numbers in test set

# define a function to calculate MAPE

MAPE <- function(forecast, actual){
  100 * sum(abs((actual - forecast) / actual)) / length(forecast)
}

# calculate MAPE of Model 1

mape_knn <- MAPE(y_hat_knn, test$orders)
mape_knn

# Create data sets if we want to predict user_activity_2 first

dat_f_2 <- dat_n %>% select(warehouse, wday, month, year, holiday_high, holiday_low, 
                            shops_closed, school_holidays, winter_school_holidays, orders, user_activity_2)

dat_f_2 <- na.omit(dat_f_2)

set.seed(1) # To ensure the code and results are reproducible
test_indices_2 <- createDataPartition(y = dat_f_2$orders, times = 1, p = 0.1, list = FALSE)

test_2 <- dat_f_2[test_indices,]
train_2 <- dat_f_2[-test_indices,]

# Model 2: kNN with predicted user_activity_2

# First predict user_activity_2
train_usract_knn <- train(user_activity_2 ~ year + month + wday + holiday_high + 
                            holiday_low + warehouse,
                          method = "knn",
                          data = train_2,
                          tuneGrid = data.frame(k = seq(5, 20)),
                          trControl = control)
train_usract_knn

# put the predicted user_activity in the test set
user_activity_2_hat_knn <- predict(train_usract_knn, test)

test_2_knn <- test %>% mutate(user_activity_2 = user_activity_2_hat_knn)

# Then build a model to predict order number
train_knn_2 <- train(orders ~ ., method = "knn",
                     data = train_2,
                     tuneGrid = data.frame(k = seq(60, 70)),
                     # the range of the tune grid is changed according to results from multiple trials
                     trControl = control)
train_knn_2

# calculate MAPE of Model 2

y_hat_knn_2 <- predict(train_knn_2, test_2_knn)

mape_knn_2 <- MAPE(y_hat_knn_2, test_2_knn$orders)
mape_knn_2

# Model 3: Random Forest without predicted user_activity_2

train_rf <- train(orders ~ ., method = "Rborist",
                  data = train,
                  tuneGrid = data.frame(predFixed = 2, minNode = seq(1:10)),
                  trControl = control)
train_rf

# calculate MAPE of Model 3

y_hat_rf <- predict(train_rf, test)

mape_rf <- MAPE(y_hat_rf, test$orders)
mape_rf

# Model 4: Random Forest with predicted user_activity_2

# First predict user_activity_2
train_usract_rf <- train(user_activity_2 ~ year + month + wday + holiday_high + holiday_low + warehouse,
                         method = "Rborist",
                         data = train_2,
                         tuneGrid = data.frame(predFixed = 2, minNode = seq(1:10)),
                         trControl = control)
train_usract_rf

# put the predicted user_activity in the test set
user_activity_2_hat_rf <- predict(train_usract_rf, test)
test_2_rf <- test %>% mutate(user_activity_2 = user_activity_2_hat_rf)

# Then build a model to predict order number

train_rf_2 <- train(orders ~ ., method = "Rborist",
                    data = train_2,
                    tuneGrid = data.frame(predFixed = 2, minNode = seq(1:10)),
                    # the range of the tune grid is changed according to results from multiple trials
                    trControl = control)
train_rf_2

# calculate MAPE of Model 4

y_hat_rf_2 <- predict(train_rf_2, test_2_rf)

mape_rf_2 <- MAPE(y_hat_rf_2, test_2_rf$orders)
mape_rf_2

##########################################################
# Results and final modelling
##########################################################

# List all the models and their MAPEs

tibble(
  "Model" = c("kNN", "kNN (with predicted user activity)", 
              "Random forest", "Random forest (with predicted user activity)"),
  "MAPE" = c(mape_knn, mape_knn_2, mape_rf, mape_rf_2)
)

# Finally train the kNN model on dat

# First predict user_activity_2
train_usract_knn <- train(user_activity_2 ~ year + month + wday + holiday_high + holiday_low + warehouse,
                          method = "knn",
                          data = dat_f_2,
                          tuneGrid = data.frame(k = seq(5, 20)),
                          trControl = control)
train_usract_knn

# put the predicted user_activity in the final_holdout_test
user_activity_2_hat_knn <- predict(train_usract_knn, final_holdout_test_f)

final_holdout_test_f <- final_holdout_test_f %>% mutate(user_activity_2 = user_activity_2_hat_knn)

# Then build a model to predict order number

train_knn_f <- train(orders ~ ., method = "knn",
                     data = dat_f_2,
                     tuneGrid = data.frame(k = seq(80, 90)),
                     # the range of the tune grid is changed according to results after multiple trials
                     trControl = control)
train_knn_f

y_hat_knn_f <- predict(train_knn_f, final_holdout_test_f)

# Create a file to submit following the guidance from Kaggle

submit_file <- final_holdout_test %>% mutate(orders = y_hat_knn_f) %>% 
  select(id, orders)

# Write the data frame to a CSV file
write.csv(submit_file, file = "./sumbit.csv", row.names = FALSE)




