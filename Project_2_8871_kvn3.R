install.packages("dplyr")
install.packages("pacman")
install.packages("pacman", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("remotes")
library(remotes)
install_github("packageauthor/foobarbaz")
install_bitbucket("packageauthor/foobarbaz")
install_gitorious("packageauthor/foobarbaz")

mypackages = c("leaps", "glmnet")   
tmp = setdiff(mypackages, rownames(installed.packages())) 
if (length(tmp) > 0) install.packages(tmp)

library(lubridate)
library(tidyverse)
library(dplyr)

# read raw data and extract date column
#train_raw <- readr::read_csv(unz('train.csv.zip', 'train.csv'))
train_raw <- readr::read_csv('/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 2/all/train.csv')

train_dates <- train_raw$Date

# training data from 2010-02 to 2011-02, i.e. one year
start_date <- ymd("2010-02-01")
end_date <- start_date %m+% months(13)

# split dataset into training / testing
train_ids <- which(train_dates >= start_date & train_dates < end_date)
train = train_raw[train_ids, ]
test = train_raw[-train_ids, ]

# write the training results to a file
readr::write_csv(train, 'train.csv')

# Create the test.csv 
# Removes weekly sales and adds model pred columns.
test %>% 
  select(-Weekly_Sales) %>% 
  mutate(Weekly_Pred1 = 0, Weekly_Pred2 = 0, Weekly_Pred3 = 0) %>% 
  readr::write_csv('test.csv')

# create 10-fold time-series CV
num_folds <- 10
test_dates <- train_dates[-train_ids]

# month 1 --> 2011-03, and month 20 --> 2012-10.
# Fold 1 : month 1 & month 2, Fold 2 : month 3 & month 4 ...
for (i in 1:num_folds) {
  # filter fold for dates
  start_date <- ymd("2011-03-01") %m+% months(2 * (i - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (i - 1))
  test_fold <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # write fold to a file
  readr::write_csv(test_fold, paste0('fold_', i, '.csv'))
}

## MISSING VALUES N/A Replace with 0 SEE

# SVD FUNCTION
preprocess.svd = function(train, n.comp){
  train[is.na(train)] = 0  # replace missing values by zero
  z = svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s = diag(z$d[1:n.comp])
  train[, 2:ncol(train)] = z$u %*% s %*% t(z$v)
  train
}

dept = sort(unique(c(train$Dept, test$Dept)))
n.dept = length(dept) # 81
d=1; 
n.comp = 12
train.tmp = train[train$Dept==dept[d], c("Store", "Wk", "Weekly_Sales"),]
tr.d.before = cast(train.tmp, Wk ~ Store, mean)  
tr.d.after = preprocess.svd(tr.d.before, n.comp)  

i=2
tmp = data.frame(Wk=rep(0:51, 2), 
                 Wk_Sales=c(tr.d.before[, (i+1)], tr.d.after[, (i+1)]),
                 ID = rep(c("Before", "After"), each=52))
ggplot(tmp, aes(x=Wk, y=Wk_Sales, colour=ID, group=ID)) + 
  geom_line()





################ Load Environment ##################
# clean workspace
rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse"
)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  if (num_model == 1) {
    test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  } else if(num_model == 2) {
    test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  } else {
    test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  }
  
  test_month
}


# update forecasts in the global test dataframe
update_test <- function(test_month) {
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}


##### Model Building Functions #####

# Forecasts out the last observation in the training data
naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- snaive(store_ts, num_forecasts)$mean
  }
  test_ts
}


##### Prediction Loop #####

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_test)
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # Dates are not the same across months!
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    ###### Model Fitting / Forecasting ######
    
    # naive forecast
    f_naive <- naive_model(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_naive, dept, 1)
    
  }
  
  # update global test dataframe
  update_test(test_month)
}
