###########################
## STAT 542 PROJECT 3 - LENDING CLUB DATA
## Karthik Venkata 

#PART 1-Installing and loading R packages

install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("caret")
install.packages("ROSE")

#start.time = Sys.time()

library(gmodels)
library(glmnet)
library(lubridate)
library(plyr)
library(ggplot2)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
library(ROSE)

#loan_data <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 3/loan_stat542.csv")
#testid <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 3/Project3_test_id.csv")

## Creating the Test-Train Splits
#train <- subset(loan_data,!(id%in%testid[,1]))
#test <- subset(loan_data,(id%in%testid[,1]))

 # train2 <- subset(loan_data,!(id%in%testid[,2]))
 # test2 <- subset(loan_data,(id%in%testid[,2]))

 # train3 <- subset(loan_data,!(id%in%testid[,3]))
  #test3 <- subset(loan_data,(id%in%testid[,3]))

#write.csv(test, "test.csv")
#write.csv(train, "train.csv")

  #write.csv(test2, "test2.csv")
  #write.csv(train2, "train2.csv")
 # 
  #write.csv(test3, "test3.csv")
  #write.csv(train3, "train3.csv")

### Read the gicen train/test .csv files
train <- read.csv("train.csv")
 #train2 <- read.csv("train2.csv")
  #train3 <- read.csv("train3.csv")
test <- read.csv("test.csv")
  #test2 <- read.csv("test2.csv")
  #test3 <- read.csv("test3.csv")


## Bind the Train and Test columns
total = rbind(train,test)
 #total2 = rbind(train2,test2)
# total3 = rbind(train3,test3)

# emp_length - Convert to numeric
total$emp_length <- as.numeric(total$emp_length) # Converting into numeric
#total2$emp_length <- as.numeric(total2$emp_length)
 #total3$emp_length <- as.numeric(total3$emp_length)

########### Factoring of variables (For all Categorical variables)
## home_ownership, verification_status, purpose, addr_state, application_type, initial_list_status, application_type
## earliest_cr_line check referecne strating date from JAn 07, 

## Drop title and emp_title
##drops <- c("title","emp_title")
# Drop more variables which have multiple factor variables

## earliest_cr_line - Take difference in time duration from 2007-08-01 and substitute accordingly
total$earliest_cr_line[is.na(total$earliest_cr_line)] = "2007-08-01"
month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
den = c("01","02","03","04","05","06","07","08","09","10","11","12")
for(i in 1:length(month)){
   total$earliest_cr_line = gsub(month[i],den[i],total$earliest_cr_line)
 }
total$earliest_cr_line = paste0("01-",total$earliest_cr_line,sep="")
total$earliest_cr_line = as.Date(total$earliest_cr_line, "%d-%m-%Y")
latest_month = "2015-04-01"
 # #total$month_earliest_cr = as.numeric(abs(difftime(total$earliest_cr_line, latest_month, units = "weeks")))
total$earliest_cr_line = as.numeric(abs(difftime(total$earliest_cr_line, latest_month, units = "weeks")))

#  ######## TRAIN2
# total2$earliest_cr_line[is.na(total2$earliest_cr_line)] = "2007-08-01"
#   month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#   den = c("01","02","03","04","05","06","07","08","09","10","11","12")
#   for(i in 1:length(month)){
#     total2$earliest_cr_line = gsub(month[i],den[i],total2$earliest_cr_line)
#   }
#   total2$earliest_cr_line = paste0("01-",total2$earliest_cr_line,sep="")
#   total2$earliest_cr_line = as.Date(total2$earliest_cr_line, "%d-%m-%Y")
#   latest_month = "2015-04-01"
#   #total$month_earliest_cr = as.numeric(abs(difftime(total$earliest_cr_line, latest_month, units = "weeks")))
#   total2$earliest_cr_line = as.numeric(abs(difftime(total2$earliest_cr_line, latest_month, units = "weeks"))) 
 
 ######## TRAIN3
 # total3$earliest_cr_line[is.na(total3$earliest_cr_line)] = "2007-08-01"
 # 
 # month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
 # den = c("01","02","03","04","05","06","07","08","09","10","11","12")
 # for(i in 1:length(month)){
 #   total3$earliest_cr_line = gsub(month[i],den[i],total3$earliest_cr_line)
 # }
 # total3$earliest_cr_line = paste0("01-",total3$earliest_cr_line,sep="")
 # total3$earliest_cr_line = as.Date(total3$earliest_cr_line, "%d-%m-%Y")
 # latest_month = "2015-04-01"
 # #total$month_earliest_cr = as.numeric(abs(difftime(total$earliest_cr_line, latest_month, units = "weeks")))
 # total3$earliest_cr_line = as.numeric(abs(difftime(total3$earliest_cr_line, latest_month, units = "weeks")))  
 # 
 
 
#### The variables title, emp_title, zip_code, grade have been dropped. Term converted to integer. home_ownership - Relevant replacement of text data
total$title <- NULL
total$emp_title <- NULL
total$zip_code <- NULL
total$grade <- NULL
total$term <- ifelse(total$term=='36 months', 36, 60)
total$home_ownership[total$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER' 
total$home_ownership <- factor(total$home_ownership, levels = unique(total$home_ownership))

# #### TRAIN2
#  total2$title <- NULL
#  total2$emp_title <- NULL
#  total2$zip_code <- NULL ## DROp??
#  #total1$issue_d <- NULL
#  total2$grade <- NULL
#  #total$fico_range_high <- NULL
#  #total$fico_range_low <- NULL
#  #total1$id <- NULL
#  #total1$addr_state <- NULL DROP??
#  #total$earliest_cr_line <- NULL
#  total2$term <- ifelse(total2$term=='36 months', 36, 60)
#  total2$home_ownership[total2$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER' #### NEEDED?
#  total2$home_ownership <- factor(total2$home_ownership, levels = unique(total2$home_ownership))

# ######### TRAIN3
# total3$title <- NULL
# total3$emp_title <- NULL
# total3$zip_code <- NULL ## DROp??
# #total1$issue_d <- NULL
# total3$grade <- NULL
# #total$fico_range_high <- NULL
# #total$fico_range_low <- NULL
# #total1$id <- NULL
# #total1$addr_state <- NULL DROP??
# #total$earliest_cr_line <- NULL
# total3$term <- ifelse(total3$term=='36 months', 36, 60)
# total3$home_ownership[total3$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER' #### NEEDED?
# total3$home_ownership <- factor(total3$home_ownership, levels = unique(total3$home_ownership))


#total1$emp_length <- 
#total1$earliest_cr_line <- as.numeric(total1$earliest_cr_line)

##### Variables fico_range_high and fico_range_low - Imputation using median values
total$fico_range_high <- as.numeric(total$fico_range_high)
index.NA <- which(is.na(total$fico_range_high))
total$fico_range_high[index.NA] <- median(total$fico_range_high, na.rm = TRUE)
anyNA(total$fico_range_high) #No missing values
# 
total$fico_range_low <- as.numeric(total$fico_range_low)
index.NA <- which(is.na(total$fico_range_low))
total$fico_range_low[index.NA] <- median(total$fico_range_low, na.rm = TRUE)
anyNA(total$fico_range_low) #No missing values

# ##### TRAIN2
#  total2$fico_range_high <- as.numeric(total2$fico_range_high)
#  index.NA <- which(is.na(total2$fico_range_high))
#  total2$fico_range_high[index.NA] <- median(total2$fico_range_high, na.rm = TRUE)
#  anyNA(total2$fico_range_high) #No missing values
# # 
#  total2$fico_range_low <- as.numeric(total2$fico_range_low)
#  index.NA <- which(is.na(total2$fico_range_low))
#  total2$fico_range_low[index.NA] <- median(total2$fico_range_low, na.rm = TRUE)
#  anyNA(total2$fico_range_low) #No missing values

##### TRAIN3
# total3$fico_range_high <- as.numeric(total3$fico_range_high)
# index.NA <- which(is.na(total3$fico_range_high))
# total3$fico_range_high[index.NA] <- median(total3$fico_range_high, na.rm = TRUE)
# anyNA(total3$fico_range_high) #No missing values
# 
# total3$fico_range_low <- as.numeric(total3$fico_range_low)
# index.NA <- which(is.na(total3$fico_range_low))
# total3$fico_range_low[index.NA] <- median(total3$fico_range_low, na.rm = TRUE)
# anyNA(total3$fico_range_low) #No missing values


#  total2$title <- NULL
#  total2$emp_title <- NULL
#  total2$zip_code <- NULL
#  #total1$issue_d <- NULL
#  total2$grade <- NULL
#  total2$fico_range_high <- NULL
#  total2$fico_range_low <- NULL
# # #total1$id <- NULL
#  total2$addr_state <- NULL
#  total2$earliest_cr_line <- NULL
#  total2$term <- ifelse(total1$term=='36 months', 36, 60)
#  total2$home_ownership[total3$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER'
# # #total1$emp_length <- 
# #total1$earliest_cr_line <- as.numeric(total1$earliest_cr_line)

#  total3$title <- NULL
#  total3$emp_title <- NULL
#  total3$zip_code <- NULL
# # #total1$issue_d <- NULL
#  total3$grade <- NULL
#  total3$fico_range_high <- NULL  ## DON'T DROP?? DO as.numeric and use median/mean imputation
#  total3$fico_range_low <- NULL
# # #total1$id <- NULL
#  total3$addr_state <- NULL
#  total3$earliest_cr_line <- NULL
#  total3$term <- ifelse(total1$term=='36 months', 36, 60)
# # #total1$emp_length <- 
# # #total1$earliest_cr_line <- as.numeric(total1$earliest_cr_line)
# # 
#  total3$home_ownership[total3$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER'
# # total3$home_ownership <- factor(total3$home_ownership, levels = unique(total3$home_ownership))


# Convert term to integers
## Drop issue_d
# Convert emp_length to integers
# Replace the values ANY and NONE with OTHER in home_ownership ##########################
#total1$addr_state <- NULL
# NO  NEED OF DUMMY VARIABLES FOR GLM?? Categorical variables
# Winsorize
# home_ownership SEE ANY/OTHER Replacement


## NA value Median/Mean Imputation DO FOR ALL THREE TRAIN/TEST SPLITS??
## annual_inc, revol_util, emp_length
## 

## Do log transform for revol_bal and annual_inc variables
total$revol_bal = log(1 + total$revol_bal)
total$annual_inc = log(1 + total$annual_inc )

#total2$revol_bal = log(1 + total2$revol_bal)
#total2$annual_inc = log(1 + total2$annual_inc )
# 
 # total3$revol_bal = log(1 + total3$revol_bal)
 # total3$annual_inc = log(1 + total3$annual_inc )

### revol_util - Imputing missing values using median
index.NA <- which(is.na(total$revol_util)) #766 missing values
total$revol_util[index.NA] <- median(total$revol_util, na.rm = TRUE) #All missing values replaced by median 0.542
anyNA(total$revol_util) #No missing values
#  
   # index.NA <- which(is.na(total2$revol_util)) #766 missing values
   # total2$revol_util[index.NA] <- median(total2$revol_util, na.rm = TRUE) #All missing values replaced by median 0.542
   # anyNA(total2$revol_util) #No missing values
# # 
    # index.NA <- which(is.na(total3$revol_util)) #766 missing values
    # total3$revol_util[index.NA] <- median(total3$revol_util, na.rm = TRUE) #All missing values replaced by median 0.542
    # anyNA(total3$revol_util) #No missing values
# # #
#   index.NA <- which(is.na(total3$revol_util)) #766 missing values
#   total3$revol_util[index.NA] <- median(total3$revol_util, na.rm = TRUE) #All missing values replaced by median 0.542
#   anyNA(total3$revol_util) #No missing values

#annual_inc - Convert to numeric and impute missing values using median
class(total$annual_inc) #It is factor, should be numeric
total$annual_inc <- as.character(total$annual_inc) #Converting into character
total$annual_inc <- as.numeric(total$annual_inc) #Converting into numeric
is.numeric(total$annual_inc) # TRUE
anyNA(total$annual_inc) #4 missing values
index.NA <- which(is.na(total$annual_inc))
total$annual_inc[index.NA] <- median(total$annual_inc, na.rm = TRUE)
anyNA(total$annual_inc) #No missing values

####### TRAIN2
#  class(total2$annual_inc) #It is factor, should be numeric
#  total2$annual_inc <- as.character(total2$annual_inc) #Converting into character
#  total2$annual_inc <- as.numeric(total2$annual_inc) #Converting into numeric
#  is.numeric(total2$annual_inc) # TRUE
#  anyNA(total2$annual_inc) #4 missing values
#  index.NA <- which(is.na(total2$annual_inc))
#  total2$annual_inc[index.NA] <- median(total2$annual_inc, na.rm = TRUE)
# anyNA(total2$annual_inc) #No missing values

####### TRAIN3
# class(total3$annual_inc) #It is factor, should be numeric
# total3$annual_inc <- as.character(total3$annual_inc) #Converting into character
# total3$annual_inc <- as.numeric(total3$annual_inc) #Converting into numeric
# is.numeric(total3$annual_inc) # TRUE
# anyNA(total3$annual_inc) #4 missing values
# index.NA <- which(is.na(total3$annual_inc))
# total3$annual_inc[index.NA] <- median(total3$annual_inc, na.rm = TRUE)
# anyNA(total3$annual_inc) #No missing values

#   index.NA <- which(is.na(total2$annual_inc))
#   total2$annual_inc[index.NA] <- median(total2$annual_inc, na.rm = TRUE)
#   anyNA(total2$annual_inc) #No missing values
# # # 
#   index.NA <- which(is.na(total3$annual_inc))
#   total3$annual_inc[index.NA] <- median(total3$annual_inc, na.rm = TRUE)
#   anyNA(total3$annual_inc) #No missing values

# addNoAnswer <- function(x){
#   if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Answer")))
#   return(x)
# }
# 
# df <- as.data.frame(lapply(df, addNoAnswer))

## emp_length Replace NA with ABSENT
# addLevel <- function(x, newlevel=NULL) {
#   if (any(is.na(x))){
#     levels(x) = c(levels(x), newlevel)
#     x[is.na(x)] = newlevel
#   }
#   return(x)
# }
# 
# total1$emp_length <- addLevel(total1$emp_length, "ABSENT")

# emp_length - Imputing missing values using median values
index.NA <- which(is.na(total$emp_length))
total$emp_length[index.NA] <- median(total$emp_length, na.rm = TRUE)
anyNA(total$emp_length) #No missing values

 # index.NA <- which(is.na(total2$emp_length))
 # total2$emp_length[index.NA] <- median(total2$emp_length, na.rm = TRUE)
 # anyNA(total2$emp_length) #No missing values

  # index.NA <- which(is.na(total3$emp_length))
  # total3$emp_length[index.NA] <- median(total3$emp_length, na.rm = TRUE)
  # anyNA(total3$emp_length) #No missing values
# # 
#  index.NA <- which(is.na(total3$emp_length))
#  total3$emp_length[index.NA] <- median(total3$emp_length, na.rm = TRUE)
#  anyNA(total3$emp_length) #No missing values

# total2$emp_length <- addLevel(total2$emp_length, "ABSENT")
# total3$emp_length <- addLevel(total3$emp_length, "ABSENT")


# mort_acc - Imputing missing values using median values
index.NA <- which(is.na(total$mort_acc))
total$mort_acc[index.NA] <- median(total$mort_acc, na.rm = TRUE)
anyNA(total$mort_acc) #No missing values

   # index.NA <- which(is.na(total2$mort_acc))
   # total2$mort_acc[index.NA] <- median(total2$mort_acc, na.rm = TRUE)
   # anyNA(total2$mort_acc) #No missing values
# # 
  # index.NA <- which(is.na(total3$mort_acc))
  # total3$mort_acc[index.NA] <- median(total3$mort_acc, na.rm = TRUE)
  # anyNA(total3$mort_acc) #No missing values

# total2$mort_acc <- addLevel(total1$mort_acc, "ABSENT")
# total3$mort_acc <- addLevel(total1$mort_acc, "ABSENT")

## pub_rec_bankruptcies Pre-processing

#total1$pub_rec_bankruptcies <- addLevel(total1$pub_rec_bankruptcies, "ABSENT")

# pub_rec_bankruptcies - Imputing missing values using median values
index.NA <- which(is.na(total$pub_rec_bankruptcies))
total$pub_rec_bankruptcies[index.NA] <- median(total$pub_rec_bankruptcies, na.rm = TRUE)
anyNA(total$pub_rec_bankruptcies) #No missing values

   # index.NA <- which(is.na(total2$pub_rec_bankruptcies))
   # total2$pub_rec_bankruptcies[index.NA] <- median(total2$pub_rec_bankruptcies, na.rm = TRUE)
   # anyNA(total2$pub_rec_bankruptcies) #No missing values
# # 
  # index.NA <- which(is.na(total3$pub_rec_bankruptcies))
  # total3$pub_rec_bankruptcies[index.NA] <- median(total3$pub_rec_bankruptcies, na.rm = TRUE)
  # anyNA(total3$pub_rec_bankruptcies) #No missing values

# total2$pub_rec_bankruptcies <- addLevel(total2$pub_rec_bankruptcies, "ABSENT")
# total3$pub_rec_bankruptcies <- addLevel(total3$pub_rec_bankruptcies, "ABSENT")

## dti Pre-Processing

## dti - Imputing missing values using median values
index.NA <- which(is.na(total$dti))
total$dti[index.NA] <- median(total$dti, na.rm = TRUE)
anyNA(total$dti) #No missing values

   # index.NA <- which(is.na(total2$dti))
   # total2$dti[index.NA] <- median(total2$dti, na.rm = TRUE)
   # anyNA(total2$dti) #No missing values
# # 
  # index.NA <- which(is.na(total3$dti))
  # total3$dti[index.NA] <- median(total3$dti, na.rm = TRUE)
  # anyNA(total3$dti) #No missing values

# index.NA <- which(is.na(total2$dti))
# total2$dti[index.NA] <- median(total2$dti, na.rm = TRUE)
# anyNA(total2$dti) #No missing values
# 
# index.NA <- which(is.na(total3$dti))
# total3$dti[index.NA] <- median(total3$dti, na.rm = TRUE)
# anyNA(total3$dti) #No missing values

#total1$pub_rec_bankruptcies[index.NA] <- as.factor("ABSENT")

## Encoding loan_status as required in the question. Fully Paid - 0, Rest - 1
total$loan_status = ifelse(total$loan_status == "Fully Paid", 0, 1)
#total2$loan_status = ifelse(total2$loan_status == "Fully Paid", 0, 1)
# total3$loan_status = ifelse(total3$loan_status == "Fully Paid", 0, 1)

### Dummify the Data
dmy <- dummyVars(" ~ .", data = total)
total <- data.frame(predict(dmy, newdata = total))

#dmy2 <- dummyVars(" ~ .", data = total2)
#total2 <- data.frame(predict(dmy2, newdata = total2))

  # dmy3 <- dummyVars(" ~ .", data = total3)
  # total3 <- data.frame(predict(dmy3, newdata = total3))





# train1 = total1[which(total1$id)]


## Resplit into train and test to feed into model
train <- subset(total,!(id%in%testid[,1]))
test <- subset(total,(id%in%testid[,1]))

# train2 <- subset(total2,!(id%in%testid[,2]))
# test2 <- subset(total2,(id%in%testid[,2]))
# # # 
   #train3 <- subset(total3,!(id%in%testid[,3]))
   #test3 <- subset(total3,(id%in%testid[,3]))
# # 
# 



##### glmnet Fit Prediction

#logistic.regressor <- glm(loan_status ~ ., family = "binomial", data = train1)
#predict(glm.model, data = test1)
#summary(logistic.regressor)

# glm.model <- glm(train1$loan_status ~ ., family = "binomial", data = train1)
# glm.prob = predict(glm.model, newdata = test1, type = "response")

cvfit = cv.glmnet(as.matrix(train[,-which(names(train) %in% c('loan_status', 'id'))]), as.matrix(train$loan_status), family = "binomial", type.measure = "class") ## Convert to matrix while fitting glmnet, exclude id while fitting
bestlam <- cvfit$lambda.min ## Best lambda to minimize error
x_test <- as.matrix(test[,-which(names(test) %in% c('id', 'loan_status'))])
probability <- predict(cvfit, newx = x_test, s ="lambda.min", type = "response") ## Predict using lambda values

## Submission file creation
mysubmission1 = cbind(test[,"id"], probability)
write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE, col.names=c("id","prob"), qmethod = c("escape", "double"), fileEncoding = "")

### TRAIN2
  # cvfit = cv.glmnet(as.matrix(train2[,-which(names(train2) %in% c('loan_status', 'id'))]), as.matrix(train2$loan_status), family = "binomial", type.measure = "class")
  # bestlam <- cvfit$lambda.min
  # x_test <- as.matrix(test2[,-which(names(test2) %in% c('id', 'loan_status'))])
  # probability <- predict(cvfit, newx = x_test, s ="lambda.min", type = "response")
# 
# mysubmission1 = cbind(test2[,"id"], probability)
# write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE, col.names=c("id","prob"), qmethod = c("escape", "double"), fileEncoding = "")
# 
# ### TRAIN 3
 # cvfit = cv.glmnet(as.matrix(train3[,-which(names(train3) %in% c('loan_status', 'id'))]), as.matrix(train3$loan_status), family = "binomial", type.measure = "class")
 # bestlam <- cvfit$lambda.min
 # x_test <- as.matrix(test3[,-which(names(test3) %in% c('id', 'loan_status'))])
 # probability <- predict(cvfit, newx = x_test, s ="lambda.min", type = "response")
# 
# mysubmission1 = cbind(test[,"id"], probability)
# write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE, col.names=c("id","prob"), qmethod = c("escape", "double"), fileEncoding = "")


#end.time = Sys.time()


#colnames()


# cvfit = cv.glmnet(as.matrix(train2[,-which(names(train2) %in% c('loan_status', 'id'))]), as.matrix(train2$loan_status), family = "binomial", type.measure = "class")
# bestlam <- cvfit$lambda.min
# x_test <- as.matrix(test2[,-which(names(test2) %in% c('id', 'loan_status'))])
# lasso.pred <- predict(cvfit, newx = x_test, s ="lambda.min", type = "response")
# 
# cvfit = cv.glmnet(as.matrix(train3[,-which(names(train3) %in% c('loan_status', 'id'))]), as.matrix(train3$loan_status), family = "binomial", type.measure = "class")
# bestlam <- cvfit$lambda.min
# x_test <- as.matrix(test3[,-which(names(test3) %in% c('id', 'loan_status'))])
# lasso.pred <- predict(cvfit, newx = x_test, s ="lambda.min", type = "response")

# # log-loss function
# logLoss = function(y, p){
#   if (length(p) != length(y)){
#     stop('Lengths of prediction and labels do not match.')
#   }
#   
#   if (any(p < 0)){
#     stop('Negative probability provided.')
#   }
#   
#   p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
#   mean(ifelse(y == 1, -log(p), -log(1 - p)))
# }
# 
# logLoss(test[,'loan_status'],probability)
#  logLoss(test2[,'loan_status'],probability)
#  logLoss(test3[,'loan_status'],probability)
# 
# cvfit$lambda.min
# cvfit$lambda.1se
# coef(cvfit, s = "lambda.min")

#summary(logistic.regressor)

# rfModel = randomForest(Y ~ ., data = mydata[-test.id, ], importance = T, ntree=400); 


## GLMnet
# cv.out <- cv.glmnet(as.matrix(train2[,-ncol(train2)]),as.matrix(train2$Sale_Price), alpha = 1)
# bestlam <- cv.out$lambda.min
# lasso.pred <- predict(cv.out, s = bestlam, newx = as.matrix(test2) )






### Random Forest
# rfModel = randomForest(log(train2$Sale_Price) ~ ., data = train2[-test2, ],
#                        importance = T, ntree=400); 


#######
## 25 variables glmnet
## Create three folders train, test splits and save files in each of them

# #### Log-Loss Calculation from Piazza
# #########################################################################
# # log-loss function
# logLoss = function(y, p){
#   if (length(p) != length(y)){
#     stop('Lengths of prediction and labels do not match.')
#   }
#   
#   if (any(p < 0)){
#     stop('Negative probability provided.')
#   }
#   
#   p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
#   mean(ifelse(y == 1, -log(p), -log(1 - p)))
# }
# 
# #########################################################################
# # Test code begins
# start.time = Sys.time()
# source('mymain.R')
# end.time = Sys.time()
# run.time = as.numeric(difftime(end.time, start.time, units = 'min'))
# 
# # submission files
# allFiles = list.files()
# subFiles = grep('mysubmission', allFiles, value = TRUE, 
#                 ignore.case = TRUE)
# 
# # calculate the test error on the test set
# test = read.csv('test.csv')
# 
# label = read.csv('label.csv', sep = ',')
# err = rep(NA, length(subFiles))
# for (met in 1:length(subFiles)){
#   
#   prediction = read.csv(subFiles[met], sep = ',')
#   yp = merge(prediction, label, by = 'id', all.y = TRUE)
#   err[met] = with(yp, logLoss(y, prob))
#   
# }
# 
# #########################################################################
# write.table(err, file = 'proj_3.csv', sep = ',', row.names = FALSE,
#             col.names = FALSE)
# write.table(run.time, file = 'proj_3.csv', sep = ',', 
#             row.names = FALSE, col.names = FALSE, append = TRUE)
# 
# 
# 



