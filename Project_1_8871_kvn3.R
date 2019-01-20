#test.id = seq(1, 2930, by=3)
set.seed(8871)

install.packages("robustHD")
install.packages("caTools")
install.packages("DescTools")
install.packages("caret")
install.packages("randomForest")
install.packages("xgboost")
install.packages("corrplot")
install.packages("Rmisc")
install.packages("ggrepel")
install.packages("psych")
install.packages("cran")
install.packages("gbm")


library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(robustHD)
library(caTools)
library(DescTools)
library(caret)
library(randomForest)
library(xgboost)
##library(cran)
library(glmnet)
library(gbm)



data <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Ames_data.csv", stringsAsFactors = F )

train <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Ames_data.csv", stringsAsFactors = F)
test <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Ames_data.csv", stringsAsFactors = F)

Project <- read.table("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Project1_test_id.txt", stringsAsFactors = F )

#sample = sample.split(data$anycolumn, SplitRatio = .70)
#s = sample.split[Project$V3, data$PID == Project$V3, SplitRatio = .70]
#train = subset(data, sample == TRUE)
#test  = subset(data, sample == FALSE)

train <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Ames_data.csv", stringsAsFactors = F)
test <- read.csv("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 1/Ames_data.csv", stringsAsFactors = F)

test = data[data$PID%in%Project[,4],1:82]
train = data[!data$PID%in%Project[,4],]

#train$Garage_Yr_Blt <- NULL
#test$Garage_Yr_Blt <- NULL

#x <- factor(c('a', 'b', 'c'), levels = c('b', 'a', 'c'))
#x_ord <- as.numeric(x)
#>> [1] 2 1 3

#scale(x, center = TRUE, scale = TRUE)

#require(stats)
#x <- matrix(1:10, ncol = 2)
#(centered.x <- scale(x, scale = FALSE))
#cov(centered.scaled.x <- scale(x)) # all 1

#require(stats)
#x <- train(1:10, ncol = 2)
#(centered.x <- scale(x, scale = FALSE))
#cov(centered.scaled.x <- scale(x)) # all 1

#winsorize(train)

#Data size and structure

#dim(train)
#dim(test)



#test$SalePrice <- NA
#all <- rbind(train, test)
#dim(all)

##Table command
table(train$Land_Slope)/dim(train)[1] ## Categorical Variables non Numeric data   Drop Land_Slope  YES
table(train$Neighborhood)/dim(train)[1]  ## No
table(train$Bldg_Type)/dim(train)[1] ##YEs     Above 95% drop else keep
table(train$MS_SubClass)/dim(train)[1] ## NO
table(train$MS_Zoning)/dim(train)[1] ## NO
table(train$Street)/dim(train)[1] ## YES
table(train$Alley)/dim(train)[1] ## NO
table(train$Lot_Shape)/dim(train)[1] ##NO
table(train$Land_Contour)/dim(train)[1]  ## NO
table(train$Utilities)/dim(train)[1]  ##YES  
table(train$Lot_Config)/dim(train)[1]  ##NO
table(train$Condition_1)/dim(train)[1]  ##NO
table(train$Condition_2)/dim(train)[1] ##YES
table(train$House_Style)/dim(train)[1] ##NO
table(train$Overall_Qual)/dim(train)[1]  ##NO
table(train$Overall_Cond)/dim(train)[1]  ##NO 
table(train$Roof_Style)/dim(train)[1]  ##NO
table(train$Roof_Matl)/dim(train)[1]  ##YES
table(train$Exterior_1st)/dim(train)[1]  ##NO
table(train$Exterior_2nd)/dim(train)[1]  ##NO
table(train$Mas_Vnr_Type)/dim(train)[1]  ##NO
table(train$Mas_Vnr_Area)/dim(train)[1] ##NO
##sapplytable(train$Mas_Vnr_Area)/dim(train)[1]
table(train$Exter_Qual)/dim(train)[1]   ##NO
table(train$Exter_Cond)/dim(train)[1]  ##NO
table(train$Foundation)/dim(train)[1]  ##NO
table(train$Bsmt_Qual)/dim(train)[1]  ##NO
table(train$Bsmt_Cond)/dim(train)[1]  ##NO
table(train$Bsmt_Exposure)/dim(train)[1]  ##NO
table(train$BsmtFin_Type_1)/dim(train)[1]  ##NO
##table(train$BsmtFin_SF_1)/dim(train)[1]  ##NO
table(train$BsmtFin_Type_2)/dim(train)[1]  ##NO
table(train$Heating)/dim(train)[1] ##YES
table(train$Heating_QC)/dim(train)[1]  ##NO
table(train$Central_Air)/dim(train)[1]  ##NO
table(train$Electrical)/dim(train)[1] ##NO
table(train$Kitchen_Qual)/dim(train)[1]  ##NO
table(train$Functional)/dim(train)[1]  ##NO
table(train$Fireplace_Qu)/dim(train)[1]  ##NO
table(train$Garage_Type)/dim(train)[1] ##NO
table(train$Garage_Finish)/dim(train)[1]  ##NO
table(train$Garage_Qual)/dim(train)[1] ##NO
table(train$Garage_Cond)/dim(train)[1]  ##NO
table(train$Paved_Drive)/dim(train)[1]  ##NO
table(train$Pool_QC)/dim(train)[1]  ##YES
table(train$Fence)/dim(train)[1]  ##NO
table(train$Misc_Feature)/dim(train)[1]  ##YES
table(train$Sale_Type)/dim(train)[1]  ##NO
table(train$Sale_Condition)/dim(train)[1]  ##NO
table(train$Sale_Condition)/dim(train)[1]  ##NO

##DROPPING VARIABLES AFTER USING TABLE COMMAND IF 95%
## DROPPING VARIABLES LONGITUDE and LATITUDE
## train changed to train1

train1 = subset(train, select = -c(Land_Slope,Bldg_Type,Street,Utilities,Condition_2,Garage_Yr_Blt,Roof_Matl,Heating,Pool_QC,Misc_Feature,Longitude,Latitude))
test1 = subset(test, select = -c(Land_Slope,Bldg_Type,Street,Utilities,Condition_2,Garage_Yr_Blt,Roof_Matl,Heating,Pool_QC,Misc_Feature,Longitude,Latitude))

names(test1)[71]=c("Sale_Price")



####################
## ONE HOT ENCODING
## Karet package Use, Bind based on Columns, One hot encoding categorical variables, Bind Encode Then Split

#Binding

total = rbind(train1,test1)





## Winsorization Loop through all columns with Numeric data


summary(train1$Lot_Frontage)
train1$Lot_Frontage = Winsorize(train1$Lot_Frontage, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
          na.rm = FALSE, type = 7)
summary(train1$Lot_Frontage)
train1$Lot_Area = Winsorize(train1$Lot_Area, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)
train1$Mas_Vnr_Area = Winsorize(train1$Mas_Vnr_Area, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7)
train1$Exter_Qual = Winsorize(train1$Exter_Qual, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)

train1$BsmtFin_SF_2 = Winsorize(train1$BsmtFin_SF_2, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)


train1$Bsmt_Unf_SF = Winsorize(train1$Bsmt_Unf_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)

train1$Total_Bsmt_SF = Winsorize(train1$Total_Bsmt_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                            na.rm = FALSE, type = 7)

train1$First_Flr_SF = Winsorize(train1$First_Flr_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                             na.rm = FALSE, type = 7)

train1$Second_Flr_SF = Winsorize(train1$Second_Flr_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                na.rm = FALSE, type = 7)

train1$Gr_Liv_Area = Winsorize(train1$Gr_Liv_Area, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                             na.rm = FALSE, type = 7)

train1$Gr_Liv_Area = Winsorize(train1$Gr_Liv_Area, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                               na.rm = FALSE, type = 7)

##TotRms SEE
train1$Garage_Area = Winsorize(train1$Garage_Area, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                            na.rm = FALSE, type = 7)

train1$Wood_Deck_SF = Winsorize(train1$Wood_Deck_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                            na.rm = FALSE, type = 7)

train1$Open_Porch_SF = Winsorize(train1$Open_Porch_SF, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                              na.rm = FALSE, type = 7)

### Enclosed_Porch  BQ??? Three_season_porch BR???   Screen_Porch  BS?? Mo_Sold?? Longitude??  Latitude??  Sale_Price??
train1$Misc_Val = Winsorize(train1$Misc_Val, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                           na.rm = FALSE, type = 7) 


train1$Sale_Price = Winsorize(train1$Sale_Price, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                            na.rm = FALSE, type = 7) 



####################
## ONE HOT ENCODING
## Karet package Use, Bind based on Columns, One hot encoding categorical variables, Bind Encode Then Split

#Binding

total = rbind(train1,test1)


### ONE HOT ENCODING

# dummify the data
dmy <- dummyVars(" ~ .", data = total)
trsf <- data.frame(predict(dmy, newdata = total))

#Resplit the data into train and test splits
train2 = trsf[1:nrow(train1),]
test2 = trsf[(nrow(train1)+1):nrow(trsf), -ncol(trsf)]


#########################
## FIT RANDOM FOREST
#########################


### fit log sale price with all variables, plot??
## fit models for train data


rfModel = randomForest(log(train2$Sale_Price) ~ ., data = train2[-test.id, ],
                       importance = T, ntree=400); 




########## Lasso
#my_control <-trainControl(method="cv", number=5)
#lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

#lasso_mod <- train(x=train2, y=train2$Sale_Price[!is.na(train2$Sale_Price)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
#lasso_mod$bestTune


##########  LASSO MAIN

###lasso <- glmnet(x= as.matrix(train2[,-train2$Sale_Price]), y= as.matrix(train2$Sale_Price), alpha = 1 )
cv.out <- cv.glmnet(as.matrix(train2[,-ncol(train2)]),as.matrix(train2$Sale_Price), alpha = 1)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(cv.out, s = bestlam, newx = as.matrix(test2) )

######## Find RMSE value as square root of mean(y - ypred)^2

########### XGBoost

#xgb_grid = expand.grid(
 # nrounds = 1000,
 # eta = c(0.1, 0.05, 0.01),
 # max_depth = c(2, 3, 4, 5, 6),
 # gamma = 0,
 # colsample_bytree=1,
 # min_child_weight=c(1, 2, 3, 4 ,5),
 # subsample=1
#)

#  xgb_caret <- train(x=train2, y=train2$Sale_Price[!is.na(tr<OwaStorableObject type="Conversations" version="1" denyCopy="false" denyMove="false"><Conversations FolderId="LgAAAACszHB5NvblSaBRvXxjZUgXAQDNxgQENlrxTL+R3Yg9YJI8AAAAkqPUAAAB" IsCrossFolder="false"><Item ItemId="CID.+i+52X0blUCuCTXG0NtMpQ==.LgAAAACszHB5NvblSaBRvXxjZUgXAQDNxgQENlrxTL+R3Yg9YJI8AAAAkqPUAAAB.riwAAACSo9AAAAAACaHuAAAAAAA=" ItemType="IPM.Conversation"/></Conversations></OwaStorableObject>ain2$Sale_Price)], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
#  xgb_caret$bestTune

######### XGBoost MAIN

#put into the xgb matrix format
#dtrain = xgb.DMatrix(data =  as.matrix(train2), label = as.matrix(train2) )
#dtest = xgb.DMatrix(data =  as.matrix(test2), label = as.matrix(test2) )

# these are the datasets the rmse is evaluated for at each iteration
#watchlist = list(train=dtrain, test=dtest)



#bst = xgb.train(data = dtrain, 
               # max.depth = 10, 
              #  eta = 0.1, 
               # nthread = 2, 
               # nround = 10000, 
               # watchlist = watchlist, 
               # objective = "reg:linear", 
               # early_stopping_rounds = 50,
               # print_every_n = 500)

##### XG BOOST FROM PIAZZA

# train GBM model
gbm.fit <- gbm(
  formula = train2$Sale_Price ~ .,
  distribution = "gaussian",
  data = train2,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

print(gbm.fit)

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)


############ TUNING FOR GBM

# train GBM model
gbm.fit2 <- gbm(
  formula = train2$Sale_Price ~ .,
  distribution = "gaussian",
  data = train2,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 23112.1

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)









# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))



#dtest = xgb.DMatrix(data =  as.matrix( test2 ))

#test the model on truly external data

#y_hat_valid = predict(bst, dtest)



###For part 1, use xgboost, fit data accordingly
###For part 2 split is different, kaggle, piazza, html
### Can we use Lasso for Part1 ? Use data.matrix as an input instead of directly.

#n = 50; 
#X1 = sample(c("A", "B", "C"), n, replace=TRUE)
#X1 = as.factor(X1)
#X2 = sample(1:4, n, replace=TRUE)
#X2 = as.factor(X2)

# obtain the one hot coding for X1 and X2
#fake.y = rep(0, n)
#tmp = model.matrix(lm(fake.y ~ X1 + X2))
#tmp = tmp[, -1]  # remove the 1st column (the intercept) of tmp















#Lot_Frontage_W = winsorize(train$Lot_Frontage, standardized = FALSE, centerFun = median, scaleFun = mad, const = 2, return = c("data", "weights"))
#hist(train$Lot_Frontage)
#hist(Lot_Frontage_W)

## Winsorize Command Numeric data, Standardized = FALSE, Put all default values
## One Hot Encoding - Linear regression lm sale_price 0 1 0, levels - Design matrix(Features obtained from design matrix) - glmnet - , NaN Values - Impute using median, Preprocessing Steps, glmnet??
## Garage belt drop, test also drop columns

##Random Forest, parameter tuning -- hyperparameters, no of trees, no of candidates, Initially use random values or default values  make a list,  highest accuracy, 1 . RF, 2. linear regression, XG Boost

## Piazza 10 random splits

#The response variable; SalePrice
#ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
 # geom_histogram(fill="blue", binwidth = 10000) +
  #scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#summary(all$SalePrice)

#### LASSO FUNCTION
#one_step_lasso = function(r, x, lam){
 # xx = sum(x^2)
  #xr = sum(r*x)
  #b = (abs(xr) -lam/2)/xx
  #b = sign(xr)*ifelse(b>0, b, 0)
  #return(b)
#}

#### FUNCTION TO IMPLEMENT COORDINATE DESCENT 


