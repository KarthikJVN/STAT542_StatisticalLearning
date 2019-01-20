set.seed(8871)
#install.packages("recommenderlab")
## TRY SVD???
## SEE Recommender lab link in RS page posted on Piazza
## Try POPULAR Method

#install.packages("reco")
#install.packages("devtools")
#install.packages("reshape2")
install.packages("weatherData",repos = "http://cran.us.r-project.org")


# List of useful packages
pkg <- c("tidyr", "dplyr", "ggplot2", "knitr", "rmarkdown")

# Check if packages are not installed and assign the
# names of the uninstalled packages to the variable new.pkg
new.pkg <- pkg[!(pkg %in% installed.packages())]

# If there are any packages in the list that aren't installed,
# install them
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}


library(dplyr)
library(ggplot2)
library(recommenderlab)  
library(reshape2)



## EDA
# ratings data
# use colClasses = 'NULL' to skip columns
setwd('/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/HW4')
ratings = read.csv('ratings.dat', sep = ':', 
                   colClasses = c('integer', 'NULL'), header = FALSE)
test = read.csv('test.csv', header = TRUE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
dim(ratings) # 1000209-by-4
ratings[1:4, ]

# movies data
# In movies.dat, some movie names contain single colon (:), so the above 
# method does not work. 

movies = readLines('movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

## Explore the relationship between movie ratings and movie genres. First, simplify movie genres: multiple genres to ‘Multiple’
movies$Genres = ifelse(grepl('\\|', movies$Genres), "Multiple", 
                       movies$Genres)

## Then merge ratings and movie datasets.
rating_merged = merge(x = ratings, y = movies, by.x = "MovieID")

## Use ggplot to make a bar graph.
ggplot(rating_merged, aes(x = factor(Genres), y = Rating), 
       color = factor(vs)) + stat_summary(fun.y = mean, position =
                                            position_dodge(), geom = "bar") + labs(x = "Genres", y = "Mean
                                                                                   ratings", title = "Mean ratings by genres") + theme(axis.text.x =
                                                                                                                                         element_text(angle = 90, hjust = 1))

## Prepare training and test data
#set.seed(8871)

ratings$Timestamp = NULL;
colnames(ratings) = c('user', 'movie', 'rating')
#set.seed(100)
train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.6)
train = ratings[train.id, ]
head(train)

test = ratings[-train.id, ]
test.id = sample(nrow(test), floor(nrow(test)) * 0.5)
test = test[test.id, ]
head(test)

label = test[c('user', 'rating')]
test$rating = NULL
head(label)
head(test)






# train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.6)
# train = ratings[train.id, ]
# write.table(train, file = 'train.dat', sep = '::', row.names = FALSE,
#             col.names = FALSE
# )
# 
# test = ratings[- train.id, ]
# test.id = sample(nrow(test), floor(nrow(test)) * 0.5)
# test = test[test.id, ]
# test$Timestamp = NULL
# test$ID = 1:nrow(test)
# label = test[c('ID', 'Rating')]
# test$Rating = NULL
# test = test[c('ID', 'UserID', 'MovieID')]
# colnames(test) = c('ID', 'user', 'movie')
# 
# write.table(test, file = 'test.csv', sep = ',', row.names = FALSE)
# write.table(label, file = 'label.csv', sep = ',', row.names = FALSE)

## Recommender System
## We train a recommender system and make prediction on the test data.
# remove timestamp column
train$Timestamp = NULL
head(train)

##Create a utility matrix:
R = acast(train, user ~ movie) ## Used user and movie from train
##R = acast(train, userid ~ movieid) ## Used user and movie from train USERID and MOVIEID??
#value.var
R = as(R, 'realRatingMatrix')

## Normalize the utility matrix and visualize data:
R_m = recommenderlab::normalize(R)
head(getRatingMatrix(R_m))

# visualize
image(R, main = "Raw Ratings")
image(R_m, main = "Normalized Ratings")

## Learn a Recommender
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
rec = Recommender(R, method = 'UBCF',
                  parameter = list(normalize = 'Z-score', method = 'Cosine', nn = 5, 
                                   minRating = 1)
)

#rec2 <- Recommender(R, method = 'IBCF', parameter = list(k = 5))
rec3 <- Recommender(R, method = 'SVD', parameter = list(k = 5))
## A short summary of the model:
print(rec)
names(getModel(rec))


## Model 1 UBCF
## Now let’s make prediction on the test data:
recom = predict(rec, R, type = 'ratings')  # predict ratings. This may be slow.
rec_list = as(recom, 'list')  # each element are ratings of that user

test$rating = NA

# For all lines in test file, one by one
for (u in 1:nrow(test)){
  
  # Read userid and movieid from columns 2 and 3 of test data
  userid = as.character(test$user[u])
  movieid = as.character(test$movie[u])
  
  rating = rec_list[[userid]][movieid]
  # 2.5 may be too arbitrary
  test$rating[u] = ifelse(is.na(rating), 2.5, rating)
  
}

# write submission file 1
write.table(test, file = 'mysubmission1.csv', row.names = FALSE,
            col.names = TRUE, sep = ',')


lab <- read.csv("label.csv")
## RMSE Value calculation
#yhat.test = predict(rfModel, mydata[test.id, ])
sqrt(mean((test$rating - lab$Rating)^2))



 ## Case 2 IBCF Model
#  recom2 = predict(rec2, R, type = 'ratings')  # predict ratings. This may be slow.
#  rec_list2 = as(recom2, 'list')  # each element are ratings of that user
# #
#  test$rating = NA
#  for (u in 1:nrow(test)){
# #
# #   # Read userid and movieid from columns 2 and 3 of test data
#    userid = as.character(test$user[u])
#    movieid = as.character(test$movie[u])
# #
#    rating = rec_list2[[userid]][movieid]
# #   # 2.5 may be too arbitrary
#    test$rating[u] = ifelse(is.na(rating), 2.5, rating)
# 
#  }
# 
#  lab <- read.csv("label.csv")
#  ## RMSE Value calculation
#  #yhat.test = predict(rfModel, mydata[test.id, ])
#  sqrt(mean((test$rating - lab$Rating)^2))
#  
#  
 
 
# Case 3 SVD Implementation
recom3 = predict(rec3, R, type = 'ratings')  # predict ratings. This may be slow.
rec_list3 = as(recom3, 'list')  # each element are ratings of that user

test$rating = NA
for (u in 1:nrow(test)){
  
  # Read userid and movieid from columns 2 and 3 of test data
  userid = as.character(test$user[u])
  movieid = as.character(test$movie[u])
  
  rating = rec_list3[[userid]][movieid]
  # 2.5 may be too arbitrary
  test$rating[u] = ifelse(is.na(rating), 2.5, rating)
  
}


# write submission file 2
write.table(test, file = 'mysubmission3.csv', row.names = FALSE,
            col.names = TRUE, sep = ',')


### SVD Approximation SEE --
#  mtx <- split_ratings(ratings_table = ratings, 
# #                      proportion = c(0.7, 0.15, 0.15))
#  model <- svd_build(mtx)
#  model_tunes <- svd_tune(model, r = 2:50)
## https://rpubs.com/tarashnot/recommender_comparison



#### End of Case 2


# test = test[with(test, order(user, movie)), ]






 

lab <- read.csv("label.csv")
## RMSE Value calculation
#yhat.test = predict(rfModel, mydata[test.id, ])
sqrt(mean((test$rating - lab$Rating)^2))
#sum((mydata$Y[-test.id] - yhat.train) ^2)/(n - ntest)

## rec2 <- Recommender(R, method = 'IBCF', parameter = list(k = 5))  

### Method 3 - Matrix factorization GD
# in_train <- rep(TRUE, nrow(ratings))
# in_train[sample(1:nrow(ratings), size = round(0.2 * length(unique(ratings$user)), 0) * 5)] <- FALSE
# 
# ratings_train <- train$ratings[(in_train)]
# ratings_test <- test$ratings[(!in_train)]
# 
# write.table(ratings_train, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
# write.table(ratings_test, file = "testset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
# 
# r = Reco()
# opts <- r$tune("trainset.txt", opts = list(dim = c(1:20), lrate = c(0.05),
#                                            nthread = 4, cost = c(0), niter = 200, nfold = 10, verbose = FALSE))
# 
# r$train("trainset.txt", opts = c(opts$min, nthread = 4, niter = 500, verbose = FALSE))


