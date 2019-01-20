install.packages("glmnet")
install.packages("ggplot2")
#install.packages("gridExtra")

set.seed(8871)

#################   BOSTON HOUSING DATA 1

library(glmnet)  # glmnet for lasso
library(ggplot2)  # qplot
library(gridExtra)  # grid.arrange, 

#Load the data and parepare for the simulation
rm(list = ls())  # remove the objects in R before loading the new data
load('/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/HW2/BostonHousing1.Rdata')
myData = Housing1
n = nrow(myData)
p = ncol(myData) - 1

# some algorithms need the matrix/vector 
# input (instead of a data frame)
X = data.matrix(myData[,-1])  
Y = myData[,1]  

a = matrix(c(1), 50, 10) #Error/Prediction values storing
b = matrix(c(2), 50, 10) #Total time for 50 iterations
c = matrix(c(3), 50, 10) #Model sisze


#Suggest to save the row IDs for test data for all T iterations, so can use the same training/test split for each method.
# all.test.id: ntestxT matrix, each column records 
ntest = round(n * 0.25)  # test set size
ntrain = n-ntest  # training set size
all.test.id = matrix(0, ntest, 50)  # 
for(t in 1:50){
  all.test.id[, t] = sample(1:n, ntest)
}


for(m in 1:50)
{

save(all.test.id, file="alltestID.RData")

test.id = all.test.id[,m] 

#Method 1: Full Model (Full)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id,])
Ytest.pred = predict(full.model, newdata = myData[test.id,])
mean((Y[test.id] - Ytest.pred)^2)
b[m,1] = (proc.time() - start.time)[1]
a[m,1] = mean((Y[test.id] - Ytest.pred)^2)
c[m,1] = length(full.model$coef) - 1

#Method 2: Forward AIC (AIC.F)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(lm(Y ~ 1, data = myData[-test.id, ]), 
               list(upper = full.model),
               trace = 0, direction = "forward")
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
a[m,2] = mean((Y[test.id] - Ytest.pred)^2)
c[m,2] = length(stepAIC$coef) - 1 

# number of predictors (excluding the intercept)
length(stepAIC$coef) - 1  
b[m,2] = (proc.time() - start.time)[1]
#a[m,2] = Ytest.pred

#Method 3: Backward AIC (AIC.B)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(full.model, trace = 0, direction = "backward")
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
#a[m,3] = Ytest.pred
a[m,3] = mean((Y[test.id] - Ytest.pred)^2)

length(stepAIC$coef) - 1
b[m,3] = (proc.time() - start.time)[1]
c[m,3] = length(stepAIC$coef) - 1

#Method 4: Forward BIC (BIC.F)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(lm(Y ~ 1, data = myData[-test.id, ]),
               list(upper = full.model),
               trace = 0, direction = "forward", k = log(ntrain))
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
#a[m,4] = Ytest.pred
a[m,4] = mean((Y[test.id] - Ytest.pred)^2)
b[m,4] = (proc.time() - start.time)[1]
c[m,4] = length(stepAIC$coef) - 1

# number of predictors (excluding the intercept)
length(stepAIC$coef) - 1 #Model size
b[m,4] = (proc.time() - start.time)[1]

#Method 5: Backward BIC (BIC.B)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(full.model, trace = 0,
               direction = "backward", k = log(ntrain))
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
length(stepAIC$coef) - 1
b[m,5] = (proc.time() - start.time)[1]
a[m,5] = mean((Y[test.id] - Ytest.pred)^2)
c[m,5] = length(full.model$coef) - 1

#a[m,5] = Ytest.pred


#Method 6
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-6,0, length=200)))
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
proc.time() - start.time
# Exclude the computation for DF when recording computing time for Ridge,

# DF for Ridge
# DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
# matrix with each column having mean zero and sd 1. Note that glmnet computes
# sd using denominator n not (n-1). 
# In addition, the objective function in glmnet for ridge is slightly different
# from the one used in class. So lam used in S (above) corresponds to lambda.min
# or lambda.1se times the sample size

ntrain = n - dim(all.test.id)[1]
tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
d = svd(tmpX)$d 

# df for Ridge with lambda_min
best.lam = cv.out$lambda.min
sum(d^2/(d^2 + best.lam*ntrain))
#c[m,6] = length(sum()) - 1
a[m,6] = mean((Y[test.id] - Ytest.pred)^2)
b[m,6] = (proc.time() - start.time)[1]
c[m,6] = sum(d^2/(d^2 + best.lam*ntrain))

start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
proc.time() - start.time

#plot(cv.out)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-6,0,length=200))   )
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])



#Method 7: Ridge with lambda_1se (R_min, R_1se)
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
best.lam = cv.out$lambda.1se
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
b[m,7] = (proc.time() - start.time)[1]
#a[m,6] = Ytest.pred
a[m,7] = mean((Y[test.id] - Ytest.pred)^2)


# Exclude the computation for DF when recording computing time for Ridge,

# DF for Ridge
# DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
# matrix with each column having mean zero and sd 1. Note that glmnet computes
# sd using denominator n not (n-1). 
# In addition, the objective function in glmnet for ridge is slightly different
# from the one used in class. So lam used in S (above) corresponds to lambda.min
# or lambda.1se times the sample size

ntrain = n - dim(all.test.id)[1]
tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
d = svd(tmpX)$d 

# df for Ridge with lambda_min
#best.lam = cv.out$lambda.min
#sum(d^2/(d^2 + best.lam*ntrain))

# df for Ridge with lambda_1se
best.lam = cv.out$lambda.1se
sum(d^2/(d^2 + best.lam*ntrain))
c[m,7] = sum(d^2/(d^2 + best.lam*ntrain))

start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
best.lam = cv.out$lambda.1se
lam_range = range(log(cv.out$lambda))
proc.time() - start.time

#plot(cv.out)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-6,0,length=200))   )
best.lam = cv.out$lambda.1se
lam_range = range(log(cv.out$lambda))
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])

#Method 8: Lasso using lambda.min (L_min)
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
#[m,8] = Ytest.pred
mean((Ytest.pred - Y[test.id])^2)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
sum(mylasso.coef != 0) - 1  # size of Lasso with lambda.min
a[m,8] = mean((Y[test.id] - Ytest.pred)^2)
b[m,8] = (proc.time() - start.time)[1]
c[m,8] = sum(mylasso.coef != 0) - 1

start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
proc.time() - start.time

#plot(cv.out)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-6,0,length=200))   )
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])

#Method 9: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.1se
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
mean((Ytest.pred - Y[test.id])^2)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se
a[m,9] = mean((Y[test.id] - Ytest.pred)^2)
b[m,9] = (proc.time() - start.time)[1]
#c[m,9] = length(sum()$coef) - 1
c[m,9] = sum(mylasso.coef != 0) - 1

start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
proc.time() - start.time

#plot(cv.out)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-6,0,length=200)))
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])

#Method 10: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.1se
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
mean((Ytest.pred - Y[test.id])^2)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se

var.sel = row.names(mylasso.coef)[nonzeroCoef(mylasso.coef)[-1]]
tmp.X = X[, colnames(X) %in% var.sel]
mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
#a[m,10] = Ytest.pred
a[m,10] = mean((Ytest.pred - Y[test.id])^2)
b[m,10] = (proc.time() - start.time)[1]
#c[m,10] = length(sum()$coef) - 1
c[m,10] = length(mylasso.refit) - 1

start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
proc.time() - start.time

#plot(cv.out)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-6,0, length=200))   )
best.lam = cv.out$lambda.min
lam_range = range(log(cv.out$lambda))
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])

}

boxplot(a)
boxplot(c)
group <- sample(1:50, 20, TRUE)
Time = colSums(b)

#Time = apply(rTime, 2, sum)


#############################  BOSTON HOUSING DATA 2

library(glmnet)  # glmnet for lasso
library(ggplot2)  # qplot
library(gridExtra)  # grid.arrange, 

#Load the data and parepare for the simulation
rm(list = ls())  # remove the objects in R before loading the new data
load('/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/HW2/BostonHousing2.Rdata')
myData = Housing2
n = nrow(myData)
p = ncol(myData) - 1

# some algorithms need the matrix/vector 
# input (instead of a data frame)
X = data.matrix(myData[,-1])  
Y = myData[,1]  

a = matrix(c(1), 50, 5) #Error/Prediction values storing
b = matrix(c(2), 50, 5) #Total time for 50 iterations
c = matrix(c(3), 50, 5) #Model sisze

#Suggest to save the row IDs for test data for all T iterations, so can use the same training/test split for each method.
# all.test.id: ntestxT matrix, each column records 
ntest = round(n * 0.25)  # test set size
ntrain = n-ntest  # training set size
all.test.id = matrix(0, ntest, 50)  # 
for(t in 1:50){
  all.test.id[, t] = sample(1:n, ntest)
}



for(m in 1:50)
{
  
  save(all.test.id, file="alltestID.RData")
  
  test.id = all.test.id[,m] 
  
  
  
  
  #Method 6
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3, length=200)))
  best.lam = cv.out$lambda.min
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  proc.time() - start.time
  # Exclude the computation for DF when recording computing time for Ridge,
  
  # DF for Ridge
  # DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
  # matrix with each column having mean zero and sd 1. Note that glmnet computes
  # sd using denominator n not (n-1). 
  # In addition, the objective function in glmnet for ridge is slightly different
  # from the one used in class. So lam used in S (above) corresponds to lambda.min
  # or lambda.1se times the sample size
  
  ntrain = n - dim(all.test.id)[1]
  tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
  d = svd(tmpX)$d 
  
  # df for Ridge with lambda_min
  best.lam = cv.out$lambda.min
  sum(d^2/(d^2 + best.lam*ntrain))
  #c[m,6] = length(sum()) - 1
  a[m,1] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,1] = (proc.time() - start.time)[1]
  c[m,1] = sum(d^2/(d^2 + best.lam*ntrain))
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  
  
  #Method 7: Ridge with lambda_1se (R_min, R_1se)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  b[m,2] = (proc.time() - start.time)[1]
  #a[m,6] = Ytest.pred
  a[m,2] = mean((Y[test.id] - Ytest.pred)^2)
  
  
  # Exclude the computation for DF when recording computing time for Ridge,
  
  # DF for Ridge
  # DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
  # matrix with each column having mean zero and sd 1. Note that glmnet computes
  # sd using denominator n not (n-1). 
  # In addition, the objective function in glmnet for ridge is slightly different
  # from the one used in class. So lam used in S (above) corresponds to lambda.min
  # or lambda.1se times the sample size
  
  ntrain = n - dim(all.test.id)[1]
  tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
  d = svd(tmpX)$d 
  
  # df for Ridge with lambda_min
  #best.lam = cv.out$lambda.min
  #sum(d^2/(d^2 + best.lam*ntrain))
  
  # df for Ridge with lambda_1se
  best.lam = cv.out$lambda.1se
  sum(d^2/(d^2 + best.lam*ntrain))
  c[m,2] = sum(d^2/(d^2 + best.lam*ntrain))
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.1se
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.1se
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 8: Lasso using lambda.min (L_min)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  #[m,8] = Ytest.pred
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1  # size of Lasso with lambda.min
  a[m,3] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,3] = (proc.time() - start.time)[1]
  c[m,3] = sum(mylasso.coef != 0) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3,length=200)))
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 9: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se
  a[m,4] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,4] = (proc.time() - start.time)[1]
  #c[m,9] = length(sum()$coef) - 1
  c[m,4] = sum(mylasso.coef != 0) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3,length=200)))
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 10: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se
  
  var.sel = row.names(mylasso.coef)[nonzeroCoef(mylasso.coef)[-1]]
  tmp.X = X[, colnames(X) %in% var.sel]
  mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
  Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
  #a[m,10] = Ytest.pred
  a[m,5] = mean((Ytest.pred - Y[test.id])^2)
  b[m,5] = (proc.time() - start.time)[1]
  #c[m,10] = length(sum()$coef) - 1
  c[m,5] = length(mylasso.refit) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3, length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
}

boxplot(a)
boxplot(c)
group <- sample(1:50, 20, TRUE)
Time = colSums(b)



################ BOSTON HOUSING DATA 3

#Load the data and parepare for the simulation
rm(list = ls())  # remove the objects in R before loading the new data
load('/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/HW2/BostonHousing3.Rdata')
myData = Housing3
n = nrow(myData)
p = ncol(myData) - 1

# some algorithms need the matrix/vector 
# input (instead of a data frame)
X = data.matrix(myData[,-1])  
Y = myData[,1]  

a = matrix(c(1), 50, 5) #Error/Prediction values storing
b = matrix(c(2), 50, 5) #Total time for 50 iterations
c = matrix(c(3), 50, 5) #Model sisze


#Suggest to save the row IDs for test data for all T iterations, so can use the same training/test split for each method.
# all.test.id: ntestxT matrix, each column records 
ntest = round(n * 0.25)  # test set size
ntrain = n-ntest  # training set size
all.test.id = matrix(0, ntest, 50)  # 
for(t in 1:50){
  all.test.id[, t] = sample(1:n, ntest)
}



for(m in 1:50)
{
  
  save(all.test.id, file="alltestID.RData")
  
  test.id = all.test.id[,m] 
  

  #Method 6
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3, length=200)))
  best.lam = cv.out$lambda.min
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  proc.time() - start.time
  # Exclude the computation for DF when recording computing time for Ridge,
  
  # DF for Ridge
  # DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
  # matrix with each column having mean zero and sd 1. Note that glmnet computes
  # sd using denominator n not (n-1). 
  # In addition, the objective function in glmnet for ridge is slightly different
  # from the one used in class. So lam used in S (above) corresponds to lambda.min
  # or lambda.1se times the sample size
  
  ntrain = n - dim(all.test.id)[1]
  tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
  d = svd(tmpX)$d 
  
  # df for Ridge with lambda_min
  best.lam = cv.out$lambda.min
  sum(d^2/(d^2 + best.lam*ntrain))
  #c[m,6] = length(sum()) - 1
  a[m,1] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,1] = (proc.time() - start.time)[1]
  c[m,1] = sum(d^2/(d^2 + best.lam*ntrain))
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  
  
  #Method 7: Ridge with lambda_1se (R_min, R_1se)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  b[m,2] = (proc.time() - start.time)[1]
  #a[m,6] = Ytest.pred
  a[m,2] = mean((Y[test.id] - Ytest.pred)^2)
  
  
  # Exclude the computation for DF when recording computing time for Ridge,
  
  # DF for Ridge
  # DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
  # matrix with each column having mean zero and sd 1. Note that glmnet computes
  # sd using denominator n not (n-1). 
  # In addition, the objective function in glmnet for ridge is slightly different
  # from the one used in class. So lam used in S (above) corresponds to lambda.min
  # or lambda.1se times the sample size
  
  ntrain = n - dim(all.test.id)[1]
  tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
  d = svd(tmpX)$d 
  
  # df for Ridge with lambda_min
  #best.lam = cv.out$lambda.min
  #sum(d^2/(d^2 + best.lam*ntrain))
  
  # df for Ridge with lambda_1se
  best.lam = cv.out$lambda.1se
  sum(d^2/(d^2 + best.lam*ntrain))
  c[m,2] = sum(d^2/(d^2 + best.lam*ntrain))
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
  best.lam = cv.out$lambda.1se
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.1se
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 8: Lasso using lambda.min (L_min)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  #[m,8] = Ytest.pred
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1  # size of Lasso with lambda.min
  a[m,3] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,3] = (proc.time() - start.time)[1]
  c[m,3] = sum(mylasso.coef != 0) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3,length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 9: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se
  a[m,4] = mean((Y[test.id] - Ytest.pred)^2)
  b[m,4] = (proc.time() - start.time)[1]
  #c[m,9] = length(sum()$coef) - 1
  c[m,4] = sum(mylasso.coef != 0) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3,length=200)))
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
  #Method 10: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.1se
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  mean((Ytest.pred - Y[test.id])^2)
  mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
  sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se
  
  var.sel = row.names(mylasso.coef)[nonzeroCoef(mylasso.coef)[-1]]
  tmp.X = X[, colnames(X) %in% var.sel]
  mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
  Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
  #a[m,10] = Ytest.pred
  a[m,5] = mean((Ytest.pred - Y[test.id])^2)
  b[m,5] = (proc.time() - start.time)[1]
  #c[m,10] = length(sum()$coef) - 1
  c[m,5] = length(mylasso.refit) - 1
  
  start.time = proc.time()
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  proc.time() - start.time
  
  #plot(cv.out)
  cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1, lambda = exp(seq(-15,3, length=200))   )
  best.lam = cv.out$lambda.min
  lam_range = range(log(cv.out$lambda))
  Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
  
}

boxplot(a)
boxplot(c)
group <- sample(1:50, 20, TRUE)
Time = colSums(b)

