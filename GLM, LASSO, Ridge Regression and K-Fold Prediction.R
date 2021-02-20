# Odell


library(ggplot2)
library(ISLR)
set.seed(1)
train <-sample(392,196) #Sample data

#Fit model with a subset for train
lm_fit <- lm(mpg ~ horsepower , data = Auto, subset = train)

attach(Auto) # Makes pieces of Auto searchable by name

mean((mpg - predict(lm_fit, Auto))[-train]^2) # Prediction for non train data

#Use poly fuction to estimate the test error for the polynomial and cubic regressions

lm_fit2 <- lm(mpg ~ poly(horsepower, 2) , data = Auto, subset = train)
mean((mpg - predict(lm_fit2, Auto))[-train]^2) # find the MSE for the new fit model

#cubic regressions
lm_fit3 <- lm(mpg ~ poly(horsepower, 3) , data = Auto, subset = train)
mean((mpg - predict(lm_fit3, Auto))[-train]^2) # find the MSE for the new fit model


#===========  repeat with new seed ========================

set.seed(2)
train <-sample(392,196) #Sample data
#Fit model with a subset for train
lm_fit <- lm(mpg ~ horsepower , data = Auto, subset = train)
mean((mpg - predict(lm_fit, Auto))[-train]^2) # Prediction for non train data

#Use poly fuction to estimate the test error for the polynomial and cubic regressions

lm_fit2 <- lm(mpg ~ poly(horsepower, 2) , data = Auto, subset = train)
mean((mpg - predict(lm_fit2, Auto))[-train]^2) # find the MSE for the new fit model

#cubic regressions
lm_fit3 <- lm(mpg ~ poly(horsepower, 3) , data = Auto, subset = train)
mean((mpg - predict(lm_fit3, Auto))[-train]^2) # find the MSE for the new fit model

# ==== LOOCV ================

glm_fit <- glm(mpg~ horsepower ,data=Auto) # GLM without family argument performs lm function
coef(glm_fit)

#Showing example of the similarity
lm_fit <- lm(mpg ~ horsepower ,data=Auto)
coef(lm_fit)

library(boot)
# gives the cross-validation estimate for these results
glm_fit <-glm(mpg ~ horsepower ,data=Auto)
cv_err <-  cv.glm(Auto, glm_fit)
cv_err$delta


cv_error <- rep ( 0, 5)
for ( i in 1:5) {
  glm_fit <- glm(mpg ~ poly (horsepower, i), data = Auto)
  cv_error[i] = cv.glm(Auto, glm_fit)$delta[1]
}

cv_error # Print results

# ======= k-fold Cross Validation ====================

# Compute a k-fold cross validation with a for loop
set.seed(17)
 cv_error_10 <- rep(0 ,10)
 for (i in 1:10){
   glm_fit <- glm(mpg ~ poly(horsepower ,i),data=Auto)
   cv_error_10[i] <- cv.glm(Auto ,glm_fit , K=10)$delta[1]
   }
 cv_error_10

# ========= Bootstrap ============
 
 # Creating a bootstrap,
 # First we have to create a function to assign the X & Y to know how to calculate the critical value
 alpha_fn <- function (data ,index){
    X <- data$X[index]
    Y <- data$Y[index]
    return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
    }
 
#Calculate the critical value estimate of Portfolio data 
alpha_fn(Portfolio, 1:100)

set.seed(1)
alpha_fn(Portfolio ,sample (100,100, replace=TRUE)) # a bootstrapped alpha hat for bootstrap

# ========= Subset part of lab ============
fix(Hitters)
names(Hitters)

dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters) # omits the NA values
dim(Hitters)

sum(is.na(Hitters$Salary)) # Check that the NA are removed

library(leaps)
regfit_full <- regsubsets (Salary~.,Hitters) # assign the RSS of best to a value
summary(regfit_full) # Find the RSS (Best fit) 

regfit_full <-  regsubsets (Salary ~ . , data = Hitters, nvmax = 19) # compute for top 19 parameters

reg_summary = summary (regfit_full)

names(reg_summary) # See the diferrent model measurements

reg_summary$rsq


# Plot the results to see what model fits the best
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type="l")
plot(reg_summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(reg_summary$adjr2) # Find the maximum point

points (11,reg_summary$adjr2[11], col="red",cex=2,pch =20) # plot the maximum point

# We will plot the Cp and BIC as well
plot(reg_summary$cp ,xlab="Number of Variables ",ylab=" Cp",
     type= "l")

which.min(reg_summary$cp )

points (10,reg_summary$cp [10], col ="red",cex=2,pch =20)

which.min(reg_summary$bic )

plot(reg_summary$bic ,xlab="Number of Variables ",ylab="BIC",
       type="l")

points (6,reg_summary$bic [6],col="red",cex=2,pch =20)

#Built in Plot command
plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")


# ========== Forward and Backward Stepwise Selection =============

#Perform Forward and Backward stepwise selection with regsubsets
regfit_fwd <- regsubsets (Salary~.,data=Hitters , nvmax=19,
                       method ="forward")
summary (regfit_fwd)

regfit_bwd <- regsubsets (Salary~.,data=Hitters , nvmax=19,
                         method ="backward")
summary (regfit_bwd)

coef(regfit_full, 7)
coef(regfit_fwd, 7)
coef(regfit_bwd, 7)


# =========== Choosing among models ===================

# Assign the data to Training sets or Test Sets
set.seed(1)
train <- sample(c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
test <- (!train)

#now we apply the resubsets to perform best subset selection
reg_fit_best <- regsubsets (Salary~ .,data=Hitters[train ,],
                             nvmax=19)

test_mat <- model.matrix(Salary~.,data=Hitters [test ,])

val_errors <- rep(NA, 19) # Create a loop to run through the 19 fit options
for(i in 1:19){
  coefi <- coef(reg_fit_best, id = i)
  pred <- test_mat [,names(coefi)]%*%coefi
  val_errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

val_errors # Get the results of the function

which.min(val_errors) # Find the minimum test error

coef(reg_fit_best, 10) # Get the information for the best fit model

# Create a function to cal the predictions 
predict_regsubsets <- function (object , newdata ,id ,...){
   form <- as.formula(object$call [[2]])
   mat <- model.matrix(form ,newdata )
   coefi <- coef(object ,id=id)
   xvars <- names(coefi)
   mat[,xvars]%*%coefi
   }

reg_fit_best <- regsubsets (Salary~.,data=Hitters ,nvmax=19) # Fit the model on the full data set
coef(reg_fit_best ,10)

k <-  10
set.seed(1)
folds <-  sample (1:k, nrow(Hitters), replace = TRUE) 
cv_errors <-  matrix(NA, k, 19, dimnames = list (NULL, paste(1:19)))


for(j in 1:k){
   best_fit <- regsubsets (Salary~.,data=Hitters [folds!=j,],
                         nvmax=19)
   for(i in 1:19){
     pred <-  predict_regsubsets(best_fit , Hitters [ folds ==j,], id=i)
     cv_errors [j,i] <-  mean( ( Hitters$Salary[ folds==j]-pred)^2)
    }
   }

mean_cv_errors <- apply(cv_errors ,2, mean)

mean_cv_errors
par(mfrow=c(1,1))
plot(mean_cv_errors ,type="b")

reg_best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg_best, 11)


# ========= Lasso =======================

library(glmnet)

#Can only use qualitative inputs
# has to be assigned to an x and a y
x <- model.matrix(Salary ~ .,Hitters )[,-1]
y <- Hitters$Salary

grid <- 10^seq(10, -2, length = 100)
ridge_mod <-  glmnet(x, y, alpha = 0, lambda = grid)

cv_lasso <-  cv.glmnet(x , y, type.measure = "mse")

summary(cv_lasso)
plot(cv_lasso)
coef(cv_lasso)
sqrt(cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.1se])

# ========= Ridge Regression ==============

cv_ridge <- cv.glmnet( x, y, type.measure = "mse", alpha = 0)

plot(cv_ridge)
coef(cv_ridge)
sqrt(cv_ridge$cvm[cv_ridge$lambda == cv_ridge$lambda.1se])

