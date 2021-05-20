library(Sleuth3)
library(ggplot2)
library(broom)

case1002
?case1002

# Exploring the data through plot
qplot(Mass, Energy, data = case1002, color = Type)

# adjustming the Energy with a log transformation
qplot(Mass, log(Energy), data = case1002, color = Type)


#Data isn't that linear, due to a change in mass so log mass as well

qplot(log(Mass), log(Energy), data = case1002, color = Type)


#==== Intersted in 3 models =======


# both birds and non-echo bats have different energy costs than echo bats
#after accounting for log energy and log mass
m1 <- lm(log(Energy) ~ log(Mass) + Type, data = case1002)


#The energy costs of non-echo bats and echo-bats is the same 
#but different than birds
#after accounting for log energy and log mass
m2 <- lm(log(Energy)~ log(Mass) + 
           I(Type == "non-echolocating birds"), data = case1002)

#All types have the same energy costs
#after accounting for log energy and log mass
m3 <- lm(log(Energy) ~ log(Mass), data = case1002)

# creating data frame for prediction between one and two
same_weight_bats <- data.frame(
  Type = rep(c("echolocating bats", "non-echolocating bats"), 3),
  Mass = rep(c(100, 400, 700), each = 2)
)
same_weight_bats

# Predictions

#m1
cbind(same_weight_bats,
      pred_log_energy = predict(m1, newdata = same_weight_bats))

#m2
cbind(same_weight_bats, pred_log_energy =
        predict(m2, newdata = same_weight_bats))

#Checking the beta 2 estimate
coef(m1)

#transforming off the log scale
cbind(same_weight_bats,
      pred_energy = exp(predict(m1, newdata = same_weight_bats)))

#augment M1, and look at the residual vs fitted
#heck out the fit of the models
case1002_diag <- augment(m1, case1002)
qplot(.fitted, .resid, data = case1002_diag)

qplot(log(Mass), .resid, data = case1002_diag)

qplot(Type, .resid, data = case1002_diag)


#Summary of M1
summary(m1)

