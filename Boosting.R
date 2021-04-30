library(MASS)
library(mvtnorm)
library(kernlab)
library(e1071)
library(class)
library(rpart)
library(ipred)
library(caret)
library(randomForest)
library(gbm)
library(adabag)

################## Handwritten Digit Recognition Data ######################

digit = read.table('Digit_recog_train.txt')
digit_no = digit[,1]
digit_pix = digit[,-1]
digit_no3 = digit_no[digit_no == 3]
digit_pix3 = digit_pix[digit_no == 3,]
digit_no13 = digit_no[union(which(digit_no == 1), which(digit_no == 3))]
digit_pix13 = digit_pix[union(which(digit_no == 1), which(digit_no == 3)),]

sel = sample(c(0,1), length(digit_no), replace = T, prob = c(1/3,2/3))
digit_pix.tr = digit_pix[sel == 1,]
digit_no.tr = digit_no[sel==1]
digit_tr = digit[sel==1,]
digit_pix.test = digit_pix[sel!=1,]
digit_no.test = digit_no[sel!=1]
digit_test = digit[sel!=1,]
ynam = names(digit_tr)[1]
xnam = names(digit_tr)[-1]
formula = as.formula(paste(ynam," ~ ", paste(xnam, collapse= "+"),sep=""))

sel = sample(c(0,1), length(digit_no13), replace = T, prob = c(1/4,3/4))
digit_pix.tr13 = digit_pix13[sel == 1,]
digit_no.tr13 = digit_no13[sel==1]
digit_no.tr13 = factor(digit_no.tr13)
digit_pix.test13 = digit_pix13[sel != 1,]
digit_no.test13 = digit_no13[sel!=1]

################### Simulated Data ############################################

n=200
pin1 = sample(1:2, n, replace = T, prob = rep(1/2, 2))
mu = matrix(c(0,0, 3,3), nrow = 2, ncol=2, byrow = T)
sd = c(0.5,1)
xsim1 = matrix(0, nrow = n, ncol = 2)
for(i in 1:n) {
  xsim1[i,] = rmvnorm(1, mu[pin1[i],], sd[pin1[i]]*diag(2))
}
#plot(xsim1[,1], xsim1[,2], col = factor(pin))
sim.data1 = data.frame(y=pin1, x=xsim1)

n=200
pin = sample(1:2, n, replace = T, prob = rep(1/2, 2))
mu = matrix(c(0,0, 2,2), nrow = 2, ncol=2, byrow = T)
sd = c(0.5,1.5)
xsim2 = matrix(0, nrow = n, ncol = 2)
for(i in 1:n) {
  xsim2[i,] = rmvnorm(1, mu[pin[i],], sd[pin[i]]*diag(2))
}
#plot(xsim1[,1], xsim1[,2], col = factor(pin))
sim.data2 = data.frame(y=pin, x=xsim2)

################### Boosting ############################################

prt = proc.time()
gbm.sim = gbm.fit(x=sim.data2[,2:3], y=(sim.data2$y-1), distribution = 'adaboost', n.trees = 500, cv.folds = 3)
proc.time() - prt
pred.gbm.sim = rep(1, length(gbm.sim$fit))
pred.gbm.sim[gbm.sim$fit > 0] = 2
plot(sim.data2$x.1, sim.data2$x.2, pch = sim.data2$y, col = factor(pred.gbm.sim))
gbm.perf(gbm.sim)

prt = proc.time()
#gbm.digit = gbm.fit(digit_pix.tr, factor(digit_no.tr), distribution = 'multinomial', n.trees = 500, shrinkage = 0.05, nTrain = (0.7*(dim(digit_pix.tr)[1])))
gbm.digit = gbm(formula, data = digit_tr, distribution = 'multinomial', n.trees = 500, shrinkage = 0.05, train.fraction = 0.7, cv.folds = 3, verbose = T)
proc.time() - prt
gbm.perf(gbm.digit)
par(mfrow = c(1,2))
plot(gbm.digit$train.error)
plot(gbm.digit$valid.error)
boostPred.digit = predict(gbm.digit, digit_pix.test, n.trees = 500)
res = apply(boostPred.digit, 1, which.max)-1
cm.gbm = confusionMatrix(data = res, reference = digit_no.test)
cm.gbm$byClass
cm.gbm$table
mean(cm.gbm$byClass[,8])

#prt = proc.time()
#adaboost.sim = boosting(y~x.1+x.2, data = sim.data2, mfinal = 50)
#proc.time() - prt
#pred.adaboost.sim = adaboost.sim$class
#plot(sim.data2$x.1, sim.data2$x.2, pch = sim.data2$y, col = factor(pred.adaboost.sim))
