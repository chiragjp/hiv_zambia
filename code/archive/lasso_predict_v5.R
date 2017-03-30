
#Create a LASSO predictor on the training data, test on testing
## author: "Chirag J Patel"
## build a lasso logistic for prediction


setwd("~/Dropbox/HIV xwas work/code")
source('util.R')
library(glmnet)

dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6.csv")
dat$v000 <- NULL
dependentVariable <- 'hiv03'

## split dataset into training and testing
set.seed(123)
trainIndex <- sample.int(nrow(dat), nrow(dat)/2)
trainDat <- dat[trainIndex, ]
testDat <- dat[-trainIndex, ]


### Now apply LASSO on training data
non.na.counts <- apply(trainDat, 2, function(x) sum(!is.na(x)))
bigColnames <- names(non.na.counts[non.na.counts > 7600]) ### select variables that occur in most of the data
trainDat2 <- trainDat[, bigColnames]
trainDat2 <- trainDat2[complete.cases(trainDat2), ]
y.train <- trainDat2$hiv03
x.train <- as.matrix(trainDat2[, -1])
tr.net<- cv.glmnet(x=x.train, y=y.train, alpha=1, family='binomial')
tr.net.mod <- glmnet(x=x.train, y=y.train, alpha=1, family='binomial', lambda=tr.net$lambda.min, standardize = F)
coefs <- coef(tr.net.mod)
coefFrame <- data.frame(glm_coef=coefs[,1], name=rownames(coefs))

# Check prediction on testing data
testDat2 <- testDat[, bigColnames]
testDat2 <- testDat2[complete.cases(testDat2), ]
testDat2 <- as.matrix(testDat2)
x.test <- testDat2[, -1]
test.pred <- predict(tr.net.mod, newx=x.test, type='response')
train.pred <- predict(tr.net.mod, newx=x.train, type='response')

save(trainDat, testDat, tr.net, tr.net.mod, coefFrame, coefs, bigColnames, test.pred, train.pred,file='lasso_predict.Rdata')