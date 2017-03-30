## build a predictor for the top xwas predictors

dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6.csv")
### v021 is cluster
### v005 is sample weight
dat$v000 <- NULL




set.seed(123)
trainIndex <- sample.int(nrow(dat), nrow(dat)/2)
trainDat <- dat[trainIndex, ]
testDat <- dat[-trainIndex, ]

vars <- c('hiv03', subset(xwasData, pvalue_train < bonferroniThreshold & abs(estimate_train) < 4)$variable_train) ### filter out overfitting variables?
vars <- setdiff(vars, 'v623_3')
trainDat2 <- trainDat[, vars]
non.na.counts <- apply(trainDat2, 2, function(x) sum(!is.na(x)))
bigColnames <- names(non.na.counts[non.na.counts > 7600]) ### select variables that occur in most of the data
trainDat2 <- trainDat2[complete.cases(trainDat2[, bigColnames]), bigColnames]
testDat2 <- testDat[complete.cases(testDat[, bigColnames]), bigColnames]
train.mod <- glm(hiv03 ~ ., trainDat2, family='binomial')

train.pred <- predict(train.mod, type='response')
test.pred <- predict(train.mod, testDat2[,-1], type='response')

save(trainDat, trainDat2,  testDat, testDat2, train.mod,vars, bigColnames, test.pred, train.pred,file='xwas_predict.Rdata')
