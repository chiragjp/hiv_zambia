## test how much predcitive value we have for replicated factors

## use those that are bonferroni signficant in one and see how they predict in training
#dat <- read.csv('../data/hiv xwas Zambia07 dataset v7.csv')
library(ROCR)
library(glmnet)
dat07 <- read.csv('../data/hiv xwas Zambia07 dataset v7.csv')
dat13 <- read.csv('../data/hiv xwas Zambia13 dataset v7.csv')
load('./merged_output/merged.Rdata')
## same for 07 and 13

getTrainIndex <- function(N) {
  set.seed(123)
  trainIndex <- sample.int(N, N/2)
  return(trainIndex)
}

##### 2007
dat <- dat07
dat <- dat[dat$hiv03 <= 1, ]
N <- nrow(dat)
trainIndex <- getTrainIndex(N)
trainDat <- dat[trainIndex, ]
testDat <- dat[-trainIndex, ]
## get those significant in 07
sigTrainVars <- unique(mergedResults$merged_data[which(mergedResults$merged_data$pvalue_train_univariate_07 < 0.05/nrow(mergedResults$merged_data)), 'variable_univariate_07'])
trainDat <- trainDat[complete.cases(trainDat[, c(sigTrainVars, 'hiv03')]), c(sigTrainVars, 'hiv03')]
testDat <- testDat[complete.cases(testDat[, c(sigTrainVars, 'hiv03')]), c(sigTrainVars, 'hiv03')]

modTrain <- glm(hiv03 ~ ., trainDat, family=binomial())
pred <- prediction(predict(modTrain, newdata=testDat), testDat$hiv03)
perf.tpfp <- performance(pred,"tpr","fpr")
performance(pred,"auc")
plot(perf.tpfp, colorize=T)

##
#modTrain.glmnet <- glmnet(x=as.matrix(trainDat[, -1]),y=trainDat$hiv03,family="binomial", standardize=F)
# pred.glmnet <- prediction(predict(modTrain.glmnet, newx=as.matrix(testDat[, -1])), testDat$hiv03)
# perf.tpfp <- performance(pred.glmnet,"tpr","fpr")
# performance(pred.glmnet ,"auc")
# plot(perf.tpfp, colorize=T)
####


## now how good does the 07 predictors replicated in 07 forecast 13????
# replicated07 <- mergedResults$merged_data[mergedResults$merged_data$replicated_univariate_07 == 1, 'variable_univariate_07']
# varsin13 <- intersect(replicated07, colnames(dat13)) ## replicated in 07
# dat <- dat13[, c(varsin13, 'hiv03')]
# dat <- dat[dat$hiv03 <= 1, ]
# dat <- dat[complete.cases(dat), ]
# 
# mod13 <- glm(hiv03 ~ ., dat, family=binomial())
# pred <- prediction(predict(mod13), dat$hiv03)
# perf.tpfp <- performance(pred,"tpr","fpr")
# performance(pred,"auc")
# plot(perf.tpfp, colorize=T) ### about 80 AUC


