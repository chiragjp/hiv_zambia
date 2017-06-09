## Prediction
## Chirag Patel

library(xgboost)
library(glmnet)

dat <- read.csv('https://www.dropbox.com/s/k87iv7ouolpvraf/hiv%20xwas%20Zambia13%20dataset%20v7.csv?dl=0') ## get data from dropbox
dat$v000 <- NULL
dat$cname <- NULL
dat$id1 <- NULL
dependentVariable <- 'hiv03'

y <- dat[, dependentVariable]
x <- as.matrix(dat[, setdiff(colnames(dat), dependentVariable)])


cv.nround <- 50
cv.nfold <- 20
param <- list("objective" = "binary:logistic", 
              "max.depth"=10, "metrics"='auc')

bst.cv = xgb.cv(param=param, data = x, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, metrics='auc')


