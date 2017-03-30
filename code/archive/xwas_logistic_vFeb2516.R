#!/usr/bin/Rscript
### Chirag Patel
### XWAS with HIV dataset "v3"
### 9/29/2015

setwd("~/Dropbox/HIV xwas work/code")
source('util.R')


dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6c.csv")
### v021 is cluster
### v005 is sample weight
dat$v000 <- NULL

## need to also account for adjustment variables (later)
dependentVariable <- 'hiv03'
#baseFormula <- as.formula(sprintf('%s~1', dependentVariable))
#adjustmentVariables <- c('v190', 'v025', 'v536_1', 'v010')
adjustmentVariables <- c('v025', 'v010')
#adjustmentVariables <- c()
adjustmentString <- paste(adjustmentVariables, collapse="+")
baseFormula <- as.formula(sprintf("%s~1", dependentVariable))
if(length(adjustmentVariables) > 1) {
    baseFormula <- as.formula(sprintf('%s~%s', dependentVariable, adjustmentString))
}



## get categorical and continuous variables
varTypes <- introspect_variables(dat, ignoreThese = c('hiv', 'v021', 'v005'))
categoricalVariables <- varTypes$categoricalVariables
## create new frame for categoricals; reference group will be _1
categoricalFrame <- t(data.frame(strsplit(categoricalVariables, '\\_')))
rownames(categoricalFrame) <- c()
categoricalFrame <- as.data.frame(categoricalFrame)
colnames(categoricalFrame) <- c('variable', 'level')
categoricalFrame$varname <- paste(categoricalFrame$variable, categoricalFrame$level, sep="_")
categoricalFrame$level <- as.numeric(as.character(categoricalFrame$level))

## split dataset into training and testing
set.seed(123)
trainIndex <- sample.int(nrow(dat), nrow(dat)/2)
trainDat <- dat[trainIndex, ]
testDat <- dat[-trainIndex, ]

xwas <- function(baseFormula, dat, varTypes) {
  allData <- data.frame() 
  dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
  variables <- c(varTypes$continuousVariables,varTypes$categoricalVariables, varTypes$factorVariables, varTypes$yesNoVariables)
  for(ii in 1:length(variables)) {
    varname <- variables[ii]
    if(varname %in% varTypes$continuousVariables) {
      cat(sprintf('continuous:%i:%s\n', ii, varname))
      newMod <- addToBase(baseFormula, sprintf('I(scale(%s))', varname)) ## scale if a continuous
    } else {
      cat(sprintf('binary:%i:%s\n', ii, varname))
      newMod <- addToBase(baseFormula, varname)  
    }
    frm <- tryCatch(logistic_svyglm(newMod, dsn), error = function(e) {print(e); return(NULL)})
    if(!is.null(frm)) {
      frm$variable <- varname
      frm$variable_model <- rownames(frm)
      allData <- rbind(allData, frm)
    }
  }
  colnames(allData) <- c('estimate', 'se', 'z', 'pvalue', 'N', 'variable', 'variable_model')
  return(allData)
}

allData.overall <- xwas(baseFormula, dat, varTypes)
allData.train <- xwas(baseFormula,trainDat, varTypes)
allData.test <- xwas(baseFormula, testDat, varTypes)

toRemove <- c(adjustmentVariables, '(Intercept)')
xwasData.train <- allData.train[!allData.train$variable_model %in% toRemove, ]
colnames(xwasData.train) <- paste(colnames(xwasData.train), '_train', sep="")
xwasData.test <- allData.test[!allData.test$variable_model %in% toRemove, ]
colnames(xwasData.test) <- paste(colnames(xwasData.test), '_test', sep="")
xwasData.overall <- allData.overall[!allData.overall$variable_model %in% toRemove, ]
xwasData <- merge(xwasData.overall, xwasData.test, by.x='variable_model', by.y='variable_model_test', all.x=T)
xwasData <- merge(xwasData, xwasData.train, by.x='variable_model', by.y='variable_model_train', all.x=T)

#####
save(xwasData,allData.overall, adjustmentVariables,file='xwas_v5.Rdata')
