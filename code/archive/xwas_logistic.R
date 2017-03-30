### Chirag Patel
### XWAS with sample HIV dataset
### 7/21/2015

source('util.R')
dat <- read.csv("~/Dropbox (RagGroup)/HIV xwas work/data/hiv xwas toy dataset v1.csv")
dat$v000 <- NULL ## source of data? Zimbabwe?

## need to also account for adjustment variables (later)
dependentVariable <- 'hiv03'
baseFormula <- as.formula(sprintf('%s~1', dependentVariable))

## get categorical and continuous variables
categoricalVariables <- colnames(dat)[grep('\\_', colnames(dat))]
continuousVariables <- setdiff(colnames(dat), c(categoricalVariables, dependentVariable))

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

xwas <- function(baseFormula, dat) {
  allData <- data.frame() 
  # first do categoricals
  variables <- unique(categoricalFrame$variable)
  m <- length(variables)
  for(ii in 1:m) {
    catVarFrame <- subset(categoricalFrame, variable == variables[ii])
    if(sum(catVarFrame$level > 1) > 0 ) {
      catVarFrame <- subset(catVarFrame, level > 1)
    }
    newMod <- addToBase(baseFormula, catVarFrame$varname)
    frm <- logistic_glm(newMod, dat)
    frm$variable <- variables[ii]
    frm$variable_model <- rownames(frm)
    allData <- rbind(allData, frm)
  }
  
  # then do continuous
  for(ii in 1:length(continuousVariables)) {
    varname <- continuousVariables[ii]
    newMod <- addToBase(baseFormula, sprintf('I(scale(%s))', varname))
    frm <- logistic_glm(newMod, dat)
    frm$variable <- varname
    frm$variable_model <- rownames(frm)
    allData <- rbind(allData, frm)
  }
  
  colnames(allData) <- c('estimate', 'se', 'z', 'pvalue', 'N', 'variable', 'variable_model')
  return(allData)
}

allData.overall <- xwas(baseFormula, dat)
allData.train <- xwas(baseFormula,trainDat)
allData.test <- xwas(baseFormula, testDat)

xwasData.train <- allData.train[-grep('Intercept', allData.train$variable_model), ]
colnames(xwasData.train) <- paste(colnames(xwasData.train), '_train', sep="")
xwasData.test <- allData.test[-grep('Intercept', allData.test$variable_model), ]
colnames(xwasData.test) <- paste(colnames(xwasData.test), '_test', sep="")
xwasData.overall <- allData.overall[-grep('Intercept', allData.overall$variable_model), ]
xwasData <- merge(xwasData.overall, xwasData.test, by.x='variable_model', by.y='variable_model_test', all.x=T)
xwasData <- merge(xwasData, xwasData.train, by.x='variable_model', by.y='variable_model_train', all.x=T)

save(xwasData,allData.overall,file='xwas_toy.Rdata')
