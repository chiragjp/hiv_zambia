### Chirag Patel
### XWAS with HIV dataset "v3"
### 9/29/2015

setwd("~/Dropbox/HIV xwas work/code")
source('util.R')

dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v4.csv")
# v021 is cluster
# v005 is sample weight
dat$hiv <- ifelse(dat$hiv03 == 'hiv  positive', 1, 0)
dat$hiv03 <- NULL
dat$v000 <- NULL

dat$v152 <- as.numeric(dat$v152)
dat$v152cat<-NA
dat$v152cat[dat$v152<36]<-0
dat$v152cat[dat$v152>35 & dat$v152<46]<-1
dat$v152cat[dat$v152>45]<-2
dat$v152cat <- as.factor(dat$v152cat)

dat$v167cat<-NA
dat$v167cat[dat$v167==0]<-0
dat$v167cat[is.na(dat$v167)]<-0
dat$v167cat[dat$v167>0 & dat$v477<3]<-1
dat$v167cat[dat$v167>2]<-2
dat$v167cat <- as.factor(dat$v167cat)

dat$v152 <- dat$v167 <- dat$v477 <- dat$v012 <- NULL

## need to also account for adjustment variables (later)
dependentVariable <- 'hiv'
baseFormula <- as.formula(sprintf('%s~1', dependentVariable))

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

xwas <- function(baseFormula, dat, categoricalFrame, varTypes) {
  allData <- data.frame() 
  # first do categoricals
  variables <- unique(categoricalFrame$variable)
  m <- length(variables)
  dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
  for(ii in 1:m) {
    catVarFrame <- subset(categoricalFrame, variable == variables[ii])
    cat(sprintf('categorical:%i:%s\n', ii, variables[ii]))
    if(sum(catVarFrame$level > 1) > 0 ) {
      catVarFrame <- subset(catVarFrame, level > 1)
    }
    newMod <- addToBase(baseFormula, catVarFrame$varname)
    ## frm <- logistic_glm(newMod, dat)
    frm <- logistic_svyglm(newMod, dsn)
    frm$variable <- variables[ii]
    frm$variable_model <- rownames(frm)
    allData <- rbind(allData, frm)
  }
  
  # then do the rest
  theRest <- c(varTypes$continuousVariables, varTypes$factorVariables, varTypes$yesNoVariables)
  for(ii in 1:length(theRest)) {
    varname <- theRest[ii]
    cat(sprintf('others:%i:%s\n', ii, varname))
    if(varname %in% varTypes$continuousVariables) {
      newMod <- addToBase(baseFormula, sprintf('I(scale(%s))', varname))  
    } else {
      newMod <- addToBase(baseFormula, varname)  
    }
    ## frm <- tryCatch(logistic_glm(newMod, dat), error = function(e) {print(e); return(NULL)})
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

allData.overall <- xwas(baseFormula, dat, categoricalFrame, varTypes)
allData.train <- xwas(baseFormula,trainDat, categoricalFrame, varTypes)
allData.test <- xwas(baseFormula, testDat, categoricalFrame, varTypes)

xwasData.train <- allData.train[-grep('Intercept', allData.train$variable_model), ]
colnames(xwasData.train) <- paste(colnames(xwasData.train), '_train', sep="")
xwasData.test <- allData.test[-grep('Intercept', allData.test$variable_model), ]
colnames(xwasData.test) <- paste(colnames(xwasData.test), '_test', sep="")
xwasData.overall <- allData.overall[-grep('Intercept', allData.overall$variable_model), ]
xwasData <- merge(xwasData.overall, xwasData.test, by.x='variable_model', by.y='variable_model_test', all.x=T)
xwasData <- merge(xwasData, xwasData.train, by.x='variable_model', by.y='variable_model_train', all.x=T)

save(xwasData,allData.overall,file='xwas_v3a.Rdata')
