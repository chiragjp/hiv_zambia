#!/usr/local/bin/Rscript
### Chirag Patel
### XWAS with HIV dataset "v6b"
### 4/5/2016
###

setwd("~/Dropbox/HIV xwas work/code")
source('util.R')


dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6b.csv")

##
# v190: wealth index (1, 2, 3, 4, 5)
# v025: type of place of residence (0,1)
# v536_1: recent sexual activity (0,1)
# v010: repondent's year of birth (as year)
# v781: ever tested for HIV
##

##
# v021 is cluster
# v005 is sample weight
## 

## categorize age
dat$age <- 2014 - dat$v010
#dat$agecut <- cut(dat$age, c(0, 23, 33, Inf), labels=c('age_le_23', 'age_gt_23_le_33', 'age_gt_33'))

## subsetting analyses and adjustments
#adjustmentVariables <- c("v501_4", "v150_1", "v605_1", "v624_1", "v404", "v763a" , "v151", 'v161_7', 'v825', 's1007a', c('agecut', 'v781', 'v536_1', 'v025', 'v190'))
exAnte <- c('age', 'v536_1', 'v025', 'v190') 
adjustmentVariables <- c("v501_4", "v150_1", "v605_1", "v536_1", "v025", "v624_1", "v404","age", "v763a", "v151", "v190")
#adjustmentVariables <- c("v501_4", "v150_1", "v605_1", "v536_1", "v025", "v624_1", "v404",  "age", "v763a", "v190"  )
#dat <- subset(dat, v190 > 3)
#DATAOUTFILE <- 'xwas_super_adjusted.Rdata'
#notes <- 'analysis adjusted by common risk factors, wealth, place, age(cut), recent sex, ever tested for year of birth PLUS top 10 factors found in unadjusted analysis'
DATAOUTFILE <- 'xwas_ex_post_ante.Rdata'
notes <- 'adjusted by the union of top 10 factors and ex ante adjustments (total of 11)'


dat$v000 <- NULL
dependentVariable <- 'hiv03'
adjustmentString <- paste(adjustmentVariables, collapse="+")
baseFormula <- as.formula(sprintf("%s~1", dependentVariable))
if(length(adjustmentVariables) > 1) {
    baseFormula <- as.formula(sprintf('%s~%s', dependentVariable, adjustmentString))
}


## get categorical and continuous variables
varTypes <- introspect_variables(dat, ignoreThese = c('hiv03', 'v021', 'v005', 'age', 'age_cut'))
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

toRemove <- c(adjustmentVariables, '(Intercept)', 'agecutage_le_23', 'agecutage_gt_23_le_33', 'agecutage_gt_33')

xwasData.train <- allData.train[!allData.train$variable_model %in% toRemove, ]
colnames(xwasData.train) <- paste(colnames(xwasData.train), '_train', sep="")
xwasData.test <- allData.test[!allData.test$variable_model %in% toRemove, ]
colnames(xwasData.test) <- paste(colnames(xwasData.test), '_test', sep="")
xwasData.overall <- allData.overall[!allData.overall$variable_model %in% toRemove, ]
xwasData <- merge(xwasData.overall, xwasData.test, by.x='variable_model', by.y='variable_model_test', all.x=T)
xwasData <- merge(xwasData, xwasData.train, by.x='variable_model', by.y='variable_model_train', all.x=T)

#####
save(xwasData,allData.overall, adjustmentVariables,notes, file=DATAOUTFILE)
