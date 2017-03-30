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
binaryFrame <- data.frame(variable_name=varTypes$yesNoVariables)
factorFrame <- data.frame(variable_name=varTypes$factorVariables)
continuousFrame <- data.frame(variable_name=varTypes$continuousVariables)
### write this out
write.csv(categoricalFrame, '../data/categorical_variables.csv', row.names=F)
write.csv(binaryFrame, '../data/binary_variables.csv', row.names=F)
write.csv(continuousFrame, '../data/continuous_variables.csv', row.names=F)
write.csv(factorFrame, '../data/factor_variables.csv', row.names=F)


