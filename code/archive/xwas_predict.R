setwd("~/Dropbox/HIV xwas work/code")
source('util.R')
library(rms)


dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6b.csv")

dat$age <- 2014 - dat$v010
#dat$agecut <- cut(dat$age, c(0, 23, 33, Inf), labels=c('age_le_23', 'age_gt_23_le_33', 'age_gt_33'))
load('./result/xwas_merged_v6.Rdata')
sigFindings.adj <- subset(xwasData.all, validate_adj == 1)
## subsetting analyses and adjustments
exAnte <- c('age', 'v536_1', 'v025', 'v190') 
removeVars <- c('v30705', 'v624_1') ## remove correlated variables
varTypes <- introspect_variables(dat, ignoreThese = c('hiv03', 'v021', 'v005', 'age_cut'))


dependentVariable <- 'hiv03'
allVars <- setdiff(unique(c(dependentVariable, exAnte, sigFindings.adj$variable)), removeVars)


set.seed(123)
trainIndex <- sample.int(nrow(dat), nrow(dat)/2)
trainDat <- dat[trainIndex, c(allVars, 'v021', 'v005')]
testDat <- dat[-trainIndex, c(allVars, 'v021', 'v005')]

## do for exAnte, sigFindings (adj), etc
baseFormula <- as.formula(sprintf("%s~1", 'hiv03'))
depVars <- setdiff(allVars, 'hiv03')
for(ii in 1:length(depVars)) {
  varname <- depVars[ii]
  if(varname %in% varTypes$continuousVariables) {
    cat(sprintf('continuous:%i:%s\n', ii, varname))
    baseFormula <- addToBase(baseFormula, sprintf('I(scale(%s))', varname)) ## scale if a continuous
  } else {
    cat(sprintf('binary:%i:%s\n', ii, varname))
    baseFormula <- addToBase(baseFormula, varname)  
  }
}

or_and_confint <- function(dataTable) {
  colnames(dataTable) <- c('estimate', 'se', 't', 'pvalue', 'N')
  dataTable$odds_ratio <- exp(dataTable$estimate)
  dataTable$odds_ratio_low <- exp(dataTable$estimate - 1.96*dataTable$se)
  dataTable$odds_ratio_high <- exp(dataTable$estimate + 1.96*dataTable$se)
  dataTable$conf_int_string <- sprintf('%.03f [%.03f, %.03f]', dataTable$odds_ratio, dataTable$odds_ratio_low, dataTable$odds_ratio_high)
  return(dataTable)
}

train.mod <- lrm(baseFormula, trainDat) ## all vars
test.mod <- lrm(baseFormula, testDat) ## all vars
total.mod <- lrm(baseFormula, data=rbind(trainDat, testDat)) ## all vars
dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
total.mod.frm <- or_and_confint(logistic_svyglm(baseFormula, dsn))
write.table(total.mod.frm[, c('conf_int_string', 'pvalue')], sep="\t")


## train: 0.267
## test: 0.245
## all: 0.25

train.mod.exante <- lrm(hiv03 ~ age + v536_1 + v025 + v190, trainDat) ## ex ante adjustments
test.mod.exante <- lrm(hiv03 ~ age + v536_1 + v025 + v190, testDat) ## ex ante adjustments
total.mod.exante <- lrm(hiv03 ~ age + v536_1 + v025 + v190, rbind(trainDat,testDat) )
total.mod.exante.frm <- or_and_confint(logistic_svyglm(hiv03 ~ scale(age) + v536_1 + v025 + v190, dsn))
write.table(total.mod.exante.frm[, c('conf_int_string', 'pvalue')], sep="\t")
## train: 0.13
## test: 0.10
## total: 0.11

