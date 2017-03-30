# Chirag Patel
# 3/22/2016
# test for the interaction between variables in HIV XWAS

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

## categorize age
dat$age <- 2014 - dat$v010
dat$agecut <- cut(dat$age, c(0, 23, 33, Inf), labels=c('age_lt_23', 'age_gt_23_lt_33', 'age_gt_33'))
test <- model.matrix(~ dat[, 'agecut'] - 1)
colnames(test) <- c('age_lt_23', 'age_gt_23_lt_33', 'age_gt_33')
dat <- cbind(dat, test)
remove(test)

dat$wealthcut <- cut(dat$v190, c(0, 3, 5))
test <- model.matrix( ~ dat[, 'wealthcut'] - 1)
colnames(test) <- c('poor', 'rich')
dat <- cbind(dat, test)
remove(test)

dat$hiv_test <- dat$v781
dat$not_hiv_test <- NA
dat$not_hiv_test <- ifelse(dat$v781==1, 0, 1)

dat$urban <- dat$v025
dat$rural <- NA
dat$rural <- ifelse(dat$v025 == 1, 0, 1)

## subsetting analyses and adjustments
#adjustmentVariables <- c("v501_4", "v150_1", "v605_1", "v624_1", "v404", "v763a" , "v151", 'v161_7', 'v825', 's1007a', c('agecut', 'v781', 'v536_1', 'v025', 'v190'))
adjustmentVariables <- c()
interactingVariables <- c('age_lt_23', 'age_gt_33', 'age_gt_23_lt_33', 'poor', 'rich', 'hiv_test','not_hiv_test', 'rural', 'urban')
load('./result/xwas_merged_v6.Rdata')
sigVariables <- subset(xwasData.all, validate_adj==1)$variable

notes <- 'interaction between variables'
# v021 is cluster
# v005 is sample weight

dat$v000 <- NULL
dependentVariable <- 'hiv03'
adjustmentString <- paste(adjustmentVariables, collapse="+")
baseFormula <- as.formula(sprintf("%s~1", dependentVariable))
if(length(adjustmentVariables) > 1) {
  baseFormula <- as.formula(sprintf('%s~%s', dependentVariable, adjustmentString))
}


## get categorical and continuous variables
varTypes <- introspect_variables(dat, ignoreThese = c('hiv', 'v021', 'v005', 'age', 'age_cut', interactingVariables))

interaction_test <- function(baseFormula, dat, varTypes, variable, interactor) {
  dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
  varname <- variable
  if(varname %in% varTypes$continuousVariables) {
    cat(sprintf('continuous:%s\n', varname))
    newMod <- addToBase(baseFormula, sprintf('I(scale(%s))*%s', varname, interactor)) ## scale if a continuous
  } else {
    cat(sprintf('binary:%s\n', varname))
    newMod <- addToBase(baseFormula, sprintf('%s*%s', varname, interactor))  
  }
  frm <- tryCatch(logistic_svyglm(newMod, dsn), error = function(e) {print(e); return(NULL)})
  if(!is.null(frm)) {
    frm$variable <- varname
    frm$interactor <- interactor
    frm$variable_model <- rownames(frm)
  }
  colnames(frm) <- c('estimate', 'se', 'z', 'pvalue', 'N', 'variable', 'interactor', 'variable_model')
  return(frm)
}


interactionCoeffs <- data.frame()
for(ii in 1:length(sigVariables)) {
  for(jj in 1:length(interactingVariables)) {
    frms <- interaction_test(baseFormula, dat, varTypes, sigVariables[ii], interactingVariables[jj])
    interactionCoeffs <- rbind(interactionCoeffs, frms)
  }
}

interactionTest <- interactionCoeffs[grep(':', interactionCoeffs$variable_model), ]
save(interactionCoeffs, interactionTest, file='./result/interaction.Rdata')
