## compute correlation between variables

setwd("~/Dropbox/HIV xwas work/code")
source('util.R')
library(polycor)


dat <- read.csv("~/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6.csv")
### v021 is cluster
### v005 is sample weight
dat$v000 <- NULL

## need to also account for adjustment variables (later)
dependentVariable <- 'hiv03'

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

toCorr <- dat

for(ii in 1:length(varTypes$categoricalVariables)) {
  toCorr[, varTypes$categoricalVariables[ii]] <- factor(toCorr[,  varTypes$categoricalVariables[ii]], levels=c(0,1))
}

for(ii in 1:length(varTypes$yesNoVariables)) {
  toCorr[, varTypes$yesNoVariables[ii]] <- factor(toCorr[,  varTypes$yesNoVariables[ii]], levels=c(0,1))
}


corrs.het <- hetcor(toCorr, std.err=F, use="pairwise.complete.obs", pd=F)

save(corrs.het, categoricalFrame, file='correlations.Rdata')


