#!/usr/local/bin/Rscript
### Chirag Patel
### XWAS with HIV dataset "v7": subsets only.
### 4/5/2016
###

setwd("~/Dropbox/HIV xwas work/code")
source('util.R')

library(getopt)

opt <- getopt(matrix(c(
  'filein', 'f', 1, 'character',
  'fileout', 'o', 1, 'character',
  'notes', 'n', 1, 'character',
  'col_suffix', 's', 1, 'character',
  'adjustment_script', 'a', 1, 'character'
), byrow =T, ncol = 4))

dat <- read.csv(opt$filein)
DATAOUTFILE <- opt$fileout; # 'xwas_ex_post_ante.Rdata'
notes <- opt$notes  # 'adjusted by the union of top 10 factors and ex ante adjustments (total of 11)'

adjustmentVariables <- c()
if(!is.null(opt$adjustment_script)) {
  source(opt$adjustment_script)  
}



N <- nrow(dat)

cat(sprintf('running analysis on file: %s\n', opt$filein))
cat(sprintf('outputting to file: %s\n', DATAOUTFILE))
cat(sprintf('notes: %s\n', opt$notes))
cat(sprintf('adjustment variables: %s\n', paste(adjustmentVariables, collapse=",")))

if(!is.null(opt$col_suffix)) {
  COLNAME_SUFFIX <- opt$col_suffix  
}


dat$v000 <- NULL
dat$cname <- NULL
dat$id1 <- NULL
dependentVariable <- 'hiv03'
adjustmentString <- paste(adjustmentVariables, collapse="+")
baseFormula <- as.formula(sprintf("%s~1", dependentVariable))
if(length(adjustmentVariables) > 1) {
  baseFormula <- as.formula(sprintf('%s~%s', dependentVariable, adjustmentString))
}
dat[which(dat[, dependentVariable] > 1 ), dependentVariable] <- NA
varTypes <- introspect_variables_easy(dat, c(dependentVariable, 'v005', 'v021'))

xwas <- function(baseFormula, dat, varTypes) {
  allData <- data.frame() 
  dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
  variables <- c(varTypes$continuousVariables,varTypes$binaryVariables)
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
toRemove <- c(adjustmentVariables, '(Intercept)')

xwasData <- allData.overall[!allData.overall$variable_model %in% toRemove, ]

if(!is.null(COLNAME_SUFFIX)) {
  colnames(xwasData) <- paste(colnames(xwasData), COLNAME_SUFFIX, sep="_")
  colnames(allData.overall) <- paste(colnames(allData.overall), COLNAME_SUFFIX, sep="_")
}

save(xwasData,allData.overall, adjustmentVariables,notes, file=DATAOUTFILE)