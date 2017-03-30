# Chirag Patel
# 11/22/16
## print out the ex-post model with all the covariates 
source('util.R')
labels <- read.delim('../output/tables_07-13/variable_labels_v2.txt', sep="\t", stringsAsFactors = F)

numCategorical <- as.data.frame(table(labels[labels$level != 'continuous', 'variable']))
colnames(numCategorical) <- c('variable', 'num_levels')
binaryVariables <- subset(numCategorical, num_levels == 2)
numCategorical <- subset(numCategorical, num_levels > 2)
labels$variable_code <- labels$variable
for(ii in 1:nrow(numCategorical)) {
  varname <- numCategorical[ii, 'variable']
  indx <- labels$variable == varname
  labels[indx, 'variable_code'] <- paste(labels$variable[which(indx)], 1:(sum(indx)), sep="_")
}

### doubled binary variables
binaryLabels <- labels[labels$variable %in% binaryVariables$variable, ]
binaryLabelsToKeep <-  subset(binaryLabels, level == 1)
labels <- labels[!(labels$variable %in% binaryLabels$variable), ]
labels <- rbind(labels, binaryLabelsToKeep)

expost_model <- function(dat, adjustmentVariables) {
  adjustmentString <- paste(adjustmentVariables, collapse="+")
  baseFormula <- as.formula(sprintf("%s~1", dependentVariable))
  baseFormula <- as.formula(sprintf('%s~%s', dependentVariable, adjustmentString))
  dat[which(dat[, dependentVariable] > 1 ), dependentVariable] <- NA
  dsn <- svydesign(ids=~v021, probs=~v005, nested=T, data=dat) ## survey design object
  mod <- logistic_svyglm(baseFormula, dsn)
  colnames(mod) <- c('estimate', 'se', 'tvalue', 'pvalue', 'N')
  mod$OR <- exp(mod$estimate)
  mod$CI_low <- exp(mod$estimate - 1.96 * mod$se)
  mod$CI_high <- exp(mod$estimate + 1.96 * mod$se)
  mod$CI_string <- sprintf('%.02f [%.02f, %.02f]', mod$OR, mod$CI_low, mod$CI_high)
  return(mod)
}

source('./super_adjustments_13.R')
filename <- '../data/hiv xwas Zambia13 dataset v7.csv'
dependentVariable <- 'hiv03'
dat <- read.csv(filename)
mod13 <- expost_model(dat, adjustmentVariables)
mod13$variable_code <- rownames(mod13)
mod13 <- merge(mod13, labels, by.x='variable_code', by.y='variable_code')
mod13$label_out <- paste(mod13$var_label, '(', mod13$level, ')', sep='')
#write.csv(file='expost_model_13.csv', mod13[, c('variable_code', 'label_out', 'CI_string', 'pvalue', 'N')], row.names = F)

source('./super_adjustments_07.R')
filename <- '../data/hiv xwas Zambia07 dataset v7.csv'
dependentVariable <- 'hiv03'
dat <- read.csv(filename)
mod.07 <- expost_model(dat, adjustmentVariables)
mod.07$variable_code <- rownames(mod.07)
mod.07 <- merge(mod.07, labels, by.x='variable_code', by.y='variable_code')
mod.07$label_out <- paste(mod.07$var_label, '(', mod.07$level, ')', sep='')
#write.csv(file='expost_model_07.csv', mod.07[, c('variable_code', 'label_out', 'CI_string', 'pvalue', 'N')], row.names = F)

toWrite <- merge(mod13[, c('variable_code', 'label_out', 'CI_string', 'pvalue', 'N')], mod.07[, c('variable_code', 'label_out', 'CI_string', 'pvalue', 'N')], by.x='variable_code', by.y='variable_code', suffixes = c('_13', '_07'), all.x = T, all.y = T)
write.csv(toWrite, row.names = F, file='expost_model.csv')
