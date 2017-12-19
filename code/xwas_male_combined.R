### This script combines all the subanalyses and adjusted and wraps into 1 large file
library(plyr)

WRITE <- TRUE

labels_07 <- read.csv('../output/tables_07-13/variable labels male 07.csv', stringsAsFactors = F)
labels_13 <- read.csv('../output/tables_07-13/variable labels male 13.csv', stringsAsFactors = F)

colnames(labels_07) <- paste(colnames(labels_07),'07',sep="_")
colnames(labels_13) <- paste(colnames(labels_13),'13',sep="_")


orComp <- function(dataTable, suffix) {
  estimateCol <- sprintf('estimate_%s', suffix)
  seCol <- sprintf('se_%s', suffix)
  orCol <- sprintf('odds_ratio_%s', suffix)
  orLowCol <- sprintf('odds_ratio_low_%s', suffix)
  orHighCol <- sprintf('odds_ratio_high_%s', suffix)
  orConfIntCol <- sprintf('conf_int_string_%s', suffix)
  dataTable[, orCol] <- exp(dataTable[, estimateCol])
  dataTable[, orLowCol] <- exp(dataTable[, estimateCol] - 1.96*dataTable[, seCol] )
  dataTable[, orHighCol] <- exp(dataTable[, estimateCol] + 1.96*dataTable[, seCol] )
  dataTable[, orConfIntCol] <- sprintf('%.03f [%.03f, %.03f]', dataTable[, orCol], dataTable[, orLowCol], dataTable[, orHighCol])
  return(dataTable)
}

or_and_confint <- function(dataTable, suffix) {
  dataTable <- orComp(dataTable, suffix)
  dataTable <- orComp(dataTable, sprintf('train_%s', suffix))
  dataTable <- orComp(dataTable, sprintf('test_%s', suffix))
  return(dataTable)
}

fdrthreshold <- function(dataTable, suffix) {
  trainP <- sprintf('pvalue_train_%s', suffix)
  testP <- sprintf('pvalue_test_%s', suffix)
  fdrTrainP <- sprintf('fdr_train_%s', suffix)
  fdrTestP <- sprintf('fdr_test_%s', suffix)
  dataTable[, fdrTrainP] <- p.adjust(dataTable[, trainP], 'fdr')
  dataTable[, fdrTestP] <- p.adjust(dataTable[, testP], 'fdr')
  return(dataTable)
}

bonfthreshold <- function(dataTable, suffix) {
  trainP <- sprintf('pvalue_train_%s', suffix)
  testP <- sprintf('pvalue_test_%s', suffix)
  bonfTrainP <- sprintf('bonf_train_%s', suffix)
  bonfTestP <- sprintf('bonf_test_%s', suffix)
  dataTable[, bonfTrainP] <- p.adjust(dataTable[, trainP], 'bonferroni')
  dataTable[, bonfTestP] <- p.adjust(dataTable[, testP], 'bonferroni')
  return(dataTable)
}


replicated <- function(dataTable, suffix) {
  trainP <- sprintf('pvalue_train_%s', suffix)
  testP <- sprintf('pvalue_test_%s', suffix)
  fdrTrainP <- sprintf('fdr_train_%s', suffix)
  fdrTestP <- sprintf('fdr_test_%s', suffix)
  estimateTrainCol <- sprintf('estimate_train_%s', suffix)
  estimateTestCol <- sprintf('estimate_test_%s', suffix)
  replicatedCol <- sprintf('replicated_%s', suffix)
  ind <- which(dataTable[, fdrTrainP] < 0.05 & dataTable[, testP] < 0.05 & (sign(dataTable[, estimateTrainCol]) == sign(dataTable[, estimateTestCol])))
  dataTable[, replicatedCol] <- 0
  dataTable[ind, replicatedCol]  <- 1
  return(dataTable)
}

merge_results <- function(datasets=c('../output/results_output/main_univariate_07.Rdata', '../output/results_output/main_univariate_13.Rdata'), 
                          suffix=c('univariate_07', 'univariate_13')) {
  xwasDatas <- list()
  variables <- list()
  for(ii in 1:length(datasets)) {
    load(datasets[ii])
    xwasData <- bonfthreshold(xwasData, suffix[ii])
    xwasData <- fdrthreshold(xwasData, suffix[ii])
    xwasData <- replicated(xwasData, suffix[ii])
    xwasData <- or_and_confint(xwasData, suffix[ii])
    key <- sprintf('variable_%s', suffix[ii])
    xwasData$key <- xwasData[, key]
    xwasDatas[[ii]] <- xwasData
    variables[[ii]] <- xwasData$key
  }
  ret <- join_all(xwasDatas, type='full', by='key')
  return(list(variables=variables, merged_data=ret))
}

concordantFrame <- function(mergedResults, key1, key2) {
  repCols <- paste('replicated', c(key1, key2), sep='_')
  repInd <- which(rowSums(mergedResults$merged_data[, repCols])==2)
  columnsSeed <- c(paste(c('conf_int_string', 'odds_ratio_low', 'odds_ratio', 'odds_ratio_high', 'pvalue'), key1, sep='_'), paste( c('conf_int_string', 'odds_ratio_low', 'odds_ratio', 'odds_ratio_high','pvalue'), key2, sep='_'))
  variableCol <-paste('variable', key1, sep='_')
  labelCols <- c('var_label_07', 'value_label_07', 'var_label_13', 'value_label_13')
  m <- (mergedResults$merged_data[repInd, c(variableCol,columnsSeed,labelCols)])
}

## now do some work -- load in the data and merge.

datasets <- c('../output/results_output/hiv_males_2007.Rdata', '../output/results_output/hiv_males_2013.Rdata',
              '../output/results_output/hiv_males_apriori_2007.Rdata', '../output/results_output/hiv_males_apriori_2013.Rdata',
              '../output/results_output/hiv_males_super_2007.Rdata', '../output/results_output/hiv_males_super_2013.Rdata'
)

keys <- c('univariate_07', 'univariate_13', 'apriori_adj_07', 'apriori_adj_13', 'super_adj_07', 'super_adj_13')
mergedResults <- merge_results(datasets, keys)

mergedResults$merged_data <- merge(mergedResults$merged_data, labels_07, by.x='variable_univariate_07', by.y='variable_07', all.x = T)
mergedResults$merged_data <- merge(mergedResults$merged_data, labels_13, by.x='variable_univariate_13', by.y='variable_13', all.x = T)

aprioriConcordant <- concordantFrame(mergedResults, 'apriori_adj_07', 'apriori_adj_13')
univariateConcordant <- concordantFrame(mergedResults, 'univariate_07', 'univariate_13')
superConcordant <- concordantFrame(mergedResults, 'super_adj_07', 'super_adj_13')

## map over the variables to the females
varmap_07 <- read.csv('../data/non_primarydata_folders/07Zambia_varmap/ZM51_varmap.csv', stringsAsFactors = F)
varmap_13 <- read.csv('../data/non_primarydata_folders/13_14Zambia_varmap/ZM62_varmap.csv', stringsAsFactors = F)

colnames(varmap_07 ) <- c('male_variable_07', 'female_variable_07')
colnames(varmap_13 ) <- c('male_variable_13', 'female_variable_13')


####
mergedResults$merged_data$root_variable_07 <- mergedResults$merged_data$variable_univariate_07
mergedResults$merged_data$root_variable_07[grep("\\_", mergedResults$merged_data$variable_univariate_07)] <- sapply(
  strsplit(mergedResults$merged_data$root_variable_07[grep("\\_", mergedResults$merged_data$variable_univariate_07)] , '_'), 
  function(arr) {return(arr[1])})

mergedResults$merged_data$root_variable_13  <- mergedResults$merged_data$variable_univariate_13
mergedResults$merged_data$root_variable_13[grep("\\_", mergedResults$merged_data$variable_univariate_13)] <- sapply(
  strsplit(mergedResults$merged_data$root_variable_13[grep("\\_", mergedResults$merged_data$variable_univariate_13)] , '_'), 
  function(arr) {return(arr[1])})

mergedResults$merged_data <- merge(mergedResults$merged_data, varmap_07, by.x='root_variable_07', by.y='male_variable_07', all.x=T)
mergedResults$merged_data <- merge(mergedResults$merged_data, varmap_13, by.x='root_variable_13', by.y='male_variable_13', all.x=T)

## if original variable has '_' and the female does not, add the stuff after the underscore
## remap to the female categorical variable
mergedResults$merged_data$female_variable_map_07 <- mergedResults$merged_data$female_variable_07
mergedResults$merged_data$female_variable_map_07[grep('\\_', mergedResults$merged_data$variable_univariate_07)] <- 
  paste(mergedResults$merged_data$female_variable_map_07[grep('\\_', mergedResults$merged_data$variable_univariate_07)],
        sapply(strsplit(mergedResults$merged_data$variable_univariate_07[grep('\\_', mergedResults$merged_data$variable_univariate_07)], '_'), function(arr) {arr[2]}),
        sep='_')


mergedResults$merged_data$female_variable_map_13 <- mergedResults$merged_data$female_variable_13
mergedResults$merged_data$female_variable_map_13[grep('\\_', mergedResults$merged_data$variable_univariate_13)] <- 
  paste(mergedResults$merged_data$female_variable_map_13[grep('\\_', mergedResults$merged_data$variable_univariate_13)],
        sapply(strsplit(mergedResults$merged_data$variable_univariate_13[grep('\\_', mergedResults$merged_data$variable_univariate_13)], '_'), function(arr) {arr[2]}),
        sep='_')

RDATA_FILENAME <- 'male_merged.Rdata'
save(mergedResults, aprioriConcordant, univariateConcordant, superConcordant, file=RDATA_FILENAME)


