### This script combines all the subanalyses and adjusted and wraps into 1 large file
library(plyr)

WRITE = TRUE


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
  ret <- join_all(xwasDatas, by='key')
  return(list(variables=variables, merged_data=ret))
}

datasets <- c('../output/results_output/main_univariate_07.Rdata', '../output/results_output/main_univariate_13.Rdata',
              '../output/results_output/apriori_adj_07.Rdata', '../output/results_output/apriori_adj_13.Rdata',
              '../output/results_output/super_07.Rdata', '../output/results_output/super_13.Rdata'
              )
keys <- c('univariate_07', 'univariate_13', 'apriori_adj_07', 'aprior_adj_13', 'super_07', 'super_13')
mergedResults <- merge_results(datasets, keys)
mergedResults$merged_data <- merge(mergedResults$merged_data, labels, by.x='variable_univariate_07', by.y='variable_code')

## which ones found in both 07 and 13
concordantFrame <- function(mergedResults, key1, key2) {
  repCols <- paste('replicated', c(key1, key2), sep='_')
  repInd <- which(rowSums(mergedResults$merged_data[, repCols])==2)
  columnsSeed <- c(paste(c('conf_int_string', 'odds_ratio_low', 'odds_ratio', 'odds_ratio_high', 'pvalue'), key1, sep='_'), paste( c('conf_int_string', 'odds_ratio_low', 'odds_ratio', 'odds_ratio_high','pvalue'), key2, sep='_'))
  variableCol <-paste('variable', key1, sep='_')
  labelCols <- c('var_label', 'value_label')
  m <- (mergedResults$merged_data[repInd, c(variableCol,columnsSeed,labelCols)])
}

aprioriConcordant <- concordantFrame(mergedResults, 'apriori_adj_07', 'aprior_adj_13')
univariateConcordant <- concordantFrame(mergedResults, 'univariate_07', 'univariate_13')
superConcordant <- concordantFrame(mergedResults, 'super_07', 'super_13')


### now which ones have interactions? ## of those found concordant, look for differences in effects in age, HIV tested, urban/rural, poor/rich, wealth
or_and_confint_differences <- function(dataTable, suffixes) {
  for(ii in 1:length(suffixes)) {
    suffix <- suffixes[ii]
    dataTable <- orComp(dataTable, suffix)
  }
  return(dataTable)
}

load_diff_files <- function(file1_07, file2_13, diff_type, merge_key07, merge_key13) {
  load(file1_07)
  mergedResults07 <- mergedResults
  mergedResults07$merged_data$differences_significant_07 <- mergedResults07$merged_data$differences_significant
  mergedResults07$merged_data$differences_significant <- NULL;
  load(file2_13)
  mergedResults13 <- mergedResults
  mergedResults13$merged_data$differences_significant_13 <- mergedResults13$merged_data$differences_significant
  mergedResults13$merged_data$differences_significant <- NULL; 
  merged_years <- merge(mergedResults07$merged_data, mergedResults13$merged_data, by.x=merge_key07, by.y=merge_key13)
  return(list(diff_type=diff_type, mergedResults07=mergedResults07, mergedResults13=mergedResults13, merged_years=merged_years))
}

add_confint_differences <- function(diffStruct, subset_suffixes=c('univariate_wealth2_13')) {
  diffStruct$merged_years <- or_and_confint_differences(diffStruct$merged_years, subset_suffixes)
  return(diffStruct)
}

ageDiffs <- load_diff_files('../output/results_output/age_diffs_07.Rdata', '../output/results_output/age_diffs_13.Rdata', 'age',  'variable_univariate_age1_07', 'variable_univariate_age1_13')
locationDiffs <- load_diff_files('../output/results_output/location_diffs_07.Rdata', '../output/results_output/location_diffs_13.Rdata', 'urban_rural', 'variable_univariate_urban_07', 'variable_univariate_urban_13') 
hivTestDiffs <- load_diff_files('../output/results_output/hiv_test_diffs_07.Rdata', '../output/results_output/hiv_test_diffs_13.Rdata', 'tested', 'variable_univariate_nevertested_07', 'variable_univariate_nevertested_13')
poorRichDiffs <- load_diff_files('../output/results_output/poor_rich_diffs_07.Rdata', '../output/results_output/poor_rich_diffs_13.Rdata', 'poor_rich', 'variable_univariate_poor_07', 'variable_univariate_poor_13')
wealthDiffs <- load_diff_files('../output/results_output/wealth_diffs_07.Rdata', '../output/results_output/wealth_diffs_13.Rdata', 'wealth', 'variable_univariate_wealth1_07', 'variable_univariate_wealth1_13')

ageDiffs <- add_confint_differences(ageDiffs, 
                                     c('univariate_age1_07', 'univariate_age2_07', 'univariate_age3_07', 'univariate_age1_13', 'univariate_age2_13', 'univariate_age3_13')
)
locationDiffs <- add_confint_differences(locationDiffs, 
                                          c('univariate_rural_07', 'univariate_urban_07', 'univariate_rural_13', 'univariate_urban_13')
)
hivTestDiffs <- add_confint_differences(hivTestDiffs, 
                                      c('univariate_nevertested_07', 'univariate_tested_07', 'univariate_nevertested_13', 'univariate_tested_13')
)
poorRichDiffs <- add_confint_differences(poorRichDiffs, 
                                      c('univariate_poor_07', 'univariate_rich_07', 'univariate_poor_13', 'univariate_rich_13')
)
wealthDiffs <- add_confint_differences(wealthDiffs, 
                                        c('univariate_wealth1_07', 'univariate_wealth2_07', 'univariate_wealth3_07', 'univariate_wealth4_07', 'univariate_wealth5_07',
                                          'univariate_wealth1_13', 'univariate_wealth2_13', 'univariate_wealth3_13', 'univariate_wealth4_13', 'univariate_wealth5_13')
)

# look to see what are in apriori concordant
pretty_concordant_frame  <- function(diffStruct, toSearchVariableName, variablesToSearch) {
  #diffStruct$merged_years <- or_and_confint_differences(diffStruct$merged_years, subset_suffixes)
  colnumbers <- c()
  colnumbers <- c(colnumbers, grep('variable', colnames(diffStruct$merged_years))[1])
  colnumbers <- c(colnumbers, grep('conf_int_string', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('pvalue_difference', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('differences_significant', colnames(diffStruct$merged_years)))
  frm <- diffStruct$merged_years[diffStruct$merged_years[, toSearchVariableName] %in% variablesToSearch, colnumbers]
  return(subset(frm, differences_significant_07 > 0 | differences_significant_13 > 0))
}

pretty_diff_frame <- function(diffStruct) {
  ## the entire list 
  colnumbers <- c()
  colnumbers <- c(colnumbers, grep('variable', colnames(diffStruct$merged_years))[1])
  colnumbers <- c(colnumbers, grep('conf_int_string', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('pvalue', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('differences_significant', colnames(diffStruct$merged_years)))
  frm <- diffStruct$merged_years[, colnumbers]
  return(frm)   
}

prettyConcordantAge <- pretty_concordant_frame(ageDiffs, 'variable_univariate_age1_07',  aprioriConcordant$variable_apriori_adj_07)
prettyConcordantLocation <- pretty_concordant_frame(locationDiffs, 'variable_univariate_rural_07',  aprioriConcordant$variable_apriori_adj_07)
prettyConcordantTest <- pretty_concordant_frame(hivTestDiffs, 'variable_univariate_nevertested_07', aprioriConcordant$variable_apriori_adj_07)
prettyConcordantPoor <- pretty_concordant_frame(poorRichDiffs, 'variable_univariate_poor_07', aprioriConcordant$variable_apriori_adj_07)
prettyConcordantWealth <- pretty_concordant_frame(wealthDiffs, 'variable_univariate_wealth1_07', aprioriConcordant$variable_apriori_adj_07)

prettyAge <- pretty_diff_frame(ageDiffs)
prettyLocation <- pretty_diff_frame(locationDiffs)
prettyTest <- pretty_diff_frame(hivTestDiffs)
prettyPoor <- pretty_diff_frame(poorRichDiffs)
prettyWealth <- pretty_diff_frame(wealthDiffs)

### now, which ones are not found to be signficant in both 2007 and 2013
diff_not_significant_frame <- function(diffStruct, toSearchVariableName, variablesToSearch) {
  colnumbers <- c()
  colnumbers <- c(colnumbers, grep('variable', colnames(diffStruct$merged_years))[1])
  colnumbers <- c(colnumbers, grep('odds_ratio', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('conf_int_string', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('pvalue_difference', colnames(diffStruct$merged_years)))
  colnumbers <- c(colnumbers, grep('differences_significant', colnames(diffStruct$merged_years)))
  frm <- diffStruct$merged_years[diffStruct$merged_years[, toSearchVariableName] %in% variablesToSearch, colnumbers]
  #return(subset(frm, differences_significant_07 > 0 | differences_significant_13 > 0))
  return(subset(frm, differences_significant_07 == 0 & differences_significant_13 == 0))
}

notSigAge <- diff_not_significant_frame(ageDiffs, 'variable_univariate_age1_07',  aprioriConcordant$variable_apriori_adj_07)
notSigLocation <- diff_not_significant_frame(locationDiffs, 'variable_univariate_rural_07',  aprioriConcordant$variable_apriori_adj_07)
notSigTest <- diff_not_significant_frame(hivTestDiffs, 'variable_univariate_nevertested_07', aprioriConcordant$variable_apriori_adj_07)
notSigPoor <- diff_not_significant_frame(poorRichDiffs, 'variable_univariate_poor_07', aprioriConcordant$variable_apriori_adj_07)
notSigWealth <- diff_not_significant_frame(wealthDiffs, 'variable_univariate_wealth1_07', aprioriConcordant$variable_apriori_adj_07)

### write out the files 
RDATA_FILENAME <- 'merged.Rdata'
save(ageDiffs, locationDiffs, hivTestDiffs, poorRichDiffs, wealthDiffs, mergedResults, aprioriConcordant, univariateConcordant, superConcordant, file=RDATA_FILENAME)

if(WRITE) {
  ## those found in both 07 and 13
  write.csv(aprioriConcordant, file='apriori_replicated_07_13.csv', row.names = F)
  write.csv(univariateConcordant, file='univariate_replicated_07_13.csv', row.names = F)
  write.csv(superConcordant, file='super_replicated_07_13.csv', row.names = F)  
}

if(WRITE) {
  ## the differences in strata for factors found in apriori
  write.csv(prettyConcordantAge, file='age_diffs_apriori.csv', row.names = F)
  write.csv(prettyConcordantLocation, file='rural_urban_diffs_apriori.csv', row.names = F)
  write.csv(prettyConcordantTest, file='tested_nevertested_diffs_apriori.csv', row.names = F)
  write.csv(prettyConcordantPoor, file='poor_rich_diffs_apriori.csv', row.names = F)
  write.csv(prettyConcordantWealth, file='wealth_diffs_apriori.csv', row.names = F)
}

if(WRITE) {
  write.csv(prettyAge, file='age_diffs_all.csv', row.names=F)
  write.csv(prettyLocation, file='location_diffs_all.csv', row.names=F)
  write.csv(prettyTest, file='hiv_test_diffs_all.csv', row.names=F)
  write.csv(prettyPoor, file='poor_rich_diffs_all.csv', row.names=F)
  write.csv(prettyWealth, file='wealth_diffs_all.csv', row.names=F)
}

if(WRITE) {
  ## the entire dataset of findings from all analytic scenarios
  columnsSeed <- c('variable', 'conf_int_string', 'pvalue', 'conf_int_string_train', 'pvalue_train', 'fdr_train' , 'conf_int_string_test', 'pvalue_test', 'replicated')
  for(ii in 1:length(keys)) {
    cols <- paste(columnsSeed, keys[ii], sep="_")
    cols <- c(cols, 'var_label', 'value_label')
    filename <- sprintf('%s.csv', keys[ii])
    notNa <- which(!is.na(mergedResults$merged_data[, cols[1]]))
    
    m <- mergedResults$merged_data[notNa,  cols]
    write.csv(m, file=filename, row.names = F)
  }
}

