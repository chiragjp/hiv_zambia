# estimates the differences in subsets
library(plyr)
library(getopt)
opt <- getopt(matrix(c(
  'fnames', 'f', 1, 'character',
  'keys', 'k', 1, 'character',
  'year','y', 1, 'character', 
  'outfile', 'o', 1, 'character'
), byrow =T, ncol = 4))

datasets <- unlist(strsplit(opt$fnames, ',')) #datasets <- c('../output/results_output/age1_univariate_07.Rdata', '../output/results_output/age2_univariate_07.Rdata',  '../output/results_output/age3_univariate_07.Rdata')
strings <- unlist(strsplit(opt$keys, ',')) #c('age1', 'age2', 'age3')
year <- opt$year #'07'
#keys <- c('univariate_age1_07', 'univariate_age2_07', 'univariate_age3_07')
keys <- paste('univariate', strings, year, sep="_")
OUTPUTFILE <- opt$outfile
print(datasets)
print(keys)

#keys <- c('univariate_rural_07', 'univariate_urban_07')
merge_results <- function(datasets=c('../output/results_output/main_univariate_07.Rdata', '../output/results_output/main_univariate_13.Rdata'), 
                          suffix=c('univariate_07', 'univariate_13')) {
  xwasDatas <- list()
  variables <- list()
  for(ii in 1:length(datasets)) {
    load(datasets[ii])
    key <- sprintf('variable_%s', suffix[ii])
    xwasData$key <- xwasData[, key]
    #print(head(xwasData$key))
    xwasDatas[[ii]] <- xwasData
    variables[[ii]] <- xwasData$key
  }
  ret <- join_all(xwasDatas, by='key')
  return(list(variables=variables, merged_data=ret))
}

norm_test_diffs <- function(mergedData, key1='univariate_urban_07', key2='univariate_rural_07') {
  estimateCol1 <- sprintf('estimate_%s', key1)
  estimateCol2 <- sprintf('estimate_%s', key2)
  seCol1 <- sprintf('se_%s', key1)
  seCol2 <- sprintf('se_%s', key2)
  newDiffColName <- sprintf('difference_%s_%s', key1, key2)
  seColName <- sprintf('se_difference_%s_%s', key1, key2)
  zColName <- sprintf('z_difference_%s_%s', key1, key2)
  pColName <- sprintf('pvalue_difference_%s_%s', key1, key2)
  mergedData[, newDiffColName] <- (mergedData[, estimateCol1] - mergedData[, estimateCol2])
  mergedData[, seColName] <- sqrt(mergedData[, seCol1]^2 + mergedData[, seCol2]^2)
  mergedData[, zColName] <- (mergedData[, newDiffColName] / mergedData[, seColName])
  mergedData[, pColName] <- pnorm(abs(mergedData[, zColName]),lower.tail=F)*2 # two-sided test
  return(mergedData)
}


mergedResults <- merge_results(datasets, keys)

#print(head(mergedResults$merged_data))

for(k in 1:length(keys)) {
  key1 <- keys[k]
  for(kk in (k+1):length(keys)) {
    if (kk > length(keys)) {
      break
    }
    key2 <- keys[kk]
    mergedResults$merged_data <- norm_test_diffs(mergedResults$merged_data, key1, key2)    
  }
}

## any pvalue_diff is lower than bonferroni?
pvalCols <- grep('pvalue_difference', colnames(mergedResults$merged_data))
bonfCorrect <- 0.05 / nrow(mergedResults$merged_data) * length(pvalCols)
if(length(pvalCols) > 1) {
  numSig <- apply(mergedResults$merged_data[, pvalCols], 1, function(arr) {sum(arr < bonfCorrect, na.rm=T)})  
} else {
  numSig <- ifelse(mergedResults$merged_data[, pvalCols] < bonfCorrect, 1, 0)  
}

mergedResults$merged_data[, 'differences_significant'] <-  numSig

save(mergedResults, file=OUTPUTFILE)





