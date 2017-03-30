library(ggplot2)
load('./merged_output/merged.Rdata') # load in the data output from xwas_combined.R
source('plot_util.R')
load('../output/results_output/meta_subset_07.Rdata')
load('../output/results_output/meta_subset_13.Rdata')


### need to stack the data to plot

stackData <- function(dataFrame, variableColumnName,analysis_type, year='07', suffix='apriori_adj', pvalue_column=NULL) {
  yearStr <- year
  oddsRatioCol <- sprintf('odds_ratio_%s_%s', suffix, yearStr)
  oddsRatioLowCol <- sprintf('odds_ratio_low_%s_%s', suffix, yearStr)
  oddsRatioHighCol <- sprintf('odds_ratio_high_%s_%s', suffix, yearStr)
  if(is.null(pvalue_column)) {
    pvalue_column <- sprintf('pvalue_%s_%s', suffix, yearStr)  
  }
  
  stackedFrame <- data.frame(
    variable=dataFrame[ , variableColumnName],
    odds_ratio_low=dataFrame[ , oddsRatioLowCol],
    odds_ratio = dataFrame[, oddsRatioCol],
    odds_ratio_high = dataFrame[, oddsRatioHighCol],
    pvalue = dataFrame[, pvalue_column],
    analysis_type = analysis_type,
    year = yearStr)
  return(stackedFrame)
}


stackedData <- rbind(
  stackData(aprioriConcordant, 'variable_apriori_adj_07', 'apriori adjusted', year='07', suffix='apriori_adj'),
  stackData(aprioriConcordant, 'variable_apriori_adj_07', 'apriori adjusted', year='13', suffix='aprior_adj'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_urban_07', 'urban', year='07', suffix='univariate_urban', pvalue_column = 'pvalue_difference_univariate_urban_07_univariate_rural_07'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_urban_07', 'rural', year='07', suffix='univariate_rural', pvalue_column = 'pvalue_difference_univariate_urban_07_univariate_rural_07'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_urban_07', 'urban', year='13', suffix='univariate_urban', pvalue_column = 'pvalue_difference_univariate_urban_13_univariate_rural_13'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_urban_07', 'rural', year='13', suffix='univariate_rural', pvalue_column = 'pvalue_difference_univariate_urban_13_univariate_rural_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 1', year='07', suffix='univariate_age1', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 2', year='07', suffix='univariate_age2', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 3', year='07', suffix='univariate_age3', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 1', year='13', suffix='univariate_age1', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 2', year='13', suffix='univariate_age2', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_age1_07', 'age 3', year='13', suffix='univariate_age3', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_nevertested_07', 'tested', year='07', suffix='univariate_tested', pvalue_column = 'pvalue_difference_univariate_nevertested_07_univariate_tested_07'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_nevertested_07', 'not tested', year='07', suffix='univariate_nevertested', pvalue_column = 'pvalue_difference_univariate_nevertested_07_univariate_tested_07'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_nevertested_07', 'tested', year='13', suffix='univariate_tested', pvalue_column = 'pvalue_difference_univariate_nevertested_13_univariate_tested_13'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% aprioriConcordant$variable_apriori_adj_07, ], 'variable_univariate_nevertested_07', 'not tested', year='13', suffix='univariate_nevertested', pvalue_column = 'pvalue_difference_univariate_nevertested_13_univariate_tested_13')
)

SELECTED_VARIABLES <- c(
  'v013_7',
  'v136_2',
  'v137_3',
  'v203_4',
  'v220_7',
  'v235_2',
  'v312_5',
  'v445',
  'v446',
  'v501_4',
  'v501_5',
  'v502_3',
  'v763b'
)


varLabels <- (mergedResults$merged_data[, c('variable_univariate_07', 'var_label','value_label')])
metaI2 <- merge(metaResults.13.all[, c('variable', 'I2')], metaResults.07.all[, c('variable', 'I2')], by.x='variable', by.y='variable',
                suffixes = c('_13', '_07'))
#varLabels <- merge(varLabels, metaI2, by.x='variable_univariate_07', by.y='variable')
varLabels$description <- paste(varLabels$var_label, "\n", varLabels$value_label)
## add in I2 of each; do this in omnigraffle
toPlot <- stackedData[stackedData$variable %in% SELECTED_VARIABLES, ]
toPlot <- merge(toPlot, varLabels, by.x='variable', by.y='variable_univariate_07')
p <- ggplot(toPlot, aes(odds_ratio, analysis_type, color=as.factor(year)))
p <- p +  geom_point(position = position_dodgev(height = .3)) + geom_errorbarh(aes(xmin=odds_ratio_low, xmax=odds_ratio_high), position = position_dodgev(height = .3), height = 0, lwd = 1) + facet_wrap(~ description, nrow=3, scales='free_x')
p <- p + geom_vline(xintercept=1) 
p <- p + theme(legend.position="bottom", legend.title=element_blank(), strip.text.x=element_text(size=6)) + xlab('Odds Ratio') + ylab('Analysis Type')
p



