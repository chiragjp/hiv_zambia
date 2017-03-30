library(ggplot2)
load('./merged_output/merged.Rdata') # load in the data output from xwas_combined.R
source('plot_util.R')
load('../output/results_output/meta_subset_07.Rdata')
load('../output/results_output/meta_subset_13.Rdata')

SELECTED_VARIABLES <- c(
  'v501_4',
  'v003_1',
  'v010',
  'v123',
  'v136_2',
  'v404',
  'v502_3',
  'v763a',
  'v763b'
  )


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
  stackData(univariateConcordant, 'variable_univariate_07', 'univariate', year='07', suffix='univariate'),
  stackData(univariateConcordant, 'variable_univariate_07', 'univariate', year='13', suffix='univariate'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_urban_07', 'urban', year='07', suffix='univariate_urban', pvalue_column = 'pvalue_difference_univariate_urban_07_univariate_rural_07'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_urban_07', 'rural', year='07', suffix='univariate_rural', pvalue_column = 'pvalue_difference_univariate_urban_07_univariate_rural_07'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_urban_07', 'urban', year='13', suffix='univariate_urban', pvalue_column = 'pvalue_difference_univariate_urban_13_univariate_rural_13'),
  stackData(locationDiffs$merged_years[locationDiffs$merged_years$variable_univariate_urban_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_urban_07', 'rural', year='13', suffix='univariate_rural', pvalue_column = 'pvalue_difference_univariate_urban_13_univariate_rural_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 1', year='07', suffix='univariate_age1', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 2', year='07', suffix='univariate_age2', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 3', year='07', suffix='univariate_age3', pvalue_column = 'pvalue_difference_univariate_age1_07_univariate_age3_07'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 1', year='13', suffix='univariate_age1', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 2', year='13', suffix='univariate_age2', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(ageDiffs$merged_years[ageDiffs$merged_years$variable_univariate_age1_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_age1_07', 'age 3', year='13', suffix='univariate_age3', pvalue_column = 'pvalue_difference_univariate_age1_13_univariate_age3_13'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_nevertested_07', 'tested', year='07', suffix='univariate_tested', pvalue_column = 'pvalue_difference_univariate_nevertested_07_univariate_tested_07'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_nevertested_07', 'not tested', year='07', suffix='univariate_nevertested', pvalue_column = 'pvalue_difference_univariate_nevertested_07_univariate_tested_07'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_nevertested_07', 'tested', year='13', suffix='univariate_tested', pvalue_column = 'pvalue_difference_univariate_nevertested_13_univariate_tested_13'),
  stackData(hivTestDiffs$merged_years[hivTestDiffs$merged_years$variable_univariate_nevertested_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_nevertested_07', 'not tested', year='13', suffix='univariate_nevertested', pvalue_column = 'pvalue_difference_univariate_nevertested_13_univariate_tested_13'),
  
  
  stackData(poorRichDiffs$merged_years[poorRichDiffs$merged_years$variable_univariate_poor_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_poor_07', 'poor', year='07', suffix='univariate_poor', pvalue_column = 'pvalue_difference_univariate_poor_07_univariate_rich_07'),
  stackData(poorRichDiffs$merged_years[poorRichDiffs$merged_years$variable_univariate_poor_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_poor_07', 'rich', year='07', suffix='univariate_rich', pvalue_column = 'pvalue_difference_univariate_poor_07_univariate_rich_07'),
  stackData(poorRichDiffs$merged_years[poorRichDiffs$merged_years$variable_univariate_poor_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_poor_07', 'poor', year='13', suffix='univariate_poor', pvalue_column = 'pvalue_difference_univariate_poor_13_univariate_rich_13'),  
  stackData(poorRichDiffs$merged_years[poorRichDiffs$merged_years$variable_univariate_poor_07 %in% univariateConcordant$variable_univariate_07, ], 'variable_univariate_poor_07', 'rich', year='13', suffix='univariate_rich', pvalue_column = 'pvalue_difference_univariate_poor_13_univariate_rich_13')  
  
)


varLabels <- (mergedResults$merged_data[, c('variable_univariate_07', 'var_label','value_label')])
metaI2 <- merge(metaResults.13.all[, c('variable', 'I2')], metaResults.07.all[, c('variable', 'I2')], by.x='variable', by.y='variable',
                suffixes = c('_13', '_07'))

varLabels <- merge(varLabels, metaI2, by.x='variable_univariate_07', by.y='variable')
#varLabels$I2_string <- sprintf('I-sq(07,13):%.0f%%,%.0f%%', varLabels$I2_07, varLabels$I2_13)
#varLabels$description <- paste(varLabels$var_label, "\n", varLabels$value_label, "\n", varLabels$I2_string)
varLabels$description <- paste(varLabels$var_label, "\n", varLabels$value_label)
stackedData$year <- as.character(stackedData$year)
stackedData$year[stackedData$year == '13'] <- '13-14'
toPlot <- stackedData[stackedData$variable %in% SELECTED_VARIABLES, ]
toPlot <- merge(toPlot, varLabels, by.x='variable', by.y='variable_univariate_07')
#toPlot$year_string <- as.character(toPlot$year)
#toPlot$year_string [toPlot$year_string == '13'] <- '13-14'
#toPlot$year[toPlot$year == '13'] <- '13-14'

p <- ggplot(toPlot, aes(odds_ratio, analysis_type, color=as.factor(year)))
p <- p +  geom_point(position = position_dodgev(height = .3)) + geom_errorbarh(aes(xmin=odds_ratio_low, xmax=odds_ratio_high), position = position_dodgev(height = .3), height = 0, lwd = 1) + facet_wrap(~ description, nrow=4, scales='free_x')
p <- p + geom_vline(xintercept=1) 
p <- p + theme(legend.position="bottom", legend.title=element_blank(), strip.text.x=element_text(size=8)) + xlab('Odds Ratio') + ylab('Analysis Type')
p


