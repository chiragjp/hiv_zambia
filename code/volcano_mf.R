# 12/12/17
# Chirag
# plotting males and females on the same plot

library(ggplot2)
library(ggrepel)

load('./merged_output/male_merged.Rdata')
mergedResults_male <- mergedResults
mergedResults_male$merged_data$variable <- mergedResults$merged_data$variable_univariate_07
mergedResults_male$merged_data$var_label <- mergedResults$merged_data$var_label_07
mergedResults_male$merged_data$value_label <- mergedResults$merged_data$value_label_07
mergedResults_male$merged_data$consistent_07_13 <- mergedResults$merged_data$replicated_univariate_07 & mergedResults$merged_data$replicated_univariate_13
### prepare the female results
load('./merged_output/merged.Rdata')
mergedResults$merged_data$consistent_07_13 <- mergedResults$merged_data$replicated_univariate_07 & mergedResults$merged_data$replicated_univariate_13

### mash them up
first <- mergedResults$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_07', 'pvalue_univariate_07')]
first$year <- '2007'
second <- mergedResults$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_13', 'pvalue_univariate_13')]
second$year <- '2013-2014'
colnames(first) <- colnames(second) <- c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate', 'pvalue', 'year')
toPlot <- rbind(first, second)
toPlot$sex <- 'female'

third <- mergedResults_male$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_07', 'pvalue_univariate_07')]
third$year <- '2007'
fourth <- mergedResults_male$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_13', 'pvalue_univariate_13')]
fourth$year <- '2013-2014'
colnames(third) <- colnames(fourth) <- c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate', 'pvalue', 'year')
toPlot2 <- rbind(third, fourth)
toPlot2$sex <- 'male'
toPlot <- rbind(toPlot, toPlot2)

toPlot <- subset(toPlot, !is.na(consistent_07_13))

toPlot$plot_label <- toPlot$var_label
ind <- toPlot$value_label != '' & !is.na(toPlot$value_label)
toPlot$plot_label[ind] <- paste(toPlot$var_label[ind], '(', toPlot$value_label[ind], ')', sep="")

toPlot <- toPlot[-grep('line number', toPlot$plot_label), ]

toLabelMale07 <- subset(toPlot, consistent_07_13==TRUE & sex == 'male' & year == '2007')
toLabelMale07 <- toLabelMale07[order(toLabelMale07$pvalue)[1:10], ]
toLabelFemale07 <- subset(toPlot, consistent_07_13==TRUE & sex == 'female' & year == '2007')
toLabelFemale07 <- toLabelFemale07[order(toLabelFemale07$pvalue)[1:10], ]
toLabelMale13 <- subset(toPlot, consistent_07_13==TRUE & sex == 'male' & year == '2013-2014')
toLabelMale13  <- toLabelMale13[order(toLabelMale13$pvalue)[1:10], ]
toLabelFemale13 <- subset(toPlot, consistent_07_13==TRUE & sex == 'female' & year == '2013-2014')
toLabelFemale13  <- toLabelFemale13[order(toLabelFemale13$pvalue)[1:10], ]

toLabel <- rbind(toLabelMale07, toLabelFemale07, toLabelMale13, toLabelFemale13)

p <- ggplot(toPlot, aes(exp(estimate), -log10(pvalue), 
                        color=consistent_07_13))
p <- p + geom_point(alpha=.5) + scale_x_log10() 
p <- p + geom_text_repel(data=toLabel, mapping=aes(exp(estimate), -log10(pvalue),label=plot_label), size=3, color='black')
p <- p + facet_grid(sex ~ year)
p <- p + theme(legend.position="none") + xlab('Odds Ratio')
p


toPrint <- mergedResults_male$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_07', 'pvalue_univariate_07', 'replicated_univariate_07', 'estimate_univariate_13', 'pvalue_univariate_13', 'replicated_univariate_13')]
toPrint <- subset(toPrint, !is.na(replicated_univariate_07) & !is.na(replicated_univariate_13))
toPrint <- subset(toPrint, replicated_univariate_07 == 1 | replicated_univariate_13 == 1)
toPrint$or_07 <- round(exp(toPrint$estimate_univariate_07),1)
toPrint$or_13 <- round(exp(toPrint$estimate_univariate_13), 1)
toPrint$nlogp_07 <- sprintf('%.02f%s', round(-log10(toPrint$pvalue_univariate_07), 1), ifelse(toPrint$consistent_07_13, '*', ''))
toPrint$nlogp_13 <- sprintf('%.02f%s', round(-log10(toPrint$pvalue_univariate_13), 1), ifelse(toPrint$consistent_07_13, '*', ''))

write.csv(toPrint[, c('variable', 'var_label', 'value_label', 'or_07', 'nlogp_07', 'or_13', 'nlogp_13')], file='../output/male_univariate.csv', row.names = F)






