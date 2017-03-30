library(ggplot2)
library(ggrepel)
load('./merged_output/merged.Rdata')


mergedResults$merged_data$consistent_07_13 <- mergedResults$merged_data$replicated_univariate_07 & mergedResults$merged_data$replicated_univariate_13
labelVars <- subset(mergedResults$merged_data, consistent_07_13 == T)

first <- mergedResults$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_07', 'pvalue_univariate_07')]
first$year <- '2007'
second <- mergedResults$merged_data[, c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate_univariate_13', 'pvalue_univariate_13')]
second$year <- '2013-2014'

colnames(first) <- colnames(second) <- c('consistent_07_13', 'variable', 'var_label', 'value_label', 'estimate', 'pvalue', 'year')
toPlot <- rbind(first, second)

toPlot$plot_label <- toPlot$var_label
toPlot$plot_label[toPlot$value_label != ''] <- paste(toPlot$var_label[toPlot$value_label != ''], 
                                                         '(', toPlot$value_label[toPlot$value_label != ''], ')', sep="")


toLabel <- subset(toPlot, consistent_07_13==TRUE)
labelVarsTop <- labelVars[order(labelVars$pvalue_univariate_13, decreasing=F)[1:10], ]
toLabel <- toLabel[toLabel$variable %in% labelVarsTop$variable, ]

p <- ggplot(toPlot, aes(exp(estimate), -log10(pvalue), color=consistent_07_13))
p <- p + geom_point(alpha=.5) + scale_x_log10() + facet_wrap(~ year, nrow=2)
p <- p + theme(legend.position="none") + xlab('Odds Ratio')
p




## without numbers
p <- ggplot(toPlot, aes(exp(estimate), -log10(pvalue), color=consistent_07_13))
p <- p + geom_point(alpha=.5) + scale_x_log10() + facet_wrap(~ year, nrow=2, scales='free_y')
p <- p + geom_text_repel(data=toLabel, mapping=aes(exp(estimate), -log10(pvalue),label=plot_label), size=3, color='black')
p <- p + theme(legend.position="none") + xlab('Odds Ratio')
p



## with numbers
# 2007
toPlot.07 <- subset(toPlot, year == '2007')
plotLabel.07 <- subset(toLabel, year == '2007')
plotLabel.07 <- plotLabel.07[order(plotLabel.07$pvalue), ]
plotLabel.07$num_label <- 1:nrow(plotLabel.07)
p <- ggplot(toPlot.07, aes(exp(estimate), -log10(pvalue), color=consistent_07_13))
p <- p + geom_point(alpha=.5) + scale_x_log10(limits=c(.07, 11)) 
p <- p + geom_text_repel(data=plotLabel.07, mapping=aes(exp(estimate), -log10(pvalue),label=num_label), size=3, color='black')
p <- p + theme(legend.position="none") + xlab('Odds Ratio') + ggtitle('2007')
p

# 2013
toPlot.13 <- subset(toPlot, year == '2013-2014')
plotLabel.13 <- subset(toLabel, year == '2013-2014')
plotLabel.13 <- merge(plotLabel.13, plotLabel.07[, c('plot_label', 'num_label')], by.x='plot_label', by.y='plot_label') 
p <- ggplot(toPlot.13, aes(exp(estimate), -log10(pvalue), color=consistent_07_13))
p <- p + geom_point(alpha=.5) + scale_x_log10(limits=c(.07, 11)) 
p <- p + geom_text_repel(data=plotLabel.13, mapping=aes(exp(estimate), -log10(pvalue),label=num_label), size=3, color='black')
p <- p + theme(legend.position="none") + xlab('Odds Ratio')  +  ggtitle('2013-2014')
p 


### this is only the replicated result
p <- ggplot(toLabel, aes(exp(estimate), -log10(pvalue)))
p <- p + geom_point() + geom_text_repel(data=toLabel, mapping=aes(exp(estimate), -log10(pvalue),label=plot_label), size=2, color='black')
p <- p + facet_wrap(~ year, nrow=2, scales='free_y')
p <- p + theme(legend.position="none") + xlab('Odds Ratio')
p


