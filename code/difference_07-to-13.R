## Chirag J Patel
## 11/26/17
## Investigating difference in estimates from 07 --> 13 in males and females

library(ggplot2)
library(ggrepel)
load('./merged_output/merged.Rdata') # load in the data output from xwas_combined.R
source('plot_util.R')


### which differences are the greatest in the univariate estimates?

mergedResults$merged_data$univariate_squared_difference <- 
  (mergedResults$merged_data$estimate_univariate_13 - mergedResults$merged_data$estimate_univariate_07)^2
repData <- subset(mergedResults$merged_data, replicated_univariate_13 + replicated_univariate_07 >= 1)
quantile(abs(repData$univariate_squared_difference), na.rm=T, probs=c(0, .025, .05, .5, .75, .95, 0.975, 1))

repData$replication_text <- 'None'
repData$replication_text[repData$replicated_univariate_13==T] <- '2013-2014'
repData$replication_text[repData$replicated_univariate_07==T] <- '2007'
repData$replication_text[(repData$replicated_univariate_13 + repData$replicated_univariate_07) == 2] <- '2007 & 2013-2014'


repData$plot_label <- repData$var_label
ind <- repData$value_label != '' & !is.na(repData$value_label)
repData$plot_label[ind] <- paste(repData$var_label[ind], '(', repData$value_label[ind], ')', sep="")
bigChanges <- subset(repData, abs(univariate_squared_difference) >= .4)


p <- ggplot(repData, aes(exp(estimate_univariate_07), exp(estimate_univariate_13), color=replication_text))
p <- p + geom_point(alpha=.5) 
p <- p + geom_abline()
p <- p + geom_text_repel(data=bigChanges, aes(exp(estimate_univariate_07), exp(estimate_univariate_13), label=plot_label), size=5)
p <- p + scale_y_log10(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5.5)) 
p <- p + scale_x_log10(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5.5))
p <- p + theme(legend.position="bottom", legend.title = element_blank()) + xlab('Univariate OR (2007)') + ylab('Univariate OR (2013)')
p



