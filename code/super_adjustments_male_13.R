load('../output/results_output/hiv_males_2013.Rdata')
source('apriori_adjustments_male.R')
aprioriAdjustments <- adjustmentVariables
#xwasData$fdr <- p.adjust(xwasData$pvalue_train, 'fdr')
#xwasData$weighted_sig <- -log10(xwasData$pvalue_train) * abs(xwasData$estimate_train)
#varnames.13 <- xwasData[which(xwasData$fdr < 0.05), 'variable_train_univariate_13']
#xwasData <- xwasData[order(xwasData$weighted_sig, decreasing = T), ]
#adjustmentVariables <- union(aprioriAdjustments, xwasData$variable_univariate_13[1:10])
#xwasData.13 <- xwasData

xwasData <- merge(xwasData, r2.train, by.x = 'variable', by.y='variable')
xwasData$fdr <- p.adjust(xwasData$pvalue_train, 'fdr')

fdrSig <- subset(xwasData, fdr < 0.05)
fdrSig <- fdrSig[order(fdrSig$r2, decreasing = T), ]
varnames.13 <- union(aprioriAdjustments, fdrSig$variable[1:10])
#xwasData$weighted_sig <- -log10(xwasData$pvalue_train) * abs(xwasData$estimate_train)
#xwasData <- xwasData[order(xwasData$weighted_sig, decreasing = T), ]
#varnames.07 <- union(aprioriAdjustments, xwasData$variable_univariate_07[1:10])
xwasData.13 <- xwasData
#adjustmentVariables <- union(aprioriAdjustments, xwasData$variable_univariate_07[1:10])
adjustmentVariables <- union(aprioriAdjustments, fdrSig$variable[1:10])


