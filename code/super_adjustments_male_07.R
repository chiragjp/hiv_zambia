load('../output/results_output/hiv_males_2007.Rdata')
source('apriori_adjustments_male.R')
aprioriAdjustments <- adjustmentVariables
xwasData <- merge(xwasData, r2.train, by.x = 'variable', by.y='variable')
xwasData$fdr <- p.adjust(xwasData$pvalue_train, 'fdr')

fdrSig <- subset(xwasData, fdr < 0.05)
fdrSig <- fdrSig[order(fdrSig$r2, decreasing = T), ]
varnames.07 <- union(aprioriAdjustments, fdrSig$variable[1:10])
#xwasData$weighted_sig <- -log10(xwasData$pvalue_train) * abs(xwasData$estimate_train)
#xwasData <- xwasData[order(xwasData$weighted_sig, decreasing = T), ]
#varnames.07 <- union(aprioriAdjustments, xwasData$variable_univariate_07[1:10])
xwasData.07 <- xwasData
#adjustmentVariables <- union(aprioriAdjustments, xwasData$variable_univariate_07[1:10])
adjustmentVariables <- union(aprioriAdjustments, fdrSig$variable[1:10])

# Rscript xwas_logistic_v6.R -f '../data/hiv xwas Zambia13 male v7.csv' -o hiv_males_super_2013.Rdata -a ./super_adjustments_male_13.R -s 'super_adj_13'
# Rscript xwas_logistic_v6.R -f '../data/hiv xwas Zambia07 male v7.csv' -o hiv_males_super_2007.Rdata -a ./super_adjustments_male_07.R -s 'super_adj_07'