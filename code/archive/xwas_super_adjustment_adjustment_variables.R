## 4/6/16
## which ones are top factors for super adjustment?
## choose the top 10
load('./result/xwas_unadjusted_v5.Rdata')
xwasData$weighted_sig <- -log10(xwasData$pvalue_train) * abs(xwasData$estimate_train)
xwasData <- xwasData[order(xwasData$weighted_sig, decreasing=T), ]
xwasData$variable[1:10]
exAnteAdjustments <- c('v010', 'v536_1', 'v025', 'v190')
adjustmentVariables <- unique(c(xwasData$variable[1:10],exAnteAdjustments))

#current marital status(v501_4)
#relationship to household head**(v150_1)
#desire for more children(v605_1)
#recent sexual activity(v536_1)
#type of place of residence(v025)
#unmet need(v624_1)
#currently breastfeeding(v404)
#respondent's year of birth(v010)
#had any std in last 12 months(v763a)
#sex of household head(v151)