### This script combines all the subanalyses and adjusted and wraps into 1 large file
or_and_confint <- function(dataTable) {
  ## put ORs in the table
  dataTable$odds_ratio <- exp(dataTable$estimate)
  dataTable$odds_ratio_low <- exp(dataTable$estimate - 1.96*dataTable$se)
  dataTable$odds_ratio_high <- exp(dataTable$estimate + 1.96*dataTable$se)
  dataTable$conf_int_string <- sprintf('%.03f [%.03f, %.03f]', dataTable$odds_ratio, dataTable$odds_ratio_low, dataTable$odds_ratio_high)
  
  dataTable$odds_ratio_train <- exp(dataTable$estimate_train)
  dataTable$odds_ratio_low_train <- exp(dataTable$estimate_train - 1.96*dataTable$se_train)
  dataTable$odds_ratio_high_train <- exp(dataTable$estimate_train + 1.96*dataTable$se_train)
  dataTable$conf_int_string_train <- sprintf('%.03f [%.03f, %.03f]', dataTable$odds_ratio_train, dataTable$odds_ratio_low_train, dataTable$odds_ratio_high_train)
  
  dataTable$odds_ratio_test <- exp(dataTable$estimate_test)
  dataTable$odds_ratio_low_test <- exp(dataTable$estimate_test - 1.96*dataTable$se_test)
  dataTable$odds_ratio_high_test <- exp(dataTable$estimate_test + 1.96*dataTable$se_test)
  dataTable$conf_int_string_test <- sprintf('%.03f [%.03f, %.03f]', dataTable$odds_ratio_test, dataTable$odds_ratio_low_test, dataTable$odds_ratio_high_test)
  return(dataTable)
}

load('./result/xwas_unadjusted_v5.Rdata') # xwas as a result of xwas_logistic_v5.R
load('./result/interaction.Rdata')
allData.overall.unadj <- allData.overall
xwasData.unadj <- or_and_confint(xwasData)

load('./result/xwas_ex_ante.Rdata') # ex ante (a priori adjustments)
xwasData.adj <- or_and_confint(xwasData)
##### from the super adjusted model (ex post ante)
load('./result/xwas_ex_post_ante.Rdata')
xwasData.super_adj <- or_and_confint(xwasData)
colnames(xwasData.super_adj) <- paste(colnames(xwasData.super_adj),'super_adj', sep="_")

###### from the subset analysis
load('./result/xwas_v5_v010_33.Rdata')
xwasData.age33 <- or_and_confint(xwasData)
colnames(xwasData.age33) <- paste(colnames(xwasData.age33),'age_gt_33', sep="_")
load('./result/xwas_v5_v010_23_33.Rdata')
xwasData.age23_33 <- or_and_confint(xwasData)
colnames(xwasData.age23_33) <- paste(colnames(xwasData.age23_33),'age_gt_23_lt_33', sep="_")
load('./result/xwas_v5_v010_23.Rdata')
xwasData.age23 <- or_and_confint(xwasData)
colnames(xwasData.age23) <- paste(colnames(xwasData.age23),'age_lt_23', sep="_")
load('./result/xwas_v5_v025_0.Rdata')
xwasData.urban <- or_and_confint(xwasData)
colnames(xwasData.urban) <- paste(colnames(xwasData.urban),'urban', sep="_")
load('./result/xwas_v5_v025_1.Rdata')
xwasData.rural <- or_and_confint(xwasData)
colnames(xwasData.rural) <- paste(colnames(xwasData.rural),'rural', sep="_")
load('./result/xwas_v5_v781_0.Rdata')
xwasData.not_hiv_test <- or_and_confint(xwasData)
colnames(xwasData.not_hiv_test) <- paste(colnames(xwasData.not_hiv_test),'not_hiv_test', sep="_")
load('./result/xwas_v5_v781_1.Rdata')
xwasData.hiv_test <- or_and_confint(xwasData)
colnames(xwasData.hiv_test) <- paste(colnames(xwasData.hiv_test),'hiv_test', sep="_")
load('./result/xwas_v5_v190_gt_3.Rdata')
xwasData.rich <- or_and_confint(xwasData)
colnames(xwasData.rich) <- paste(colnames(xwasData.rich),'rich', sep="_")
load('./result/xwas_v5_v190_lt_3.Rdata')
xwasData.poor <- or_and_confint(xwasData)
colnames(xwasData.poor) <- paste(colnames(xwasData.poor),'poor', sep="_")
#######

### merge and replicate
xwasData.all <- merge(xwasData.unadj, xwasData.adj, by.x='variable', by.y='variable', all.x=T, suffixes = c('_unadj', '_adj'))
bonferroniThreshold <- 0.05/nrow(xwasData.all)
xwasData.all$validate_unadj <- 0
xwasData.all$validate_unadj[which(xwasData.all$pvalue_test_unadj < bonferroniThreshold & xwasData.all$pvalue_train_unadj < bonferroniThreshold & sign(xwasData.all$estimate_test_unadj) == sign(xwasData.all$estimate_train_unadj))] <- 1
xwasData.all$validate_adj <- 0
xwasData.all$validate_adj[which(xwasData.all$pvalue_test_adj < bonferroniThreshold & xwasData.all$pvalue_train_adj < bonferroniThreshold & sign(xwasData.all$estimate_test_adj) == sign(xwasData.all$estimate_train_adj))] <- 1

### merge the data.
xwasData.all <- merge(xwasData.all, xwasData.age23, by.x='variable', by.y='variable_age_lt_23', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.age23_33, by.x='variable', by.y='variable_age_gt_23_lt_33', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.age33, by.x='variable', by.y='variable_age_gt_33', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.urban, by.x='variable', by.y='variable_urban', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.rural, by.x='variable', by.y='variable_rural', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.not_hiv_test, by.x='variable', by.y='variable_not_hiv_test', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.hiv_test, by.x='variable', by.y='variable_hiv_test', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.poor, by.x='variable', by.y='variable_poor', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.rich, by.x='variable', by.y='variable_rich', all.x=T)
xwasData.all <- merge(xwasData.all, xwasData.super_adj, by.x='variable', by.y='variable_super_adj', all.x=T)
xwasData.all$validate_super_adj <- 0
xwasData.all$validate_super_adj[which(xwasData.all$pvalue_test_super_adj < bonferroniThreshold & xwasData.all$pvalue_train_super_adj < bonferroniThreshold & sign(xwasData.all$estimate_test_super_adj) == sign(xwasData.all$estimate_train_super_adj))] <- 1


### now merge with the catalog
numberVariablesWomen <- subset(read.csv('../data/completeness ir_hiv files.csv'), cname == 'ZM')
variables <- unique(numberVariablesWomen[, c('variable')])
varDesc <- data.frame()
for(ii in 1:length(variables)) {
  varDesc <- rbind(varDesc, subset(numberVariablesWomen, variable == variables[ii])[1, c('variable', 'varlabel')])
}
xwasData.all$variable_split <- sapply(strsplit(xwasData.all$variable, '_'), function(aList) {aList[1]})
xwasData.all <- merge(xwasData.all, varDesc, by.x='variable_split', by.y='variable', all.x=T)
save(xwasData.all, interactionTest, interactionCoeffs,file='./result/xwas_merged_v6.Rdata')

xwasData.all$varlabel <- as.character(xwasData.all$varlabel)
xwasData.all$varlabel[is.na(xwasData.all$varlabel)] <- xwasData.all$variable[is.na(xwasData.all$varlabel)]
xwasData.all$varlabel_long <- paste(xwasData.all$varlabel, '(' ,xwasData.all$variable, ')', sep="")

## and write out the files to the result folder:
## unadj:
colnamesToWrite <- c('varlabel_long', 'conf_int_string_unadj', 'pvalue_unadj', 'odds_ratio_train_unadj', 'pvalue_train_unadj', 'odds_ratio_test_unadj', 'pvalue_test_unadj', 'validate_unadj')
write.csv(xwasData.all[, colnamesToWrite], row.names=F, file='./result/xwas_baseline.csv')
## adj:
colnamesToWrite <- c('varlabel_long', 'conf_int_string_adj', 'pvalue_adj', 'odds_ratio_train_adj', 'pvalue_train_adj', 'odds_ratio_test_adj', 'pvalue_test_adj', 'validate_adj')
write.csv(xwasData.all[, colnamesToWrite], row.names=F, file='./result/xwas_adjusted.csv')
## super_adj:
colnamesToWrite <- c('varlabel_long', 'conf_int_string_super_adj', 'pvalue_super_adj', 'odds_ratio_train_super_adj', 'pvalue_train_super_adj', 'odds_ratio_test_super_adj', 'pvalue_test_super_adj', 'validate_super_adj')
write.csv(xwasData.all[, colnamesToWrite], row.names=F, file='./result/xwas_super_adjusted.csv')


