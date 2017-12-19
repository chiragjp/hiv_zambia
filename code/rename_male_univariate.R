
remove(list=ls())
load('../output/results_output/hiv_males_2007.Rdata')
colnames(xwasData) <- paste(colnames(xwasData), 'univariate_07', sep="_")
colnames(allData.overall) <- paste(colnames(allData.overall), 'univariate_07', sep="_")
save.image('../output/results_output/hiv_males_2007.Rdata')

remove(list=ls())
load('../output/results_output/hiv_males_2013.Rdata')
colnames(xwasData) <- paste(colnames(xwasData), 'univariate_13', sep="_")
colnames(allData.overall) <- paste(colnames(allData.overall), 'univariate_13', sep="_")
save.image('../output/results_output/hiv_males_2013.Rdata')