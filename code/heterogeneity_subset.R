#heterogeneity estimates for subsets
library(metafor)
load('./merged_output/merged.Rdata')


diffData.07 <- list(ageDiffs$mergedResults07$merged_data, 
                    hivTestDiffs$mergedResults07$merged_data,
                    locationDiffs$mergedResults07$merged_data,
                    poorRichDiffs$mergedResults07$merged_data)

diffData.13 <- list(ageDiffs$mergedResults13$merged_data, 
                    hivTestDiffs$mergedResults13$merged_data,
                    locationDiffs$mergedResults13$merged_data,
                    poorRichDiffs$mergedResults13$merged_data)

dat.07 <- Reduce(function(x, y) {
  cls.x <- grep('estimate_univariate', colnames(x))
  cls.x <- c(cls.x,grep('se_univariate', colnames(x)))
  cls.x <- c(cls.x,grep('key', colnames(x)))
  cls.y <- grep('estimate_univariate', colnames(y))
  cls.y <- c(cls.y,grep('se_univariate', colnames(y)))
  cls.y <- c(cls.y,grep('key', colnames(y)))
  merge(x[, cls.x], y[, cls.y], by.y='key', by.x='key', all=T)
}, diffData.07)

dat.13 <- Reduce(function(x, y) {
  cls.x <- grep('estimate_univariate', colnames(x))
  cls.x <- c(cls.x,grep('se_univariate', colnames(x)))
  cls.x <- c(cls.x,grep('key', colnames(x)))
  cls.y <- grep('estimate_univariate', colnames(y))
  cls.y <- c(cls.y,grep('se_univariate', colnames(y)))
  cls.y <- c(cls.y,grep('key', colnames(y)))
  merge(x[, cls.x], y[, cls.y], by.y='key', by.x='key', all=T)
}, diffData.13)



meta_analyze_each_variable <- function(dat) {
  ## dat is a merged data frame from the Reduce function above
  ind.est <- grep('estimate_univariate', colnames(dat))
  ind.se <- grep('se_univariate', colnames(dat))
  ## break apart
  ests <- dat[, ind.est]
  ses <- dat[, ind.se]
  ests <- ests[, sort(colnames(ests))]
  ses <- ses[, sort(colnames(ses))]
  variables <- dat$key
  
  ## now do the meta-analysis
  
  collect_meta_estimates <- function(metaA) {
    return(data.frame(b=metaA$b, se=metaA$se, pval=metaA$pval, k=metaA$k, tau2 =metaA$tau2, tau2se=metaA$se.tau2, QE=metaA$QE, QEp=metaA$QEp, I2=metaA$I2, H2=metaA$H2))
  }
  
  metaResults <- data.frame()
  for(ii in 1:nrow(ests)) {
    yi <- as.matrix(ests[ii, ])
    sei <- as.matrix(ses[ii, ])
    ind <- !is.na(yi) & !is.na(sei)
    m <- length(ind)
    metaA <- rma.uni(yi=yi[ind], sei=sei[ind], method = 'DL')
    frm <- collect_meta_estimates(metaA)
    frm$variable <- variables[ii]
    metaResults <- rbind(metaResults, frm)
  }
  return(metaResults)
}

metaResults.07 <- meta_analyze_each_variable(dat.07)
metaResults.13 <- meta_analyze_each_variable(dat.13)


metaResults.13.all <- merge(metaResults.13, dat.13, by.x = 'variable', by.y='key')
metaResults.07.all <- merge(metaResults.07, dat.07, by.x = 'variable', by.y='key')

save(metaResults.07.all, file='meta_subset_07.Rdata')
save(metaResults.13.all, file='meta_subset_13.Rdata')

write.csv(file='meta_heterogeneity_results_07.csv', metaResults.07.all, row.names=F)
write.csv(file='meta_heterogeneity_results_13.csv', metaResults.13.all, row.names=F)





