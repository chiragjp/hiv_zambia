# compute and visualize power for re-submission 
# 9/3/2016

# Compute power for each country / sex

library(ggplot2)
library(pwr)

dat <- read.csv('../data/all hiv test results v1.csv')
dataSummary <- as.data.frame(table(dat$hiv03, dat$svyid, dat$hv104))
colnames(dataSummary) <- c('hiv', 'dataset', 'sex', 'freq')

compute_logistic_power <- function(sample_1_n, sample_2_n, OR, numTests) {
  ### compute power for a logistic regression with 2 different sample sizes
  ### first convert OR --> d
  d <- log(OR)*sqrt(3)/pi ## this is mean difference
  p <- pwr.t2n.test(n1=sample_1_n, n2=sample_2_n, d=d, sig.level = 0.05/numTests) # need to adjust for FDR?
  return(p)
}

compute_logistic_power_sig_level <-  function(sample_1_n, sample_2_n, OR, sig.level) {
  d <- log(OR)*sqrt(3)/pi ## this is mean difference
  p <- pwr.t2n.test(n1=sample_1_n, n2=sample_2_n, d=d, sig.level = sig.level) 
  return(p)
}


power_range <- function(N1, N2, ors=c(1.1,1.25,1.5,1.75, 2, 2.1, 2.25, 2.5, 2.75, 3.0), numTests=c(500, 1000, 5000, 10000)) {
  retFrame <- data.frame()
  for(or in ors) {
    for(n in numTests) {
      p <- compute_logistic_power(N1, N2, or, n)
      retFrame <- rbind(retFrame, data.frame(OR=or, d=p$d, numTest=n, sigLevel=p$sig.level, power=p$power))
    }
  }
  retFrame$cases <- N1
  retFrame$controls <- N2
  return(retFrame)
}

power_range_sig_levels <- function(N1, N2, ors =c(1.1,1.25,1.5,1.75, 2, 2.1, 2.25, 2.5, 2.75, 3.0), sig.levels=0.05) {
  retFrame <- data.frame()
  for(or in ors) {
    for(sl in sig.levels) {
      p <- compute_logistic_power_sig_level(N1, N2, or, sl)
      retFrame <- rbind(retFrame, data.frame(OR=or, d=p$d, sigLevel=p$sig.level, power=p$power))
    }
  }
  retFrame$cases <- N1
  retFrame$controls <- N2
  return(retFrame)
}

# vary OR, numTests, numTest == 1000
## cycle through each scenario

## how to account for FDR?
## from Practical guidelines for assessing power and false discovery rate for a fixed sample size in microarray experiments
## Tong et al, Stat Med 2008
##


fdr_from_sig_level <- function(pvalues, powers, pi0=0.9) {
  fdr <- (pi0 * pvalues) / ((pvalues * pi0) +  ((1-pi0)*powers))
  return(fdr)
}

sig_levels <- c(0.001, 0.05/1000)

cohorts <- dataSummary$dataset
powers <- data.frame()
for(ii in 1:length(cohorts)) {
  cohort <- cohorts[ii]
  femaleCountNeg <- subset(dataSummary, dataset == cohort & hiv == 'hiv negative' & sex == 'female')$freq
  femaleCountPos <- subset(dataSummary, dataset == cohort & hiv == 'hiv  positive' & sex == 'female')$freq
  rngf <- power_range_sig_levels(femaleCountPos, femaleCountNeg, sig.levels = sig_levels)
  rngf$cohort <- cohort
  rngf$sex <- 'female'
  
  maleCountNeg <- subset(dataSummary, dataset == cohort & hiv == 'hiv negative' & sex == 'male')$freq
  maleCountPos <- subset(dataSummary, dataset == cohort & hiv == 'hiv  positive' & sex == 'male')$freq
  rngm <- power_range_sig_levels(maleCountPos, maleCountNeg, sig.levels=sig_levels)
  rngm$cohort <- cohort
  rngm$sex <- 'male'
  powers <- rbind(powers, rngf, rngm)
}

## ZM6_2013
## RW6_2010
## SN4_2005

cohorts <- c('ZM6_2013', 'RW6_2010', 'SN4_2005')
g <- ggplot(powers[powers$cohort %in% cohorts, ], aes(OR, power))
g <- g + geom_point(aes(color=cohort)) + geom_line(aes(color=cohort)) + geom_hline(yintercept = 0.8)
g <- g + geom_line(aes(OR, power, color=cohort), alpha=.4, data=powers[-(powers$cohort %in% cohorts), ]) + facet_wrap(sex~sigLevel)
g




