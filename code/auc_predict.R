## predict v2
# 1.) AUC of each of the three analytic scenarios in the full dataset
# 2.) of those that were replicated in 07, how do they predict in 13?

library(ROCR)
library(rms)
library(ggplot2)

dat07 <- read.csv('../data/hiv xwas Zambia07 dataset v7.csv')
dat13 <- read.csv('../data/hiv xwas Zambia13 dataset v7.csv')
load('./merged_output/merged.Rdata')

dat07 <- subset(dat07, hiv03 <= 1)
dat13 <- subset(dat13, hiv03 <= 1)


auc_for_model <- function(dataFrame, variables) {
  vars <- c('hiv03', variables)
  dat <- dataFrame[, vars]
  dat <- dat[complete.cases(dat), ]
  mod <- lrm(hiv03 ~ ., dat)
  pred <- prediction(predict(mod, newdata=dat), dat$hiv03)
  auc <- attr(performance(pred,"auc"), 'y.values')[[1]]
  sens <- attr(performance(pred,"sens"), 'y.values')[[1]]
  spec <- attr(performance(pred,"spec"), 'y.values')[[1]]
  r2 <- mod$stats[10]
  return(list(pred=pred, model=mod, R2=r2, spec=spec, sens=sens, auc=auc, N=nrow(dat), M=length(variables)))
}

## first, do model with only univariate predictors
vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_univariate_07==1)])
auc07.univariate <- auc_for_model(dat07, vars)
cat(sprintf('2007:\n'))
cat(sprintf('univariate AUC=%f\n', auc07.univariate$auc))
cat(sprintf('univariate R2=%f\n', auc07.univariate$R2))
cat(sprintf('M=%f\n', auc07.univariate$M))

## apriori
vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_apriori_adj_07==1)])
vars <- setdiff(vars, 'v447_4')
auc07.apriori <- auc_for_model(dat07, vars)

cat(sprintf('apriori AUC=%f\n', auc07.apriori$auc))
cat(sprintf('apriori R2=%f\n', auc07.apriori$R2))
cat(sprintf('M=%f\n', auc07.apriori$M))

##
vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_super_07==1)])
auc07.super <- auc_for_model(dat07, vars)
cat(sprintf('super AUC=%f\n', auc07.super$auc))
cat(sprintf('super R2=%f\n', auc07.super$R2))
cat(sprintf('M=%f\n', auc07.super$M))

##### 2013

vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_univariate_13==1)])
vars <- setdiff(vars, c('v717_4', 'v624_5', 'v605_3', 'v624_1'))
auc13.univariate <- auc_for_model(dat13, vars)
cat(sprintf('2013:\n'))
cat(sprintf('univariate AUC=%f\n', auc13.univariate$auc))
cat(sprintf('univariate R2=%f\n', auc13.univariate$R2))
cat(sprintf('M=%f\n', auc13.univariate$M))

## apriori
vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_aprior_adj_13==1)])
auc13.apriori <- auc_for_model(dat13, vars)
cat(sprintf('apriori AUC=%f\n', auc13.apriori$auc))
cat(sprintf('apriori R2=%f\n', auc13.apriori$R2))
cat(sprintf('M=%f\n', auc13.apriori$M))

##
vars <- c(mergedResults$merged_data$variable_univariate_07[which(mergedResults$merged_data$replicated_super_13==1)])
auc13.super <- auc_for_model(dat13, vars)
cat(sprintf('super AUC=%f\n', auc13.super$auc))
cat(sprintf('super R2=%f\n', auc13.super$R2))
cat(sprintf('M=%f\n', auc13.super$M))

toPlot.2013 <- rbind(
  data.frame(spec=auc13.super$spec, sens=auc13.super$sens, year=rep('2013-2014', length(auc13.super$sens)), model=rep('2013-14: ex post (M=86;AUC=0.79;R2=0.25)', length(auc13.super$sens))),
  data.frame(spec=auc13.apriori$spec, sens=auc13.apriori$sens, year=rep('2013-2014', length(auc13.apriori$sens)), model=rep('2013-14: ex ante (M=95;AUC=0.79;R2=0.26)', length(auc13.apriori$sens))),
  data.frame(spec=auc13.univariate$spec, sens=auc13.univariate$sens, year=rep('2013-2014',length(auc13.univariate$sens)), model=rep('2013-14: univariate (M=178;AUC=0.82;R2=0.32)', length(auc13.univariate$sens)))
  )


toPlot.2007 <- rbind(
  data.frame(spec=auc07.super$spec, sens=auc07.super$sens, year=rep('2007', length(auc07.super$sens)), model=rep('2007: ex post (M=18;AUC=0.69;R2=0.11)', length(auc07.super$sens))),
  data.frame(spec=auc07.apriori$spec, sens=auc07.apriori$sens, year=rep('2007', length(auc07.apriori$sens)), model=rep('2007: ex ante (M=37;AUC=0.76;R2=0.22)', length(auc07.apriori$sens))),
  data.frame(spec=auc07.univariate$spec, sens=auc07.univariate$sens, year=rep('2007',length(auc07.univariate$sens)), model=rep('2007: univariate (M=102;AUC=0.80;R2=0.29)', length(auc07.univariate$sens)))
)
  
toPlot <- rbind(toPlot.2013, toPlot.2007)
p <- ggplot(toPlot, aes(1-spec, sens, color=model))
p <- p + geom_point(size=.5) + facet_wrap(~year, nrow=1, ncol=2) + geom_abline() + xlab('1-specificity') + ylab('sensitivity')
p
