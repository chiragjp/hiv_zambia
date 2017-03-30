source('super_adjustments_07.R')

weirdVars <- c('v137_7', 'v624_1')
dat <- read.csv('../data/hiv xwas Zambia07 dataset v7.csv')
table(dat$v137_7)
table(dat$v624_1)
test <- cor(dat[, c(adjustmentVariables, weirdVars)], use='pairwise.complete.obs')
library(gplots)
heatmap.2(test)