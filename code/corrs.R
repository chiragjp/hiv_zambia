library(polycor)
#dat <- read.csv('../data/hiv xwas Zambia07 dataset v7.csv')
dat <- read.csv('../data/hiv xwas Zambia13 dataset v7.csv')
dat$v000 <- NULL
dat$cname <- NULL
dat$id1 <- NULL
dat$v005 <- NULL
dat$v021 <- NULL
dat$v001 <- NULL
dat$v007 <- NULL

dependentVariable <- 'hiv03'

dat[which(dat[, dependentVariable] > 1 ), dependentVariable] <- NA

### read in the labels
labels <- read.delim('../output/tables_07-13/variable_labels_v2.txt', sep="\t", stringsAsFactors = F)
numCategorical <- as.data.frame(table(labels[labels$level != 'continuous', 'variable']))
colnames(numCategorical) <- c('variable', 'num_levels')
binaryVariables <- subset(numCategorical, num_levels == 2)
numCategorical <- subset(numCategorical, num_levels > 2)
labels$variable_code <- labels$variable
for(ii in 1:nrow(numCategorical)) {
  varname <- numCategorical[ii, 'variable']
  indx <- labels$variable == varname
  labels[indx, 'variable_code'] <- paste(labels$variable[which(indx)], 1:(sum(indx)), sep="_")
}

### doubled binary variables
binaryLabels <- labels[labels$variable %in% binaryVariables$variable, ]
binaryLabelsToKeep <-  subset(binaryLabels, level == 1)
labels <- labels[!(labels$variable %in% binaryLabels$variable), ]
labels <- rbind(labels, binaryLabelsToKeep)

###
factorCols <- subset(labels, level != 'continuous')$variable_code
datToProcess <- dat
factorThese <- intersect(colnames(datToProcess) , factorCols)
datToProcess[, factorThese] <- lapply(datToProcess[, factorThese], as.factor)


datToProcess <- datToProcess[, colnames(datToProcess) %in% labels$variable_code]
datToProcess$hiv03 <- dat$hiv03

##
corrs.het <- hetcor(datToProcess, std.err=F, use="pairwise.complete.obs", pd=F)
#save(corrs.het, file='correlations_07.Rdata')
save(corrs.het, file='correlations_13.Rdata')


