## produce a heatmap of correlations 
library(gplots)

heatmapColors <- function(numColors=16) {
  c1 <- rainbow(numColors,v=seq(0.5,1,length=numColors),s=seq(1,0.3,length=numColors),start=4/6,end=4.0001/6);
  c2 <- rainbow(numColors,v=seq(0.5,1,length=numColors),s=seq(1,0.3,length=numColors),start=1/6,end=1.0001/6);
  c3 <- c(c1,rev(c2)); 
  return(c3)
}

load('./merged_output/merged.Rdata')
load('./merged_output/correlations_13.Rdata')
corrs.het.13 <- corrs.het
load('./merged_output/correlations_07.Rdata')
corrs.het.07 <- corrs.het


replicatedColnames <- colnames(mergedResults$merged_data)[grep('replicated', colnames(mergedResults$merged_data))]
numberOfReplications <- rowSums(mergedResults$merged_data[, replicatedColnames], na.rm = T) ## this gives the number of times a result was replicated in each of the modeling scenarios

repRows07 <- (which(mergedResults$merged_data[, 'replicated_apriori_adj_07'] == 1))
repRows13 <- (which(mergedResults$merged_data[, 'replicated_aprior_adj_13'] == 1))
# 
repInBoth <- intersect(repRows13, repRows07)

indx <- repInBoth
variableNames <- mergedResults$merged_data[indx, 'variable_univariate_07']
rws <- which(colnames(corrs.het.07$correlations) %in% variableNames)
corrSelect.07 <- corrs.het.07$correlations[rws, rws]

rws <- which(colnames(corrs.het.13$correlations) %in% variableNames)
corrSelect.13 <- corrs.het.13$correlations[rws, rws]

## rename rows and columns
lblName <- c()
nmes <- colnames(corrSelect.13)
for(ii in 1:length(nmes)) {
  lblName <- c(lblName, subset(mergedResults$merged_data, variable_univariate_07 == nmes[ii])$var_label[1])
}
colnames(corrSelect.13) <- rownames(corrSelect.13) <- lblName
colnames(corrSelect.07) <- rownames(corrSelect.07) <- lblName
test <- data.frame(nmes=nmes, var_label=lblName)

pdf('heatmap_07.pdf', height=10, width=10)
retHeat.07 <- heatmap.2(corrSelect.07, trace='none', col=heatmapColors(8), main='07', margins=c(20,20))
#retHeat.07 <- heatmap.2(corrSelect.07, trace='none', col=heatmapColors(8), main='07')
dev.off()


pdf('heatmap_13.pdf', height=10, width=10)
heatmap.2(corrSelect.13, Rowv=retHeat.07$rowDendrogram, Colv=retHeat.07$colDendrogram, trace='none', col=heatmapColors(8), main='13', margins=c(20,20))
dev.off()

## correlation of correlations
corr.upper13 <- corrSelect.13[upper.tri(corrSelect.13)]
corr.upper07 <- corrSelect.07[upper.tri(corrSelect.07)]
cor(corr.upper07, corr.upper13, method = 'spearman')

### single category variable names
variableNames.single.remove <- c('v137_3', 'v203_4', 'v238_2', 'v220_7', 'v219_2', 'v446', 'v208_1', 'v501_4', 'v501_5', 'v501_2', 'v502_3')
variableNames.single <- setdiff(variableNames, variableNames.single.remove)

rws <- which(colnames(corrs.het.07$correlations) %in% variableNames.single)
corrSelect.07.single <- corrs.het.07$correlations[rws, rws]

rws <- which(colnames(corrs.het.13$correlations) %in% variableNames.single)
corrSelect.13.single <- corrs.het.13$correlations[rws, rws]

lblName <- c()
nmes <- colnames(corrSelect.13.single)
for(ii in 1:length(nmes)) {
  lblName <- c(lblName, subset(mergedResults$merged_data, variable_univariate_07 == nmes[ii])$var_label[1])
}
colnames(corrSelect.13.single) <- rownames(corrSelect.13.single) <- lblName
colnames(corrSelect.07.single) <- rownames(corrSelect.07.single) <- lblName

pdf('heatmap_07_single.pdf', height=10, width=10)
retHeat.07 <- heatmap.2(corrSelect.07.single, trace='none', col=heatmapColors(8), main='07', margins=c(20,20))
#retHeat.07 <- heatmap.2(corrSelect.07, trace='none', col=heatmapColors(8), main='07')
dev.off()


pdf('heatmap_13_single.pdf', height=10, width=10)
heatmap.2(corrSelect.13.single, Rowv=retHeat.07$rowDendrogram, Colv=retHeat.07$colDendrogram, trace='none', col=heatmapColors(8), main='13', margins=c(20,20))
dev.off()

corr.upper13.single <- corrSelect.13.single[upper.tri(corrSelect.13.single)]
corr.upper07.single <- corrSelect.07.single[upper.tri(corrSelect.07.single)]
cor(corr.upper07.single, corr.upper13.single, method = 'spearman')