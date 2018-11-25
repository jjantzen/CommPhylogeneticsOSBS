#Reorganizing dataframe for prop taxa
library(tidyr)
library(data.table)
ntax <- read.csv("./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_prop_subsets.csv", stringsAsFactors = FALSE)

colnames(ntax)

ntax <- ntax[,-1]
expanded <- spread(ntax, key = variable, value = value)
expanded
aggregate(.~community, data=ntax, FUN="xx")

require(data.table)
DT <- data.table(expanded)
# ignore the warning here...
db <- unique(DT[, lapply(.SD, function(x) x[!is.na(x)]), by = community])

write.csv(expanded, "./PD_files/R_calc_picante/Significance_values/Ntaxa_spread_avg_prop_subsets.csv", row.names = FALSE)


db
