#Making tables for paper

#Question 1 - Table of significance values for subsets by community (Tukey)
#Read tukey files
Tukey_NRI <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_NRI_random_tukey.csv", stringsAsFactors = FALSE)
Tukey_NTI <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_NTI_random_tukey.csv", stringsAsFactors = FALSE)
Tukey_PD <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_PD_random_tukey.csv", stringsAsFactors = FALSE)

#Choose only pvalue and community columns
NRI_pvalues <- as.data.frame(cbind((Tukey_NRI[,5]), Tukey_NRI[,7]))
NTI_pvalues <- as.data.frame(cbind((Tukey_NTI[,5]), Tukey_NTI[,7]))
PD_pvalues <- as.data.frame(cbind((Tukey_PD[,5]), Tukey_PD[,7]))

#Set column names 
colnames(NRI_pvalues) <- c("pvalue", "comm")
colnames(NTI_pvalues) <- c("pvalue", "comm")
colnames(PD_pvalues) <- c("pvalue", "comm")

#Set row names
NRI_pvalues$subset <- c(rep(Tukey_NRI$X[1:10], 14))
NTI_pvalues$subset <- c(rep(Tukey_NTI$X[1:10], 14))
PD_pvalues$subset <- c(rep(Tukey_PD$X[1:10], 14))

#Spread pvalues by community
NRI_pvalues_spread <- spread(NRI_pvalues, key = subset, value = pvalue, convert = TRUE)
NTI_pvalues_spread <- spread(NTI_pvalues, key = subset, value = pvalue, convert = TRUE)
PD_pvalues_spread <- spread(PD_pvalues, key = subset, value = pvalue, convert = TRUE)

#Change column names 
colnames(NRI_pvalues_spread) <- c(paste0("NRI", colnames(NRI_pvalues_spread)))
colnames(NTI_pvalues_spread) <- c(paste0("NTI", colnames(NTI_pvalues_spread)))
colnames(PD_pvalues_spread) <- c(paste0("PD", colnames(PD_pvalues_spread)))

#Replace - in column names
colnames(NRI_pvalues_spread) <- gsub("-", "_", colnames(NRI_pvalues_spread))
colnames(NTI_pvalues_spread) <- gsub("-", "_", colnames(NTI_pvalues_spread))
colnames(PD_pvalues_spread) <- gsub("-", "_", colnames(PD_pvalues_spread))

#Write function for rounding numeric columns
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Round values
NRI_pvalues_spread[,2:11] <- round(NRI_pvalues_spread[,2:11], 4)
NTI_pvalues_spread[,2:11] <- round(NTI_pvalues_spread[,2:11], 4)
PD_pvalues_spread[,2:11] <- round(PD_pvalues_spread[,2:11], 4)

#Replace value if less than 1x10e-4

NRI_pvalues_spread[,2:11][NRI_pvalues_spread[,2:11] < 0.001] <- "<0.001"
NTI_pvalues_spread[,2:11][NTI_pvalues_spread[,2:11] < 0.001] <- "<0.001"
PD_pvalues_spread[,2:11][PD_pvalues_spread[,2:11] < 0.001] <- "<0.001"

# for (i in 1:nrow(NRI_pvalues_spread)){
#   for (j in 2:ncol(NRI_pvalues_spread)){
#     if (NRI_pvalues_spread[i,j] < 0.001) {
#       NRI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(NTI_pvalues_spread)){
#   for (j in 2:ncol(NTI_pvalues_spread)){
#     if (NTI_pvalues_spread[i,j] < 0.001) {
#       NTI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(PD_pvalues_spread)){
#   for (j in 2:ncol(PD_pvalues_spread)){
#     if (PD_pvalues_spread[i,j] < 0.001) {
#       PD_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 


#Combine indices to make x/x/x within cell
all_indices_tukey <- as.data.frame(cbind(NRI_pvalues_spread, NTI_pvalues_spread, PD_pvalues_spread))

all_indices_tukey_200_100 <- unite(all_indices_tukey, "NRI/NTI/PD_200_100", NRI200_100, NTI200_100, PD200_100, sep = "/", remove = TRUE)
all_indices_tukey_300_100 <- unite(all_indices_tukey, "NRI/NTI/PD_300_100", NRI300_100, NTI300_100, PD300_100, sep = "/", remove = TRUE)
all_indices_tukey_400_100 <- unite(all_indices_tukey, "NRI/NTI/PD_400_100", NRI400_100, NTI400_100, PD400_100, sep = "/", remove = TRUE)
all_indices_tukey_500_100 <- unite(all_indices_tukey, "NRI/NTI/PD_500_100", NRI500_100, NTI500_100, PD500_100, sep = "/", remove = TRUE)
all_indices_tukey_300_200 <- unite(all_indices_tukey, "NRI/NTI/PD_300_200", NRI300_200, NTI300_200, PD300_200, sep = "/", remove = TRUE)
all_indices_tukey_400_200 <- unite(all_indices_tukey, "NRI/NTI/PD_400_200", NRI400_200, NTI400_200, PD400_200, sep = "/", remove = TRUE)
all_indices_tukey_500_200 <- unite(all_indices_tukey, "NRI/NTI/PD_500_200", NRI500_200, NTI500_200, PD500_200, sep = "/", remove = TRUE)
all_indices_tukey_400_300 <- unite(all_indices_tukey, "NRI/NTI/PD_400_300", NRI400_300, NTI400_300, PD400_300, sep = "/", remove = TRUE)
all_indices_tukey_500_300 <- unite(all_indices_tukey, "NRI/NTI/PD_500_300", NRI500_300, NTI500_300, PD500_300, sep = "/", remove = TRUE)
all_indices_tukey_500_400 <- unite(all_indices_tukey, "NRI/NTI/PD_500_400", NRI500_400, NTI500_400, PD500_400, sep = "/", remove = TRUE)

all_indices_tukey_joined <- as.data.frame(cbind(all_indices_tukey_200_100$`NRI/NTI/PD_200_100`, all_indices_tukey_300_100$`NRI/NTI/PD_300_100`, all_indices_tukey_400_100$`NRI/NTI/PD_400_100`, all_indices_tukey_500_100$`NRI/NTI/PD_500_100`, all_indices_tukey_300_200$`NRI/NTI/PD_300_200`, all_indices_tukey_400_200$`NRI/NTI/PD_400_200`, all_indices_tukey_500_200$`NRI/NTI/PD_500_200`, all_indices_tukey_400_300$`NRI/NTI/PD_400_300`, all_indices_tukey_500_300$`NRI/NTI/PD_500_300`, all_indices_tukey_500_400$`NRI/NTI/PD_500_400`))

#Set row names and column names
rownames(all_indices_tukey_joined) <- as.character(all_indices_tukey_200_100$NRIcomm)
colnames(all_indices_tukey_joined) <- c("200v100", "300v100", "400v100", "500v100", "300v200", "400v200", "500v200", "400v300", "500v300", "500v400")

all_indices_tukey_joined

#Write table
write.csv(all_indices_tukey_joined, "../Writing/Tables/Q1_sig_values_tukey.csv", row.names = TRUE)


######
#Question 2 - Table of t-test results for between reconstruction method comparisons

#Read ttest files
ttest_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_reconprune_NRI.csv", stringsAsFactors = FALSE)
ttest_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_reconprune_NTI.csv", stringsAsFactors = FALSE)
ttest_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_reconprune_PD.csv", stringsAsFactors = FALSE)

#Choose only pvalue and community columns
NRI_pvalues <- as.data.frame(cbind((ttest_NRI[,4]), ttest_NRI[,3]))
NTI_pvalues <- as.data.frame(cbind((ttest_NTI[,4]), ttest_NTI[,3]))
PD_pvalues <- as.data.frame(cbind((ttest_PD[,4]), ttest_PD[,3]))

#Set column names 
colnames(NRI_pvalues) <- c("pvalue", "comm")
colnames(NTI_pvalues) <- c("pvalue", "comm")
colnames(PD_pvalues) <- c("pvalue", "comm")

#Set row names
NRI_pvalues$subset <- ttest_NRI$subset
NTI_pvalues$subset <- ttest_NTI$subset
PD_pvalues$subset <- ttest_PD$subset

#Spread pvalues by community
NRI_pvalues_spread <- spread(NRI_pvalues, key = subset, value = pvalue, convert = TRUE)
NTI_pvalues_spread <- spread(NTI_pvalues, key = subset, value = pvalue, convert = TRUE)
PD_pvalues_spread <- spread(PD_pvalues, key = subset, value = pvalue, convert = TRUE)

#Change column names 
colnames(NRI_pvalues_spread) <- c(paste0("NRI", colnames(NRI_pvalues_spread)))
colnames(NTI_pvalues_spread) <- c(paste0("NTI", colnames(NTI_pvalues_spread)))
colnames(PD_pvalues_spread) <- c(paste0("PD", colnames(PD_pvalues_spread)))

#Write function for rounding numeric columns
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


#Round values
NRI_pvalues_spread[,2:6] <- round(NRI_pvalues_spread[,2:6], 4)
NTI_pvalues_spread[,2:6] <- round(NTI_pvalues_spread[,2:6], 4)
PD_pvalues_spread[,2:6] <- round(PD_pvalues_spread[,2:6], 4)


#Replace value if less than 1x10e-4
NRI_pvalues_spread[,2:6][NRI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
NTI_pvalues_spread[,2:6][NTI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
PD_pvalues_spread[,2:6][PD_pvalues_spread[,2:6] < 0.001] <- "<0.001"


# for (i in 1:nrow(NRI_pvalues_spread)){
#   for (j in 2:ncol(NRI_pvalues_spread)){
#     if (NRI_pvalues_spread[i,j] < 0.001) {
#       NRI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(NTI_pvalues_spread)){
#   for (j in 2:ncol(NTI_pvalues_spread)){
#     if (NTI_pvalues_spread[i,j] < 0.001) {
#       NTI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(PD_pvalues_spread)){
#   for (j in 2:ncol(PD_pvalues_spread)){
#     if (PD_pvalues_spread[i,j] < 0.001) {
#       PD_pvalues_spread[i,j] <- "<0.001"
#     }
#   } 
# }
# 
#Combine indices to make x/x/x within cell
all_indices_ttest <- as.data.frame(cbind(NRI_pvalues_spread, NTI_pvalues_spread, PD_pvalues_spread))

all_indices_ttest_100 <- unite(all_indices_ttest, "NRI/NTI/PD_100", NRI100, NTI100, PD100, sep = "/", remove = TRUE)
all_indices_ttest_200 <- unite(all_indices_ttest, "NRI/NTI/PD_200", NRI200, NTI200, PD200, sep = "/", remove = TRUE)
all_indices_ttest_300 <- unite(all_indices_ttest, "NRI/NTI/PD_300", NRI300, NTI300, PD300, sep = "/", remove = TRUE)
all_indices_ttest_400 <- unite(all_indices_ttest, "NRI/NTI/PD_400", NRI400, NTI400, PD400, sep = "/", remove = TRUE)
all_indices_ttest_500 <- unite(all_indices_ttest, "NRI/NTI/PD_500", NRI500, NTI500, PD500, sep = "/", remove = TRUE)

all_indices_ttest_joined <- as.data.frame(cbind(all_indices_ttest_100$`NRI/NTI/PD_100`, all_indices_ttest_200$`NRI/NTI/PD_200`, all_indices_ttest_300$`NRI/NTI/PD_300`, all_indices_ttest_400$`NRI/NTI/PD_400`, all_indices_ttest_500$`NRI/NTI/PD_500`))

#Set row names and column names
rownames(all_indices_ttest_joined) <- as.character(all_indices_ttest_100$NRIcomm)
colnames(all_indices_ttest_joined) <- c("100", "200", "300", "400", "500")

#Write table
write.csv(all_indices_ttest_joined, "../Writing/Tables/Q2_sig_values_ttest.csv", row.names = TRUE)

#######
#Question 3 - Table of ttest significance levels for chronograms vs phylograms

#Read ttest files
ttest_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_cladult_NRI.csv", stringsAsFactors = FALSE)
ttest_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_cladult_NTI.csv", stringsAsFactors = FALSE)
ttest_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_cladult_PD.csv", stringsAsFactors = FALSE)

#Choose only pvalue and community columns
NRI_pvalues <- as.data.frame(cbind((ttest_NRI[,4]), ttest_NRI[,3]))
NTI_pvalues <- as.data.frame(cbind((ttest_NTI[,4]), ttest_NTI[,3]))
PD_pvalues <- as.data.frame(cbind((ttest_PD[,4]), ttest_PD[,3]))

#Set column names 
colnames(NRI_pvalues) <- c("pvalue", "comm")
colnames(NTI_pvalues) <- c("pvalue", "comm")
colnames(PD_pvalues) <- c("pvalue", "comm")

#Set row names
NRI_pvalues$subset <- ttest_NRI$subset
NTI_pvalues$subset <- ttest_NTI$subset
PD_pvalues$subset <- ttest_PD$subset

#Spread pvalues by community
NRI_pvalues_spread <- spread(NRI_pvalues, key = subset, value = pvalue, convert = TRUE)
NTI_pvalues_spread <- spread(NTI_pvalues, key = subset, value = pvalue, convert = TRUE)
PD_pvalues_spread <- spread(PD_pvalues, key = subset, value = pvalue, convert = TRUE)

#Change column names 
colnames(NRI_pvalues_spread) <- c(paste0("NRI", colnames(NRI_pvalues_spread)))
colnames(NTI_pvalues_spread) <- c(paste0("NTI", colnames(NTI_pvalues_spread)))
colnames(PD_pvalues_spread) <- c(paste0("PD", colnames(PD_pvalues_spread)))

#Write function for rounding numeric columns
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Round values
NRI_pvalues_spread[,2:6] <- round(NRI_pvalues_spread[,2:6], 4)
NTI_pvalues_spread[,2:6] <- round(NTI_pvalues_spread[,2:6], 4)
PD_pvalues_spread[,2:6] <- round(PD_pvalues_spread[,2:6], 4)

#Replace value if less than 1x10e-4
NRI_pvalues_spread[,2:6][NRI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
NTI_pvalues_spread[,2:6][NTI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
PD_pvalues_spread[,2:6][PD_pvalues_spread[,2:6] < 0.001] <- "<0.001"
# 
# #Replace value if less than 1x10e-4
# for (i in 1:nrow(NRI_pvalues_spread)){
#   for (j in 2:ncol(NRI_pvalues_spread)){
#     if (NRI_pvalues_spread[i,j] < 0.001) {
#       NRI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(NTI_pvalues_spread)){
#   for (j in 2:ncol(NTI_pvalues_spread)){
#     if (NTI_pvalues_spread[i,j] < 0.001) {
#       NTI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(PD_pvalues_spread)){
#   for (j in 2:ncol(PD_pvalues_spread)){
#     if (PD_pvalues_spread[i,j] < 0.001) {
#       PD_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 


#Combine indices to make x/x/x within cell
all_indices_ttest <- as.data.frame(cbind(NRI_pvalues_spread, NTI_pvalues_spread, PD_pvalues_spread))

all_indices_ttest_100 <- unite(all_indices_ttest, "NRI/NTI/PD_100", NRI100, NTI100, PD100, sep = "/", remove = TRUE)
all_indices_ttest_200 <- unite(all_indices_ttest, "NRI/NTI/PD_200", NRI200, NTI200, PD200, sep = "/", remove = TRUE)
all_indices_ttest_300 <- unite(all_indices_ttest, "NRI/NTI/PD_300", NRI300, NTI300, PD300, sep = "/", remove = TRUE)
all_indices_ttest_400 <- unite(all_indices_ttest, "NRI/NTI/PD_400", NRI400, NTI400, PD400, sep = "/", remove = TRUE)
all_indices_ttest_500 <- unite(all_indices_ttest, "NRI/NTI/PD_500", NRI500, NTI500, PD500, sep = "/", remove = TRUE)

all_indices_ttest_joined <- as.data.frame(cbind(all_indices_ttest_100$`NRI/NTI/PD_100`, all_indices_ttest_200$`NRI/NTI/PD_200`, all_indices_ttest_300$`NRI/NTI/PD_300`, all_indices_ttest_400$`NRI/NTI/PD_400`, all_indices_ttest_500$`NRI/NTI/PD_500`))

#Set row names and column names
rownames(all_indices_ttest_joined) <- as.character(all_indices_ttest_100$NRIcomm)
colnames(all_indices_ttest_joined) <- c("100", "200", "300", "400", "500")

#Write table
write.csv(all_indices_ttest_joined, "../Writing/Tables/Q3_sig_values_ttest.csv", row.names = TRUE)

#######
#Question 4 - Table of ttest significance levels for random vs proportional subsets

#Read ttest files
ttest_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_proprandom_NRI.csv", stringsAsFactors = FALSE)
ttest_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_proprandom_NTI.csv", stringsAsFactors = FALSE)
ttest_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Ttest_proprandom_PD.csv", stringsAsFactors = FALSE)

#Choose only pvalue and community columns
NRI_pvalues <- as.data.frame(cbind((ttest_NRI[,4]), ttest_NRI[,3]))
NTI_pvalues <- as.data.frame(cbind((ttest_NTI[,4]), ttest_NTI[,3]))
PD_pvalues <- as.data.frame(cbind((ttest_PD[,4]), ttest_PD[,3]))

#Set column names 
colnames(NRI_pvalues) <- c("pvalue", "comm")
colnames(NTI_pvalues) <- c("pvalue", "comm")
colnames(PD_pvalues) <- c("pvalue", "comm")

#Set row names
NRI_pvalues$subset <- ttest_NRI$subset
NTI_pvalues$subset <- ttest_NTI$subset
PD_pvalues$subset <- ttest_PD$subset

#Spread pvalues by community
NRI_pvalues_spread <- spread(NRI_pvalues, key = subset, value = pvalue, convert = TRUE)
NTI_pvalues_spread <- spread(NTI_pvalues, key = subset, value = pvalue, convert = TRUE)
PD_pvalues_spread <- spread(PD_pvalues, key = subset, value = pvalue, convert = TRUE)

#Change column names 
colnames(NRI_pvalues_spread) <- c(paste0("NRI", colnames(NRI_pvalues_spread)))
colnames(NTI_pvalues_spread) <- c(paste0("NTI", colnames(NTI_pvalues_spread)))
colnames(PD_pvalues_spread) <- c(paste0("PD", colnames(PD_pvalues_spread)))

#Write function for rounding numeric columns
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Round values
NRI_pvalues_spread[,2:6] <- round(NRI_pvalues_spread[,2:6], 4)
NTI_pvalues_spread[,2:6] <- round(NTI_pvalues_spread[,2:6], 4)
PD_pvalues_spread[,2:6] <- round(PD_pvalues_spread[,2:6], 4)

#Replace value if less than 1x10e-4
NRI_pvalues_spread[,2:6][NRI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
NTI_pvalues_spread[,2:6][NTI_pvalues_spread[,2:6] < 0.001] <- "<0.001"
PD_pvalues_spread[,2:6][PD_pvalues_spread[,2:6] < 0.001] <- "<0.001"
# 
# #Replace value if less than 1x10e-4
# for (i in 1:nrow(NRI_pvalues_spread)){
#   for (j in 2:ncol(NRI_pvalues_spread)){
#     if (NRI_pvalues_spread[i,j] < 0.001) {
#       NRI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(NTI_pvalues_spread)){
#   for (j in 2:ncol(NTI_pvalues_spread)){
#     if (NTI_pvalues_spread[i,j] < 0.001) {
#       NTI_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 
# for (i in 1:nrow(PD_pvalues_spread)){
#   for (j in 2:ncol(PD_pvalues_spread)){
#     if (PD_pvalues_spread[i,j] < 0.001) {
#       PD_pvalues_spread[i,j] <- "<0.001"
#     }
#   }
# }
# 


#Combine indices to make x/x/x within cell
all_indices_ttest <- as.data.frame(cbind(NRI_pvalues_spread, NTI_pvalues_spread, PD_pvalues_spread))

all_indices_ttest_100 <- unite(all_indices_ttest, "NRI/NTI/PD_100", NRI100, NTI100, PD100, sep = "/", remove = TRUE)
all_indices_ttest_200 <- unite(all_indices_ttest, "NRI/NTI/PD_200", NRI200, NTI200, PD200, sep = "/", remove = TRUE)
all_indices_ttest_300 <- unite(all_indices_ttest, "NRI/NTI/PD_300", NRI300, NTI300, PD300, sep = "/", remove = TRUE)
all_indices_ttest_400 <- unite(all_indices_ttest, "NRI/NTI/PD_400", NRI400, NTI400, PD400, sep = "/", remove = TRUE)
all_indices_ttest_500 <- unite(all_indices_ttest, "NRI/NTI/PD_500", NRI500, NTI500, PD500, sep = "/", remove = TRUE)

all_indices_ttest_joined <- as.data.frame(cbind(all_indices_ttest_100$`NRI/NTI/PD_100`, all_indices_ttest_200$`NRI/NTI/PD_200`, all_indices_ttest_300$`NRI/NTI/PD_300`, all_indices_ttest_400$`NRI/NTI/PD_400`, all_indices_ttest_500$`NRI/NTI/PD_500`))

#Set row names and column names
rownames(all_indices_ttest_joined) <- as.character(all_indices_ttest_100$NRIcomm)
colnames(all_indices_ttest_joined) <- c("100", "200", "300", "400", "500")

#Write table
write.csv(all_indices_ttest_joined, "../Writing/Tables/Q4_sig_values_ttest_proprandom_test.csv", row.names = TRUE)

#######
#Question 1 - Table of number of taxa per community per subset

#Read PD files
PD_clad_100_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_100/", pattern=".csv", full.names=TRUE)
PD_clad_200_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_200/", pattern=".csv", full.names=TRUE)
PD_clad_300_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_300/", pattern=".csv", full.names=TRUE)
PD_clad_400_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_400/", pattern=".csv", full.names=TRUE)
PD_clad_500_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_500/", pattern=".csv", full.names=TRUE)

reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

pd_clad_100 <- lapply(PD_clad_100_files, reading_PD_files)
pd_clad_200 <- lapply(PD_clad_200_files, reading_PD_files)
pd_clad_300 <- lapply(PD_clad_300_files, reading_PD_files)
pd_clad_400 <- lapply(PD_clad_400_files, reading_PD_files)
pd_clad_500 <- lapply(PD_clad_500_files, reading_PD_files)

clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Get list of all numbers of taxa for all replicates
clad_ntax_100 <- list()
clad_ntax_200 <- list()
clad_ntax_300 <- list()
clad_ntax_400 <- list()
clad_ntax_500 <- list()

for (i in 1:length(pd_clad_100)) {
  ntax <- (pd_clad_100[[i]]$ntaxa) 
  clad_ntax_100[[i]] <- c(ntax)
}

for (i in 1:length(pd_clad_200)) {
  ntax <- (pd_clad_200[[i]]$ntaxa) 
  clad_ntax_200[[i]] <- c(ntax)
}

for (i in 1:length(pd_clad_300)) {
  ntax <- (pd_clad_300[[i]]$ntaxa) 
  clad_ntax_300[[i]] <- c(ntax)
}

for (i in 1:length(pd_clad_400)) {
  ntax <- (pd_clad_400[[i]]$ntaxa) 
  clad_ntax_400[[i]] <- c(ntax)
}

for (i in 1:length(pd_clad_500)) {
  ntax <- (pd_clad_500[[i]]$ntaxa) 
  clad_ntax_500[[i]] <- c(ntax)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Correct spelling
row.names(tcomm_designations)[10] <- "Successional.Hardwood.Forest"

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
clad_ntax_100_df <- as.data.frame(clad_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_clad_100))), row.names = row.names(tcomm_designations))
clad_ntax_200_df <- as.data.frame(clad_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_clad_200))), row.names = row.names(tcomm_designations))
clad_ntax_300_df <- as.data.frame(clad_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_clad_300))), row.names = row.names(tcomm_designations))
clad_ntax_400_df <- as.data.frame(clad_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_clad_400))), row.names = row.names(tcomm_designations))
clad_ntax_500_df <- as.data.frame(clad_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_clad_500))), row.names = row.names(tcomm_designations))

#Add column for averages
clad_ntax_100_df$avg <- rowMeans(clad_ntax_100_df, na.rm = TRUE)
clad_ntax_200_df$avg <- rowMeans(clad_ntax_200_df, na.rm = TRUE)
clad_ntax_300_df$avg <- rowMeans(clad_ntax_300_df, na.rm = TRUE)
clad_ntax_400_df$avg <- rowMeans(clad_ntax_400_df, na.rm = TRUE)
clad_ntax_500_df$avg <- rowMeans(clad_ntax_500_df, na.rm = TRUE)

#Add column for sd
clad_ntax_100_df$sd <- apply(clad_ntax_100_df[1:100],1, sd, na.rm = TRUE)
clad_ntax_200_df$sd <- apply(clad_ntax_200_df[1:100],1, sd, na.rm = TRUE)
clad_ntax_300_df$sd <- apply(clad_ntax_300_df[1:100],1, sd, na.rm = TRUE)
clad_ntax_400_df$sd <- apply(clad_ntax_400_df[1:100],1, sd, na.rm = TRUE)
clad_ntax_500_df$sd <- apply(clad_ntax_500_df[1:100],1, sd, na.rm = TRUE)

#Add column for min
clad_ntax_100_df$min <- apply(clad_ntax_100_df[1:100],1, min, na.rm = TRUE)
clad_ntax_200_df$min <- apply(clad_ntax_200_df[1:100],1, min, na.rm = TRUE)
clad_ntax_300_df$min <- apply(clad_ntax_300_df[1:100],1, min, na.rm = TRUE)
clad_ntax_400_df$min <- apply(clad_ntax_400_df[1:100],1, min, na.rm = TRUE)
clad_ntax_500_df$min <- apply(clad_ntax_500_df[1:100],1, min, na.rm = TRUE)

#Add column for max
clad_ntax_100_df$max <- apply(clad_ntax_100_df[1:100],1, max, na.rm = TRUE)
clad_ntax_200_df$max <- apply(clad_ntax_200_df[1:100],1, max, na.rm = TRUE)
clad_ntax_300_df$max <- apply(clad_ntax_300_df[1:100],1, max, na.rm = TRUE)
clad_ntax_400_df$max <- apply(clad_ntax_400_df[1:100],1, max, na.rm = TRUE)
clad_ntax_500_df$max <- apply(clad_ntax_500_df[1:100],1, max, na.rm = TRUE)

#Round sd
clad_ntax_100_df$sd <- round(clad_ntax_100_df$sd, 2)
clad_ntax_200_df$sd <- round(clad_ntax_200_df$sd, 2)
clad_ntax_300_df$sd <- round(clad_ntax_300_df$sd, 2)
clad_ntax_400_df$sd <- round(clad_ntax_400_df$sd, 2)
clad_ntax_500_df$sd <- round(clad_ntax_500_df$sd, 2)

#Make column of final avg +- sd
clad_ntax_100_df <- unite(clad_ntax_100_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
clad_ntax_200_df <- unite(clad_ntax_200_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
clad_ntax_300_df <- unite(clad_ntax_300_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
clad_ntax_400_df <- unite(clad_ntax_400_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
clad_ntax_500_df <- unite(clad_ntax_500_df, avg_sd, avg, sd, sep = "±", remove = FALSE)

clad_ntax_100_df <- unite(clad_ntax_100_df, min_max, min, max, sep = "-", remove = FALSE)
clad_ntax_200_df <- unite(clad_ntax_200_df, min_max, min, max, sep = "-", remove = FALSE)
clad_ntax_300_df <- unite(clad_ntax_300_df, min_max, min, max, sep = "-", remove = FALSE)
clad_ntax_400_df <- unite(clad_ntax_400_df, min_max, min, max, sep = "-", remove = FALSE)
clad_ntax_500_df <- unite(clad_ntax_500_df, min_max, min, max, sep = "-", remove = FALSE)

clad_ntax_100_df <- unite(clad_ntax_100_df, sum_100, avg_sd, min_max, sep = " (", remove = FALSE)
clad_ntax_200_df <- unite(clad_ntax_200_df, sum_200, avg_sd, min_max, sep = " (", remove = FALSE)
clad_ntax_300_df <- unite(clad_ntax_300_df, sum_300, avg_sd, min_max, sep = " (", remove = FALSE)
clad_ntax_400_df <- unite(clad_ntax_400_df, sum_400, avg_sd, min_max, sep = " (", remove = FALSE)
clad_ntax_500_df <- unite(clad_ntax_500_df, sum_500, avg_sd, min_max, sep = " (", remove = FALSE)

clad_ntax_100_df$sum_100 <- paste0(clad_ntax_100_df$sum_100, ")")
clad_ntax_200_df$sum_200 <- paste0(clad_ntax_200_df$sum_200, ")")
clad_ntax_300_df$sum_300 <- paste0(clad_ntax_300_df$sum_300, ")")
clad_ntax_400_df$sum_400 <- paste0(clad_ntax_400_df$sum_400, ")")
clad_ntax_500_df$sum_500 <- paste0(clad_ntax_500_df$sum_500, ")")

#Make summary table
ntax_phylograms_subsets <- as.data.frame(cbind(clad_ntax_100_df$sum_100, clad_ntax_200_df$sum_200, clad_ntax_300_df$sum_300, clad_ntax_400_df$sum_400, clad_ntax_500_df$sum_500))
rownames(ntax_phylograms_subsets) <- rownames(clad_ntax_100_df)
colnames(ntax_phylograms_subsets) <- c("100", "200", "300", "400", "500")

# #Just averages
# ntax_avg_subsets <- as.data.frame(cbind(clad_ntax_100_df$avg, clad_ntax_200_df$avg, clad_ntax_300_df$avg, clad_ntax_400_df$avg, clad_ntax_500_df$avg))
# rownames(ntax_avg_subsets) <- rownames(clad_ntax_100_df)
# colnames(ntax_avg_subsets) <- c("100", "200", "300", "400", "500")
# ntax_avg_subsets <- round(ntax_avg_subsets, 0)
# 
# #Write just avg numbers of taxa
# write.csv(ntax_avg_subsets, "./Plots/2017_botany/Files_to_plot/Num_taxa_q1.csv", row.names = TRUE)

#Write table
write.csv(ntax_phylograms_subsets, "../Writing/Tables/Q1_num_taxa_random_sub.csv", row.names = TRUE)

#####
#Question 2 - Table of number of taxa per community per subset

#Read files
PD_recon_100_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_100/", pattern=".csv", full.names=TRUE)
PD_recon_200_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_200/", pattern=".csv", full.names=TRUE)
PD_recon_300_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_300/", pattern=".csv", full.names=TRUE)
PD_recon_400_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_400/", pattern=".csv", full.names=TRUE)
PD_recon_500_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_500/", pattern=".csv", full.names=TRUE)

reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

pd_recon_100 <- lapply(PD_recon_100_files, reading_PD_files)
pd_recon_200 <- lapply(PD_recon_200_files, reading_PD_files)
pd_recon_300 <- lapply(PD_recon_300_files, reading_PD_files)
pd_recon_400 <- lapply(PD_recon_400_files, reading_PD_files)
pd_recon_500 <- lapply(PD_recon_500_files, reading_PD_files)

clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Get list of all numbers of taxa for all replicates
recon_ntax_100 <- list()
recon_ntax_200 <- list()
recon_ntax_300 <- list()
recon_ntax_400 <- list()
recon_ntax_500 <- list()

for (i in 1:length(pd_recon_100)) {
  ntax <- (pd_recon_100[[i]]$ntaxa) 
  recon_ntax_100[[i]] <- c(ntax)
}

for (i in 1:length(pd_recon_200)) {
  ntax <- (pd_recon_200[[i]]$ntaxa) 
  recon_ntax_200[[i]] <- c(ntax)
}

for (i in 1:length(pd_recon_300)) {
  ntax <- (pd_recon_300[[i]]$ntaxa) 
  recon_ntax_300[[i]] <- c(ntax)
}

for (i in 1:length(pd_recon_400)) {
  ntax <- (pd_recon_400[[i]]$ntaxa) 
  recon_ntax_400[[i]] <- c(ntax)
}

for (i in 1:length(pd_recon_500)) {
  ntax <- (pd_recon_500[[i]]$ntaxa) 
  recon_ntax_500[[i]] <- c(ntax)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Correct spelling
row.names(tcomm_designations)[10] <- "Successional.Hardwood.Forest"

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
recon_ntax_100_df <- as.data.frame(recon_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_recon_100))), row.names = row.names(tcomm_designations))
recon_ntax_200_df <- as.data.frame(recon_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_recon_200))), row.names = row.names(tcomm_designations))
recon_ntax_300_df <- as.data.frame(recon_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_recon_300))), row.names = row.names(tcomm_designations))
recon_ntax_400_df <- as.data.frame(recon_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_recon_400))), row.names = row.names(tcomm_designations))
recon_ntax_500_df <- as.data.frame(recon_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_recon_500))), row.names = row.names(tcomm_designations))

#Add column for averages
recon_ntax_100_df$avg <- rowMeans(recon_ntax_100_df, na.rm = TRUE)
recon_ntax_200_df$avg <- rowMeans(recon_ntax_200_df, na.rm = TRUE)
recon_ntax_300_df$avg <- rowMeans(recon_ntax_300_df, na.rm = TRUE)
recon_ntax_400_df$avg <- rowMeans(recon_ntax_400_df, na.rm = TRUE)
recon_ntax_500_df$avg <- rowMeans(recon_ntax_500_df, na.rm = TRUE)

#Add column for sd
recon_ntax_100_df$sd <- apply(recon_ntax_100_df[1:(ncol(recon_ntax_100_df)-1)],1, sd, na.rm = TRUE)
recon_ntax_200_df$sd <- apply(recon_ntax_200_df[1:(ncol(recon_ntax_200_df)-1)],1, sd, na.rm = TRUE)
recon_ntax_300_df$sd <- apply(recon_ntax_300_df[1:(ncol(recon_ntax_300_df)-1)],1, sd, na.rm = TRUE)
recon_ntax_400_df$sd <- apply(recon_ntax_400_df[1:(ncol(recon_ntax_400_df)-1)],1, sd, na.rm = TRUE)
recon_ntax_500_df$sd <- apply(recon_ntax_500_df[1:(ncol(recon_ntax_500_df)-1)],1, sd, na.rm = TRUE)

#Add column for min
recon_ntax_100_df$min <- apply(recon_ntax_100_df[1:(ncol(recon_ntax_100_df)-2)],1, min, na.rm = TRUE)
recon_ntax_200_df$min <- apply(recon_ntax_200_df[1:(ncol(recon_ntax_200_df)-2)],1, min, na.rm = TRUE)
recon_ntax_300_df$min <- apply(recon_ntax_300_df[1:(ncol(recon_ntax_300_df)-2)],1, min, na.rm = TRUE)
recon_ntax_400_df$min <- apply(recon_ntax_400_df[1:(ncol(recon_ntax_400_df)-2)],1, min, na.rm = TRUE)
recon_ntax_500_df$min <- apply(recon_ntax_500_df[1:(ncol(recon_ntax_500_df)-2)],1, min, na.rm = TRUE)

#Add column for max
recon_ntax_100_df$max <- apply(recon_ntax_100_df[1:(ncol(recon_ntax_100_df)-3)],1, max, na.rm = TRUE)
recon_ntax_200_df$max <- apply(recon_ntax_200_df[1:(ncol(recon_ntax_200_df)-3)],1, max, na.rm = TRUE)
recon_ntax_300_df$max <- apply(recon_ntax_300_df[1:(ncol(recon_ntax_300_df)-3)],1, max, na.rm = TRUE)
recon_ntax_400_df$max <- apply(recon_ntax_400_df[1:(ncol(recon_ntax_400_df)-3)],1, max, na.rm = TRUE)
recon_ntax_500_df$max <- apply(recon_ntax_500_df[1:(ncol(recon_ntax_500_df)-3)],1, max, na.rm = TRUE)

#Round sd
recon_ntax_100_df$sd <- round(recon_ntax_100_df$sd, 2)
recon_ntax_200_df$sd <- round(recon_ntax_200_df$sd, 2)
recon_ntax_300_df$sd <- round(recon_ntax_300_df$sd, 2)
recon_ntax_400_df$sd <- round(recon_ntax_400_df$sd, 2)
recon_ntax_500_df$sd <- round(recon_ntax_500_df$sd, 2)

#Round avg
recon_ntax_100_df$avg <- round(recon_ntax_100_df$avg, 2)
recon_ntax_200_df$avg <- round(recon_ntax_200_df$avg, 2)
recon_ntax_300_df$avg <- round(recon_ntax_300_df$avg, 2)
recon_ntax_400_df$avg <- round(recon_ntax_400_df$avg, 2)
recon_ntax_500_df$avg <- round(recon_ntax_500_df$avg, 2)

#Make column of final avg +- sd
recon_ntax_100_df <- unite(recon_ntax_100_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
recon_ntax_200_df <- unite(recon_ntax_200_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
recon_ntax_300_df <- unite(recon_ntax_300_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
recon_ntax_400_df <- unite(recon_ntax_400_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
recon_ntax_500_df <- unite(recon_ntax_500_df, avg_sd, avg, sd, sep = "±", remove = FALSE)

recon_ntax_100_df <- unite(recon_ntax_100_df, min_max, min, max, sep = "-", remove = FALSE)
recon_ntax_200_df <- unite(recon_ntax_200_df, min_max, min, max, sep = "-", remove = FALSE)
recon_ntax_300_df <- unite(recon_ntax_300_df, min_max, min, max, sep = "-", remove = FALSE)
recon_ntax_400_df <- unite(recon_ntax_400_df, min_max, min, max, sep = "-", remove = FALSE)
recon_ntax_500_df <- unite(recon_ntax_500_df, min_max, min, max, sep = "-", remove = FALSE)

recon_ntax_100_df <- unite(recon_ntax_100_df, sum_100, avg_sd, min_max, sep = " (", remove = FALSE)
recon_ntax_200_df <- unite(recon_ntax_200_df, sum_200, avg_sd, min_max, sep = " (", remove = FALSE)
recon_ntax_300_df <- unite(recon_ntax_300_df, sum_300, avg_sd, min_max, sep = " (", remove = FALSE)
recon_ntax_400_df <- unite(recon_ntax_400_df, sum_400, avg_sd, min_max, sep = " (", remove = FALSE)
recon_ntax_500_df <- unite(recon_ntax_500_df, sum_500, avg_sd, min_max, sep = " (", remove = FALSE)

recon_ntax_100_df$sum_100 <- paste0(recon_ntax_100_df$sum_100, ")")
recon_ntax_200_df$sum_200 <- paste0(recon_ntax_200_df$sum_200, ")")
recon_ntax_300_df$sum_300 <- paste0(recon_ntax_300_df$sum_300, ")")
recon_ntax_400_df$sum_400 <- paste0(recon_ntax_400_df$sum_400, ")")
recon_ntax_500_df$sum_500 <- paste0(recon_ntax_500_df$sum_500, ")")

#Make summary table
ntax_recon_subsets <- as.data.frame(cbind(recon_ntax_100_df$sum_100, recon_ntax_200_df$sum_200, recon_ntax_300_df$sum_300, recon_ntax_400_df$sum_400, recon_ntax_500_df$sum_500))
rownames(ntax_recon_subsets) <- rownames(recon_ntax_100_df)
colnames(ntax_recon_subsets) <- c("100", "200", "300", "400", "500")

# #Just averages
# ntax_avg_subsets <- as.data.frame(cbind(recon_ntax_100_df$avg, recon_ntax_200_df$avg, recon_ntax_300_df$avg, recon_ntax_400_df$avg, recon_ntax_500_df$avg))
# rownames(ntax_avg_subsets) <- rownames(recon_ntax_100_df)
# colnames(ntax_avg_subsets) <- c("100", "200", "300", "400", "500")
# ntax_avg_subsets <- round(ntax_avg_subsets, 0)
# ntax_avg_subsets
# #Write just avg numbers of taxa
# write.csv(ntax_avg_subsets, "./Plots/2017_botany/Files_to_plot/Num_taxa_q2.csv", row.names = TRUE)


#Write table
write.csv(ntax_recon_subsets, "../Writing/Tables/Q2_num_taxa_random_sub_recon.csv", row.names = TRUE)


#####
#Question 4 - Table of number of taxa per community per subset

#Read files
Prop_100_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_100/", pattern=".csv", full.names=TRUE)
Prop_200_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_200/", pattern=".csv", full.names=TRUE)
Prop_300_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_300/", pattern=".csv", full.names=TRUE)
Prop_400_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_400/", pattern=".csv", full.names=TRUE)
Prop_500_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_500/", pattern=".csv", full.names=TRUE)

reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

pd_prop_100 <- lapply(Prop_100_files, reading_PD_files)
pd_prop_200 <- lapply(Prop_200_files, reading_PD_files)
pd_prop_300 <- lapply(Prop_300_files, reading_PD_files)
pd_prop_400 <- lapply(Prop_400_files, reading_PD_files)
pd_prop_500 <- lapply(Prop_500_files, reading_PD_files)

clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Get list of all numbers of taxa for all replicates
prop_ntax_100 <- list()
prop_ntax_200 <- list()
prop_ntax_300 <- list()
prop_ntax_400 <- list()
prop_ntax_500 <- list()

for (i in 1:length(pd_prop_100)) {
  ntax <- (pd_prop_100[[i]]$ntaxa) 
  prop_ntax_100[[i]] <- c(ntax)
}

for (i in 1:length(pd_prop_200)) {
  ntax <- (pd_prop_200[[i]]$ntaxa) 
  prop_ntax_200[[i]] <- c(ntax)
}

for (i in 1:length(pd_prop_300)) {
  ntax <- (pd_prop_300[[i]]$ntaxa) 
  prop_ntax_300[[i]] <- c(ntax)
}

for (i in 1:length(pd_prop_400)) {
  ntax <- (pd_prop_400[[i]]$ntaxa) 
  prop_ntax_400[[i]] <- c(ntax)
}

for (i in 1:length(pd_prop_500)) {
  ntax <- (pd_prop_500[[i]]$ntaxa) 
  prop_ntax_500[[i]] <- c(ntax)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Correct spelling
row.names(tcomm_designations)[10] <- "Successional.Hardwood.Forest"

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
prop_ntax_100_df <- as.data.frame(prop_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_prop_100))), row.names = row.names(tcomm_designations))
prop_ntax_200_df <- as.data.frame(prop_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_prop_200))), row.names = row.names(tcomm_designations))
prop_ntax_300_df <- as.data.frame(prop_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_prop_300))), row.names = row.names(tcomm_designations))
prop_ntax_400_df <- as.data.frame(prop_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_prop_400))), row.names = row.names(tcomm_designations))
prop_ntax_500_df <- as.data.frame(prop_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_prop_500))), row.names = row.names(tcomm_designations))

#Add column for averages
prop_ntax_100_df$avg <- rowMeans(prop_ntax_100_df, na.rm = TRUE)
prop_ntax_200_df$avg <- rowMeans(prop_ntax_200_df, na.rm = TRUE)
prop_ntax_300_df$avg <- rowMeans(prop_ntax_300_df, na.rm = TRUE)
prop_ntax_400_df$avg <- rowMeans(prop_ntax_400_df, na.rm = TRUE)
prop_ntax_500_df$avg <- rowMeans(prop_ntax_500_df, na.rm = TRUE)

#Add column for sd
prop_ntax_100_df$sd <- apply(prop_ntax_100_df[1:100],1, sd, na.rm = TRUE)
prop_ntax_200_df$sd <- apply(prop_ntax_200_df[1:100],1, sd, na.rm = TRUE)
prop_ntax_300_df$sd <- apply(prop_ntax_300_df[1:100],1, sd, na.rm = TRUE)
prop_ntax_400_df$sd <- apply(prop_ntax_400_df[1:100],1, sd, na.rm = TRUE)
prop_ntax_500_df$sd <- apply(prop_ntax_500_df[1:100],1, sd, na.rm = TRUE)

#Add column for min
prop_ntax_100_df$min <- apply(prop_ntax_100_df[1:100],1, min, na.rm = TRUE)
prop_ntax_200_df$min <- apply(prop_ntax_200_df[1:100],1, min, na.rm = TRUE)
prop_ntax_300_df$min <- apply(prop_ntax_300_df[1:100],1, min, na.rm = TRUE)
prop_ntax_400_df$min <- apply(prop_ntax_400_df[1:100],1, min, na.rm = TRUE)
prop_ntax_500_df$min <- apply(prop_ntax_500_df[1:100],1, min, na.rm = TRUE)

#Add column for max
prop_ntax_100_df$max <- apply(prop_ntax_100_df[1:100],1, max, na.rm = TRUE)
prop_ntax_200_df$max <- apply(prop_ntax_200_df[1:100],1, max, na.rm = TRUE)
prop_ntax_300_df$max <- apply(prop_ntax_300_df[1:100],1, max, na.rm = TRUE)
prop_ntax_400_df$max <- apply(prop_ntax_400_df[1:100],1, max, na.rm = TRUE)
prop_ntax_500_df$max <- apply(prop_ntax_500_df[1:100],1, max, na.rm = TRUE)

#Round sd
prop_ntax_100_df$sd <- round(prop_ntax_100_df$sd, 2)
prop_ntax_200_df$sd <- round(prop_ntax_200_df$sd, 2)
prop_ntax_300_df$sd <- round(prop_ntax_300_df$sd, 2)
prop_ntax_400_df$sd <- round(prop_ntax_400_df$sd, 2)
prop_ntax_500_df$sd <- round(prop_ntax_500_df$sd, 2)

#Make column of final avg +- sd
prop_ntax_100_df <- unite(prop_ntax_100_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
prop_ntax_200_df <- unite(prop_ntax_200_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
prop_ntax_300_df <- unite(prop_ntax_300_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
prop_ntax_400_df <- unite(prop_ntax_400_df, avg_sd, avg, sd, sep = "±", remove = FALSE)
prop_ntax_500_df <- unite(prop_ntax_500_df, avg_sd, avg, sd, sep = "±", remove = FALSE)

prop_ntax_100_df <- unite(prop_ntax_100_df, min_max, min, max, sep = "-", remove = FALSE)
prop_ntax_200_df <- unite(prop_ntax_200_df, min_max, min, max, sep = "-", remove = FALSE)
prop_ntax_300_df <- unite(prop_ntax_300_df, min_max, min, max, sep = "-", remove = FALSE)
prop_ntax_400_df <- unite(prop_ntax_400_df, min_max, min, max, sep = "-", remove = FALSE)
prop_ntax_500_df <- unite(prop_ntax_500_df, min_max, min, max, sep = "-", remove = FALSE)

prop_ntax_100_df <- unite(prop_ntax_100_df, sum_100, avg_sd, min_max, sep = " (", remove = FALSE)
prop_ntax_200_df <- unite(prop_ntax_200_df, sum_200, avg_sd, min_max, sep = " (", remove = FALSE)
prop_ntax_300_df <- unite(prop_ntax_300_df, sum_300, avg_sd, min_max, sep = " (", remove = FALSE)
prop_ntax_400_df <- unite(prop_ntax_400_df, sum_400, avg_sd, min_max, sep = " (", remove = FALSE)
prop_ntax_500_df <- unite(prop_ntax_500_df, sum_500, avg_sd, min_max, sep = " (", remove = FALSE)

prop_ntax_100_df$sum_100 <- paste0(prop_ntax_100_df$sum_100, ")")
prop_ntax_200_df$sum_200 <- paste0(prop_ntax_200_df$sum_200, ")")
prop_ntax_300_df$sum_300 <- paste0(prop_ntax_300_df$sum_300, ")")
prop_ntax_400_df$sum_400 <- paste0(prop_ntax_400_df$sum_400, ")")
prop_ntax_500_df$sum_500 <- paste0(prop_ntax_500_df$sum_500, ")")

#Make summary table
ntax_prop_subsets <- as.data.frame(cbind(prop_ntax_100_df$sum_100, prop_ntax_200_df$sum_200, prop_ntax_300_df$sum_300, prop_ntax_400_df$sum_400, prop_ntax_500_df$sum_500))
rownames(ntax_prop_subsets) <- rownames(prop_ntax_100_df)
colnames(ntax_prop_subsets) <- c("100", "200", "300", "400", "500")

# #Just averages
# ntax_avg_subsets <- as.data.frame(cbind(prop_ntax_100_df$avg, prop_ntax_200_df$avg, prop_ntax_300_df$avg, prop_ntax_400_df$avg, prop_ntax_500_df$avg))
# rownames(ntax_avg_subsets) <- rownames(prop_ntax_100_df)
# colnames(ntax_avg_subsets) <- c("100", "200", "300", "400", "500")
# ntax_avg_subsets <- round(ntax_avg_subsets, 0)
# ntax_avg_subsets
# #Write just avg numbers of taxa
# write.csv(ntax_avg_subsets, "./Plots/2017_botany/Files_to_plot/Num_taxa_q4.csv", row.names = TRUE)

#Write table
write.csv(ntax_prop_subsets, "../Writing/Tables/Q4_num_taxa_prop_sub.csv", row.names = TRUE)


######
#Question 5 - Table of number of taxa in woody/herb/total comparison

woody_pd <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_woody.csv", stringsAsFactors = FALSE)
herb_pd <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_herb.csv", stringsAsFactors = FALSE)
tree572_pd <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_tree572_clad.csv", stringsAsFactors = FALSE)

combined_ntaxa <- as.data.frame(cbind(woody_pd$X, woody_pd$ntaxa, herb_pd$ntaxa, tree572_pd$ntaxa))
colnames(combined_ntaxa) <- c("Community", "Woody", "Herb", "Overall")

#Write table
write.csv(combined_ntaxa, "../Writing/Tables/Q5_num_taxa_growth_form.csv", row.names = TRUE)

######
#Question 6 - Table of number of taxa in family comparison

#Make files of just correct PDs for families
PD_Asteraceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv", stringsAsFactors = FALSE)
PD_Cyperaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv", stringsAsFactors = FALSE)
PD_Ericaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fabaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fagaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv", stringsAsFactors = FALSE)
PD_Poaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv", stringsAsFactors = FALSE)
PD_rosids <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv", stringsAsFactors = FALSE)
PD_overall <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Correct spelling
PD_overall$X[10] <- "Successional.Hardwood.Forest"

#Set rownames to communities
rownames(PD_Asteraceae) <- PD_overall$X
rownames(PD_Cyperaceae) <- PD_overall$X
rownames(PD_Ericaceae) <- PD_overall$X
rownames(PD_Fabaceae) <- PD_overall$X
rownames(PD_Fagaceae) <- PD_overall$X
rownames(PD_Poaceae) <- PD_overall$X
rownames(PD_rosids) <- PD_overall$X
rownames(PD_overall) <- PD_overall$X

combined_ntaxa <- as.data.frame(cbind(PD_overall$X, PD_Asteraceae$ntaxa, PD_Cyperaceae$ntaxa, PD_Ericaceae$ntaxa, PD_Fabaceae$ntaxa, PD_Fagaceae$ntaxa, PD_Poaceae$ntaxa, PD_rosids$ntaxa, PD_overall$ntaxa))
colnames(combined_ntaxa) <- c("Community", "Asteraceae", "Cyperaceae", "Ericaceae", "Fabaceae", "Fagaceae", "Poaceae", "Rosids", "Overall")

#Write table
write.csv(combined_ntaxa, "../Writing/Tables/Q6_num_taxa_family.csv", row.names = TRUE)


######
#Question 2 - table of number of replicates per subset

#Read files
PD_recon_100_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_100/", pattern=".csv", full.names=TRUE)
PD_recon_200_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_200/", pattern=".csv", full.names=TRUE)
PD_recon_300_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_300/", pattern=".csv", full.names=TRUE)
PD_recon_400_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_400/", pattern=".csv", full.names=TRUE)
PD_recon_500_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_500/", pattern=".csv", full.names=TRUE)

#Get length of list
recon_100 <- length(PD_recon_100_files)
recon_200 <- length(PD_recon_200_files)
recon_300 <- length(PD_recon_300_files)
recon_400 <- length(PD_recon_400_files)
recon_500 <- length(PD_recon_500_files)

#Make table of lengths
replicate_numbers <- as.data.frame(cbind(subset = c("100", "200", "300", "400", "500"), reconstructed = c(recon_100, recon_200, recon_300, recon_400, recon_500), pruned = rep("100", 5)))

#Write table
write.csv(replicate_numbers, "../Writing/Tables/Q2_num_replicates_reconprune.csv", row.names = TRUE)
