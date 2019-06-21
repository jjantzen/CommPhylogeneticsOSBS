#Get difference between reconstructed and pruned subsets for phylograms
###############
#See Comparing_PDs etc for actually running comparisons
#Read pruned cladogram tree files
PD_clad_100_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_100/", pattern=".csv", full.names=TRUE)
PD_clad_200_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_200/", pattern=".csv", full.names=TRUE)
PD_clad_300_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_300/", pattern=".csv", full.names=TRUE)
PD_clad_400_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_400/", pattern=".csv", full.names=TRUE)
PD_clad_500_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_500/", pattern=".csv", full.names=TRUE)

PD_recon_100_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_100/", pattern=".csv", full.names=TRUE)
PD_recon_200_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_200/", pattern=".csv", full.names=TRUE)
PD_recon_300_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_300/", pattern=".csv", full.names=TRUE)
PD_recon_400_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_400/", pattern=".csv", full.names=TRUE)
PD_recon_500_files <- list.files("./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_500/", pattern=".csv", full.names=TRUE)

#Write function to read PD files
reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

#Read the files
pd_clad_100 <- lapply(PD_clad_100_files, reading_PD_files)
pd_clad_200 <- lapply(PD_clad_200_files, reading_PD_files)
pd_clad_300 <- lapply(PD_clad_300_files, reading_PD_files)
pd_clad_400 <- lapply(PD_clad_400_files, reading_PD_files)
pd_clad_500 <- lapply(PD_clad_500_files, reading_PD_files)

pd_recon_100 <- lapply(PD_recon_100_files, reading_PD_files)
pd_recon_200 <- lapply(PD_recon_200_files, reading_PD_files)
pd_recon_300 <- lapply(PD_recon_300_files, reading_PD_files)
pd_recon_400 <- lapply(PD_recon_400_files, reading_PD_files)
pd_recon_500 <- lapply(PD_recon_500_files, reading_PD_files)

#Create empty lists for difference calculations between recon and pruned
PD_diff_list_100 <- list()
PD_diff_list_200 <- list()
PD_diff_list_300 <- list()
PD_diff_list_400 <- list()
PD_diff_list_500 <- list()


#Calculate difference between recon and pruned for each replicate in subset
#Here: for PD
for (i in 1:length(pd_recon_100)) {
  PD_diff_100 <- (pd_recon_100[[i]]$pd.obs.z - pd_clad_100[[i]]$pd.obs.z) 
  PD_diff_list_100[[i]] <- c(PD_diff_100)
}
for (i in 1:length(pd_recon_200)) {
  PD_diff_200 <- (pd_recon_200[[i]]$pd.obs.z - pd_clad_200[[i]]$pd.obs.z)
  PD_diff_list_200[[i]] <- c(PD_diff_200)
}
for (i in 1:length(pd_recon_300)) {
  PD_diff_300 <- (pd_recon_300[[i]]$pd.obs.z - pd_clad_300[[i]]$pd.obs.z)
  PD_diff_list_300[[i]] <- c(PD_diff_300)
}
for (i in 1:length(pd_recon_400)) {
  PD_diff_400 <- (pd_recon_400[[i]]$pd.obs.z - pd_clad_400[[i]]$pd.obs.z) 
  PD_diff_list_400[[i]] <- c(PD_diff_400)
}
for (i in 1:length(pd_recon_500)) {
  PD_diff_500 <- (pd_recon_500[[i]]$pd.obs.z - pd_clad_500[[i]]$pd.obs.z) 
  PD_diff_list_500[[i]] <- c(PD_diff_500)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)


#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
#Only PD is used (for other indices, go back to for loops and rerun with different $PD)
PD_diff_100_df <- as.data.frame(PD_diff_list_100, col.names = c(1:length(pd_recon_100)), row.names = row.names(tcomm_designations))
PD_diff_200_df <- as.data.frame(PD_diff_list_200, col.names = c(1:length(pd_recon_200)), row.names = row.names(tcomm_designations))
PD_diff_300_df <- as.data.frame(PD_diff_list_300, col.names = c(1:length(pd_recon_300)), row.names = row.names(tcomm_designations))
PD_diff_400_df <- as.data.frame(PD_diff_list_400, col.names = c(1:length(pd_recon_400)), row.names = row.names(tcomm_designations))
PD_diff_500_df <- as.data.frame(PD_diff_list_500, col.names = c(1:length(pd_recon_500)), row.names = row.names(tcomm_designations))

#Add subset to name of column replicates
colnames(PD_diff_100_df) <- paste0(colnames(PD_diff_100_df), "_100")
colnames(PD_diff_200_df) <- paste0(colnames(PD_diff_200_df), "_200")
colnames(PD_diff_300_df) <- paste0(colnames(PD_diff_300_df), "_300")
colnames(PD_diff_400_df) <- paste0(colnames(PD_diff_400_df), "_400")
colnames(PD_diff_500_df) <- paste0(colnames(PD_diff_500_df), "_500")

#Transpose dataframes so replicates are rows and communities are columns
PD_diff_100_df_tr <- t(PD_diff_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_diff_200_df_tr <- t(PD_diff_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_diff_300_df_tr <- t(PD_diff_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_diff_400_df_tr <- t(PD_diff_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_diff_500_df_tr <- t(PD_diff_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Add column to each dataframe including subset type
PD_diff_100_df_tr$subset <- "100"
PD_diff_200_df_tr$subset <- "200"
PD_diff_300_df_tr$subset <- "300"
PD_diff_400_df_tr$subset <- "400"
PD_diff_500_df_tr$subset <- "500"


PD_diff_100_df_tr$type <- "reconprune"
PD_diff_200_df_tr$type <- "reconprune"
PD_diff_300_df_tr$type <- "reconprune"
PD_diff_400_df_tr$type <- "reconprune"
PD_diff_500_df_tr$type <- "reconprune"

#Make dataframe of all subsets 
PD_diff_all <- data.frame(PD_diff_100_df_tr, stringsAsFactors = FALSE)
PD_diff_all <- rbind(PD_diff_all, data.frame(PD_diff_200_df_tr, stringsAsFactors = FALSE))
PD_diff_all <- rbind(PD_diff_all, data.frame(PD_diff_300_df_tr, stringsAsFactors = FALSE))
PD_diff_all <- rbind(PD_diff_all, data.frame(PD_diff_400_df_tr, stringsAsFactors = FALSE))
PD_diff_all <- rbind(PD_diff_all, data.frame(PD_diff_500_df_tr, stringsAsFactors = FALSE))

#Write difference dataset 
write.csv(PD_diff_all, "./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_PD.csv")

