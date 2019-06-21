#Calc differences for NTI for reconstructed and pruned phylograms for random subsets

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
NTI_diff_list_100 <- list()
NTI_diff_list_200 <- list()
NTI_diff_list_300 <- list()
NTI_diff_list_400 <- list()
NTI_diff_list_500 <- list()

#Calculate difference between recon and pruned for each replicate in subset
#Here: for mpd.obs.z = which is equivalent of -NRI which is often used in comm phylo
for (i in 1:length(pd_recon_100)) {
  NTI_diff_100 <- (pd_recon_100[[i]]$mntd.obs.z - pd_clad_100[[i]]$mntd.obs.z) 
  NTI_diff_list_100[[i]] <- c(NTI_diff_100)
}
for (i in 1:length(pd_recon_200)) {
  NTI_diff_200 <- (pd_recon_200[[i]]$mntd.obs.z - pd_clad_200[[i]]$mntd.obs.z)
  NTI_diff_list_200[[i]] <- c(NTI_diff_200)
}
for (i in 1:length(pd_recon_300)) {
  NTI_diff_300 <- (pd_recon_300[[i]]$mntd.obs.z - pd_clad_300[[i]]$mntd.obs.z)
  NTI_diff_list_300[[i]] <- c(NTI_diff_300)
}
for (i in 1:length(pd_recon_400)) {
  NTI_diff_400 <- (pd_recon_400[[i]]$mntd.obs.z - pd_clad_400[[i]]$mntd.obs.z) 
  NTI_diff_list_400[[i]] <- c(NTI_diff_400)
}
for (i in 1:length(pd_recon_500)) {
  NTI_diff_500 <- (pd_recon_500[[i]]$mntd.obs.z - pd_clad_500[[i]]$mntd.obs.z) 
  NTI_diff_list_500[[i]] <- c(NTI_diff_500)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
#Only PD is used (for other indices, go back to for loops and rerun with different $PD)
NTI_diff_100_df <- as.data.frame(NTI_diff_list_100, col.names = c(1:length(pd_recon_100)), row.names = row.names(tcomm_designations))
NTI_diff_200_df <- as.data.frame(NTI_diff_list_200, col.names = c(1:length(pd_recon_200)), row.names = row.names(tcomm_designations))
NTI_diff_300_df <- as.data.frame(NTI_diff_list_300, col.names = c(1:length(pd_recon_300)), row.names = row.names(tcomm_designations))
NTI_diff_400_df <- as.data.frame(NTI_diff_list_400, col.names = c(1:length(pd_recon_400)), row.names = row.names(tcomm_designations))
NTI_diff_500_df <- as.data.frame(NTI_diff_list_500, col.names = c(1:length(pd_recon_500)), row.names = row.names(tcomm_designations))

#Add subset to name of column replicates
colnames(NTI_diff_100_df) <- paste0(colnames(NTI_diff_100_df), "_100")
colnames(NTI_diff_200_df) <- paste0(colnames(NTI_diff_200_df), "_200")
colnames(NTI_diff_300_df) <- paste0(colnames(NTI_diff_300_df), "_300")
colnames(NTI_diff_400_df) <- paste0(colnames(NTI_diff_400_df), "_400")
colnames(NTI_diff_500_df) <- paste0(colnames(NTI_diff_500_df), "_500")

#Transpose dataframes so replicates are rows and communities are columns
NTI_diff_100_df_tr <- t(NTI_diff_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_diff_200_df_tr <- t(NTI_diff_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_diff_300_df_tr <- t(NTI_diff_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_diff_400_df_tr <- t(NTI_diff_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_diff_500_df_tr <- t(NTI_diff_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


#Add column to each dataframe including subset type
NTI_diff_100_df_tr <- cbind(NTI_diff_100_df_tr, "subset" = "100")
NTI_diff_200_df_tr <- cbind(NTI_diff_200_df_tr, "subset" = "200")
NTI_diff_300_df_tr <- cbind(NTI_diff_300_df_tr, "subset" = "300")
NTI_diff_400_df_tr <- cbind(NTI_diff_400_df_tr, "subset" = "400")
NTI_diff_500_df_tr <- cbind(NTI_diff_500_df_tr, "subset" = "500")

#Make dataframe with all NRI differences
NTI_all_diff <- data.frame(NTI_diff_100_df_tr)
NTI_all_diff <- rbind(NTI_all_diff, data.frame(NTI_diff_200_df_tr))
NTI_all_diff <- rbind(NTI_all_diff, data.frame(NTI_diff_300_df_tr))
NTI_all_diff <- rbind(NTI_all_diff, data.frame(NTI_diff_400_df_tr))
NTI_all_diff <- rbind(NTI_all_diff, data.frame(NTI_diff_500_df_tr))

#Write file
write.csv(NTI_all_diff, "./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NTI.csv")

