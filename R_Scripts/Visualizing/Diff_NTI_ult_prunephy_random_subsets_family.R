#Calc difference in NTI for ultrametric and pruned phylograms for random subsets and families

#Read pruned cladogram tree files
PD_clad_100_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_100/", pattern=".csv", full.names=TRUE)
PD_clad_200_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_200/", pattern=".csv", full.names=TRUE)
PD_clad_300_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_300/", pattern=".csv", full.names=TRUE)
PD_clad_400_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_400/", pattern=".csv", full.names=TRUE)
PD_clad_500_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_500/", pattern=".csv", full.names=TRUE)

PD_ult_100_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_100/", pattern=".csv", full.names=TRUE)
PD_ult_200_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_200/", pattern=".csv", full.names=TRUE)
PD_ult_300_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_300/", pattern=".csv", full.names=TRUE)
PD_ult_400_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_400/", pattern=".csv", full.names=TRUE)
PD_ult_500_files <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_500/", pattern=".csv", full.names=TRUE)

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

pd_ult_100 <- lapply(PD_ult_100_files, reading_PD_files)
pd_ult_200 <- lapply(PD_ult_200_files, reading_PD_files)
pd_ult_300 <- lapply(PD_ult_300_files, reading_PD_files)
pd_ult_400 <- lapply(PD_ult_400_files, reading_PD_files)
pd_ult_500 <- lapply(PD_ult_500_files, reading_PD_files)

ult_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_tree572_ult.csv", stringsAsFactors = FALSE)
ult_rosid <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_rosid_ult.csv", stringsAsFactors = FALSE)
ult_Aster <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Asteraceae_ult.csv", stringsAsFactors = FALSE)
ult_Eric <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Ericaceae_ult.csv", stringsAsFactors = FALSE)
ult_Cyper <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Cyperaceae_ult.csv", stringsAsFactors = FALSE)
ult_Fab <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fabaceae_ult.csv", stringsAsFactors = FALSE)
ult_Fag <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fagaceae_ult.csv", stringsAsFactors = FALSE)
ult_Po <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Poaceae_ult.csv", stringsAsFactors = FALSE)

clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)
clad_rosid <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv", stringsAsFactors = FALSE)
clad_Aster <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv", stringsAsFactors = FALSE)
clad_Eric <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv", stringsAsFactors = FALSE)
clad_Cyper <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv", stringsAsFactors = FALSE)
clad_Fab <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv", stringsAsFactors = FALSE)
clad_Fag <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv", stringsAsFactors = FALSE)
clad_Po <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv", stringsAsFactors = FALSE)

#Create empty lists for difference calculations between recon and pruned
cladult_NTI_diff_list_100 <- list()
cladult_NTI_diff_list_200 <- list()
cladult_NTI_diff_list_300 <- list()
cladult_NTI_diff_list_400 <- list()
cladult_NTI_diff_list_500 <- list()

cladult_NTI_diff_572 <- (ult_572$mntd.obs.z - clad_572$mntd.obs.z)
cladult_NTI_diff_Aster <- (ult_Aster$mntd.obs.z - clad_Aster$mntd.obs.z)
cladult_NTI_diff_Cyper <- (ult_Cyper$mntd.obs.z - clad_Cyper$mntd.obs.z)
cladult_NTI_diff_Eric <- (ult_Eric$mntd.obs.z - clad_Eric$mntd.obs.z)
cladult_NTI_diff_Fab <- (ult_Fab$mntd.obs.z - clad_Fab$mntd.obs.z)
cladult_NTI_diff_Fag <- (ult_Fag$mntd.obs.z - clad_Fag$mntd.obs.z)
cladult_NTI_diff_Po <- (ult_Po$mntd.obs.z - clad_Po$mntd.obs.z)
cladult_NTI_diff_rosid <- (ult_rosid$mntd.obs.z - clad_rosid$mntd.obs.z)

#Calculate difference between recon and pruned for each replicate in subset
#Here: for PD
for (i in 1:length(pd_ult_100)) {
  NTI_diff_100 <- (pd_ult_100[[i]]$mntd.obs.z - pd_clad_100[[i]]$mntd.obs.z) 
  cladult_NTI_diff_list_100[[i]] <- c(NTI_diff_100)
}
for (i in 1:length(pd_ult_200)) {
  NTI_diff_200 <- (pd_ult_200[[i]]$mntd.obs.z - pd_clad_200[[i]]$mntd.obs.z)
  cladult_NTI_diff_list_200[[i]] <- c(NTI_diff_200)
}
for (i in 1:length(pd_ult_300)) {
  NTI_diff_300 <- (pd_ult_300[[i]]$mntd.obs.z - pd_clad_300[[i]]$mntd.obs.z)
  cladult_NTI_diff_list_300[[i]] <- c(NTI_diff_300)
}
for (i in 1:length(pd_ult_400)) {
  NTI_diff_400 <- (pd_ult_400[[i]]$mntd.obs.z - pd_clad_400[[i]]$mntd.obs.z) 
  cladult_NTI_diff_list_400[[i]] <- c(NTI_diff_400)
}
for (i in 1:length(pd_ult_500)) {
  NTI_diff_500 <- (pd_ult_500[[i]]$mntd.obs.z - pd_clad_500[[i]]$mntd.obs.z) 
  cladult_NTI_diff_list_500[[i]] <- c(NTI_diff_500)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
#Only PD is used (for other indices, go back to for loops and rerun with different $PD)
cladult_NTI_diff_100_df <- as.data.frame(cladult_NTI_diff_list_100, col.names = c(1:length(pd_ult_100)), row.names = row.names(tcomm_designations))
cladult_NTI_diff_200_df <- as.data.frame(cladult_NTI_diff_list_200, col.names = c(1:length(pd_ult_200)), row.names = row.names(tcomm_designations))
cladult_NTI_diff_300_df <- as.data.frame(cladult_NTI_diff_list_300, col.names = c(1:length(pd_ult_300)), row.names = row.names(tcomm_designations))
cladult_NTI_diff_400_df <- as.data.frame(cladult_NTI_diff_list_400, col.names = c(1:length(pd_ult_400)), row.names = row.names(tcomm_designations))
cladult_NTI_diff_500_df <- as.data.frame(cladult_NTI_diff_list_500, col.names = c(1:length(pd_ult_500)), row.names = row.names(tcomm_designations))

cladult_NTI_diff_rosid_df <- as.data.frame(cladult_NTI_diff_rosid, col.names = "Rosids", row.names = row.names(tcomm_designations))
cladult_NTI_diff_572_df <- as.data.frame(cladult_NTI_diff_572, col.names = "All_taxa", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Aster_df <- as.data.frame(cladult_NTI_diff_Aster, col.names = "Asteraceae", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Cyper_df <- as.data.frame(cladult_NTI_diff_Cyper, col.names = "Cyperaceae", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Eric_df <- as.data.frame(cladult_NTI_diff_Eric, col.names = "Ericaceae", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Fab_df <- as.data.frame(cladult_NTI_diff_Fab, col.names = "Fabaceae", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Fag_df <- as.data.frame(cladult_NTI_diff_Fag, col.names = "Fagaceae", row.names = row.names(tcomm_designations))
cladult_NTI_diff_Po_df <- as.data.frame(cladult_NTI_diff_Po, col.names = "Poaceae", row.names = row.names(tcomm_designations))


#Add subset to name of column replicates
colnames(cladult_NTI_diff_100_df) <- paste0(colnames(cladult_NTI_diff_100_df), "_100")
colnames(cladult_NTI_diff_200_df) <- paste0(colnames(cladult_NTI_diff_200_df), "_200")
colnames(cladult_NTI_diff_300_df) <- paste0(colnames(cladult_NTI_diff_300_df), "_300")
colnames(cladult_NTI_diff_400_df) <- paste0(colnames(cladult_NTI_diff_400_df), "_400")
colnames(cladult_NTI_diff_500_df) <- paste0(colnames(cladult_NTI_diff_500_df), "_500")

#Transpose dataframes so replicates are rows and communities are columns
cladult_NTI_diff_100_df_tr <- t(cladult_NTI_diff_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_NTI_diff_200_df_tr <- t(cladult_NTI_diff_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_NTI_diff_300_df_tr <- t(cladult_NTI_diff_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_NTI_diff_400_df_tr <- t(cladult_NTI_diff_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_NTI_diff_500_df_tr <- t(cladult_NTI_diff_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_NTI_diff_rosid_df_tr <- t(cladult_NTI_diff_rosid_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Aster_df_tr <- t(cladult_NTI_diff_Aster_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Cyper_df_tr <- t(cladult_NTI_diff_Cyper_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Eric_df_tr <- t(cladult_NTI_diff_Eric_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Fab_df_tr <- t(cladult_NTI_diff_Fab_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Fag_df_tr <- t(cladult_NTI_diff_Fag_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_Po_df_tr <- t(cladult_NTI_diff_Po_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_NTI_diff_572_df_tr <- t(cladult_NTI_diff_572_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Add column to each dataframe including subset type
cladult_NTI_diff_100_df_tr <- cbind(cladult_NTI_diff_100_df_tr, "subset" = "100")
cladult_NTI_diff_200_df_tr <- cbind(cladult_NTI_diff_200_df_tr, "subset" = "200")
cladult_NTI_diff_300_df_tr <- cbind(cladult_NTI_diff_300_df_tr, "subset" = "300")
cladult_NTI_diff_400_df_tr <- cbind(cladult_NTI_diff_400_df_tr, "subset" = "400")
cladult_NTI_diff_500_df_tr <- cbind(cladult_NTI_diff_500_df_tr, "subset" = "500")

cladult_NTI_diff_rosid_df_tr <- cbind(cladult_NTI_diff_rosid_df_tr, "family" = "Rosids")
cladult_NTI_diff_Aster_df_tr <- cbind(cladult_NTI_diff_Aster_df_tr, "family" = "Asteraceae")
cladult_NTI_diff_Cyper_df_tr <- cbind(cladult_NTI_diff_Cyper_df_tr, "family" = "Cyperaceae")
cladult_NTI_diff_Eric_df_tr <- cbind(cladult_NTI_diff_Eric_df_tr, "family" = "Ericaceae")
cladult_NTI_diff_Fab_df_tr <- cbind(cladult_NTI_diff_Fab_df_tr, "family" = "Fabaceae")
cladult_NTI_diff_Fag_df_tr <- cbind(cladult_NTI_diff_Fag_df_tr, "family" = "Fagaceae")
cladult_NTI_diff_Po_df_tr <- cbind(cladult_NTI_diff_Po_df_tr, "family" = "Poaceae")
cladult_NTI_diff_572_df_tr <- cbind(cladult_NTI_diff_572_df_tr, "family" = "All_Taxa")

#Make dataframe with all NRI differences
cladult_NTI_all_diff <- data.frame(cladult_NTI_diff_100_df_tr)
cladult_NTI_all_diff <- rbind(cladult_NTI_all_diff, data.frame(cladult_NTI_diff_200_df_tr))
cladult_NTI_all_diff <- rbind(cladult_NTI_all_diff, data.frame(cladult_NTI_diff_300_df_tr))
cladult_NTI_all_diff <- rbind(cladult_NTI_all_diff, data.frame(cladult_NTI_diff_400_df_tr))
cladult_NTI_all_diff <- rbind(cladult_NTI_all_diff, data.frame(cladult_NTI_diff_500_df_tr))

cladult_NTI_family_diff <- data.frame(cladult_NTI_diff_rosid_df_tr)
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Aster_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Cyper_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Eric_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Fab_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Fag_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_Po_df_tr))
cladult_NTI_family_diff <- rbind(cladult_NTI_family_diff, data.frame(cladult_NTI_diff_572_df_tr))

#Write files
write.csv(cladult_NTI_all_diff, "./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_random_subsets_NTI.csv")
write.csv(cladult_NTI_family_diff, "./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_family_subsets_NTI.csv")
