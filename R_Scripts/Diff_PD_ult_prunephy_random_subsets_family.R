#Calculating difference between ultrametric and pruned phylograms for random subsets

#Read PD files
#Read PD files - Clad and Ult

PD_clad_100_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_100/", pattern=".csv", full.names=TRUE)
PD_clad_200_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_200/", pattern=".csv", full.names=TRUE)
PD_clad_300_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_300/", pattern=".csv", full.names=TRUE)
PD_clad_400_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_400/", pattern=".csv", full.names=TRUE)
PD_clad_500_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_500/", pattern=".csv", full.names=TRUE)

PD_ult_100_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_100/", pattern=".csv", full.names=TRUE)
PD_ult_200_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_200/", pattern=".csv", full.names=TRUE)
PD_ult_300_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_300/", pattern=".csv", full.names=TRUE)
PD_ult_400_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_400/", pattern=".csv", full.names=TRUE)
PD_ult_500_list <- list.files("./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_500/", pattern=".csv", full.names=TRUE)

#Write function to read PD files
reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

#Read the trees
PD_clad_100 <- lapply(PD_clad_100_list, reading_PD_files)
PD_clad_200 <- lapply(PD_clad_200_list, reading_PD_files)
PD_clad_300 <- lapply(PD_clad_300_list, reading_PD_files)
PD_clad_400 <- lapply(PD_clad_400_list, reading_PD_files)
PD_clad_500 <- lapply(PD_clad_500_list, reading_PD_files)

PD_ult_100 <- lapply(PD_ult_100_list, reading_PD_files)
PD_ult_200 <- lapply(PD_ult_200_list, reading_PD_files)
PD_ult_300 <- lapply(PD_ult_300_list, reading_PD_files)
PD_ult_400 <- lapply(PD_ult_400_list, reading_PD_files)
PD_ult_500 <- lapply(PD_ult_500_list, reading_PD_files)

PD_clad_Ast <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_Cyp <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_Eric <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_Fab <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_Fag <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_Po <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv", stringsAsFactors = FALSE)
PD_clad_rosid <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv", stringsAsFactors = FALSE)
PD_clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

PD_ult_Ast <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Asteraceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_Cyp <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Cyperaceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_Eric <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Ericaceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_Fab <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fabaceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_Fag <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fagaceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_Po <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Poaceae_ult.csv", stringsAsFactors = FALSE)
PD_ult_rosid <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_rosid_ult.csv", stringsAsFactors = FALSE)
PD_ult_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_tree572_ult.csv", stringsAsFactors = FALSE)

#Create empty lists for difference calculations between ult and clad
PD_UC_list_100 <- list()
PD_UC_list_200 <- list()
PD_UC_list_300 <- list()
PD_UC_list_400 <- list()
PD_UC_list_500 <- list()

#Calculate difference between recon and pruned for each replicate in subset
#Here: for PD
for (i in 1:length(PD_ult_100)) {
  PD_diff_100 <- (PD_ult_100[[i]]$pd.obs.z - PD_clad_100[[i]]$pd.obs.z) 
  PD_UC_list_100[[i]] <- c(PD_diff_100)
}
for (i in 1:length(PD_ult_200)) {
  PD_diff_200 <- (PD_ult_200[[i]]$pd.obs.z - PD_clad_200[[i]]$pd.obs.z)
  PD_UC_list_200[[i]] <- c(PD_diff_200)
}
for (i in 1:length(PD_ult_300)) {
  PD_diff_300 <- (PD_ult_300[[i]]$pd.obs.z - PD_clad_300[[i]]$pd.obs.z)
  PD_UC_list_300[[i]] <- c(PD_diff_300)
}
for (i in 1:length(PD_ult_400)) {
  PD_diff_400 <- (PD_ult_400[[i]]$pd.obs.z - PD_clad_400[[i]]$pd.obs.z) 
  PD_UC_list_400[[i]] <- c(PD_diff_400)
}
for (i in 1:length(PD_ult_500)) {
  PD_diff_500 <- (PD_ult_500[[i]]$pd.obs.z - PD_clad_500[[i]]$pd.obs.z) 
  PD_UC_list_500[[i]] <- c(PD_diff_500)
}


cladult_PD_diff_572 <- (PD_ult_572$pd.obs.z - PD_clad_572$pd.obs.z)
cladult_PD_diff_Aster <- (PD_ult_Ast$pd.obs.z - PD_clad_Ast$pd.obs.z)
cladult_PD_diff_Cyper <- (PD_ult_Cyp$pd.obs.z - PD_clad_Cyp$pd.obs.z)
cladult_PD_diff_Eric <- (PD_ult_Eric$pd.obs.z - PD_clad_Eric$pd.obs.z)
cladult_PD_diff_Fab <- (PD_ult_Fab$pd.obs.z - PD_clad_Fab$pd.obs.z)
cladult_PD_diff_Fag <- (PD_ult_Fag$pd.obs.z - PD_clad_Fag$pd.obs.z)
cladult_PD_diff_Po <- (PD_ult_Po$pd.obs.z - PD_clad_Po$pd.obs.z)
cladult_PD_diff_rosid <- (PD_ult_rosid$pd.obs.z - PD_clad_rosid$pd.obs.z)

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)


#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
#Only PD is used (for other indices, go back to for loops and rerun with different $PD)
PD_UC_100_df <- as.data.frame(PD_UC_list_100, col.names = c(1:length(PD_ult_100)), row.names = row.names(tcomm_designations))
PD_UC_200_df <- as.data.frame(PD_UC_list_200, col.names = c(1:length(PD_ult_200)), row.names = row.names(tcomm_designations))
PD_UC_300_df <- as.data.frame(PD_UC_list_300, col.names = c(1:length(PD_ult_300)), row.names = row.names(tcomm_designations))
PD_UC_400_df <- as.data.frame(PD_UC_list_400, col.names = c(1:length(PD_ult_400)), row.names = row.names(tcomm_designations))
PD_UC_500_df <- as.data.frame(PD_UC_list_500, col.names = c(1:length(PD_ult_500)), row.names = row.names(tcomm_designations))

cladult_PD_diff_rosid_df <- as.data.frame(cladult_PD_diff_rosid, col.names = "Rosids", row.names = row.names(tcomm_designations))
cladult_PD_diff_572_df <- as.data.frame(cladult_PD_diff_572, col.names = "All_taxa", row.names = row.names(tcomm_designations))
cladult_PD_diff_Aster_df <- as.data.frame(cladult_PD_diff_Aster, col.names = "Asteraceae", row.names = row.names(tcomm_designations))
cladult_PD_diff_Cyper_df <- as.data.frame(cladult_PD_diff_Cyper, col.names = "Cyperaceae", row.names = row.names(tcomm_designations))
cladult_PD_diff_Eric_df <- as.data.frame(cladult_PD_diff_Eric, col.names = "Ericaceae", row.names = row.names(tcomm_designations))
cladult_PD_diff_Fab_df <- as.data.frame(cladult_PD_diff_Fab, col.names = "Fabaceae", row.names = row.names(tcomm_designations))
cladult_PD_diff_Fag_df <- as.data.frame(cladult_PD_diff_Fag, col.names = "Fagaceae", row.names = row.names(tcomm_designations))
cladult_PD_diff_Po_df <- as.data.frame(cladult_PD_diff_Po, col.names = "Poaceae", row.names = row.names(tcomm_designations))

#Add subset to name of column replicates
colnames(PD_UC_100_df) <- paste0(colnames(PD_UC_100_df), "_100")
colnames(PD_UC_200_df) <- paste0(colnames(PD_UC_200_df), "_200")
colnames(PD_UC_300_df) <- paste0(colnames(PD_UC_300_df), "_300")
colnames(PD_UC_400_df) <- paste0(colnames(PD_UC_400_df), "_400")
colnames(PD_UC_500_df) <- paste0(colnames(PD_UC_500_df), "_500")

#Transpose dataframes so replicates are rows and communities are columns
PD_UC_100_df_tr <- t(PD_UC_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_UC_200_df_tr <- t(PD_UC_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_UC_300_df_tr <- t(PD_UC_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_UC_400_df_tr <- t(PD_UC_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_UC_500_df_tr <- t(PD_UC_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

cladult_PD_diff_rosid_df_tr <- t(cladult_PD_diff_rosid_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Aster_df_tr <- t(cladult_PD_diff_Aster_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Cyper_df_tr <- t(cladult_PD_diff_Cyper_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Eric_df_tr <- t(cladult_PD_diff_Eric_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Fab_df_tr <- t(cladult_PD_diff_Fab_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Fag_df_tr <- t(cladult_PD_diff_Fag_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_Po_df_tr <- t(cladult_PD_diff_Po_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cladult_PD_diff_572_df_tr <- t(cladult_PD_diff_572_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Add column to each dataframe including subset type
PD_UC_100_df_tr$subset <- "100"
PD_UC_200_df_tr$subset <- "200"
PD_UC_300_df_tr$subset <- "300"
PD_UC_400_df_tr$subset <- "400"
PD_UC_500_df_tr$subset <- "500"

PD_UC_100_df_tr$type <- "ultprune"
PD_UC_200_df_tr$type <- "ultprune"
PD_UC_300_df_tr$type <- "ultprune"
PD_UC_400_df_tr$type <- "ultprune"
PD_UC_500_df_tr$type <- "ultprune"

cladult_PD_diff_rosid_df_tr <- cbind(cladult_PD_diff_rosid_df_tr, "family" = "Rosids")
cladult_PD_diff_Aster_df_tr <- cbind(cladult_PD_diff_Aster_df_tr, "family" = "Asteraceae")
cladult_PD_diff_Cyper_df_tr <- cbind(cladult_PD_diff_Cyper_df_tr, "family" = "Cyperaceae")
cladult_PD_diff_Eric_df_tr <- cbind(cladult_PD_diff_Eric_df_tr, "family" = "Ericaceae")
cladult_PD_diff_Fab_df_tr <- cbind(cladult_PD_diff_Fab_df_tr, "family" = "Fabaceae")
cladult_PD_diff_Fag_df_tr <- cbind(cladult_PD_diff_Fag_df_tr, "family" = "Fagaceae")
cladult_PD_diff_Po_df_tr <- cbind(cladult_PD_diff_Po_df_tr, "family" = "Poaceae")
cladult_PD_diff_572_df_tr <- cbind(cladult_PD_diff_572_df_tr, "family" = "All_Taxa")


#Make one dataframe for all subsets
PD_UC_all <- data.frame(PD_UC_100_df_tr, stringsAsFactors = FALSE)
PD_UC_all <- rbind(PD_UC_all, data.frame(PD_UC_200_df_tr, stringsAsFactors = FALSE))
PD_UC_all <- rbind(PD_UC_all, data.frame(PD_UC_300_df_tr, stringsAsFactors = FALSE))
PD_UC_all <- rbind(PD_UC_all, data.frame(PD_UC_400_df_tr, stringsAsFactors = FALSE))
PD_UC_all <- rbind(PD_UC_all, data.frame(PD_UC_500_df_tr, stringsAsFactors = FALSE))

cladult_PD_family_diff <- data.frame(cladult_PD_diff_rosid_df_tr)
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Aster_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Cyper_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Eric_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Fab_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Fag_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_Po_df_tr))
cladult_PD_family_diff <- rbind(cladult_PD_family_diff, data.frame(cladult_PD_diff_572_df_tr))


#Write dataframe to file
write.csv(PD_UC_all, "./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_random_subsets_PD.csv")
write.csv(cladult_PD_family_diff, "./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_family_subsets_PD.csv")
