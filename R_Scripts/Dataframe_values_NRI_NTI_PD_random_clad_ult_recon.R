#Plot actual values of NTI and NRI to look for changes in direction
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
pd_prune_100 <- lapply(PD_clad_100_files, reading_PD_files)
pd_prune_200 <- lapply(PD_clad_200_files, reading_PD_files)
pd_prune_300 <- lapply(PD_clad_300_files, reading_PD_files)
pd_prune_400 <- lapply(PD_clad_400_files, reading_PD_files)
pd_prune_500 <- lapply(PD_clad_500_files, reading_PD_files)

pd_recon_100 <- lapply(PD_recon_100_files, reading_PD_files)
pd_recon_200 <- lapply(PD_recon_200_files, reading_PD_files)
pd_recon_300 <- lapply(PD_recon_300_files, reading_PD_files)
pd_recon_400 <- lapply(PD_recon_400_files, reading_PD_files)
pd_recon_500 <- lapply(PD_recon_500_files, reading_PD_files)


pd_ult_100 <- lapply(PD_ult_100_files, reading_PD_files)
pd_ult_200 <- lapply(PD_ult_200_files, reading_PD_files)
pd_ult_300 <- lapply(PD_ult_300_files, reading_PD_files)
pd_ult_400 <- lapply(PD_ult_400_files, reading_PD_files)
pd_ult_500 <- lapply(PD_ult_500_files, reading_PD_files)



#Get dataframe of 
#NTI recon
NTI_recon_100 <- list()
for (i in 1:length(pd_recon_100)) {
  NTI <- pd_recon_100[[i]]$mntd.obs.z
  pval <- pd_recon_100[[i]]$mntd.obs.p
  SR <- pd_recon_100[[i]]$SR
  NTI_recon_100[[i]] <- cbind(NTI, pval, SR)
}

NTI_recon_200 <- list()
for (i in 1:length(pd_recon_200)) {
  NTI <- pd_recon_200[[i]]$mntd.obs.z 
  pval <- pd_recon_200[[i]]$mntd.obs.p
  SR <- pd_recon_200[[i]]$SR
  NTI_recon_200[[i]] <- cbind(NTI, pval, SR)
}

NTI_recon_300 <- list()
for (i in 1:length(pd_recon_300)) {
  NTI <- pd_recon_300[[i]]$mntd.obs.z 
  pval <- pd_recon_300[[i]]$mntd.obs.p 
  SR <- pd_recon_300[[i]]$SR
  NTI_recon_300[[i]] <- cbind(NTI, pval, SR)
}

NTI_recon_400 <- list()
for (i in 1:length(pd_recon_400)) {
  NTI <- pd_recon_400[[i]]$mntd.obs.z 
  pval <- pd_recon_400[[i]]$mntd.obs.p
  SR <- pd_recon_400[[i]]$SR 
  NTI_recon_400[[i]] <- cbind(NTI, pval, SR)
}

NTI_recon_500 <- list()
for (i in 1:length(pd_recon_500)) {
  NTI <- pd_recon_500[[i]]$mntd.obs.z 
  pval <- pd_recon_500[[i]]$mntd.obs.p
  SR <- pd_recon_500[[i]]$SR 
  NTI_recon_500[[i]] <- cbind(NTI, pval, SR)
}

#NTI prune
NTI_prune_100 <- list()
for (i in 1:length(pd_prune_100)) {
  NTI <- pd_prune_100[[i]]$mntd.obs.z 
  pval <- pd_prune_100[[i]]$mntd.obs.p
  SR <- pd_prune_100[[i]]$SR
  NTI_prune_100[[i]] <- cbind(NTI, pval, SR)
}

NTI_prune_200 <- list()
for (i in 1:length(pd_prune_200)) {
  NTI <- pd_prune_200[[i]]$mntd.obs.z 
  pval <- pd_prune_200[[i]]$mntd.obs.p
  SR <- pd_prune_200[[i]]$SR
  NTI_prune_200[[i]] <- cbind(NTI, pval, SR)
}

NTI_prune_300 <- list()
for (i in 1:length(pd_prune_300)) {
  NTI <- pd_prune_300[[i]]$mntd.obs.z
  pval <- pd_prune_300[[i]]$mntd.obs.p
  SR <- pd_prune_300[[i]]$SR
  NTI_prune_300[[i]] <- cbind(NTI, pval, SR)
}

NTI_prune_400 <- list()
for (i in 1:length(pd_prune_400)) {
  NTI <- pd_prune_400[[i]]$mntd.obs.z 
  pval <- pd_prune_400[[i]]$mntd.obs.p
  SR <- pd_prune_400[[i]]$SR
  NTI_prune_400[[i]] <- cbind(NTI, pval, SR)
}

NTI_prune_500 <- list()
for (i in 1:length(pd_prune_500)) {
  NTI <- pd_prune_500[[i]]$mntd.obs.z 
  pval <- pd_prune_500[[i]]$mntd.obs.p
  SR <- pd_prune_500[[i]]$SR
  NTI_prune_500[[i]] <- cbind(NTI, pval, SR)
}

#NTI ult
NTI_ult_100 <- list()
for (i in 1:length(pd_ult_100)) {
  NTI <- pd_ult_100[[i]]$mntd.obs.z 
  pval <- pd_ult_100[[i]]$mntd.obs.p
  SR <- pd_ult_100[[i]]$SR
  NTI_ult_100[[i]] <- cbind(NTI, pval, SR)
}

NTI_ult_200 <- list()
for (i in 1:length(pd_ult_200)) {
  NTI <- pd_ult_200[[i]]$mntd.obs.z 
  pval <- pd_ult_200[[i]]$mntd.obs.p
  SR <- pd_ult_200[[i]]$SR 
  NTI_ult_200[[i]] <- cbind(NTI, pval, SR)
}

NTI_ult_300 <- list()
for (i in 1:length(pd_ult_300)) {
  NTI <- pd_ult_300[[i]]$mntd.obs.z 
  pval <- pd_ult_300[[i]]$mntd.obs.p
  SR <- pd_ult_300[[i]]$SR
  NTI_ult_300[[i]] <- cbind(NTI, pval, SR)
}

NTI_ult_400 <- list()
for (i in 1:length(pd_ult_400)) {
  NTI <- pd_ult_400[[i]]$mntd.obs.z 
  pval <- pd_ult_400[[i]]$mntd.obs.p
  SR <- pd_ult_400[[i]]$SR
  NTI_ult_400[[i]] <- cbind(NTI, pval, SR)
}

NTI_ult_500 <- list()
for (i in 1:length(pd_ult_500)) {
  NTI <- pd_ult_500[[i]]$mntd.obs.z 
  pval <- pd_ult_500[[i]]$mntd.obs.p
  SR <- pd_ult_500[[i]]$SR 
  NTI_ult_500[[i]] <- cbind(NTI, pval, SR)
}

#NRI recon
NRI_recon_100 <- list()
for (i in 1:length(pd_recon_100)) {
  NRI <- pd_recon_100[[i]]$mpd.obs.z 
  pval <- pd_recon_100[[i]]$mpd.obs.p
  SR <- pd_recon_100[[i]]$SR
  NRI_recon_100[[i]] <- cbind(NRI, pval, SR)
}

NRI_recon_200 <- list()
for (i in 1:length(pd_recon_200)) {
  NRI <- pd_recon_200[[i]]$mpd.obs.z 
  pval <- pd_recon_200[[i]]$mpd.obs.p
  SR <- pd_recon_200[[i]]$SR 
  NRI_recon_200[[i]] <- cbind(NRI, pval, SR)
}

NRI_recon_300 <- list()
for (i in 1:length(pd_recon_300)) {
  NRI <- pd_recon_300[[i]]$mpd.obs.z 
  pval <- pd_recon_300[[i]]$mpd.obs.p
  SR <- pd_recon_300[[i]]$SR
  NRI_recon_300[[i]] <- cbind(NRI, pval, SR)
}

NRI_recon_400 <- list()
for (i in 1:length(pd_recon_400)) {
  NRI <- pd_recon_400[[i]]$mpd.obs.z 
  pval <- pd_recon_400[[i]]$mpd.obs.p
  SR <- pd_recon_400[[i]]$SR
  NRI_recon_400[[i]] <- cbind(NRI, pval, SR)
}

NRI_recon_500 <- list()
for (i in 1:length(pd_recon_500)) {
  NRI <- pd_recon_500[[i]]$mpd.obs.z 
  pval <- pd_recon_500[[i]]$mpd.obs.p
  SR <- pd_recon_500[[i]]$SR
  NRI_recon_500[[i]] <- cbind(NRI, pval, SR)
}

#NRI prune
NRI_prune_100 <- list()
for (i in 1:length(pd_prune_100)) {
  NRI <- pd_prune_100[[i]]$mpd.obs.z 
  pval <- pd_prune_100[[i]]$mpd.obs.p
  SR <- pd_prune_100[[i]]$SR
  NRI_prune_100[[i]] <- cbind(NRI, pval, SR)
}

NRI_prune_200 <- list()
for (i in 1:length(pd_prune_200)) {
  NRI <- pd_prune_200[[i]]$mpd.obs.z 
  pval <- pd_prune_200[[i]]$mpd.obs.p
  SR <- pd_prune_200[[i]]$SR 
  NRI_prune_200[[i]] <- cbind(NRI, pval, SR)
}

NRI_prune_300 <- list()
for (i in 1:length(pd_prune_300)) {
  NRI <- pd_prune_300[[i]]$mpd.obs.z 
  pval <- pd_prune_300[[i]]$mpd.obs.p
  SR <- pd_prune_300[[i]]$SR
  NRI_prune_300[[i]] <- cbind(NRI, pval, SR)
}

NRI_prune_400 <- list()
for (i in 1:length(pd_prune_400)) {
  NRI <- pd_prune_400[[i]]$mpd.obs.z 
  pval <- pd_prune_400[[i]]$mpd.obs.p
  SR <- pd_prune_400[[i]]$SR
  NRI_prune_400[[i]] <- cbind(NRI, pval, SR)
}

NRI_prune_500 <- list()
for (i in 1:length(pd_prune_500)) {
  NRI <- pd_prune_500[[i]]$mpd.obs.z 
  pval <- pd_prune_500[[i]]$mpd.obs.p
  SR <- pd_prune_500[[i]]$SR
  NRI_prune_500[[i]] <- cbind(NRI, pval, SR)
}

#NRI ult

NRI_ult_100 <- list()
for (i in 1:length(pd_ult_100)) {
  NRI <- pd_ult_100[[i]]$mpd.obs.z 
  pval <- pd_ult_100[[i]]$mpd.obs.p
  SR <- pd_ult_100[[i]]$SR
  NRI_ult_100[[i]] <- cbind(NRI, pval, SR)
}

NRI_ult_200 <- list()
for (i in 1:length(pd_ult_200)) {
  NRI <- pd_ult_200[[i]]$mpd.obs.z 
  pval <- pd_ult_200[[i]]$mpd.obs.p
  SR <- pd_ult_200[[i]]$SR
  NRI_ult_200[[i]] <- cbind(NRI, pval, SR)
}

NRI_ult_300 <- list()
for (i in 1:length(pd_ult_300)) {
  NRI <- pd_ult_300[[i]]$mpd.obs.z 
  pval <- pd_ult_300[[i]]$mpd.obs.p
  SR <- pd_ult_300[[i]]$SR
  NRI_ult_300[[i]] <- cbind(NRI, pval, SR)
}

NRI_ult_400 <- list()
for (i in 1:length(pd_ult_400)) {
  NRI <- pd_ult_400[[i]]$mpd.obs.z 
  pval <- pd_ult_400[[i]]$mpd.obs.p
  SR <- pd_ult_400[[i]]$SR
  NRI_ult_400[[i]] <- cbind(NRI, pval, SR)
}

NRI_ult_500 <- list()
for (i in 1:length(pd_ult_500)) {
  NRI <- pd_ult_500[[i]]$mpd.obs.z 
  pval <- pd_ult_500[[i]]$mpd.obs.p
  SR <- pd_ult_500[[i]]$SR 
  NRI_ult_500[[i]] <- cbind(NRI, pval, SR)
}


#PD recon
raw_PD_recon_100 <- list()
for (i in 1:length(pd_recon_100)) {
  raw_PD <- pd_recon_100[[i]]$pd.obs.z
  pval <- pd_recon_100[[i]]$pd.obs.p
  SR <- pd_recon_100[[i]]$SR
  raw_PD_recon_100[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_recon_200 <- list()
for (i in 1:length(pd_recon_200)) {
  raw_PD <- pd_recon_200[[i]]$pd.obs.z
  pval <- pd_recon_200[[i]]$pd.obs.p
  SR <- pd_recon_200[[i]]$SR
  raw_PD_recon_200[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_recon_300 <- list()
for (i in 1:length(pd_recon_300)) {
  raw_PD <- pd_recon_300[[i]]$pd.obs.z 
  pval <- pd_recon_300[[i]]$pd.obs.p
  SR <- pd_recon_300[[i]]$SR
  raw_PD_recon_300[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_recon_400 <- list()
for (i in 1:length(pd_recon_400)) {
  raw_PD <- pd_recon_400[[i]]$pd.obs.z 
  pval <- pd_recon_400[[i]]$pd.obs.p
  SR <- pd_recon_400[[i]]$SR 
  raw_PD_recon_400[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_recon_500 <- list()
for (i in 1:length(pd_recon_500)) {
  raw_PD <- pd_recon_500[[i]]$pd.obs.z 
  pval <- pd_recon_500[[i]]$pd.obs.p
  SR <- pd_recon_500[[i]]$SR 
  raw_PD_recon_500[[i]] <- cbind(raw_PD, pval, SR)
}

#PD clad
raw_PD_prune_100 <- list()
for (i in 1:length(pd_prune_100)) {
  raw_PD <- pd_prune_100[[i]]$pd.obs.z
  pval <- pd_prune_100[[i]]$pd.obs.p
  SR <- pd_prune_100[[i]]$SR
  raw_PD_prune_100[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_prune_200 <- list()
for (i in 1:length(pd_prune_200)) {
  raw_PD <- pd_prune_200[[i]]$pd.obs.z 
  pval <- pd_prune_200[[i]]$pd.obs.p
  SR <- pd_prune_200[[i]]$SR
  raw_PD_prune_200[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_prune_300 <- list()
for (i in 1:length(pd_prune_300)) {
  raw_PD <- pd_prune_300[[i]]$pd.obs.z 
  pval <- pd_prune_300[[i]]$pd.obs.p
  SR <- pd_prune_300[[i]]$SR 
  raw_PD_prune_300[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_prune_400 <- list()
for (i in 1:length(pd_prune_400)) {
  raw_PD <- pd_prune_400[[i]]$pd.obs.z 
  pval <- pd_prune_400[[i]]$pd.obs.p
  SR <- pd_prune_400[[i]]$SR 
  raw_PD_prune_400[[i]] <- cbind(raw_PD, pval, SR)
}

raw_PD_prune_500 <- list()
for (i in 1:length(pd_prune_500)) {
  raw_PD <- pd_prune_500[[i]]$pd.obs.z
  pval <- pd_prune_500[[i]]$pd.obs.p
  SR <- pd_prune_500[[i]]$SR
  raw_PD_prune_500[[i]] <- cbind(raw_PD, pval, SR)
}

#PD ult
PD_ult_100 <- list()
for (i in 1:length(pd_ult_100)) {
  PD <- pd_ult_100[[i]]$pd.obs.z 
  pval <- pd_ult_100[[i]]$pd.obs.p
  SR <- pd_ult_100[[i]]$SR 
  PD_ult_100[[i]] <- cbind(PD, pval, SR)
}

PD_ult_200 <- list()
for (i in 1:length(pd_ult_200)) {
  PD <- pd_ult_200[[i]]$pd.obs.z 
  pval <- pd_ult_200[[i]]$pd.obs.p
  SR <- pd_ult_200[[i]]$SR
  PD_ult_200[[i]] <- cbind(PD, pval, SR)
}

PD_ult_300 <- list()
for (i in 1:length(pd_ult_300)) {
  PD <- pd_ult_300[[i]]$pd.obs.z 
  pval <- pd_ult_300[[i]]$pd.obs.p
  SR <- pd_ult_300[[i]]$SR 
  PD_ult_300[[i]] <- cbind(PD, pval, SR)
}

PD_ult_400 <- list()
for (i in 1:length(pd_ult_400)) {
  PD <- pd_ult_400[[i]]$pd.obs.z 
  pval <- pd_ult_400[[i]]$pd.obs.p
  SR <- pd_ult_400[[i]]$SR 
  PD_ult_400[[i]] <- cbind(PD, pval, SR)
}

PD_ult_500 <- list()
for (i in 1:length(pd_ult_500)) {
  PD <- pd_ult_500[[i]]$pd.obs.z 
  pval <- pd_ult_500[[i]]$pd.obs.p
  SR <- pd_ult_500[[i]]$SR
  PD_ult_500[[i]] <- cbind(PD, pval, SR)
}

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
NTI_recon_100_df <- as.data.frame(NTI_recon_100, col.names = c(1:length(pd_recon_100)), row.names = row.names(tcomm_designations))
NTI_recon_200_df <- as.data.frame(NTI_recon_200, col.names = c(1:length(pd_recon_200)), row.names = row.names(tcomm_designations))
NTI_recon_300_df <- as.data.frame(NTI_recon_300, col.names = c(1:length(pd_recon_300)), row.names = row.names(tcomm_designations))
NTI_recon_400_df <- as.data.frame(NTI_recon_400, col.names = c(1:length(pd_recon_400)), row.names = row.names(tcomm_designations))
NTI_recon_500_df <- as.data.frame(NTI_recon_500, col.names = c(1:length(pd_recon_500)), row.names = row.names(tcomm_designations))

NRI_recon_100_df <- as.data.frame(NRI_recon_100, col.names = c(1:length(pd_recon_100)), row.names = row.names(tcomm_designations))
NRI_recon_200_df <- as.data.frame(NRI_recon_200, col.names = c(1:length(pd_recon_200)), row.names = row.names(tcomm_designations))
NRI_recon_300_df <- as.data.frame(NRI_recon_300, col.names = c(1:length(pd_recon_300)), row.names = row.names(tcomm_designations))
NRI_recon_400_df <- as.data.frame(NRI_recon_400, col.names = c(1:length(pd_recon_400)), row.names = row.names(tcomm_designations))
NRI_recon_500_df <- as.data.frame(NRI_recon_500, col.names = c(1:length(pd_recon_500)), row.names = row.names(tcomm_designations))


NTI_prune_100_df <- as.data.frame(NTI_prune_100, col.names = c(1:length(pd_prune_100)), row.names = row.names(tcomm_designations))
NTI_prune_200_df <- as.data.frame(NTI_prune_200, col.names = c(1:length(pd_prune_200)), row.names = row.names(tcomm_designations))
NTI_prune_300_df <- as.data.frame(NTI_prune_300, col.names = c(1:length(pd_prune_300)), row.names = row.names(tcomm_designations))
NTI_prune_400_df <- as.data.frame(NTI_prune_400, col.names = c(1:length(pd_prune_400)), row.names = row.names(tcomm_designations))
NTI_prune_500_df <- as.data.frame(NTI_prune_500, col.names = c(1:length(pd_prune_500)), row.names = row.names(tcomm_designations))

NRI_prune_100_df <- as.data.frame(NRI_prune_100, col.names = c(1:length(pd_prune_100)), row.names = row.names(tcomm_designations))
NRI_prune_200_df <- as.data.frame(NRI_prune_200, col.names = c(1:length(pd_prune_200)), row.names = row.names(tcomm_designations))
NRI_prune_300_df <- as.data.frame(NRI_prune_300, col.names = c(1:length(pd_prune_300)), row.names = row.names(tcomm_designations))
NRI_prune_400_df <- as.data.frame(NRI_prune_400, col.names = c(1:length(pd_prune_400)), row.names = row.names(tcomm_designations))
NRI_prune_500_df <- as.data.frame(NRI_prune_500, col.names = c(1:length(pd_prune_500)), row.names = row.names(tcomm_designations))

NTI_ult_100_df <- as.data.frame(NTI_ult_100, col.names = c(1:length(pd_ult_100)), row.names = row.names(tcomm_designations))
NTI_ult_200_df <- as.data.frame(NTI_ult_200, col.names = c(1:length(pd_ult_200)), row.names = row.names(tcomm_designations))
NTI_ult_300_df <- as.data.frame(NTI_ult_300, col.names = c(1:length(pd_ult_300)), row.names = row.names(tcomm_designations))
NTI_ult_400_df <- as.data.frame(NTI_ult_400, col.names = c(1:length(pd_ult_400)), row.names = row.names(tcomm_designations))
NTI_ult_500_df <- as.data.frame(NTI_ult_500, col.names = c(1:length(pd_ult_500)), row.names = row.names(tcomm_designations))

NRI_ult_100_df <- as.data.frame(NRI_ult_100, col.names = c(1:length(pd_ult_100)), row.names = row.names(tcomm_designations))
NRI_ult_200_df <- as.data.frame(NRI_ult_200, col.names = c(1:length(pd_ult_200)), row.names = row.names(tcomm_designations))
NRI_ult_300_df <- as.data.frame(NRI_ult_300, col.names = c(1:length(pd_ult_300)), row.names = row.names(tcomm_designations))
NRI_ult_400_df <- as.data.frame(NRI_ult_400, col.names = c(1:length(pd_ult_400)), row.names = row.names(tcomm_designations))
NRI_ult_500_df <- as.data.frame(NRI_ult_500, col.names = c(1:length(pd_ult_500)), row.names = row.names(tcomm_designations))


raw_PD_recon_100_df <- as.data.frame(raw_PD_recon_100, col.names = c(1:length(pd_recon_100)), row.names = row.names(tcomm_designations))
raw_PD_recon_200_df <- as.data.frame(raw_PD_recon_200, col.names = c(1:length(pd_recon_200)), row.names = row.names(tcomm_designations))
raw_PD_recon_300_df <- as.data.frame(raw_PD_recon_300, col.names = c(1:length(pd_recon_300)), row.names = row.names(tcomm_designations))
raw_PD_recon_400_df <- as.data.frame(raw_PD_recon_400, col.names = c(1:length(pd_recon_400)), row.names = row.names(tcomm_designations))
raw_PD_recon_500_df <- as.data.frame(raw_PD_recon_500, col.names = c(1:length(pd_recon_500)), row.names = row.names(tcomm_designations))

raw_PD_prune_100_df <- as.data.frame(raw_PD_prune_100, col.names = c(1:length(pd_prune_100)), row.names = row.names(tcomm_designations))
raw_PD_prune_200_df <- as.data.frame(raw_PD_prune_200, col.names = c(1:length(pd_prune_200)), row.names = row.names(tcomm_designations))
raw_PD_prune_300_df <- as.data.frame(raw_PD_prune_300, col.names = c(1:length(pd_prune_300)), row.names = row.names(tcomm_designations))
raw_PD_prune_400_df <- as.data.frame(raw_PD_prune_400, col.names = c(1:length(pd_prune_400)), row.names = row.names(tcomm_designations))
raw_PD_prune_500_df <- as.data.frame(raw_PD_prune_500, col.names = c(1:length(pd_prune_500)), row.names = row.names(tcomm_designations))

PD_ult_100_df <- as.data.frame(PD_ult_100, col.names = c(1:length(pd_ult_100)), row.names = row.names(tcomm_designations))
PD_ult_200_df <- as.data.frame(PD_ult_200, col.names = c(1:length(pd_ult_200)), row.names = row.names(tcomm_designations))
PD_ult_300_df <- as.data.frame(PD_ult_300, col.names = c(1:length(pd_ult_300)), row.names = row.names(tcomm_designations))
PD_ult_400_df <- as.data.frame(PD_ult_400, col.names = c(1:length(pd_ult_400)), row.names = row.names(tcomm_designations))
PD_ult_500_df <- as.data.frame(PD_ult_500, col.names = c(1:length(pd_ult_500)), row.names = row.names(tcomm_designations))

#Transpose dataframes so replicates are rows and communities are columns
NTI_recon_100_df_tr <- t(NTI_recon_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_recon_200_df_tr <- t(NTI_recon_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_recon_300_df_tr <- t(NTI_recon_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_recon_400_df_tr <- t(NTI_recon_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_recon_500_df_tr <- t(NTI_recon_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


NRI_recon_100_df_tr <- t(NRI_recon_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_recon_200_df_tr <- t(NRI_recon_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_recon_300_df_tr <- t(NRI_recon_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_recon_400_df_tr <- t(NRI_recon_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_recon_500_df_tr <- t(NRI_recon_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Prune

NTI_prune_100_df_tr <- t(NTI_prune_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_prune_200_df_tr <- t(NTI_prune_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_prune_300_df_tr <- t(NTI_prune_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_prune_400_df_tr <- t(NTI_prune_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_prune_500_df_tr <- t(NTI_prune_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


NRI_prune_100_df_tr <- t(NRI_prune_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_prune_200_df_tr <- t(NRI_prune_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_prune_300_df_tr <- t(NRI_prune_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_prune_400_df_tr <- t(NRI_prune_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_prune_500_df_tr <- t(NRI_prune_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Ult

NTI_ult_100_df_tr <- t(NTI_ult_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_ult_200_df_tr <- t(NTI_ult_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_ult_300_df_tr <- t(NTI_ult_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_ult_400_df_tr <- t(NTI_ult_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NTI_ult_500_df_tr <- t(NTI_ult_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


NRI_ult_100_df_tr <- t(NRI_ult_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_ult_200_df_tr <- t(NRI_ult_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_ult_300_df_tr <- t(NRI_ult_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_ult_400_df_tr <- t(NRI_ult_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_ult_500_df_tr <- t(NRI_ult_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)



raw_PD_recon_100_df_tr <- t(raw_PD_recon_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_recon_200_df_tr <- t(raw_PD_recon_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_recon_300_df_tr <- t(raw_PD_recon_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_recon_400_df_tr <- t(raw_PD_recon_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_recon_500_df_tr <- t(raw_PD_recon_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


raw_PD_prune_100_df_tr <- t(raw_PD_prune_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_prune_200_df_tr <- t(raw_PD_prune_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_prune_300_df_tr <- t(raw_PD_prune_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_prune_400_df_tr <- t(raw_PD_prune_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

raw_PD_prune_500_df_tr <- t(raw_PD_prune_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


PD_ult_100_df_tr <- t(PD_ult_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_ult_200_df_tr <- t(PD_ult_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_ult_300_df_tr <- t(PD_ult_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_ult_400_df_tr <- t(PD_ult_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_ult_500_df_tr <- t(PD_ult_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)


#Add column to each dataframe including subset type
NTI_recon_100_df_tr <- cbind(NTI_recon_100_df_tr, "subset" = "100", "type" = "recon")
NTI_recon_200_df_tr <- cbind(NTI_recon_200_df_tr, "subset" = "200", "type" = "recon")
NTI_recon_300_df_tr <- cbind(NTI_recon_300_df_tr, "subset" = "300", "type" = "recon")
NTI_recon_400_df_tr <- cbind(NTI_recon_400_df_tr, "subset" = "400", "type" = "recon")
NTI_recon_500_df_tr <- cbind(NTI_recon_500_df_tr, "subset" = "500", "type" = "recon")

NRI_recon_100_df_tr <- cbind(NRI_recon_100_df_tr, "subset" = "100", "type" = "recon")
NRI_recon_200_df_tr <- cbind(NRI_recon_200_df_tr, "subset" = "200", "type" = "recon")
NRI_recon_300_df_tr <- cbind(NRI_recon_300_df_tr, "subset" = "300", "type" = "recon")
NRI_recon_400_df_tr <- cbind(NRI_recon_400_df_tr, "subset" = "400", "type" = "recon")
NRI_recon_500_df_tr <- cbind(NRI_recon_500_df_tr, "subset" = "500", "type" = "recon")

#Prune
NTI_prune_100_df_tr <- cbind(NTI_prune_100_df_tr, "subset" = "100", "type" = "prune")
NTI_prune_200_df_tr <- cbind(NTI_prune_200_df_tr, "subset" = "200", "type" = "prune")
NTI_prune_300_df_tr <- cbind(NTI_prune_300_df_tr, "subset" = "300", "type" = "prune")
NTI_prune_400_df_tr <- cbind(NTI_prune_400_df_tr, "subset" = "400", "type" = "prune")
NTI_prune_500_df_tr <- cbind(NTI_prune_500_df_tr, "subset" = "500", "type" = "prune")

NRI_prune_100_df_tr <- cbind(NRI_prune_100_df_tr, "subset" = "100", "type" = "prune")
NRI_prune_200_df_tr <- cbind(NRI_prune_200_df_tr, "subset" = "200", "type" = "prune")
NRI_prune_300_df_tr <- cbind(NRI_prune_300_df_tr, "subset" = "300", "type" = "prune")
NRI_prune_400_df_tr <- cbind(NRI_prune_400_df_tr, "subset" = "400", "type" = "prune")
NRI_prune_500_df_tr <- cbind(NRI_prune_500_df_tr, "subset" = "500", "type" = "prune")

#Ult
NTI_ult_100_df_tr <- cbind(NTI_ult_100_df_tr, "subset" = "100", "type" = "ultrametric")
NTI_ult_200_df_tr <- cbind(NTI_ult_200_df_tr, "subset" = "200", "type" = "ultrametric")
NTI_ult_300_df_tr <- cbind(NTI_ult_300_df_tr, "subset" = "300", "type" = "ultrametric")
NTI_ult_400_df_tr <- cbind(NTI_ult_400_df_tr, "subset" = "400", "type" = "ultrametric")
NTI_ult_500_df_tr <- cbind(NTI_ult_500_df_tr, "subset" = "500", "type" = "ultrametric")

NRI_ult_100_df_tr <- cbind(NRI_ult_100_df_tr, "subset" = "100", "type" = "ultrametric")
NRI_ult_200_df_tr <- cbind(NRI_ult_200_df_tr, "subset" = "200", "type" = "ultrametric")
NRI_ult_300_df_tr <- cbind(NRI_ult_300_df_tr, "subset" = "300", "type" = "ultrametric")
NRI_ult_400_df_tr <- cbind(NRI_ult_400_df_tr, "subset" = "400", "type" = "ultrametric")
NRI_ult_500_df_tr <- cbind(NRI_ult_500_df_tr, "subset" = "500", "type" = "ultrametric")

raw_PD_recon_100_df_tr <- cbind(raw_PD_recon_100_df_tr, "subset" = "100", "type" = "recon")
raw_PD_recon_200_df_tr <- cbind(raw_PD_recon_200_df_tr, "subset" = "200", "type" = "recon")
raw_PD_recon_300_df_tr <- cbind(raw_PD_recon_300_df_tr, "subset" = "300", "type" = "recon")
raw_PD_recon_400_df_tr <- cbind(raw_PD_recon_400_df_tr, "subset" = "400", "type" = "recon")
raw_PD_recon_500_df_tr <- cbind(raw_PD_recon_500_df_tr, "subset" = "500", "type" = "recon")

raw_PD_prune_100_df_tr <- cbind(raw_PD_prune_100_df_tr, "subset" = "100", "type" = "prune")
raw_PD_prune_200_df_tr <- cbind(raw_PD_prune_200_df_tr, "subset" = "200", "type" = "prune")
raw_PD_prune_300_df_tr <- cbind(raw_PD_prune_300_df_tr, "subset" = "300", "type" = "prune")
raw_PD_prune_400_df_tr <- cbind(raw_PD_prune_400_df_tr, "subset" = "400", "type" = "prune")
raw_PD_prune_500_df_tr <- cbind(raw_PD_prune_500_df_tr, "subset" = "500", "type" = "prune")

PD_ult_100_df_tr <- cbind(PD_ult_100_df_tr, "subset" = "100", "type" = "ultrametric")
PD_ult_200_df_tr <- cbind(PD_ult_200_df_tr, "subset" = "200", "type" = "ultrametric")
PD_ult_300_df_tr <- cbind(PD_ult_300_df_tr, "subset" = "300", "type" = "ultrametric")
PD_ult_400_df_tr <- cbind(PD_ult_400_df_tr, "subset" = "400", "type" = "ultrametric")
PD_ult_500_df_tr <- cbind(PD_ult_500_df_tr, "subset" = "500", "type" = "ultrametric")


#Make dataframe with all NRI differences
NTI_recon_all <- data.frame(NTI_recon_100_df_tr)
NTI_recon_all <- rbind(NTI_recon_all, data.frame(NTI_recon_200_df_tr))
NTI_recon_all <- rbind(NTI_recon_all, data.frame(NTI_recon_300_df_tr))
NTI_recon_all <- rbind(NTI_recon_all, data.frame(NTI_recon_400_df_tr))
NTI_recon_all <- rbind(NTI_recon_all, data.frame(NTI_recon_500_df_tr))

NRI_recon_all <- data.frame(NRI_recon_100_df_tr)
NRI_recon_all <- rbind(NRI_recon_all, data.frame(NRI_recon_200_df_tr))
NRI_recon_all <- rbind(NRI_recon_all, data.frame(NRI_recon_300_df_tr))
NRI_recon_all <- rbind(NRI_recon_all, data.frame(NRI_recon_400_df_tr))
NRI_recon_all <- rbind(NRI_recon_all, data.frame(NRI_recon_500_df_tr))

#Prune
NTI_prune_all <- data.frame(NTI_prune_100_df_tr)
NTI_prune_all <- rbind(NTI_prune_all, data.frame(NTI_prune_200_df_tr))
NTI_prune_all <- rbind(NTI_prune_all, data.frame(NTI_prune_300_df_tr))
NTI_prune_all <- rbind(NTI_prune_all, data.frame(NTI_prune_400_df_tr))
NTI_prune_all <- rbind(NTI_prune_all, data.frame(NTI_prune_500_df_tr))

NRI_prune_all <- data.frame(NRI_prune_100_df_tr)
NRI_prune_all <- rbind(NRI_prune_all, data.frame(NRI_prune_200_df_tr))
NRI_prune_all <- rbind(NRI_prune_all, data.frame(NRI_prune_300_df_tr))
NRI_prune_all <- rbind(NRI_prune_all, data.frame(NRI_prune_400_df_tr))
NRI_prune_all <- rbind(NRI_prune_all, data.frame(NRI_prune_500_df_tr))

#Ult
NTI_ult_all <- data.frame(NTI_ult_100_df_tr)
NTI_ult_all <- rbind(NTI_ult_all, data.frame(NTI_ult_200_df_tr))
NTI_ult_all <- rbind(NTI_ult_all, data.frame(NTI_ult_300_df_tr))
NTI_ult_all <- rbind(NTI_ult_all, data.frame(NTI_ult_400_df_tr))
NTI_ult_all <- rbind(NTI_ult_all, data.frame(NTI_ult_500_df_tr))

NRI_ult_all <- data.frame(NRI_ult_100_df_tr)
NRI_ult_all <- rbind(NRI_ult_all, data.frame(NRI_ult_200_df_tr))
NRI_ult_all <- rbind(NRI_ult_all, data.frame(NRI_ult_300_df_tr))
NRI_ult_all <- rbind(NRI_ult_all, data.frame(NRI_ult_400_df_tr))
NRI_ult_all <- rbind(NRI_ult_all, data.frame(NRI_ult_500_df_tr))


raw_PD_recon_all <- data.frame(raw_PD_recon_100_df_tr)
raw_PD_recon_all <- rbind(raw_PD_recon_all, data.frame(raw_PD_recon_200_df_tr))
raw_PD_recon_all <- rbind(raw_PD_recon_all, data.frame(raw_PD_recon_300_df_tr))
raw_PD_recon_all <- rbind(raw_PD_recon_all, data.frame(raw_PD_recon_400_df_tr))
raw_PD_recon_all <- rbind(raw_PD_recon_all, data.frame(raw_PD_recon_500_df_tr))

raw_PD_prune_all <- data.frame(raw_PD_prune_100_df_tr)
raw_PD_prune_all <- rbind(raw_PD_prune_all, data.frame(raw_PD_prune_200_df_tr))
raw_PD_prune_all <- rbind(raw_PD_prune_all, data.frame(raw_PD_prune_300_df_tr))
raw_PD_prune_all <- rbind(raw_PD_prune_all, data.frame(raw_PD_prune_400_df_tr))
raw_PD_prune_all <- rbind(raw_PD_prune_all, data.frame(raw_PD_prune_500_df_tr))

PD_ult_all <- data.frame(PD_ult_100_df_tr)
PD_ult_all <- rbind(PD_ult_all, data.frame(PD_ult_200_df_tr))
PD_ult_all <- rbind(PD_ult_all, data.frame(PD_ult_300_df_tr))
PD_ult_all <- rbind(PD_ult_all, data.frame(PD_ult_400_df_tr))
PD_ult_all <- rbind(PD_ult_all, data.frame(PD_ult_500_df_tr))

#Write files
write.csv(NTI_recon_all, "./Plots/2017_botany/Files_to_plot/Raw_NTI_recon_random_subsets.csv")
write.csv(NRI_recon_all, "./Plots/2017_botany/Files_to_plot/Raw_NRI_recon_random_subsets.csv")
write.csv(NTI_prune_all, "./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv")
write.csv(NRI_prune_all, "./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv")
write.csv(NTI_ult_all, "./Plots/2017_botany/Files_to_plot/Raw_NTI_ult_random_subsets.csv")
write.csv(NRI_ult_all, "./Plots/2017_botany/Files_to_plot/Raw_NRI_ult_random_subsets.csv")
write.csv(raw_PD_recon_all, "./Plots/2017_botany/Files_to_plot/Raw_PD_recon_random_subsets.csv")
write.csv(raw_PD_prune_all, "./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv")
write.csv(PD_ult_all, "./Plots/2017_botany/Files_to_plot/Raw_PD_ult_random_subsets.csv")
#Read files
NTI_recon_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_recon_random_subsets.csv", stringsAsFactors = FALSE)
NRI_recon_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_recon_random_subsets.csv", stringsAsFactors = FALSE)
NTI_prune_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)
NRI_prune_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
NTI_ult_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_ult_random_subsets.csv", stringsAsFactors = FALSE)
NRI_ult_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_ult_random_subsets.csv", stringsAsFactors = FALSE)
raw_PD_recon_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_recon_random_subsets.csv", stringsAsFactors = FALSE)
raw_PD_prune_all <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)



#Melt dataframes for plotting
PD_recon_melt <- melt(raw_PD_recon_all, id = c("subset", "type"))
PD_prune_melt <- melt(raw_PD_prune_all, id = c("subset", "type"))
NRI_recon_melt <- melt(NRI_recon_all, id = c("subset", "type"))
NTI_recon_melt <- melt(NTI_recon_all, id = c("subset", "type"))
colnames(PD_recon_melt) <- c("subset", "type", "community", "PD")
colnames(NRI_recon_melt) <- c("subset", "type", "community", "NRI")
colnames(NTI_recon_melt) <- c("subset", "type", "community", "NTI")

#PD_prune_melt <- melt(raw_PD_prune_all, id = c("subset", "type"))
NRI_prune_melt <- melt(NRI_prune_all, id = c("subset", "type"))
NTI_prune_melt <- melt(NTI_prune_all, id = c("subset", "type"))
#colnames(PD_prune_melt) <- c("subset", "type", "community", "PD")
colnames(NRI_prune_melt) <- c("subset", "type", "community", "NRI")
colnames(NTI_prune_melt) <- c("subset", "type", "community", "NTI")

#PD_ult_melt <- melt(raw_PD_ult_all, id = c("subset", "type"))
NRI_ult_melt <- melt(NRI_ult_all, id = c("subset", "type"))
NTI_ult_melt <- melt(NTI_ult_all, id = c("subset", "type"))
#colnames(PD_ult_melt) <- c("subset", "type", "community", "PD")
colnames(NRI_ult_melt) <- c("subset", "type", "community", "NRI")
colnames(NTI_ult_melt) <- c("subset", "type", "community", "NTI")

#Plot PD, NRI, NTI
png("./Plots/2017_botany/Actual_values/Ult_v_recon_v_prune_mesichammock_NTI.png")
ggplot()+
  geom_boxplot(data = NTI_recon_all, aes(x = subset, y = Mesic.Hammock, color = "recon"))+
  geom_boxplot(data = NTI_prune_all, aes(x = subset, y = Mesic.Hammock))+
  #geom_boxplot(data = NTI_ult_all, aes(x = subset, y = Mesic.Hammock, color = "ult"))+
  geom_jitter(data = NTI_recon_all, aes(x = subset, y = Mesic.Hammock, color = "recon"))+
  geom_jitter(data = NTI_prune_all, aes(x = subset, y = Mesic.Hammock))+
  #geom_jitter(data = NTI_ult_all, aes(x = subset, y = Mesic.Hammock, color = "ult"))+
  ggtitle("-NTI values for reconstructed, pruned and ultrametric phylogenies for Mesic Hammock community")+
  xlab("Number of taxa in subset")+
  ylab("Raw -NTI")+
  theme(text = element_text(size=10))
dev.off()

png("./Plots/2017_botany/Actual_values/Ult_v_recon_v_prune_mesichammock_NRI.png")
ggplot()+
  geom_boxplot(data = NRI_recon_all, aes(x = subset, y = Mesic.Hammock, color = "recon"))+
  geom_boxplot(data = NRI_prune_all, aes(x = subset, y = Mesic.Hammock))+
  geom_boxplot(data = NRI_ult_all, aes(x = subset, y = Mesic.Hammock, color = "ult"))+
  geom_jitter(data = NRI_recon_all, aes(x = subset, y = Mesic.Hammock, color = "recon"))+
  geom_jitter(data = NRI_prune_all, aes(x = subset, y = Mesic.Hammock))+
  geom_jitter(data = NRI_ult_all, aes(x = subset, y = Mesic.Hammock, color = "ult"))+
  ggtitle("-NRI values for reconstructed, pruned and ultrametric phylogenies for Mesic Hammock community")+
  xlab("Number of taxa in subset")+
  ylab("Raw -NRI")+
  theme(text = element_text(size=10))
dev.off()

raw_PD_recon_all_trans <- as.data.frame(t(raw_PD_recon_all))
raw_PD_recon_all_trans$community <- rownames(raw_PD_recon_all_trans)
raw_PD_recon_all_trans$X30
png("./Plots/2017_botany/Actual_values/recon_subset_PD.png")
ggplot(data = raw_PD_recon_all)+
  geom_point(aes(x = subset, y = Taxa))+
  ylab("PD")
dev.off()

png("./Plots/2017_botany/Actual_values/recon_subset_PD.png")
ggplot(data = raw_PD_recon_all_trans)+
  geom_boxplot(aes(x = community, y = X30))+
  ylab("PD")
dev.off()

