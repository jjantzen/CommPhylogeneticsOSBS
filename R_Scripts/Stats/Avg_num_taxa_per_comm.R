#Get average number of taxa in community for subsets and reconstruction type

#Read PD files
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

Prop_100_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_100/", pattern=".csv", full.names=TRUE)
Prop_200_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_200/", pattern=".csv", full.names=TRUE)
Prop_300_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_300/", pattern=".csv", full.names=TRUE)
Prop_400_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_400/", pattern=".csv", full.names=TRUE)
Prop_500_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_500/", pattern=".csv", full.names=TRUE)


reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}


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

pd_ult_100 <- lapply(PD_ult_100_files, reading_PD_files)
pd_ult_200 <- lapply(PD_ult_200_files, reading_PD_files)
pd_ult_300 <- lapply(PD_ult_300_files, reading_PD_files)
pd_ult_400 <- lapply(PD_ult_400_files, reading_PD_files)
pd_ult_500 <- lapply(PD_ult_500_files, reading_PD_files)

prop_pd_100 <- lapply(Prop_100_files, reading_PD_files)
prop_pd_200 <- lapply(Prop_200_files, reading_PD_files)
prop_pd_300 <- lapply(Prop_300_files, reading_PD_files)
prop_pd_400 <- lapply(Prop_400_files, reading_PD_files)
prop_pd_500 <- lapply(Prop_500_files, reading_PD_files)


clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Read community data 
tcomm_designations <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Get average number of taxa per community for each subset for each type

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

ult_ntax_100 <- list()
ult_ntax_200 <- list()
ult_ntax_300 <- list()
ult_ntax_400 <- list()
ult_ntax_500 <- list()

for (i in 1:length(pd_ult_100)) {
  ntax <- (pd_ult_100[[i]]$ntaxa) 
  ult_ntax_100[[i]] <- c(ntax)
}

for (i in 1:length(pd_ult_200)) {
  ntax <- (pd_ult_200[[i]]$ntaxa) 
  ult_ntax_200[[i]] <- c(ntax)
}

for (i in 1:length(pd_ult_300)) {
  ntax <- (pd_ult_300[[i]]$ntaxa) 
  ult_ntax_300[[i]] <- c(ntax)
}

for (i in 1:length(pd_ult_400)) {
  ntax <- (pd_ult_400[[i]]$ntaxa) 
  ult_ntax_400[[i]] <- c(ntax)
}

for (i in 1:length(pd_ult_500)) {
  ntax <- (pd_ult_500[[i]]$ntaxa) 
  ult_ntax_500[[i]] <- c(ntax)
}

prop_ntax_100 <- list()
prop_ntax_200 <- list()
prop_ntax_300 <- list()
prop_ntax_400 <- list()
prop_ntax_500 <- list()

for (i in 1:length(prop_pd_100)) {
  ntax <- (prop_pd_100[[i]]$ntaxa) 
  prop_ntax_100[[i]] <- c(ntax)
}

for (i in 1:length(prop_pd_200)) {
  ntax <- (prop_pd_200[[i]]$ntaxa) 
  prop_ntax_200[[i]] <- c(ntax)
}

for (i in 1:length(prop_pd_300)) {
  ntax <- (prop_pd_300[[i]]$ntaxa) 
  prop_ntax_300[[i]] <- c(ntax)
}

for (i in 1:length(prop_pd_400)) {
  ntax <- (prop_pd_400[[i]]$ntaxa) 
  prop_ntax_400[[i]] <- c(ntax)
}

for (i in 1:length(prop_pd_500)) {
  ntax <- (prop_pd_500[[i]]$ntaxa) 
  prop_ntax_500[[i]] <- c(ntax)
}

#Make dataframe with values


#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
clad_ntax_100_df <- as.data.frame(clad_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_clad_100))), row.names = row.names(tcomm_designations))
clad_ntax_200_df <- as.data.frame(clad_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_clad_200))), row.names = row.names(tcomm_designations))
clad_ntax_300_df <- as.data.frame(clad_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_clad_300))), row.names = row.names(tcomm_designations))
clad_ntax_400_df <- as.data.frame(clad_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_clad_400))), row.names = row.names(tcomm_designations))
clad_ntax_500_df <- as.data.frame(clad_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_clad_500))), row.names = row.names(tcomm_designations))

recon_ntax_100_df <- as.data.frame(recon_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_recon_100))), row.names = row.names(tcomm_designations))
recon_ntax_200_df <- as.data.frame(recon_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_recon_200))), row.names = row.names(tcomm_designations))
recon_ntax_300_df <- as.data.frame(recon_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_recon_300))), row.names = row.names(tcomm_designations))
recon_ntax_400_df <- as.data.frame(recon_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_recon_400))), row.names = row.names(tcomm_designations))
recon_ntax_500_df <- as.data.frame(recon_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_recon_500))), row.names = row.names(tcomm_designations))

ult_ntax_100_df <- as.data.frame(ult_ntax_100, col.names = c(paste0("ntaxa", 1:length(pd_ult_100))), row.names = row.names(tcomm_designations))
ult_ntax_200_df <- as.data.frame(ult_ntax_200, col.names = c(paste0("ntaxa", 1:length(pd_ult_200))), row.names = row.names(tcomm_designations))
ult_ntax_300_df <- as.data.frame(ult_ntax_300, col.names = c(paste0("ntaxa", 1:length(pd_ult_300))), row.names = row.names(tcomm_designations))
ult_ntax_400_df <- as.data.frame(ult_ntax_400, col.names = c(paste0("ntaxa", 1:length(pd_ult_400))), row.names = row.names(tcomm_designations))
ult_ntax_500_df <- as.data.frame(ult_ntax_500, col.names = c(paste0("ntaxa", 1:length(pd_ult_500))), row.names = row.names(tcomm_designations))

prop_ntax_100_df <- as.data.frame(prop_ntax_100, col.names = c(paste0("ntaxa", 1:length(prop_pd_100))), row.names = row.names(tcomm_designations))
prop_ntax_200_df <- as.data.frame(prop_ntax_200, col.names = c(paste0("ntaxa", 1:length(prop_pd_200))), row.names = row.names(tcomm_designations))
prop_ntax_300_df <- as.data.frame(prop_ntax_300, col.names = c(paste0("ntaxa", 1:length(prop_pd_300))), row.names = row.names(tcomm_designations))
prop_ntax_400_df <- as.data.frame(prop_ntax_400, col.names = c(paste0("ntaxa", 1:length(prop_pd_400))), row.names = row.names(tcomm_designations))
prop_ntax_500_df <- as.data.frame(prop_ntax_500, col.names = c(paste0("ntaxa", 1:length(prop_pd_500))), row.names = row.names(tcomm_designations))

#Transpose dataframes for saving and plotting overall numbers
prop_ntax_100_df_tr <- t(prop_ntax_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
prop_ntax_200_df_tr <- t(prop_ntax_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
prop_ntax_300_df_tr <- t(prop_ntax_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
prop_ntax_400_df_tr <- t(prop_ntax_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
prop_ntax_500_df_tr <- t(prop_ntax_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

ult_ntax_100_df_tr <- t(ult_ntax_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
ult_ntax_200_df_tr <- t(ult_ntax_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
ult_ntax_300_df_tr <- t(ult_ntax_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
ult_ntax_400_df_tr <- t(ult_ntax_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
ult_ntax_500_df_tr <- t(ult_ntax_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

recon_ntax_100_df_tr <- t(recon_ntax_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
recon_ntax_200_df_tr <- t(recon_ntax_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
recon_ntax_300_df_tr <- t(recon_ntax_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
recon_ntax_400_df_tr <- t(recon_ntax_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
recon_ntax_500_df_tr <- t(recon_ntax_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

clad_ntax_100_df_tr <- t(clad_ntax_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
clad_ntax_200_df_tr <- t(clad_ntax_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
clad_ntax_300_df_tr <- t(clad_ntax_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
clad_ntax_400_df_tr <- t(clad_ntax_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
clad_ntax_500_df_tr <- t(clad_ntax_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Add column for subset number
prop_ntax_100_df_tr$subset <- "100"
prop_ntax_200_df_tr$subset <- "200"
prop_ntax_300_df_tr$subset <- "300"
prop_ntax_400_df_tr$subset <- "400"
prop_ntax_500_df_tr$subset <- "500"

clad_ntax_100_df_tr$subset <- "100"
clad_ntax_200_df_tr$subset <- "200"
clad_ntax_300_df_tr$subset <- "300"
clad_ntax_400_df_tr$subset <- "400"
clad_ntax_500_df_tr$subset <- "500"

ult_ntax_100_df_tr$subset <- "100"
ult_ntax_200_df_tr$subset <- "200"
ult_ntax_300_df_tr$subset <- "300"
ult_ntax_400_df_tr$subset <- "400"
ult_ntax_500_df_tr$subset <- "500"

recon_ntax_100_df_tr$subset <- "100"
recon_ntax_200_df_tr$subset <- "200"
recon_ntax_300_df_tr$subset <- "300"
recon_ntax_400_df_tr$subset <- "400"
recon_ntax_500_df_tr$subset <- "500"

#Combine datasets
recon_ntax_total <- rbind(recon_ntax_100_df_tr, recon_ntax_200_df_tr, recon_ntax_300_df_tr, recon_ntax_400_df_tr, recon_ntax_500_df_tr)
clad_ntax_total <- rbind(clad_ntax_100_df_tr, clad_ntax_200_df_tr, clad_ntax_300_df_tr, clad_ntax_400_df_tr, clad_ntax_500_df_tr)
ult_ntax_total <- rbind(ult_ntax_100_df_tr, ult_ntax_200_df_tr, ult_ntax_300_df_tr, ult_ntax_400_df_tr, ult_ntax_500_df_tr)
prop_ntax_total <- rbind(prop_ntax_100_df_tr, prop_ntax_200_df_tr, prop_ntax_300_df_tr, prop_ntax_400_df_tr, prop_ntax_500_df_tr)

#Melt dataset 
melt_recon_ntax <- melt(recon_ntax_total, id = "subset")
melt_prop_ntax <- melt(prop_ntax_total, id = "subset")
melt_ult_ntax <- melt(ult_ntax_total, id = "subset")
melt_clad_ntax <- melt(clad_ntax_total, id = "subset")

#Write files for plotting
write.csv(melt_clad_ntax, "./Plots/2017_botany/Files_to_plot/Num_taxa_all_prune.csv")
write.csv(melt_recon_ntax, "./Plots/2017_botany/Files_to_plot/Num_taxa_all_recon.csv")
write.csv(melt_prop_ntax, "./Plots/2017_botany/Files_to_plot/Num_taxa_all_prop.csv")
write.csv(melt_ult_ntax, "./Plots/2017_botany/Files_to_plot/Num_taxa_all_ult.csv")

#Make new dataframes for averages
clad_ntax_avgs100 <- as.data.frame(matrix(ncol = 0, nrow = 15))
clad_ntax_avgs200 <- as.data.frame(matrix(ncol = 0, nrow = 15))
clad_ntax_avgs300 <- as.data.frame(matrix(ncol = 0, nrow = 15))
clad_ntax_avgs400 <- as.data.frame(matrix(ncol = 0, nrow = 15))
clad_ntax_avgs500 <- as.data.frame(matrix(ncol = 0, nrow = 15))

recon_ntax_avgs100 <- as.data.frame(matrix(ncol = 0, nrow = 15))
recon_ntax_avgs200 <- as.data.frame(matrix(ncol = 0, nrow = 15))
recon_ntax_avgs300 <- as.data.frame(matrix(ncol = 0, nrow = 15))
recon_ntax_avgs400 <- as.data.frame(matrix(ncol = 0, nrow = 15))
recon_ntax_avgs500 <- as.data.frame(matrix(ncol = 0, nrow = 15))

ult_ntax_avgs100 <- as.data.frame(matrix(ncol = 0, nrow = 15))
ult_ntax_avgs200 <- as.data.frame(matrix(ncol = 0, nrow = 15))
ult_ntax_avgs300 <- as.data.frame(matrix(ncol = 0, nrow = 15))
ult_ntax_avgs400 <- as.data.frame(matrix(ncol = 0, nrow = 15))
ult_ntax_avgs500 <- as.data.frame(matrix(ncol = 0, nrow = 15))

prop_ntax_avgs100 <- as.data.frame(matrix(ncol = 0, nrow = 15))
prop_ntax_avgs200 <- as.data.frame(matrix(ncol = 0, nrow = 15))
prop_ntax_avgs300 <- as.data.frame(matrix(ncol = 0, nrow = 15))
prop_ntax_avgs400 <- as.data.frame(matrix(ncol = 0, nrow = 15))
prop_ntax_avgs500 <- as.data.frame(matrix(ncol = 0, nrow = 15))

#Add column for average, sd, min and max
clad_ntax_avgs100$avgtax <- apply(clad_ntax_100_df, 1, mean)
clad_ntax_avgs100$std <- apply(clad_ntax_100_df, 1, sd)
clad_ntax_avgs100$mintax <- apply(clad_ntax_100_df, 1, min)
clad_ntax_avgs100$maxtax <- apply(clad_ntax_100_df, 1, max)
clad_ntax_avgs100$subset <- "100"
clad_ntax_avgs100$community <- rownames(tcomm_designations)

clad_ntax_avgs200$avgtax <- apply(clad_ntax_200_df, 1, mean)
clad_ntax_avgs200$std <- apply(clad_ntax_200_df, 1, sd)
clad_ntax_avgs200$mintax <- apply(clad_ntax_200_df, 1, min)
clad_ntax_avgs200$maxtax <- apply(clad_ntax_200_df, 1, max)
clad_ntax_avgs200$subset <- "200"
clad_ntax_avgs200$community <- rownames(tcomm_designations)

clad_ntax_avgs300$avgtax <- apply(clad_ntax_300_df, 1, mean)
clad_ntax_avgs300$std <- apply(clad_ntax_300_df, 1, sd)
clad_ntax_avgs300$mintax <- apply(clad_ntax_300_df, 1, min)
clad_ntax_avgs300$maxtax <- apply(clad_ntax_300_df, 1, max)
clad_ntax_avgs300$subset <- "300"
clad_ntax_avgs300$community <- rownames(tcomm_designations)

clad_ntax_avgs400$avgtax <- apply(clad_ntax_400_df, 1, mean)
clad_ntax_avgs400$std <- apply(clad_ntax_400_df, 1, sd)
clad_ntax_avgs400$mintax <- apply(clad_ntax_400_df, 1, min)
clad_ntax_avgs400$maxtax <- apply(clad_ntax_400_df, 1, max)
clad_ntax_avgs400$subset <- "400"
clad_ntax_avgs400$community <- rownames(tcomm_designations)

clad_ntax_avgs500$avgtax <- apply(clad_ntax_500_df, 1, mean)
clad_ntax_avgs500$std <- apply(clad_ntax_500_df, 1, sd)
clad_ntax_avgs500$mintax <- apply(clad_ntax_500_df, 1, min)
clad_ntax_avgs500$maxtax <- apply(clad_ntax_500_df, 1, max)
clad_ntax_avgs500$subset <- "500"
clad_ntax_avgs500$community <- rownames(tcomm_designations)


recon_ntax_avgs100$avgtax <- apply(recon_ntax_100_df, 1, mean)
recon_ntax_avgs100$std <- apply(recon_ntax_100_df, 1, sd)
recon_ntax_avgs100$mintax <- apply(recon_ntax_100_df, 1, min)
recon_ntax_avgs100$maxtax <- apply(recon_ntax_100_df, 1, max)
recon_ntax_avgs100$subset <- "100"
recon_ntax_avgs100$community <- rownames(tcomm_designations)

recon_ntax_avgs200$avgtax <- apply(recon_ntax_200_df, 1, mean)
recon_ntax_avgs200$std <- apply(recon_ntax_200_df, 1, sd)
recon_ntax_avgs200$mintax <- apply(recon_ntax_200_df, 1, min)
recon_ntax_avgs200$maxtax <- apply(recon_ntax_200_df, 1, max)
recon_ntax_avgs200$subset <- "200"
recon_ntax_avgs200$community <- rownames(tcomm_designations)

recon_ntax_avgs300$avgtax <- apply(recon_ntax_300_df, 1, mean)
recon_ntax_avgs300$std <- apply(recon_ntax_300_df, 1, sd)
recon_ntax_avgs300$mintax <- apply(recon_ntax_300_df, 1, min)
recon_ntax_avgs300$maxtax <- apply(recon_ntax_300_df, 1, max)
recon_ntax_avgs300$subset <- "300"
recon_ntax_avgs300$community <- rownames(tcomm_designations)

recon_ntax_avgs400$avgtax <- apply(recon_ntax_400_df, 1, mean)
recon_ntax_avgs400$std <- apply(recon_ntax_400_df, 1, sd)
recon_ntax_avgs400$mintax <- apply(recon_ntax_400_df, 1, min)
recon_ntax_avgs400$maxtax <- apply(recon_ntax_400_df, 1, max)
recon_ntax_avgs400$subset <- "400"
recon_ntax_avgs400$community <- rownames(tcomm_designations)

recon_ntax_avgs500$avgtax <- apply(recon_ntax_500_df, 1, mean)
recon_ntax_avgs500$std <- apply(recon_ntax_500_df, 1, sd)
recon_ntax_avgs500$mintax <- apply(recon_ntax_500_df, 1, min)
recon_ntax_avgs500$maxtax <- apply(recon_ntax_500_df, 1, max)
recon_ntax_avgs500$subset <- "500"
recon_ntax_avgs500$community <- rownames(tcomm_designations)

ult_ntax_avgs100$avgtax <- apply(ult_ntax_100_df, 1, mean)
ult_ntax_avgs100$std <- apply(ult_ntax_100_df, 1, sd)
ult_ntax_avgs100$mintax <- apply(ult_ntax_100_df, 1, min)
ult_ntax_avgs100$maxtax <- apply(ult_ntax_100_df, 1, max)
ult_ntax_avgs100$subset <- "100"
ult_ntax_avgs100$community <- rownames(tcomm_designations)

ult_ntax_avgs200$avgtax <- apply(ult_ntax_200_df, 1, mean)
ult_ntax_avgs200$std <- apply(ult_ntax_200_df, 1, sd)
ult_ntax_avgs200$mintax <- apply(ult_ntax_200_df, 1, min)
ult_ntax_avgs200$maxtax <- apply(ult_ntax_200_df, 1, max)
ult_ntax_avgs200$subset <- "200"
ult_ntax_avgs200$community <- rownames(tcomm_designations)

ult_ntax_avgs300$avgtax <- apply(ult_ntax_300_df, 1, mean)
ult_ntax_avgs300$std <- apply(ult_ntax_300_df, 1, sd)
ult_ntax_avgs300$mintax <- apply(ult_ntax_300_df, 1, min)
ult_ntax_avgs300$maxtax <- apply(ult_ntax_300_df, 1, max)
ult_ntax_avgs300$subset <- "300"
ult_ntax_avgs300$community <- rownames(tcomm_designations)

ult_ntax_avgs400$avgtax <- apply(ult_ntax_400_df, 1, mean)
ult_ntax_avgs400$std <- apply(ult_ntax_400_df, 1, sd)
ult_ntax_avgs400$mintax <- apply(ult_ntax_400_df, 1, min)
ult_ntax_avgs400$maxtax <- apply(ult_ntax_400_df, 1, max)
ult_ntax_avgs400$subset <- "400"
ult_ntax_avgs400$community <- rownames(tcomm_designations)

ult_ntax_avgs500$avgtax <- apply(ult_ntax_500_df, 1, mean)
ult_ntax_avgs500$std <- apply(ult_ntax_500_df, 1, sd)
ult_ntax_avgs500$mintax <- apply(ult_ntax_500_df, 1, min)
ult_ntax_avgs500$maxtax <- apply(ult_ntax_500_df, 1, max)
ult_ntax_avgs500$subset <- "500"
ult_ntax_avgs500$community <- rownames(tcomm_designations)


prop_ntax_avgs100$avgtax <- apply(prop_ntax_100_df, 1, mean)
prop_ntax_avgs100$std <- apply(prop_ntax_100_df, 1, sd)
prop_ntax_avgs100$mintax <- apply(prop_ntax_100_df, 1, min)
prop_ntax_avgs100$maxtax <- apply(prop_ntax_100_df, 1, max)
prop_ntax_avgs100$subset <- "100"
prop_ntax_avgs100$community <- rownames(tcomm_designations)

prop_ntax_avgs200$avgtax <- apply(prop_ntax_200_df, 1, mean)
prop_ntax_avgs200$std <- apply(prop_ntax_200_df, 1, sd)
prop_ntax_avgs200$mintax <- apply(prop_ntax_200_df, 1, min)
prop_ntax_avgs200$maxtax <- apply(prop_ntax_200_df, 1, max)
prop_ntax_avgs200$subset <- "200"
prop_ntax_avgs200$community <- rownames(tcomm_designations)

prop_ntax_avgs300$avgtax <- apply(prop_ntax_300_df, 1, mean)
prop_ntax_avgs300$std <- apply(prop_ntax_300_df, 1, sd)
prop_ntax_avgs300$mintax <- apply(prop_ntax_300_df, 1, min)
prop_ntax_avgs300$maxtax <- apply(prop_ntax_300_df, 1, max)
prop_ntax_avgs300$subset <- "300"
prop_ntax_avgs300$community <- rownames(tcomm_designations)

prop_ntax_avgs400$avgtax <- apply(prop_ntax_400_df, 1, mean)
prop_ntax_avgs400$std <- apply(prop_ntax_400_df, 1, sd)
prop_ntax_avgs400$mintax <- apply(prop_ntax_400_df, 1, min)
prop_ntax_avgs400$maxtax <- apply(prop_ntax_400_df, 1, max)
prop_ntax_avgs400$subset <- "400"
prop_ntax_avgs400$community <- rownames(tcomm_designations)

prop_ntax_avgs500$avgtax <- apply(prop_ntax_500_df, 1, mean)
prop_ntax_avgs500$std <- apply(prop_ntax_500_df, 1, sd)
prop_ntax_avgs500$mintax <- apply(prop_ntax_500_df, 1, min)
prop_ntax_avgs500$maxtax <- apply(prop_ntax_500_df, 1, max)
prop_ntax_avgs500$subset <- "500"
prop_ntax_avgs500$community <- rownames(tcomm_designations)


#Melt dataframes
m_clad_ntax_avgs100 <- melt(clad_ntax_avgs100, id = c("subset", "community"))
m_clad_ntax_avgs200 <- melt(clad_ntax_avgs200, id = c("subset", "community"))
m_clad_ntax_avgs300 <- melt(clad_ntax_avgs300, id = c("subset", "community"))
m_clad_ntax_avgs400 <- melt(clad_ntax_avgs400, id = c("subset", "community"))
m_clad_ntax_avgs500 <- melt(clad_ntax_avgs500, id = c("subset", "community"))

m_recon_ntax_avgs100 <- melt(recon_ntax_avgs100, id = c("subset", "community"))
m_recon_ntax_avgs200 <- melt(recon_ntax_avgs200, id = c("subset", "community"))
m_recon_ntax_avgs300 <- melt(recon_ntax_avgs300, id = c("subset", "community"))
m_recon_ntax_avgs400 <- melt(recon_ntax_avgs400, id = c("subset", "community"))
m_recon_ntax_avgs500 <- melt(recon_ntax_avgs500, id = c("subset", "community"))

m_ult_ntax_avgs100 <- melt(ult_ntax_avgs100, id = c("subset", "community"))
m_ult_ntax_avgs200 <- melt(ult_ntax_avgs200, id = c("subset", "community"))
m_ult_ntax_avgs300 <- melt(ult_ntax_avgs300, id = c("subset", "community"))
m_ult_ntax_avgs400 <- melt(ult_ntax_avgs400, id = c("subset", "community"))
m_ult_ntax_avgs500 <- melt(ult_ntax_avgs500, id = c("subset", "community"))

m_prop_ntax_avgs100 <- melt(prop_ntax_avgs100, id = c("subset", "community"))
m_prop_ntax_avgs200 <- melt(prop_ntax_avgs200, id = c("subset", "community"))
m_prop_ntax_avgs300 <- melt(prop_ntax_avgs300, id = c("subset", "community"))
m_prop_ntax_avgs400 <- melt(prop_ntax_avgs400, id = c("subset", "community"))
m_prop_ntax_avgs500 <- melt(prop_ntax_avgs500, id = c("subset", "community"))

#Combine melted datasets
clad_ntax_avgs <- rbind(m_clad_ntax_avgs100, m_clad_ntax_avgs200, m_clad_ntax_avgs300, m_clad_ntax_avgs400, m_clad_ntax_avgs500)
recon_ntax_avgs <- rbind(m_recon_ntax_avgs100, m_recon_ntax_avgs200, m_recon_ntax_avgs300, m_recon_ntax_avgs400, m_recon_ntax_avgs500)
ult_ntax_avgs <- rbind(m_ult_ntax_avgs100, m_ult_ntax_avgs200, m_ult_ntax_avgs300, m_ult_ntax_avgs400, m_ult_ntax_avgs500)
prop_ntax_avgs <- rbind(m_prop_ntax_avgs100, m_prop_ntax_avgs200, m_prop_ntax_avgs300, m_prop_ntax_avgs400, m_prop_ntax_avgs500)

clad_ntax_avgs <- clad_ntax_avgs[which(clad_ntax_avgs$community != "Taxa"),]
recon_ntax_avgs <- recon_ntax_avgs[which(recon_ntax_avgs$community != "Taxa"),]
ult_ntax_avgs <- ult_ntax_avgs[which(ult_ntax_avgs$community != "Taxa"),]
prop_ntax_avgs <- prop_ntax_avgs[which(prop_ntax_avgs$community != "Taxa"),]


write.csv(clad_ntax_avgs, "./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_pruned_phylograms.csv")
write.csv(recon_ntax_avgs, "./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_recon_phylograms.csv")
write.csv(ult_ntax_avgs, "./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_ultrametric.csv")
write.csv(prop_ntax_avgs, "./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_prop_subsets.csv")


#Number of replicates per dataset

rep_numbers <- as.data.frame(matrix(ncol = 0, nrow = 5))
rep_numbers$pruned <- c(length(pd_clad_100), length(pd_clad_200), length(pd_clad_300), length(pd_clad_400), length(pd_clad_500))
rep_numbers$recon <- c(length(pd_recon_100), length(pd_recon_200), length(pd_recon_300), length(pd_recon_400), length(pd_recon_500))
rep_numbers$ult <- c(length(pd_ult_100), length(pd_ult_200), length(pd_ult_300), length(pd_ult_400), length(pd_ult_500))
rep_numbers$prop <- c(length(pd_prop_100), length(pd_prop_200), length(pd_prop_300), length(pd_prop_400), length(pd_prop_500))
