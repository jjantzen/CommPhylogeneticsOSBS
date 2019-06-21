#Plot proportion vs set number vs random for subset type numbers
#Calculate PD values for proportional tree/datasets

#Read proportional tree files
prop_trees100_files <- list.files("./Subset_files/Nonrandom_subsets/Proportional/Trees_100/", pattern=".tre", full.names=TRUE)
prop_trees200_files <- list.files("./Subset_files/Nonrandom_subsets/Proportional/Trees_200/", pattern=".tre", full.names=TRUE)
prop_trees300_files <- list.files("./Subset_files/Nonrandom_subsets/Proportional/Trees_300/", pattern=".tre", full.names=TRUE)
prop_trees400_files <- list.files("./Subset_files/Nonrandom_subsets/Proportional/Trees_400/", pattern=".tre", full.names=TRUE)
prop_trees500_files <- list.files("./Subset_files/Nonrandom_subsets/Proportional/Trees_500/", pattern=".tre", full.names=TRUE)

#Create function with right parameters for reading tree
reading_trees <- function(tree){
  tree <- read.tree(tree, comment.char = "@")
}

#Read the trees
prop_trees100 <- lapply(prop_trees100_files, reading_trees)
prop_trees200 <- lapply(prop_trees200_files, reading_trees)
prop_trees300 <- lapply(prop_trees300_files, reading_trees)
prop_trees400 <- lapply(prop_trees400_files, reading_trees)
prop_trees500 <- lapply(prop_trees500_files, reading_trees)


class(prop_trees100) <- "multiPhylo"
class(prop_trees200) <- "multiPhylo"
class(prop_trees300) <- "multiPhylo"
class(prop_trees400) <- "multiPhylo"
class(prop_trees500) <- "multiPhylo"

#Read overall tree file 
tree572 <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)

#Read community dataset 
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Calclate PD using picante
PD_list <- list()

#Write function that trims taxa to match tree and comms
#Then calc pd indices
#Creates list of tables for each replicate within a subset
pd_calc_function <- function(phylo) {
  matched <- match.phylo.comm(phylo, comm_dataset)
  matchtree <- matched$phy
  matchcomm <- matched$comm 
  phydist <- cophenetic(matched$phy)
  ses_mpd_result <- ses.mpd(matchcomm, phydist, null.model="taxa.labels", abundance.weighted = FALSE, runs=1000)
  ses_mntd_result <- ses.mntd(matchcomm, phydist, null.model="taxa.labels", runs=1000)
  ses_pd_result <-ses.pd(matchcomm, matchtree, null.model="taxa.labels", runs=1000)
  pd_result <- pd(matchcomm, matchtree, include.root = TRUE)
  PD_list <- data.frame(pd_result, ses_pd_result, ses_mpd_result, ses_mntd_result)
}
#Did the PD function work properly for the replicates before?
#Apply function to replicates for each subset

prop_pd_100 <- lapply(prop_trees100, pd_calc_function)
prop_pd_200 <- lapply(prop_trees200, pd_calc_function)
prop_pd_300 <- lapply(prop_trees300, pd_calc_function)
prop_pd_400 <- lapply(prop_trees400, pd_calc_function)
prop_pd_500 <- lapply(prop_trees500, pd_calc_function)

prop_pd_500[[100]]


#Write function to write csv files
write_csv <- function(pd_file, filename){
  write.csv(pd_file, filename)
}

#For cladograms
prop_100subsets_names <- gsub(".tre", ".csv", prop_trees100_files) 
prop_100subsets_names <- gsub("./Subset_files/Nonrandom_subsets/Proportional/Trees_100/", "./PD_files/R_calc_picante/Proportional/Prop_PD_100/PD_", prop_100subsets_names)

prop_200subsets_names <- gsub(".tre", ".csv", prop_trees200_files) 
prop_200subsets_names <- gsub("./Subset_files/Nonrandom_subsets/Proportional/Trees_200/", "./PD_files/R_calc_picante/Proportional/Prop_PD_200/PD_", prop_200subsets_names)

prop_300subsets_names <- gsub(".tre", ".csv", prop_trees300_files) 
prop_300subsets_names <- gsub("./Subset_files/Nonrandom_subsets/Proportional/Trees_300/", "./PD_files/R_calc_picante/Proportional/Prop_PD_300/PD_", prop_300subsets_names)

prop_400subsets_names <- gsub(".tre", ".csv", prop_trees400_files) 
prop_400subsets_names <- gsub("./Subset_files/Nonrandom_subsets/Proportional/Trees_400/", "./PD_files/R_calc_picante/Proportional/Prop_PD_400/PD_", prop_400subsets_names)

prop_500subsets_names <- gsub(".tre", ".csv", prop_trees500_files) 
prop_500subsets_names <- gsub("./Subset_files/Nonrandom_subsets/Proportional/Trees_500/", "./PD_files/R_calc_picante/Proportional/Prop_PD_500/PD_", prop_500subsets_names)

#Write files
mapply(write_csv, pd_file = prop_pd_100, filename = prop_100subsets_names)
mapply(write_csv, pd_file = prop_pd_200, filename = prop_200subsets_names)
mapply(write_csv, pd_file = prop_pd_300, filename = prop_300subsets_names)
mapply(write_csv, pd_file = prop_pd_400, filename = prop_400subsets_names)
mapply(write_csv, pd_file = prop_pd_500, filename = prop_500subsets_names)

#Read files
Prop_100_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_100/", pattern=".csv", full.names=TRUE)
Prop_200_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_200/", pattern=".csv", full.names=TRUE)
Prop_300_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_300/", pattern=".csv", full.names=TRUE)
Prop_400_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_400/", pattern=".csv", full.names=TRUE)
Prop_500_files <- list.files("./PD_files/R_calc_picante/Proportional/Prop_PD_500/", pattern=".csv", full.names=TRUE)

#Write function to read PD files
reading_PD_files <- function(PD_file){
  PD <- read.csv(PD_file, stringsAsFactors = FALSE)
}

#Read the files
prop_pd_100 <- lapply(Prop_100_files, reading_PD_files)
prop_pd_200 <- lapply(Prop_200_files, reading_PD_files)
prop_pd_300 <- lapply(Prop_300_files, reading_PD_files)
prop_pd_400 <- lapply(Prop_400_files, reading_PD_files)
prop_pd_500 <- lapply(Prop_500_files, reading_PD_files)

#Make dataset with all proportional pds
PD_prop_100 <- list()
for (i in 1:length(prop_pd_100)) {
  PD <- prop_pd_100[[i]]$pd.obs.z 
  SR <- prop_pd_100[[i]]$SR
  pval <- prop_pd_100[[i]]$pd.obs.p
  PD_prop_100[[i]] <- cbind(PD, SR, pval)
}

PD_prop_200 <- list()
for (i in 1:length(prop_pd_200)) {
  PD <- prop_pd_200[[i]]$pd.obs.z 
  SR <- prop_pd_200[[i]]$SR
  pval <- prop_pd_200[[i]]$pd.obs.p
  PD_prop_200[[i]] <- cbind(PD, SR, pval)
}

PD_prop_300 <- list()
for (i in 1:length(prop_pd_300)) {
  PD <- prop_pd_300[[i]]$pd.obs.z
  SR <- prop_pd_300[[i]]$SR
  pval <- prop_pd_300[[i]]$pd.obs.p
  PD_prop_300[[i]] <- cbind(PD, SR, pval)
}

PD_prop_400 <- list()
for (i in 1:length(prop_pd_400)) {
  PD <- prop_pd_400[[i]]$pd.obs.z
  SR <- prop_pd_400[[i]]$SR
  pval <- prop_pd_400[[i]]$pd.obs.p
  PD_prop_400[[i]] <- cbind(PD, SR, pval)
}


PD_prop_500 <- list()
for (i in 1:length(prop_pd_500)) {
  PD <- prop_pd_500[[i]]$pd.obs.z
  SR <- prop_pd_500[[i]]$SR
  pval <- prop_pd_500[[i]]$pd.obs.p
  PD_prop_500[[i]] <- cbind(PD, SR, pval)
}

NRI_prop_100 <- list()
for (i in 1:length(prop_pd_100)) {
  NRI <- prop_pd_100[[i]]$mpd.obs.z 
  SR <- prop_pd_100[[i]]$SR
  pval <- prop_pd_100[[i]]$mpd.obs.p
  NRI_prop_100[[i]] <- cbind(NRI, SR, pval)
}

NRI_prop_200 <- list()
for (i in 1:length(prop_pd_200)) {
  NRI <- prop_pd_200[[i]]$mpd.obs.z 
  SR <- prop_pd_200[[i]]$SR
  pval <- prop_pd_200[[i]]$mpd.obs.p
  NRI_prop_200[[i]] <- cbind(NRI, SR, pval)
}

NRI_prop_300 <- list()
for (i in 1:length(prop_pd_300)) {
  NRI <- prop_pd_300[[i]]$mpd.obs.z 
  SR <- prop_pd_300[[i]]$SR
  pval <- prop_pd_300[[i]]$mpd.obs.p
  NRI_prop_300[[i]] <- cbind(NRI, SR, pval)
}

NRI_prop_400 <- list()
for (i in 1:length(prop_pd_400)) {
  NRI <- prop_pd_400[[i]]$mpd.obs.z 
  SR <- prop_pd_400[[i]]$SR
  pval <- prop_pd_400[[i]]$mpd.obs.p
  NRI_prop_400[[i]] <- cbind(NRI, SR, pval)
}

NRI_prop_500 <- list()
for (i in 1:length(prop_pd_500)) {
  NRI <- prop_pd_500[[i]]$mpd.obs.z 
  SR <- prop_pd_500[[i]]$SR
  pval <- prop_pd_500[[i]]$mpd.obs.p
  NRI_prop_500[[i]] <- cbind(NRI, SR, pval)
}

NTI_prop_100 <- list()
for (i in 1:length(prop_pd_100)) {
  NTI <- prop_pd_100[[i]]$mntd.obs.z 
  SR <- prop_pd_100[[i]]$SR
  pval <- prop_pd_100[[i]]$mntd.obs.p
  NTI_prop_100[[i]] <- cbind(NTI, SR, pval)
}

NTI_prop_200 <- list()
for (i in 1:length(prop_pd_200)) {
  NTI <- prop_pd_200[[i]]$mntd.obs.z 
  SR <- prop_pd_200[[i]]$SR
  pval <- prop_pd_200[[i]]$mntd.obs.p
  NTI_prop_200[[i]] <- cbind(NTI, SR, pval)
}

NTI_prop_300 <- list()
for (i in 1:length(prop_pd_300)) {
  NTI <- prop_pd_300[[i]]$mntd.obs.z 
  SR <- prop_pd_300[[i]]$SR
  pval <- prop_pd_300[[i]]$mntd.obs.p
  NTI_prop_300[[i]] <- cbind(NTI, SR, pval)
}

NTI_prop_400 <- list()
for (i in 1:length(prop_pd_400)) {
  NTI <- prop_pd_400[[i]]$mntd.obs.z 
  SR <- prop_pd_400[[i]]$SR
  pval <- prop_pd_400[[i]]$mntd.obs.p
  NTI_prop_400[[i]] <- cbind(NTI, SR, pval)
}

NTI_prop_500 <- list()
for (i in 1:length(prop_pd_500)) {
  NTI <- prop_pd_500[[i]]$mntd.obs.z 
  SR <- prop_pd_500[[i]]$SR
  pval <- prop_pd_500[[i]]$mntd.obs.p
  NTI_prop_500[[i]] <- cbind(NTI, SR, pval)
}

#Read community data 
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Convert results to dataframe - each dataframe contains replicates in columns and each community in rows
#Column names
names(NTI_prop_100) <- c(1:length(prop_pd_100))
names(NRI_prop_100) <- c(1:length(prop_pd_100))
names(PD_prop_100) <- c(1:length(prop_pd_100))
names(NTI_prop_200) <- c(1:length(prop_pd_200))
names(NRI_prop_200) <- c(1:length(prop_pd_200))
names(PD_prop_200) <- c(1:length(prop_pd_200))
names(NTI_prop_300) <- c(1:length(prop_pd_300))
names(NRI_prop_300) <- c(1:length(prop_pd_300))
names(PD_prop_300) <- c(1:length(prop_pd_300))
names(NTI_prop_400) <- c(1:length(prop_pd_400))
names(NRI_prop_400) <- c(1:length(prop_pd_400))
names(PD_prop_400) <- c(1:length(prop_pd_400))
names(NTI_prop_500) <- c(1:length(prop_pd_500))
names(NRI_prop_500) <- c(1:length(prop_pd_500))
names(PD_prop_500) <- c(1:length(prop_pd_500))

NTI_prop_100_df <- as.data.frame(NTI_prop_100, col.names = names(NTI_prop_100), row.names = row.names(comm_dataset))
NTI_prop_200_df <- as.data.frame(NTI_prop_200, col.names = names(NTI_prop_200), row.names = row.names(comm_dataset))
NTI_prop_300_df <- as.data.frame(NTI_prop_300, col.names = names(NTI_prop_300), row.names = row.names(comm_dataset))
NTI_prop_400_df <- as.data.frame(NTI_prop_400, col.names = names(NTI_prop_400), row.names = row.names(comm_dataset))
NTI_prop_500_df <- as.data.frame(NTI_prop_500, col.names = names(NTI_prop_500), row.names = row.names(comm_dataset))

NRI_prop_100_df <- as.data.frame(NRI_prop_100, col.names = names(NRI_prop_100), row.names = row.names(comm_dataset))
NRI_prop_200_df <- as.data.frame(NRI_prop_200, col.names = names(NRI_prop_200), row.names = row.names(comm_dataset))
NRI_prop_300_df <- as.data.frame(NRI_prop_300, col.names = names(NRI_prop_300), row.names = row.names(comm_dataset))
NRI_prop_400_df <- as.data.frame(NRI_prop_400, col.names = names(NRI_prop_400), row.names = row.names(comm_dataset))
NRI_prop_500_df <- as.data.frame(NRI_prop_500, col.names = names(NRI_prop_500), row.names = row.names(comm_dataset))

PD_prop_100_df <- as.data.frame(PD_prop_100, col.names = names(PD_prop_100), row.names = row.names(comm_dataset))
PD_prop_200_df <- as.data.frame(PD_prop_200, col.names = names(PD_prop_200), row.names = row.names(comm_dataset))
PD_prop_300_df <- as.data.frame(PD_prop_300, col.names = names(PD_prop_300), row.names = row.names(comm_dataset))
PD_prop_400_df <- as.data.frame(PD_prop_400, col.names = names(PD_prop_400), row.names = row.names(comm_dataset))
PD_prop_500_df <- as.data.frame(PD_prop_500, col.names = names(PD_prop_500), row.names = row.names(comm_dataset))

#Add column of community to datasets
#PD_prop_100_df$community <- rownames(PD_prop_100_df)
#PD_prop_200_df$community <- rownames(PD_prop_100_df)
#PD_prop_300_df$community <- rownames(PD_prop_100_df)
#PD_prop_400_df$community <- rownames(PD_prop_100_df)
#PD_prop_500_df$community <- rownames(PD_prop_100_df)

#NRI_prop_100_df$community <- rownames(NRI_prop_100_df)
#NRI_prop_200_df$community <- rownames(NRI_prop_100_df)
#NRI_prop_300_df$community <- rownames(NRI_prop_100_df)
#NRI_prop_400_df$community <- rownames(NRI_prop_100_df)
#NRI_prop_500_df$community <- rownames(NRI_prop_100_df)

#NTI_prop_100_df$community <- rownames(NTI_prop_100_df)
#NTI_prop_200_df$community <- rownames(NTI_prop_100_df)
#NTI_prop_300_df$community <- rownames(NTI_prop_100_df)
#NTI_prop_400_df$community <- rownames(NTI_prop_100_df)
#NTI_prop_500_df$community <- rownames(NTI_prop_100_df)

#Transpose dataframes so replicates are rows and communities are columns
NTI_prop_100_df_tr <- t(NTI_prop_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NTI_prop_200_df_tr <- t(NTI_prop_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NTI_prop_300_df_tr <- t(NTI_prop_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NTI_prop_400_df_tr <- t(NTI_prop_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NTI_prop_500_df_tr <- t(NTI_prop_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

NRI_prop_100_df_tr <- t(NRI_prop_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NRI_prop_200_df_tr <- t(NRI_prop_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NRI_prop_300_df_tr <- t(NRI_prop_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NRI_prop_400_df_tr <- t(NRI_prop_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
NRI_prop_500_df_tr <- t(NRI_prop_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

PD_prop_100_df_tr <- t(PD_prop_100_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_prop_200_df_tr <- t(PD_prop_200_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_prop_300_df_tr <- t(PD_prop_300_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_prop_400_df_tr <- t(PD_prop_400_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_prop_500_df_tr <- t(PD_prop_500_df) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Add column to each dataframe including subset type
NTI_prop_100_df_tr <- cbind(NTI_prop_100_df_tr, "subset" = "100", "type" = "prop")
NTI_prop_200_df_tr <- cbind(NTI_prop_200_df_tr, "subset" = "200", "type" = "prop")
NTI_prop_300_df_tr <- cbind(NTI_prop_300_df_tr, "subset" = "300", "type" = "prop")
NTI_prop_400_df_tr <- cbind(NTI_prop_400_df_tr, "subset" = "400", "type" = "prop")
NTI_prop_500_df_tr <- cbind(NTI_prop_500_df_tr, "subset" = "500", "type" = "prop")

NRI_prop_100_df_tr <- cbind(NRI_prop_100_df_tr, "subset" = "100", "type" = "prop")
NRI_prop_200_df_tr <- cbind(NRI_prop_200_df_tr, "subset" = "200", "type" = "prop")
NRI_prop_300_df_tr <- cbind(NRI_prop_300_df_tr, "subset" = "300", "type" = "prop")
NRI_prop_400_df_tr <- cbind(NRI_prop_400_df_tr, "subset" = "400", "type" = "prop")
NRI_prop_500_df_tr <- cbind(NRI_prop_500_df_tr, "subset" = "500", "type" = "prop")

PD_prop_100_df_tr <- cbind(PD_prop_100_df_tr, "subset" = "100", "type" = "prop")
PD_prop_200_df_tr <- cbind(PD_prop_200_df_tr, "subset" = "200", "type" = "prop")
PD_prop_300_df_tr <- cbind(PD_prop_300_df_tr, "subset" = "300", "type" = "prop")
PD_prop_400_df_tr <- cbind(PD_prop_400_df_tr, "subset" = "400", "type" = "prop")
PD_prop_500_df_tr <- cbind(PD_prop_500_df_tr, "subset" = "500", "type" = "prop")

#Make dataframe with all NRI differences
NTI_prop_all <- data.frame(NTI_prop_100_df_tr)
NTI_prop_all <- rbind(NTI_prop_all, data.frame(NTI_prop_200_df_tr))
NTI_prop_all <- rbind(NTI_prop_all, data.frame(NTI_prop_300_df_tr))
NTI_prop_all <- rbind(NTI_prop_all, data.frame(NTI_prop_400_df_tr))
NTI_prop_all <- rbind(NTI_prop_all, data.frame(NTI_prop_500_df_tr))

NRI_prop_all <- data.frame(NRI_prop_100_df_tr)
NRI_prop_all <- rbind(NRI_prop_all, data.frame(NRI_prop_200_df_tr))
NRI_prop_all <- rbind(NRI_prop_all, data.frame(NRI_prop_300_df_tr))
NRI_prop_all <- rbind(NRI_prop_all, data.frame(NRI_prop_400_df_tr))
NRI_prop_all <- rbind(NRI_prop_all, data.frame(NRI_prop_500_df_tr))

PD_prop_all <- data.frame(PD_prop_100_df_tr)
PD_prop_all <- rbind(PD_prop_all, data.frame(PD_prop_200_df_tr))
PD_prop_all <- rbind(PD_prop_all, data.frame(PD_prop_300_df_tr))
PD_prop_all <- rbind(PD_prop_all, data.frame(PD_prop_400_df_tr))
PD_prop_all <- rbind(PD_prop_all, data.frame(PD_prop_500_df_tr))

#Write file
write.csv(NTI_prop_all, "./Plots/2017_botany/Files_to_plot/Prop_all_NTI.csv", row.names = TRUE)
write.csv(NRI_prop_all, "./Plots/2017_botany/Files_to_plot/Prop_all_NRI.csv", row.names = TRUE)
write.csv(PD_prop_all, "./Plots/2017_botany/Files_to_plot/Prop_all_PD.csv", row.names = TRUE)

#Read files
NTI_prop_all <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NTI.csv", stringsAsFactors = FALSE)
NRI_prop_all <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NRI.csv", stringsAsFactors = FALSE)
PD_prop_all <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_PD.csv", stringsAsFactors = FALSE)

#Melt dataframe for plotting by community
PD_prop_melt <- melt(PD_prop_all, id = c("subset", "type"))
NRI_prop_melt <- melt(NRI_prop_all, id = c("subset", "type"))
NTI_prop_melt <- melt(NTI_prop_all, id = c("subset", "type"))
colnames(PD_prop_melt) <- c("subset", "type", "community", "PD")
colnames(NRI_prop_melt) <- c("subset", "type", "community", "NRI")
colnames(NTI_prop_melt) <- c("subset", "type", "community", "NTI")

#Plot proportions for subsets

png("./Plots/2017_botany/Actual_values/PD_proportional_by_subsets.png")
ggplot()+
  geom_boxplot(data = PD_prop_melt, aes(x = subset, y = PD))+
  ggtitle("PD values for proportional datasets by subset number")+
  facet_wrap( ~ community, ncol = 5, scales = "free_y")+
  xlab("Subset")+
  ylab("PD")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png("./Plots/2017_botany/Actual_values/NRI_proportional_by_subsets.png")
ggplot()+
  geom_boxplot(data = NRI_prop_melt, aes(x = subset, y = NRI))+
  ggtitle("-NRI values for proportional datasets by subset number")+
  facet_wrap( ~ community, ncol = 5, scales = "free_y")+
  xlab("Subset")+
  ylab("-NRI")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png("./Plots/2017_botany/Actual_values/NTI_proportional_by_subsets.png")
ggplot()+
  geom_boxplot(data = NTI_prop_melt, aes(x = subset, y = NTI))+
  ggtitle("-NTI values for proportional datasets by subset number")+
  facet_wrap( ~ community, ncol = 5, scales = "free_y")+
  xlab("Subset")+
  ylab("-NTI")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


#Plot all three combined
png("./Plots/2017_botany/Actual_values/NTI_prop_set_random_by_subsets.png")
ggplot()+
  geom_boxplot(data = NTI_prop_melt, aes(x = subset, y = NTI, color = "Proportional"))+
  geom_boxplot(data = NTI_set_melt, aes(x = subset, y = NTI, color = "Set number"))+
  geom_boxplot(data = NTI_recon_melt, aes(x = subset, y = NTI, color = "Reconstructed"))+
  ggtitle("-NTI values for proportional datasets by subset number")+
  facet_wrap( ~ community, ncol = 5, scales = "free_y")+
  xlab("Subset")+
  ylab("-NTI")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png("./Plots/2017_botany/Actual_values/NRI_prop_set_random_by_subsets.png")
ggplot()+
  geom_boxplot(data = NRI_prop_melt, aes(x = subset, y = NRI, color = "Proportional"))+
  geom_boxplot(data = NRI_set_melt, aes(x = subset, y = NRI, color = "Set number"))+
  geom_boxplot(data = NRI_recon_melt, aes(x = subset, y = NRI, color = "Reconstructed"))+
  ggtitle("-NRI values for proportional datasets by subset number")+
  facet_wrap( ~ community, ncol = 5, scales = "free_y")+
  xlab("Subset")+
  ylab("-NRI")+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


