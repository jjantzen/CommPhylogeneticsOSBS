#Calculate PD indices for pruned trees and calculate difference between prune adn reconstructed trees

#Read pruned cladogram tree files
trees100_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_100/", pattern=".tre", full.names=TRUE)
trees200_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_200/", pattern=".tre", full.names=TRUE)
trees300_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_300/", pattern=".tre", full.names=TRUE)
trees400_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_400/", pattern=".tre", full.names=TRUE)
trees500_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_500/", pattern=".tre", full.names=TRUE)

#Read reconstructed cladogram files
recon_trees100_files <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_100/", pattern = ".tre", full.names=TRUE)
recon_trees200_files <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_200/", pattern = ".tre", full.names=TRUE)
recon_trees300_files <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_300/", pattern = ".tre", full.names=TRUE)
recon_trees400_files <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_400/", pattern = ".tre", full.names=TRUE)
recon_trees500_files <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_500/", pattern = ".tre", full.names=TRUE)

#Create function with right parameters for reading tree
reading_trees <- function(tree){
  tree <- read.tree(tree, comment.char = "@")
}

#Read the trees
trees100 <- lapply(trees100_files, reading_trees)
trees200 <- lapply(trees200_files, reading_trees)
trees300 <- lapply(trees300_files, reading_trees)
trees400 <- lapply(trees400_files, reading_trees)
trees500 <- lapply(trees500_files, reading_trees)
recon_trees100 <- lapply(recon_trees100_files, reading_trees)
recon_trees200 <- lapply(recon_trees200_files, reading_trees)
recon_trees300 <- lapply(recon_trees300_files, reading_trees)
recon_trees400 <- lapply(recon_trees400_files, reading_trees)
recon_trees500 <- lapply(recon_trees500_files, reading_trees)

class(trees100) <- "multiPhylo"
class(trees200) <- "multiPhylo"
class(trees300) <- "multiPhylo"
class(trees400) <- "multiPhylo"
class(trees500) <- "multiPhylo"
class(recon_trees100) <- "multiPhylo"
class(recon_trees200) <- "multiPhylo"
class(recon_trees300) <- "multiPhylo"
class(recon_trees400) <- "multiPhylo"
class(recon_trees500) <- "multiPhylo"

#Correct names in reconstructed tree files - may be able to remove if using rooted reconstructed trees
for (i in 1:length(recon_trees100)){
  recon_trees100[[i]]$tip.label <- gsub("'", "", recon_trees100[[i]]$tip.label)
  recon_trees100[[i]]$tip.label <- gsub("#\\d+_", "", recon_trees100[[i]]$tip.label)
  
}

for (i in 1:length(recon_trees200)){
  recon_trees200[[i]]$tip.label <- gsub("'", "", recon_trees200[[i]]$tip.label)
  recon_trees200[[i]]$tip.label <- gsub("#\\d+_", "", recon_trees200[[i]]$tip.label)
  
}

for (i in 1:length(recon_trees300)){
  recon_trees300[[i]]$tip.label <- gsub("'", "", recon_trees300[[i]]$tip.label)
  recon_trees300[[i]]$tip.label <- gsub("#\\d+_", "", recon_trees300[[i]]$tip.label)
  
}

for (i in 1:length(recon_trees400)){
  recon_trees400[[i]]$tip.label <- gsub("'", "", recon_trees400[[i]]$tip.label)
  recon_trees400[[i]]$tip.label <- gsub("#\\d+_", "", recon_trees400[[i]]$tip.label)
  
}

for (i in 1:length(recon_trees500)){
  recon_trees500[[i]]$tip.label <- gsub("'", "", recon_trees500[[i]]$tip.label)
  recon_trees500[[i]]$tip.label <- gsub("#\\d+_", "", recon_trees500[[i]]$tip.label)
  
}

#Read overall tree file 
tree572 <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)

#Read community data 
comm_designations <- read.csv("./Basedata_Prep/Ordway_communities.csv", header=TRUE, stringsAsFactors = FALSE)
#Get rid of numbers before names
comm_designations$Taxa <- gsub("#\\d+_", "", comm_designations$Taxa)

#Transpose community data
tcomm_designations <- t(comm_designations)
#Add column names
colnames(tcomm_designations) <- tcomm_designations[1,]
#Make as matrix
tcomm_designations <- as.matrix(tcomm_designations, stringsAsFactors = FALSE)

#Write transposed data to file
write.csv(tcomm_designations, "./Basedata_Prep/R_community_designations2.csv", row.names = TRUE)

#Calclate PD using picante
#Need empty list before running function
PD_list <- list()
#Write function that trims taxa to match tree and comms, then calc pd indices, creates list of tables for each replicate within a subset
pd_calc_function <- function(phylo) {
  matched <- match.phylo.comm(phylo, tcomm_designations)
  matchtree <- matched$phy
  matchcomm <- matched$comm 
  phydist <- cophenetic(matched$phy)
  ses_mpd_result <- ses.mpd(matchcomm, phydist, null.model="taxa.labels", abundance.weighted = FALSE, runs=1000)
  ses_mntd_result <- ses.mntd(matchcomm, phydist, null.model="taxa.labels", runs=1000)
  ses_pd_result <-ses.pd(matchcomm, matchtree, null.model="taxa.labels", runs=1000)
  pd_result <- pd(matchcomm, matchtree, include.root = TRUE)
  PD_list[[i]] <- data.frame(pd_result, ses_pd_result, ses_mpd_result, ses_mntd_result)
}

#Apply function to calculate PD for subsets
pd_100 <- lapply(trees100, pd_calc_function)
pd_200 <- lapply(trees200, pd_calc_function)
pd_300 <- lapply(trees300, pd_calc_function)
pd_400 <- lapply(trees400, pd_calc_function)
pd_500 <- lapply(trees500, pd_calc_function)
pd_recon_100 <- lapply(recon_trees100, pd_calc_function)
pd_recon_200 <- lapply(recon_trees200, pd_calc_function)
pd_recon_300 <- lapply(recon_trees300, pd_calc_function)
pd_recon_400 <- lapply(recon_trees400, pd_calc_function)
pd_recon_500 <- lapply(recon_trees500, pd_calc_function)

#Write function to write csv files
write_csv <- function(pd_file, filename){
  write.csv(pd_file, filename)
}

#Get names for writing phylograms
pruned_100subsets_names <- gsub(".tre", ".csv", trees100_files) 
pruned_100subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_100/", "./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_100/PD_", pruned_100subsets_names)

pruned_200subsets_names <- gsub(".tre", ".csv", trees200_files) 
pruned_200subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_200/", "./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_200/PD_", pruned_200subsets_names)

pruned_300subsets_names <- gsub(".tre", ".csv", trees300_files) 
pruned_300subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_300/", "./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_300/PD_", pruned_300subsets_names)

pruned_400subsets_names <- gsub(".tre", ".csv", trees400_files) 
pruned_400subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_400/", "./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_400/PD_", pruned_400subsets_names)

pruned_500subsets_names <- gsub(".tre", ".csv", trees500_files) 
pruned_500subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_500/", "./PD_files/R_calc_picante/Prune_vs_recon/Prune_PD_500/PD_", pruned_500subsets_names)

#Get names for reconstructed phylograms
recon_100subsets_names <- gsub("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_100/", "./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_100/PD_", recon_trees100_files)
recon_100subsets_names <- gsub(".tre$", ".csv", recon_100subsets_names) 

recon_200subsets_names <- gsub("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_200/", "./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_200/PD_", recon_trees200_files)
recon_200subsets_names <- gsub(".tre$", ".csv", recon_200subsets_names) 

recon_300subsets_names <- gsub("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_300/", "./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_300/PD_", recon_trees300_files)
recon_300subsets_names <- gsub(".tre$", ".csv", recon_300subsets_names) 

recon_400subsets_names <- gsub("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_400/", "./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_400/PD_", recon_trees400_files)
recon_400subsets_names <- gsub(".tre$", ".csv", recon_400subsets_names) 

recon_500subsets_names <- gsub("./Subset_files/Random_subsets/Reconstructed/Cladograms/Rooted/Trees_500/", "./PD_files/R_calc_picante/Prune_vs_recon/Recon_PD_500/PD_", recon_trees500_files)
recon_500subsets_names <- gsub(".tre$", ".csv", recon_500subsets_names) 


#Write files
mapply(write_csv, pd_file = pd_100, filename = pruned_100subsets_names)
mapply(write_csv, pd_file = pd_200, filename = pruned_200subsets_names)
mapply(write_csv, pd_file = pd_300, filename = pruned_300subsets_names)
mapply(write_csv, pd_file = pd_400, filename = pruned_400subsets_names)
mapply(write_csv, pd_file = pd_500, filename = pruned_500subsets_names)

mapply(write_csv, pd_file = pd_recon_100, filename = recon_100subsets_names)
mapply(write_csv, pd_file = pd_recon_200, filename = recon_200subsets_names)
mapply(write_csv, pd_file = pd_recon_300, filename = recon_300subsets_names)
mapply(write_csv, pd_file = pd_recon_400, filename = recon_400subsets_names)
mapply(write_csv, pd_file = pd_recon_500, filename = recon_500subsets_names)
