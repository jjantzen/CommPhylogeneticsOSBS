#Calculate PD indices for ultrametric trees and calculate difference between prune phylogram and ultrametric trees
#For subsets and families

#Read pruned phylogram tree files
trees100_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_100/", pattern=".tre", full.names=TRUE)
trees200_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_200/", pattern=".tre", full.names=TRUE)
trees300_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_300/", pattern=".tre", full.names=TRUE)
trees400_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_400/", pattern=".tre", full.names=TRUE)
trees500_files <- list.files("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_500/", pattern=".tre", full.names=TRUE)

#Read pruned ultrametric files
ult_trees100_files <- list.files("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_100/", pattern = ".tre", full.names=TRUE)
ult_trees200_files <- list.files("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_200/", pattern = ".tre", full.names=TRUE)
ult_trees300_files <- list.files("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_300/", pattern = ".tre", full.names=TRUE)
ult_trees400_files <- list.files("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_400/", pattern = ".tre", full.names=TRUE)
ult_trees500_files <- list.files("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_500/", pattern = ".tre", full.names=TRUE)

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
ult_trees100 <- lapply(ult_trees100_files, reading_trees)
ult_trees200 <- lapply(ult_trees200_files, reading_trees)
ult_trees300 <- lapply(ult_trees300_files, reading_trees)
ult_trees400 <- lapply(ult_trees400_files, reading_trees)
ult_trees500 <- lapply(ult_trees500_files, reading_trees)

class(trees100) <- "multiPhylo"
class(trees200) <- "multiPhylo"
class(trees300) <- "multiPhylo"
class(trees400) <- "multiPhylo"
class(trees500) <- "multiPhylo"
class(ult_trees100) <- "multiPhylo"
class(ult_trees200) <- "multiPhylo"
class(ult_trees300) <- "multiPhylo"
class(ult_trees400) <- "multiPhylo"
class(ult_trees500) <- "multiPhylo"

#Read overall tree file 
tree572 <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)

#Read overall tree ultrametric
ult_tree572 <- read.tree("./Basedata_Prep/Ordway572_dated.tre")
ult_tree572$tip.label <- gsub("'", "", ult_tree572$tip.label)
ult_tree572$tip.label <- gsub("#\\d+_", "", ult_tree572$tip.label)

#Read family trees
Asteraceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Asteraceae_tree.tre")
Cyperaceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Cyperaceae_tree.tre")
Ericaceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Ericaceae_tree.tre")
Fabaceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Fabaceae_tree.tre")
Fagaceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Fagaceae_tree.tre")
Poaceae_tree <- read.tree("./Subset_files/By_family/Cladograms/Poaceae_tree.tre")
rosid_tree <- read.tree("./Subset_files/By_family/Cladograms/rosid_tree.tre")

ult_Asteraceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Asteraceae_tree.tre")
ult_Cyperaceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Cyperaceae_tree.tre")
ult_Ericaceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Ericaceae_tree.tre")
ult_Fabaceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Fabaceae_tree.tre")
ult_Fagaceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Fagaceae_tree.tre")
ult_Poaceae_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_Poaceae_tree.tre")
ult_rosid_tree <- read.tree("./Subset_files/By_family/Ultrametric/ult_rosid_tree.tre")

#Read community data 
comm_data <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Calclate PD using picante
#Need empty list before running function
PD_list <- list()
#Write function that trims taxa to match tree and comms
#Then calc pd indices
#Creates list of tables for each replicate within a subset
pd_calc_function <- function(phylo) {
  matched <- match.phylo.comm(phylo, comm_data)
  matchtree <- matched$phy
  matchcomm <- matched$comm 
  phydist <- cophenetic(matched$phy)
  ses_mpd_result <- ses.mpd(matchcomm, phydist, null.model="taxa.labels", abundance.weighted = FALSE, runs=1000)
  ses_mntd_result <- ses.mntd(matchcomm, phydist, null.model="taxa.labels", runs=1000)
  ses_pd_result <-ses.pd(matchcomm, matchtree, null.model="taxa.labels", runs=1000)
  pd_result <- pd(matchcomm, matchtree, include.root = TRUE)
  PD_list <- data.frame(pd_result, ses_pd_result, ses_mpd_result, ses_mntd_result)
}

#Apply function to replicates for each subset
prune_pd_100 <- lapply(trees100, pd_calc_function)
prune_pd_200 <- lapply(trees200, pd_calc_function)
prune_pd_300 <- lapply(trees300, pd_calc_function)
prune_pd_400 <- lapply(trees400, pd_calc_function)
prune_pd_500 <- lapply(trees500, pd_calc_function)
ult_pd_100 <- lapply(ult_trees100, pd_calc_function)
ult_pd_200 <- lapply(ult_trees200, pd_calc_function)
ult_pd_300 <- lapply(ult_trees300, pd_calc_function)
ult_pd_400 <- lapply(ult_trees400, pd_calc_function)
ult_pd_500 <- lapply(ult_trees500, pd_calc_function)
tree572_pd <- pd_calc_function(tree572)
ult_tree572_pd <- pd_calc_function(ult_tree572)
Asteraceae_pd <- pd_calc_function(Asteraceae_tree)
Cyperaceae_pd <- pd_calc_function(Cyperaceae_tree)
Ericaceae_pd <- pd_calc_function(Ericaceae_tree)
Fabaceae_pd <- pd_calc_function(Fabaceae_tree)
Fagaceae_pd <- pd_calc_function(Fagaceae_tree)
Poaceae_pd <- pd_calc_function(Poaceae_tree)
rosid_pd <- pd_calc_function(rosid_tree)
ult_Asteraceae_pd <- pd_calc_function(ult_Asteraceae_tree)
ult_Cyperaceae_pd <- pd_calc_function(ult_Cyperaceae_tree)
ult_Ericaceae_pd <- pd_calc_function(ult_Ericaceae_tree)
ult_Fabaceae_pd <- pd_calc_function(ult_Fabaceae_tree)
ult_Fagaceae_pd <- pd_calc_function(ult_Fagaceae_tree)
ult_Poaceae_pd <- pd_calc_function(ult_Poaceae_tree)
ult_rosid_pd <- pd_calc_function(ult_rosid_tree)

#Write to file - but changes structure of object
write_csv <- function(pd_file, filename){
  write.csv(pd_file, filename)
}

#For phylograms
pruned_100subsets_names <- gsub(".tre", ".csv", trees100_files) 
pruned_100subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_100/", "./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_100/PD_", pruned_100subsets_names)

pruned_200subsets_names <- gsub(".tre", ".csv", trees200_files) 
pruned_200subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_200/", "./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_200/PD_", pruned_200subsets_names)

pruned_300subsets_names <- gsub(".tre", ".csv", trees300_files) 
pruned_300subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_300/", "./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_300/PD_", pruned_300subsets_names)

pruned_400subsets_names <- gsub(".tre", ".csv", trees400_files) 
pruned_400subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_400/", "./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_400/PD_", pruned_400subsets_names)

pruned_500subsets_names <- gsub(".tre", ".csv", trees500_files) 
pruned_500subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Cladograms/Trees_500/", "./PD_files/R_calc_picante/Ult_vs_clad/Clad/Trees_500/PD_", pruned_500subsets_names)

#For ultrametric trees
ult_100subsets_names <- gsub(".tre", ".csv", ult_trees100_files) 
ult_100subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_100/", "./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_100/PD_", ult_100subsets_names)

ult_200subsets_names <- gsub(".tre", ".csv", ult_trees200_files) 
ult_200subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_200/", "./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_200/PD_", ult_200subsets_names)

ult_300subsets_names <- gsub(".tre", ".csv", ult_trees300_files) 
ult_300subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_300/", "./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_300/PD_", ult_300subsets_names)

ult_400subsets_names <- gsub(".tre", ".csv", ult_trees400_files) 
ult_400subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_400/", "./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_400/PD_", ult_400subsets_names)

ult_500subsets_names <- gsub(".tre", ".csv", ult_trees500_files) 
ult_500subsets_names <- gsub("./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_500/", "./PD_files/R_calc_picante/Ult_vs_clad/Ult/Trees_500/PD_", ult_500subsets_names)

#Write files
mapply(write_csv, pd_file = prune_pd_100, filename = pruned_100subsets_names)
mapply(write_csv, pd_file = prune_pd_200, filename = pruned_200subsets_names)
mapply(write_csv, pd_file = prune_pd_300, filename = pruned_300subsets_names)
mapply(write_csv, pd_file = prune_pd_400, filename = pruned_400subsets_names)
mapply(write_csv, pd_file = prune_pd_500, filename = pruned_500subsets_names)

mapply(write_csv, pd_file = ult_pd_100, filename = ult_100subsets_names)
mapply(write_csv, pd_file = ult_pd_200, filename = ult_200subsets_names)
mapply(write_csv, pd_file = ult_pd_300, filename = ult_300subsets_names)
mapply(write_csv, pd_file = ult_pd_400, filename = ult_400subsets_names)
mapply(write_csv, pd_file = ult_pd_500, filename = ult_500subsets_names)

write.csv(tree572_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv")
write.csv(ult_tree572_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_tree572_ult.csv")
write.csv(Asteraceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv")
write.csv(Cyperaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv")
write.csv(Ericaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv")
write.csv(Fabaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv")
write.csv(Fagaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv")
write.csv(Poaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv")
write.csv(rosid_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv")
write.csv(ult_Asteraceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Asteraceae_ult.csv")
write.csv(ult_Cyperaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Cyperaceae_ult.csv")
write.csv(ult_Ericaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Ericaceae_ult.csv")
write.csv(ult_Fabaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fabaceae_ult.csv")
write.csv(ult_Fagaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Fagaceae_ult.csv")
write.csv(ult_Poaceae_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_Poaceae_ult.csv")
write.csv(ult_rosid_pd, "./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_rosid_ult.csv")
