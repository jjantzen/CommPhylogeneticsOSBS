#Pruning phylogenies to nonrandom proportional subsets

#Read in subset taxa names for each proportional subset
#Get list of files to read
prop_100_list <- list.files("./Subset_files/Nonrandom_subsets/Proportional/For_100/", pattern=".csv", full.names=TRUE)
prop_200_list <- list.files("./Subset_files/Nonrandom_subsets/Proportional/For_200/", pattern=".csv", full.names=TRUE)
prop_300_list <- list.files("./Subset_files/Nonrandom_subsets/Proportional/For_300/", pattern=".csv", full.names=TRUE)
prop_400_list <- list.files("./Subset_files/Nonrandom_subsets/Proportional/For_400/", pattern=".csv", full.names=TRUE)
prop_500_list <- list.files("./Subset_files/Nonrandom_subsets/Proportional/For_500/", pattern=".csv", full.names=TRUE)

#Write function to read files
read_files <- function(file_name){
  file <- read.csv(file_name, stringsAsFactors = FALSE)
}

#Read files in based on list of file names
prop_100 <- lapply(prop_100_list, read_files)
prop_200 <- lapply(prop_200_list, read_files)
prop_300 <- lapply(prop_300_list, read_files)
prop_400 <- lapply(prop_400_list, read_files)
prop_500 <- lapply(prop_500_list, read_files)


#Make new column of whole taxa names
prop_100_names <- list()
prop_200_names <- list()
prop_300_names <- list()
prop_400_names <- list()
prop_500_names <- list()

#Get names from prop subset files in list format 
for (i in 1: length(prop_100)){
  prop_100[[i]]$Name <- paste0(prop_100[[i]]$Family, "_", prop_100[[i]]$Taxa)
  prop_100_names[[i]] <- prop_100[[i]]$Name
}
for (i in 1: length(prop_200)){
  prop_200[[i]]$Name <- paste0(prop_200[[i]]$Family, "_", prop_200[[i]]$Taxa)
  prop_200_names[[i]] <- prop_200[[i]]$Name
}
for (i in 1: length(prop_300)){
  prop_300[[i]]$Name <- paste0(prop_300[[i]]$Family, "_", prop_300[[i]]$Taxa)
  prop_300_names[[i]] <- prop_300[[i]]$Name
}
for (i in 1: length(prop_400)){
  prop_400[[i]]$Name <- paste0(prop_400[[i]]$Family, "_", prop_400[[i]]$Taxa)
  prop_400_names[[i]] <- prop_400[[i]]$Name
}
for (i in 1: length(prop_500)){
  prop_500[[i]]$Name <- paste0(prop_500[[i]]$Family, "_", prop_500[[i]]$Taxa)
  prop_500_names[[i]] <- prop_500[[i]]$Name
}

#Read in 572_tree phylogram
tree572 <- read.tree("./Basedata_Prep/Ordway_full_tree.tre", comment.char = "@")
#Make tip labels match names in subsets
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)

#Prune phylogeny to each subset 
#Write function
prune_tree <- function(taxa){
  prunedtree <- drop.tip(tree572, tree572$tip.label[-match(taxa, tree572$tip.label)])
}

#prune trees 
prop_100_trees <- lapply(prop_100_names, prune_tree)
prop_200_trees <- lapply(prop_200_names, prune_tree)
prop_300_trees <- lapply(prop_300_names, prune_tree)
prop_400_trees <- lapply(prop_400_names, prune_tree)
prop_500_trees <- lapply(prop_500_names, prune_tree)

class(prop_100_trees) <- "multiPhylo"
class(prop_200_trees) <- "multiPhylo"
class(prop_300_trees) <- "multiPhylo"
class(prop_400_trees) <- "multiPhylo"
class(prop_500_trees) <- "multiPhylo"

#Save each phylogeny for calculating PD 
#Write function
write_subset_trees <- function(tree, filename){
  write.tree(tree, filename)
}

#Set names for files to be written
names_of_100 <- gsub(".csv", ".tre",prop_100_list) 
names_of_100 <- gsub("/For_100/", "/Trees_100/", names_of_100)

names_of_200 <- gsub(".csv", ".tre",prop_200_list) 
names_of_200 <- gsub("/For_200/", "/Trees_200/", names_of_200)

names_of_300 <- gsub(".csv", ".tre",prop_300_list) 
names_of_300 <- gsub("/For_300/", "/Trees_300/", names_of_300)

names_of_400 <- gsub(".csv", ".tre",prop_400_list) 
names_of_400 <- gsub("/For_400/", "/Trees_400/", names_of_400)

names_of_500 <- gsub(".csv", ".tre",prop_500_list) 
names_of_500 <- gsub("/For_500/", "/Trees_500/", names_of_500)

#Actually write trees
mapply(write_subset_trees, tree=prop_100_trees, filename=names_of_100)
mapply(write_subset_trees, tree=prop_200_trees, filename=names_of_200)
mapply(write_subset_trees, tree=prop_300_trees, filename=names_of_300)
mapply(write_subset_trees, tree=prop_400_trees, filename=names_of_400)
mapply(write_subset_trees, tree=prop_500_trees, filename=names_of_500)
