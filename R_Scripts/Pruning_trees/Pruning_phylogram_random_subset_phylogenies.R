#Pruning random subset phylogenies from 572 tree

#Read in subset taxa names
subset100_list <- list.files("../Subset_analysis_files/100_taxon_subsets/", pattern=".fasta", full.names=TRUE)
subset200_list <- list.files("../Subset_analysis_files/200_taxon_subsets/Subsets/", pattern=".fasta", full.names=TRUE)
subset300_list <- list.files("../Subset_analysis_files/300_taxon_subsets/Subsets/", pattern=".fasta", full.names=TRUE)
subset400_list <- list.files("../Subset_analysis_files/400_taxon_subsets/Subsets/", pattern=".fasta", full.names=TRUE)
subset500_list <- list.files("../Subset_analysis_files/500_taxon_subsets/Subsets/", pattern=".fasta", full.names=TRUE)

#Write function to get names from fasta file
get_names <- function(file){
  names <- read.fasta(file) %>% 
    names()
  names <- gsub("#\\d+_", "", names)
}

#Run function for each file for each subset to get names
subset100_all_taxa <- lapply(subset100_list, get_names)
subset200_all_taxa <- lapply(subset200_list, get_names)
subset300_all_taxa <- lapply(subset300_list, get_names)
subset400_all_taxa <- lapply(subset400_list, get_names)
subset500_all_taxa <- lapply(subset500_list, get_names)

#Read in 572_tree phylogram
tree572 <- read.tree("./Basedata_Prep/Ordway_full_tree.tre", comment.char = "@")
#Match tip labels on tree to name format in subset lists
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)
#Write tree with matched names
write.tree(tree572, "./Basedata_Prep/Ordway_572_reduced_names.tre")

#Prune phylogeny to each subset

#Write function to prune phylogeny
prune_tree <- function(taxa){
  prunedtree <- drop.tip(tree572,tree572$tip.label[-match(taxa, tree572$tip.label)])
}

#Prune trees
all_100_trees <- lapply(subset100_all_taxa, prune_tree)
all_200_trees <- lapply(subset200_all_taxa, prune_tree)
all_300_trees <- lapply(subset300_all_taxa, prune_tree)
all_400_trees <- lapply(subset400_all_taxa, prune_tree)
all_500_trees <- lapply(subset500_all_taxa, prune_tree)

#set class of trees
class(all_100_trees) <- "multiPhylo"
class(all_200_trees) <- "multiPhylo"
class(all_300_trees) <- "multiPhylo"
class(all_400_trees) <- "multiPhylo"
class(all_500_trees) <- "multiPhylo"

#Save each phylogeny for calculating PD

#Write function to write trees
write_subset_trees <- function(tree, filename){
  write.tree(tree, filename)
}

#Set names for files to be written
names_of_100subsets <- gsub(".fasta", ".tre",subset100_list) 
names_of_100subsets <- gsub("../Subset_analysis_files/100_taxon_subsets", "./Subset_files/Pruned/Cladograms/Trees_100", names_of_100subsets)

names_of_200subsets <- gsub(".fasta", ".tre",subset200_list) 
names_of_200subsets <- gsub("../Subset_analysis_files/200_taxon_subsets/Subsets", "./Subset_files/Pruned/Cladograms/Trees_200", names_of_200subsets)

names_of_300subsets <- gsub(".fasta", ".tre",subset300_list) 
names_of_300subsets <- gsub("../Subset_analysis_files/300_taxon_subsets/Subsets", "./Subset_files/Pruned/Cladograms/Trees_300", names_of_300subsets)

names_of_400subsets <- gsub(".fasta", ".tre",subset400_list) 
names_of_400subsets <- gsub("../Subset_analysis_files/400_taxon_subsets/Subsets", "./Subset_files/Pruned/Cladograms/Trees_400", names_of_400subsets)

names_of_500subsets <- gsub(".fasta", ".tre",subset500_list) 
names_of_500subsets <- gsub("../Subset_analysis_files/500_taxon_subsets/Subsets", "./Subset_files/Pruned/Cladograms/Trees_500", names_of_500subsets)

#Actually write trees to folders
mapply(write_subset_trees, tree=all_100_trees, filename=names_of_100subsets)
mapply(write_subset_trees, tree=all_200_trees, filename=names_of_200subsets)
mapply(write_subset_trees, tree=all_300_trees, filename=names_of_300subsets)
mapply(write_subset_trees, tree=all_400_trees, filename=names_of_400subsets)
mapply(write_subset_trees, tree=all_500_trees, filename=names_of_500subsets)

