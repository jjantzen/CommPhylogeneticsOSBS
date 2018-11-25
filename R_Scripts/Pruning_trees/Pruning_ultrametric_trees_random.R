#Pruning ultrametric phylogeny to random subsets

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

#Run function for each file for each subset
subset100_all_taxa <- lapply(subset100_list, get_names)
subset200_all_taxa <- lapply(subset200_list, get_names)
subset300_all_taxa <- lapply(subset300_list, get_names)
subset400_all_taxa <- lapply(subset400_list, get_names)
subset500_all_taxa <- lapply(subset500_list, get_names)

#Read in 572_tree ultrametric tree
tree572 <- read.tree("./Basedata_Prep/Ordway572_dated.tre")
#Match tip labels
tree572$tip.label <- gsub("'", "", tree572$tip.label)
tree572$tip.label <- gsub("#\\d+_", "", tree572$tip.label)

#Prune phylogeny to each subset 
prune_tree <- function(taxa){
  prunedtree <- drop.tip(tree572,tree572$tip.label[-match(taxa, tree572$tip.label)])
}

all_100_trees <- lapply(subset100_all_taxa, uprune_tree)
all_200_trees <- lapply(subset200_all_taxa, prune_tree)
all_300_trees <- lapply(subset300_all_taxa, prune_tree)
all_400_trees <- lapply(subset400_all_taxa, prune_tree)
all_500_trees <- lapply(subset500_all_taxa, prune_tree)


class(all_100_trees) <- "multiPhylo"
class(all_200_trees) <- "multiPhylo"
class(all_300_trees) <- "multiPhylo"
class(all_400_trees) <- "multiPhylo"
class(all_500_trees) <- "multiPhylo"

#Save each phylogeny for calculating PD in Biodiverse
write_subset_trees <- function(tree, filename){
  write.tree(tree, filename)
}

#Set names for files to be written
names_of_100subsets <- gsub(".fasta", ".tre",subset100_list) 
names_of_100subsets <- gsub("../Subset_analysis_files/100_taxon_subsets", "./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_100", names_of_100subsets)

names_of_200subsets <- gsub(".fasta", ".tre",subset200_list) 
names_of_200subsets <- gsub("../Subset_analysis_files/200_taxon_subsets/Subsets", "./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_200", names_of_200subsets)

names_of_300subsets <- gsub(".fasta", ".tre",subset300_list) 
names_of_300subsets <- gsub("../Subset_analysis_files/300_taxon_subsets/Subsets", "./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_300", names_of_300subsets)

names_of_400subsets <- gsub(".fasta", ".tre",subset400_list) 
names_of_400subsets <- gsub("../Subset_analysis_files/400_taxon_subsets/Subsets", "./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_400", names_of_400subsets)

names_of_500subsets <- gsub(".fasta", ".tre",subset500_list) 
names_of_500subsets <- gsub("../Subset_analysis_files/500_taxon_subsets/Subsets", "./Subset_files/Random_subsets/Pruned/Ultrametric/Trees_500", names_of_500subsets)

#Actually write trees

mapply(write_subset_trees, tree=all_100_trees, filename=names_of_100subsets)
mapply(write_subset_trees, tree=all_200_trees, filename=names_of_200subsets)
mapply(write_subset_trees, tree=all_300_trees, filename=names_of_300subsets)
mapply(write_subset_trees, tree=all_400_trees, filename=names_of_400subsets)
mapply(write_subset_trees, tree=all_500_trees, filename=names_of_500subsets)
