#Pruning both phylogram and chronogram
#Prune by family/clade
#Only for Asteraceae, Poaceae, Ericaceae, Cyperaceae, Fabaceae, Fagaceae, Rosids

prop_rep <- read.csv("../Names/Proportional_representation.csv", stringsAsFactors = FALSE)

taxa_list <- read.csv("../Names/Taxa_names_572.csv", stringsAsFactors = FALSE, header= TRUE, col.names = c("Number", "Family", "Taxa"))

taxa_list$Name <- paste0(taxa_list$Family, "_", taxa_list$Taxa)

families_list <- prop_rep[1:6,1:2]

#Read trees to prune 

#Phylogram with matched names
tree572 <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")

#Ultrametric tree
ult_tree572 <- read.tree("./Basedata_Prep/Ordway572_dated.tre")
#Match names for ultrametric tree
ult_tree572$tip.label <- gsub("'", "", ult_tree572$tip.label)
ult_tree572$tip.label <- gsub("#\\d+_", "", ult_tree572$tip.label)
#Not saving ultrametric tree with short names - is it saved elsewhere?

#Get taxa for each clade (looking at tree to determine mrca)
rosids <- tips(tree572, mrca(tree572)["Sapindaceae_Acer_rubrum", "Fabaceae_Dalea_pinnata"])

Asteraceae <- tips(tree572, mrca(tree572)["Asteraceae_Liatris_gracilis", "Asteraceae_Cirsium_nuttallii"])

Poaceae <- tips(tree572, mrca(tree572)["Poaceae_Panicum_commutatum", "Poaceae_Poa_annua"])

Cyperaceae <- tips(tree572, mrca(tree572)["Cyperaceae_Cyperus_croceus", "Cyperaceae_Scleria_reticularis"])

Ericaceae <- tips(tree572, mrca(tree572)["Ericaceae_Vaccinium_arboreum", "Ericaceae_Rhododendron_simsii"])

Fabaceae <- tips(tree572, mrca(tree572)["Fabaceae_Desmodium_floridanum", "Fabaceae_Mimosa_quadrivalvis"])

Fagaceae <- tips(tree572, mrca(tree572)["Fagaceae_Quercus_myrtifolia", "Fagaceae_Quercus_chapmanii"])

#Prune phylogram to each subset of clades

#write function
prune_tree <- function(taxa){
  prunedtree <- drop.tip(tree572, tree572$tip.label[-match(taxa, tree572$tip.label)])
}

#prune phylogram to clade
Asteraceae_tree <- prune_tree(Asteraceae)
Poaceae_tree <- prune_tree(Poaceae)
Cyperaceae_tree <- prune_tree(Cyperaceae)
Ericaceae_tree <- prune_tree(Ericaceae)
Fabaceae_tree <- prune_tree(Fabaceae)
Fagaceae_tree <- prune_tree(Fagaceae)
rosid_tree <- prune_tree(rosids)

#Prune ultrametric phylogeny

#Write function
ult_prune_tree <- function(taxa){
  prunedtree <- drop.tip(ult_tree572, ult_tree572$tip.label[-match(taxa, ult_tree572$tip.label)])
}

#prune chronogram to clade
ult_Asteraceae_tree <- ult_prune_tree(Asteraceae)
ult_Poaceae_tree <- ult_prune_tree(Poaceae)
ult_Cyperaceae_tree <- ult_prune_tree(Cyperaceae)
ult_Ericaceae_tree <- ult_prune_tree(Ericaceae)
ult_Fabaceae_tree <- ult_prune_tree(Fabaceae)
ult_Fagaceae_tree <- ult_prune_tree(Fagaceae)
ult_rosid_tree <- ult_prune_tree(rosids)

#Write trees - both phylogram and chronogram
write.tree(Asteraceae_tree, file = "./Subset_files/By_family/Cladograms/Asteraceae_tree.tre")
write.tree(Poaceae_tree, file = "./Subset_files/By_family/Cladograms/Poaceae_tree.tre")
write.tree(Cyperaceae_tree, file = "./Subset_files/By_family/Cladograms/Cyperaceae_tree.tre")
write.tree(Ericaceae_tree, file = "./Subset_files/By_family/Cladograms/Ericaceae_tree.tre")
write.tree(Fabaceae_tree, file = "./Subset_files/By_family/Cladograms/Fabaceae_tree.tre")
write.tree(Fagaceae_tree, file = "./Subset_files/By_family/Cladograms/Fagaceae_tree.tre")
write.tree(rosid_tree, file = "./Subset_files/By_family/Cladograms/rosid_tree.tre")
write.tree(ult_Asteraceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Asteraceae_tree.tre")
write.tree(ult_Poaceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Poaceae_tree.tre")
write.tree(ult_Cyperaceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Cyperaceae_tree.tre")
write.tree(ult_Ericaceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Ericaceae_tree.tre")
write.tree(ult_Fabaceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Fabaceae_tree.tre")
write.tree(ult_Fagaceae_tree, file = "./Subset_files/By_family/Ultrametric/ult_Fagaceae_tree.tre")
write.tree(ult_rosid_tree, file = "./Subset_files/By_family/Ultrametric/ult_rosid_tree.tre")
