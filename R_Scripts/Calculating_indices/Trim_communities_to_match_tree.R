#Trimming community dataset to match tree

library(picante)

comms <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE)


tree <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")

tree$tip.label <- gsub("'", "", tree$tip.label)
tree$tip.label <- gsub("#\\d+_", "", tree$tip.label)

matched <- match.phylo.comm(tree, comms)
matchtree <- matched$phy
matchcomm <- matched$comm 
rownames(matchcomm) <- comms[,1]
matchcomm
write.csv(matchcomm, "./Basedata_Prep/Reduced_comms.csv")


