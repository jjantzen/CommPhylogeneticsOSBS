#Reading reconstructed trees from raxml and rewriting as rooted tre files

#Read in file names
recon_100_list <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_100/", pattern="RAxML", full.names=TRUE)
recon_200_list <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_200/", pattern="RAxML", full.names=TRUE)
recon_300_list <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_300/", pattern="RAxML", full.names=TRUE)
recon_400_list_tre <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_400/", pattern="RAxML", full.names=TRUE)
recon_400_list_nex <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_400/", pattern=".nex", full.names=TRUE)
recon_500_list <- list.files("./Subset_files/Random_subsets/Reconstructed/Cladograms/Trees_500/", pattern="RAxML", full.names=TRUE)

#Combine 400 lists into one list
recon_400_list <- c(recon_400_list_nex, recon_400_list_tre)

#Get names for each tree
tree_number_100 <- strsplit(recon_100_list, "/") %>% 
  sapply(`[`,7) %>% 
  strsplit(split = "[.]")%>% 
  sapply(`[`,2) %>% 
  strsplit("_") %>% 
  sapply(`[`,1) 

tree_number_200 <- strsplit(recon_200_list, "/") %>% 
  sapply(`[`,7) %>% 
  strsplit(split = "[.]")%>% 
  sapply(`[`,3) %>% 
  strsplit("_") %>% 
  sapply(`[`,1) 


tree_number_300 <- strsplit(recon_300_list, "/") %>% 
  sapply(`[`,7) %>% 
  strsplit(split = "[.]")%>% 
  sapply(`[`,3) %>% 
  strsplit("_") %>% 
  sapply(`[`,1) 

tree_number_400 <- strsplit(recon_400_list, "/") %>% 
  sapply(`[`,7) %>% 
  strsplit(split = "[.]")%>% 
  sapply(`[`,3) %>% 
  strsplit("_") %>% 
  sapply(`[`,1) 

tree_number_500 <- strsplit(recon_500_list, "/") %>% 
  sapply(`[`,7) %>% 
  strsplit(split = "[.]")%>% 
  sapply(`[`,3) %>% 
  strsplit("_") %>% 
  sapply(`[`,1) 


tree_name_100 <- paste0("tree", tree_number_100)
tree_name_200 <- paste0("tree", tree_number_200)
tree_name_300 <- paste0("tree", tree_number_300)
tree_name_400 <- paste0("tree", tree_number_400)
tree_name_500 <- paste0("tree", tree_number_500)


#Write function to read tree files
reading_recon_trees <- function(tree){
  tree_obj <- read.tree(tree, comment.char = "@")
}

#Read trees in and set class as multiphylo
recon_trees100 <- lapply(recon_100_list, reading_recon_trees)
recon_trees200 <- lapply(recon_200_list, reading_recon_trees)
recon_trees300 <- lapply(recon_300_list, reading_recon_trees)
recon_trees400_tre <- lapply(recon_400_list_tre, reading_recon_trees)
recon_trees400_nex <- lapply(recon_400_list_nex, read.nexus)
recon_trees400 <- c(recon_trees400_nex, recon_trees400_tre)
recon_trees500 <- lapply(recon_500_list, reading_recon_trees)

class(recon_trees100) <- "multiPhylo"
class(recon_trees200) <- "multiPhylo"
class(recon_trees300) <- "multiPhylo"
class(recon_trees400) <- "multiPhylo"
class(recon_trees500) <- "multiPhylo"

#Set names for each tree to number 
names(recon_trees100) <- tree_name_100
names(recon_trees200) <- tree_name_200
names(recon_trees300) <- tree_name_300
names(recon_trees400) <- tree_name_400
names(recon_trees500) <- tree_name_500

#Get rid of # and sample number in names
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

#Reroot trees using different taxa depending on which taxa included in tree

#Read whole tree for finding outgroups
tree572 <- read.tree("./Basedata_Prep/Ordway_572_reduced_names.tre")
#Set list of ferns from whole tree
ferns <- tips(tree572, mrca(tree572)["Psilotaceae_Psilotum_nudum", "Polypodiaceae_Phlebodium_aureum"])
#Set list of conifers from whole tree
conifers <- tips (tree572, mrca(tree572)["Zamiaceae_Zamia_pumila", "Pinaceae_Pinus_taeda"])

#Make empty list to add trees to
trees <- list()
class(trees) <- "multiPhylo"

#For each tree in list of trees (recon_trees100)
for (i in 1:length(recon_trees100)){
  #Set list of ferns to zero 
  fern_clade <- list()
  #Determine which ferns are in subtree
  for (j in 1:length(ferns)){
    #If  fern from 572 tree is in subtree
    if (ferns[j] %in% recon_trees100[[i]]$tip.label ==TRUE)
      #Add fern to list of ferns in subtree
      fern_clade <- c(fern_clade, ferns[j])
  }
  #Set list of conifers to zero
  conifer_clade <- list()
  #Determine which conifers are in subtree
  for (k in 1:length(conifers)){
    if (conifers[k] %in% recon_trees100[[i]]$tip.label == TRUE)
      #Add conifer to list of conifers in subtree
      conifer_clade <- c(conifer_clade, conifers[k])
  }
  #If Lycopod - root with that
    if ("Lycopodiaceae_Lycopodiella_appressa" %in% recon_trees100[[i]]$tip.label == TRUE) {
      tip_node <- which(recon_trees100[[i]]$tip.label=="Lycopodiaceae_Lycopodiella_appressa")
      rooted <- reroot(recon_trees100[[i]], tip_node, resolve.root = TRUE, position = 0.5*recon_trees100[[i]]$edge.length[which(recon_trees100[[i]]$edge[,2]==tip_node)])
    } 
  #If no Lycopod, root with fern
      else if (length(fern_clade) != 0) {
      rooting_node <- mrca(recon_trees100[[i]])[fern_clade[[1]], fern_clade[[length(fern_clade)]]]
      rooted <- reroot(recon_trees100[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees100[[i]]$edge.length[which(recon_trees100[[i]]$edge[,2]==rooting_node)])
    } #If no fern, root with conifer
      else if (length(conifer_clade) != 0) {
      rooting_node <- mrca(recon_trees100[[i]])[conifer_clade[[1]],  conifer_clade[[length(conifer_clade)]]]
      rooted <- reroot(recon_trees100[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees100[[i]]$edge.length[which(recon_trees100[[i]]$edge[,2]==rooting_node)])
    } #Otherwise, print "need new outgroup"
      else {
      print ("need new outgroup")
      }
  #Add tree to list of trees
  if (i == 1) {
    trees <- c(rooted)
    class(trees) <- "multiPhylo"
  } else {
    trees <- c(trees, rooted)
  }
  #Write rerooted trees to file
  write.tree(rooted, file = paste0("./Subset_files/Reconstructed/Cladograms/Rooted/Trees_100/", names(recon_trees100[i]), ".tre"))
}


#For trees200
#Make empty list to add trees to
trees <- list()
class(trees) <- "multiPhylo"

#FOr each tree in list of trees (recon_trees200)
for (i in 1:length(recon_trees200)){
  #Set list of ferns to zero 
  fern_clade <- list()
  #Determine which ferns are in subtree
  for (j in 1:length(ferns)){
    #If  fern from 572 tree is in subtree
    if (ferns[j] %in% recon_trees200[[i]]$tip.label ==TRUE)
      #Add fern to list of ferns in subtree
      fern_clade <- c(fern_clade, ferns[j])
  }
  #Set list of conifers to zero
  conifer_clade <- list()
  #Determine which conifers are in subtree
  for (k in 1:length(conifers)){
    if (conifers[k] %in% recon_trees200[[i]]$tip.label == TRUE)
      #Add conifer to list of conifers in subtree
      conifer_clade <- c(conifer_clade, conifers[k])
  }
  #If Lycopod - root with that
  if ("Lycopodiaceae_Lycopodiella_appressa" %in% recon_trees200[[i]]$tip.label == TRUE) {
    tip_node <- which(recon_trees200[[i]]$tip.label=="Lycopodiaceae_Lycopodiella_appressa")
    rooted <- reroot(recon_trees200[[i]], tip_node, resolve.root = TRUE, position = 0.5*recon_trees200[[i]]$edge.length[which(recon_trees200[[i]]$edge[,2]==tip_node)])
  } 
  #If no Lycopod, root with fern
  else if (length(fern_clade) != 0) {
    rooting_node <- mrca(recon_trees200[[i]])[fern_clade[[1]], fern_clade[[length(fern_clade)]]]
    rooted <- reroot(recon_trees200[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees200[[i]]$edge.length[which(recon_trees200[[i]]$edge[,2]==rooting_node)])
  } #If no fern, root with conifer
  else if (length(conifer_clade) != 0) {
    rooting_node <- mrca(recon_trees200[[i]])[conifer_clade[[1]],  conifer_clade[[length(conifer_clade)]]]
    rooted <- reroot(recon_trees200[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees200[[i]]$edge.length[which(recon_trees200[[i]]$edge[,2]==rooting_node)])
  } #Otherwise, print "need new outgroup"
  else {
    print ("need new outgroup")
  }
  if (i == 1) {
    trees <- c(rooted)
    class(trees) <- "multiPhylo"
  } else {
    trees <- c(trees, rooted)
  }
  write.tree(rooted, file = paste0("./Subset_files/Reconstructed/Cladograms/Rooted/Trees_200/", names(recon_trees200[i]), ".tre"))
}


#For trees300
#Make empty list to add trees to
trees <- list()
class(trees) <- "multiPhylo"

#FOr each tree in list of trees (recon_trees300)
for (i in 1:length(recon_trees300)){
  #Set list of ferns to zero 
  fern_clade <- list()
  #Determine which ferns are in subtree
  for (j in 1:length(ferns)){
    #If  fern from 572 tree is in subtree
    if (ferns[j] %in% recon_trees300[[i]]$tip.label ==TRUE)
      #Add fern to list of ferns in subtree
      fern_clade <- c(fern_clade, ferns[j])
  }
  #Set list of conifers to zero
  conifer_clade <- list()
  #Determine which conifers are in subtree
  for (k in 1:length(conifers)){
    if (conifers[k] %in% recon_trees300[[i]]$tip.label == TRUE)
      #Add conifer to list of conifers in subtree
      conifer_clade <- c(conifer_clade, conifers[k])
  }
  #If Lycopod - root with that
  if ("Lycopodiaceae_Lycopodiella_appressa" %in% recon_trees300[[i]]$tip.label == TRUE) {
    tip_node <- which(recon_trees300[[i]]$tip.label=="Lycopodiaceae_Lycopodiella_appressa")
    rooted <- reroot(recon_trees300[[i]], tip_node, resolve.root = TRUE, position = 0.5*recon_trees300[[i]]$edge.length[which(recon_trees300[[i]]$edge[,2]==tip_node)])
  } else if (length(fern_clade) != 0) {
  #If no Lycopod, root with fern
    rooting_node <- mrca(recon_trees300[[i]])[fern_clade[[1]], fern_clade[[length(fern_clade)]]]
    rooted <- reroot(recon_trees300[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees300[[i]]$edge.length[which(recon_trees300[[i]]$edge[,2]==rooting_node)])
  } #If no fern, root with conifer
  else if (length(conifer_clade) != 0) {
    rooting_node <- mrca(recon_trees300[[i]])[conifer_clade[[1]],  conifer_clade[[length(conifer_clade)]]]
    rooted <- reroot(recon_trees300[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees300[[i]]$edge.length[which(recon_trees300[[i]]$edge[,2]==rooting_node)])
  } #Otherwise, print "need new outgroup"
  else {
    print ("need new outgroup")
  }
  if (i == 1) {
    trees <- c(rooted)
    class(trees) <- "multiPhylo"
  } else {
    trees <- c(trees, rooted)
  }
  write.tree(rooted, file = paste0("./Subset_files/Reconstructed/Cladograms/Rooted/Trees_300/", names(recon_trees300[i]), ".tre"))
}

#For trees 400
#Make empty list to add trees to
trees <- list()
class(trees) <- "multiPhylo"

#FOr each tree in list of trees (eg recon_trees400)
for (i in 1:length(recon_trees400)){
  #Set list of ferns to zero 
  fern_clade <- list()
  #Determine which ferns are in subtree
  for (j in 1:length(ferns)){
    #If  fern from 572 tree is in subtree
    if (ferns[j] %in% recon_trees400[[i]]$tip.label ==TRUE)
      #Add fern to list of ferns in subtree
      fern_clade <- c(fern_clade, ferns[j])
  }
  #Set list of conifers to zero
  conifer_clade <- list()
  #Determine which conifers are in subtree
  for (k in 1:length(conifers)){
    if (conifers[k] %in% recon_trees400[[i]]$tip.label == TRUE)
      #Add conifer to list of conifers in subtree
      conifer_clade <- c(conifer_clade, conifers[k])
  }
  #If Lycopod - root with that
  if ("Lycopodiaceae_Lycopodiella_appressa" %in% recon_trees400[[i]]$tip.label == TRUE) {
    tip_node <- which(recon_trees400[[i]]$tip.label=="Lycopodiaceae_Lycopodiella_appressa")
    rooted <- reroot(recon_trees400[[i]], tip_node, resolve.root = TRUE, position = 0.5*recon_trees400[[i]]$edge.length[which(recon_trees400[[i]]$edge[,2]==tip_node)])
  } 
  #If no Lycopod, root with fern
  else if (length(fern_clade) != 0) {
    rooting_node <- mrca(recon_trees400[[i]])[fern_clade[[1]], fern_clade[[length(fern_clade)]]]
    rooted <- reroot(recon_trees400[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees400[[i]]$edge.length[which(recon_trees400[[i]]$edge[,2]==rooting_node)])
  } #If no fern, root with conifer
  else if (length(conifer_clade) != 0) {
    rooting_node <- mrca(recon_trees400[[i]])[conifer_clade[[1]],  conifer_clade[[length(conifer_clade)]]]
    rooted <- reroot(recon_trees400[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees400[[i]]$edge.length[which(recon_trees400[[i]]$edge[,2]==rooting_node)])
  } #Otherwise, print "need new outgroup"
  else {
    print ("need new outgroup")
  }
  if (i == 1) {
    trees <- c(rooted)
    class(trees) <- "multiPhylo"
  } else {
    trees <- c(trees, rooted)
  }
  write.tree(rooted, file = paste0("./Subset_files/Reconstructed/Cladograms/Rooted/Trees_400/", names(recon_trees400[i]), ".tre"))
}


#for trees500
#Make empty list to add trees to
trees <- list()
class(trees) <- "multiPhylo"

#FOr each tree in list of trees (eg recon_trees500)
for (i in 1:length(recon_trees500)){
  #Set list of ferns to zero 
  fern_clade <- list()
  #Determine which ferns are in subtree
  for (j in 1:length(ferns)){
    #If  fern from 572 tree is in subtree
    if (ferns[j] %in% recon_trees500[[i]]$tip.label ==TRUE)
      #Add fern to list of ferns in subtree
      fern_clade <- c(fern_clade, ferns[j])
  }
  #Set list of conifers to zero
  conifer_clade <- list()
  #Determine which conifers are in subtree
  for (k in 1:length(conifers)){
    if (conifers[k] %in% recon_trees500[[i]]$tip.label == TRUE)
      #Add conifer to list of conifers in subtree
      conifer_clade <- c(conifer_clade, conifers[k])
  }
  #If Lycopod - root with that
  if ("Lycopodiaceae_Lycopodiella_appressa" %in% recon_trees500[[i]]$tip.label == TRUE) {
    tip_node <- which(recon_trees500[[i]]$tip.label=="Lycopodiaceae_Lycopodiella_appressa")
    rooted <- reroot(recon_trees500[[i]], tip_node, resolve.root = TRUE, position = 0.5*recon_trees500[[i]]$edge.length[which(recon_trees500[[i]]$edge[,2]==tip_node)])
  } 
  #If no Lycopod, root with fern
  else if (length(fern_clade) != 0) {
    rooting_node <- mrca(recon_trees500[[i]])[fern_clade[[1]], fern_clade[[length(fern_clade)]]]
    rooted <- reroot(recon_trees500[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees500[[i]]$edge.length[which(recon_trees500[[i]]$edge[,2]==rooting_node)])
  } #If no fern, root with conifer
  else if (length(conifer_clade) != 0) {
    rooting_node <- mrca(recon_trees500[[i]])[conifer_clade[[1]],  conifer_clade[[length(conifer_clade)]]]
    rooted <- reroot(recon_trees500[[i]], node.number = rooting_node, resolve.root = TRUE, position = 0.5*recon_trees500[[i]]$edge.length[which(recon_trees500[[i]]$edge[,2]==rooting_node)])
  } #Otherwise, print "need new outgroup"
  else {
    print ("need new outgroup")
  }
  if (i == 1) {
    trees <- c(rooted)
    class(trees) <- "multiPhylo"
  } else {
    trees <- c(trees, rooted)
  }
  write.tree(rooted, file = paste0("./Subset_files/Reconstructed/Cladograms/Rooted/Trees_500/", names(recon_trees500[i]), ".tre"))
}

