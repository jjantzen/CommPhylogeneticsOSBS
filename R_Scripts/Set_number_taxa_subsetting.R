#Nonrandom_set_number_subsetting

prop_rep <- read.csv("../Names/Proportional_representation.csv", stringsAsFactors = FALSE)

taxa_list <- read.csv("../Names/Taxa_names_572.csv", stringsAsFactors = FALSE, header= TRUE, col.names = c("Number", "Family", "Taxa"))

taxa_list$Name <- paste0(taxa_list$Family, "_", taxa_list$Taxa)

###105
#Create dataframe for subset taxa to be written to 
set_105 <- list()

for (n in 1:100) {
  taxa_for_105 <- data.frame()
  #Create for loop to run through all families
  for (i in 1:nrow(prop_rep)){
    #Create empty dataframe for each family
    matching_taxa <- data.frame()
    #Create for loop to go through each taxon to add to list for family
    for (j in 1:nrow(taxa_list)){
      #Check to see if family matches current family
      if (prop_rep$Family[i] == taxa_list$Family[j]){
        #Add matching family taxa
        matching_taxa <- rbind(matching_taxa, taxa_list[j,])
      }
    }
    #Choose random subset of n (according to family number) from family taxa
    if (prop_rep$Total105[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$Total105[i])
      taxa_for_105 <- rbind(taxa_for_105, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    } else {
      taxa_for_105 <- taxa_for_105
    }
  }
  set_105[[n]] <- taxa_for_105
}


#write each table in reps to file
names(set_105) <- c(1:100)
lapply(1:length(set_105), function(i) write.csv(set_105[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Set_number/For_105/Taxa_for_105_", names(set_105[i]), ".csv"), row.names = FALSE))

###182
#Create dataframe for subset taxa to be written to 
set_182 <- list()

for (n in 1:100) {
  taxa_for_182 <- data.frame()
  #Create for loop to run through all families
  for (i in 1:nrow(prop_rep)){
    #Create empty dataframe for each family
    matching_taxa <- data.frame()
    #Create for loop to go through each taxon to add to list for family
    for (j in 1:nrow(taxa_list)){
      #Check to see if family matches current family
      if (prop_rep$Family[i] == taxa_list$Family[j]){
        #Add matching family taxa
        matching_taxa <- rbind(matching_taxa, taxa_list[j,])
      }
    }
    #Choose random subset of n (according to family number) from family taxa
    if (prop_rep$Total182[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$Total182[i])
      taxa_for_182 <- rbind(taxa_for_182, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    } else {
      taxa_for_182 <- taxa_for_182
    }
  }
  set_182[[n]] <- taxa_for_182
}

#write each table in reps to file
names(set_182) <- c(1:100)
lapply(1:length(set_182), function(i) write.csv(set_182[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Set_number/For_182/Taxa_for_182_", names(set_182[i]), ".csv"), row.names = FALSE))

###292
#Create dataframe for subset taxa to be written to 
set_292 <- list()

for (n in 1:100) {
  taxa_for_292 <- data.frame()
  #Create for loop to run through all families
  for (i in 1:nrow(prop_rep)){
    #Create empty dataframe for each family
    matching_taxa <- data.frame()
    #Create for loop to go through each taxon to add to list for family
    for (j in 1:nrow(taxa_list)){
      #Check to see if family matches current family
      if (prop_rep$Family[i] == taxa_list$Family[j]){
        #Add matching family taxa
        matching_taxa <- rbind(matching_taxa, taxa_list[j,])
      }
    }
    #Choose random subset of n (according to family number) from family taxa
    if (prop_rep$Total292[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$Total292[i])
      taxa_for_292 <- rbind(taxa_for_292, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    } else {
      taxa_for_292 <- taxa_for_292
    }
  }
  set_292[[n]] <- taxa_for_292
}

#write each table in reps to file
names(set_292) <- c(1:100)
lapply(1:length(set_292), function(i) write.csv(set_292[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Set_number/For_292/Taxa_for_292_", names(set_292[i]), ".csv"), row.names = FALSE))

###332
#Create dataframe for subset taxa to be written to 
set_332 <- list()

for (n in 1:100) {
  taxa_for_332 <- data.frame()
  #Create for loop to run through all families
  for (i in 1:nrow(prop_rep)){
    #Create empty dataframe for each family
    matching_taxa <- data.frame()
    #Create for loop to go through each taxon to add to list for family
    for (j in 1:nrow(taxa_list)){
      #Check to see if family matches current family
      if (prop_rep$Family[i] == taxa_list$Family[j]){
        #Add matching family taxa
        matching_taxa <- rbind(matching_taxa, taxa_list[j,])
      }
    }
    #Choose random subset of n (according to family number) from family taxa
    if (prop_rep$Total332[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$Total332[i])
      taxa_for_332 <- rbind(taxa_for_332, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    } else {
      taxa_for_332 <- taxa_for_332
    }
  }
  set_332[[n]] <- taxa_for_332
}

#write each table in reps to file
names(set_332) <- c(1:100)
lapply(1:length(set_332), function(i) write.csv(set_332[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Set_number/For_332/Taxa_for_332_", names(set_332[i]), ".csv"), row.names = FALSE))

###264
#Create dataframe for subset taxa to be written to 
set_264 <- list()

for (n in 1:100) {
  taxa_for_264 <- data.frame()
  #Create for loop to run through all families
  for (i in 1:nrow(prop_rep)){
    #Create empty dataframe for each family
    matching_taxa <- data.frame()
    #Create for loop to go through each taxon to add to list for family
    for (j in 1:nrow(taxa_list)){
      #Check to see if family matches current family
      if (prop_rep$Family[i] == taxa_list$Family[j]){
        #Add matching family taxa
        matching_taxa <- rbind(matching_taxa, taxa_list[j,])
      }
    }
    #Choose random subset of n (according to family number) from family taxa
    if (prop_rep$Total264[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$Total264[i])
      taxa_for_264 <- rbind(taxa_for_264, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    } else {
      taxa_for_264 <- taxa_for_264
    }
  }
  set_264[[n]] <- taxa_for_264
}

#write each table in reps to file
names(set_264) <- c(1:100)
lapply(1:length(set_264), function(i) write.csv(set_264[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Set_number/For_264/Taxa_for_264_", names(set_264[i]), ".csv"), row.names = FALSE))
