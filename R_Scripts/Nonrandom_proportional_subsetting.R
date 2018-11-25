#Non-randomly subsetting taxa by proportion

prop_rep <- read.csv("../Names/Proportional_representation.csv", stringsAsFactors = FALSE)

taxa_list <- read.csv("../Names/Taxa_names_572.csv", stringsAsFactors = FALSE, header= TRUE, col.names = c("Number", "Family", "Taxa"))

taxa_list$Name <- paste0(taxa_list$Family, "_", taxa_list$Taxa)

###100
#Create dataframe for subset taxa to be written to 
reps_100 <- list()

for (n in 1:100) {
  taxa_for_100 <- data.frame()
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
    if (prop_rep$For.100[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$For.100[i])
      taxa_for_100 <- rbind(taxa_for_100, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    }
  }
  reps_100[[n]] <- taxa_for_100
}

#write each table in reps to file
names(reps_100) <- c(1:100)
lapply(1:length(reps_100), function(i) write.csv(reps_100[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Proportional/For_100/Taxa_for_100_", names(reps_100[i]), ".csv"), row.names = FALSE))

###200
#Create dataframe for subset taxa to be written to 
reps_200 <- list()

for (n in 1:100) {
  taxa_for_200 <- data.frame()
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
    if (prop_rep$For.200[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$For.200[i])
      taxa_for_200 <- rbind(taxa_for_200, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    }
  }
  reps_200[[n]] <- taxa_for_200
}

#write each table in reps to file
names(reps_200) <- c(1:100)
lapply(1:length(reps_200), function(i) write.csv(reps_200[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Proportional/For_200/Taxa_for_200_", names(reps_200[i]), ".csv"), row.names = FALSE))

###300
#Create dataframe for subset taxa to be written to 
reps_300 <- list()

for (n in 1:100) {
  taxa_for_300 <- data.frame()
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
    if (prop_rep$For.300[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$For.300[i])
      taxa_for_300 <- rbind(taxa_for_300, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    }
  }
  reps_300[[n]] <- taxa_for_300
}

#write each table in reps to file
names(reps_300) <- c(1:100)
lapply(1:length(reps_300), function(i) write.csv(reps_300[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Proportional/For_300/Taxa_for_300_", names(reps_300[i]), ".csv"), row.names = FALSE))

###400
#Create dataframe for subset taxa to be written to 
reps_400 <- list()

for (n in 1:100) {
  taxa_for_400 <- data.frame()
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
    if (prop_rep$For.400[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$For.400[i])
      taxa_for_400 <- rbind(taxa_for_400, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    }
  }
  reps_400[[n]] <- taxa_for_400
}

#write each table in reps to file
names(reps_400) <- c(1:100)
lapply(1:length(reps_400), function(i) write.csv(reps_400[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Proportional/For_400/Taxa_for_400_", names(reps_400[i]), ".csv"), row.names = FALSE))

###500
#Create dataframe for subset taxa to be written to 
reps_500 <- list()

for (n in 1:100) {
  taxa_for_500 <- data.frame()
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
    if (prop_rep$For.500[i] > 0) {
      subset <- sample(matching_taxa$Taxa, prop_rep$For.500[i])
      taxa_for_500 <- rbind(taxa_for_500, data.frame("Family" = prop_rep$Family[i], "Taxa" = subset))
      #Add subset to larger dataframe for saving 
    }
  }
  reps_500[[n]] <- taxa_for_500
}

#write each table in reps to file
names(reps_500) <- c(1:100)
lapply(1:length(reps_500), function(i) write.csv(reps_500[[i]], file = paste0("./Subset_files/Nonrandom_subsets/Proportional/For_500/Taxa_for_500_", names(reps_500[i]), ".csv"), row.names = FALSE))
