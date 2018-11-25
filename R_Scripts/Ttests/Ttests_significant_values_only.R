#Significant only values for chronograms etc

#Read files with dataframes to plot
#####
Ult_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
Ult_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
Ult_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/PD_ult_index_pvalue.csv", stringsAsFactors = FALSE)

Random_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
Random_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
Random_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

Recon_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
Recon_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
Recon_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/PD_recon_index_pvalue.csv", stringsAsFactors = FALSE)

#Read community data
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Read full tree pd values
ult_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_tree572_ult.csv", stringsAsFactors = FALSE)
clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Make dataframe for indices - change to ult or clad
full_tree_values <- as.data.frame(clad_572$mpd.obs.z)
full_tree_values$NTI <- clad_572$mntd.obs.z
full_tree_values$PD <- clad_572$pd.obs.z

#Add row and column names
rownames(full_tree_values) <- rownames(comm_dataset)
colnames(full_tree_values) <- c("full_NRI", "full_NTI", "full_PD")
full_tree_values

#Remove 
full_tree_values <- full_tree_values[-1,]

#Make column for communities
full_tree_values$variable <- rownames(full_tree_values)

#Melt data to get column with index values, with index type and column of communities
full_tree_values <- melt(full_tree_values, id = c("variable"))

#Change column names
colnames(full_tree_values) <- c("variable", "type_index", "index")

#Make subset and reps columns
full_tree_values$subset <- "572"
full_tree_values$reps <- "1"
full_tree_values
#Separate dataframe into three based on index
full_tree_NRI <- full_tree_values[which(full_tree_values$type_index == "full_NRI"),c(1,3:5)]
full_tree_NTI <- full_tree_values[which(full_tree_values$type_index == "full_NTI"),c(1,3:5)]
full_tree_PD <- full_tree_values[which(full_tree_values$type_index == "full_PD"),c(1,3:5)]

#Keep only significant values
Ult_all_NRI_sig <- Ult_all_NRI[which(Ult_all_NRI$pvalue >= 0.95 | Ult_all_NRI$pvalue <= 0.05),]
Ult_all_NTI_sig <- Ult_all_NTI[which(Ult_all_NTI$pvalue >= 0.95 | Ult_all_NTI$pvalue <= 0.05),]
Ult_all_PD_sig <- Ult_all_PD[which(Ult_all_PD$pvalue >= 0.95 | Ult_all_PD$pvalue <= 0.05),]

Random_all_NRI_sig <- Random_all_NRI[which(Random_all_NRI$pvalue >= 0.95 | Random_all_NRI$pvalue <= 0.05),]
Random_all_NTI_sig <- Random_all_NTI[which(Random_all_NTI$pvalue >= 0.95 | Random_all_NTI$pvalue <= 0.05),]
Random_all_PD_sig <- Random_all_PD[which(Random_all_PD$pvalue >= 0.95 | Random_all_PD$pvalue <= 0.05),]

Recon_all_NRI_sig <- Recon_all_NRI[which(Recon_all_NRI$pvalue >= 0.95 | Recon_all_NRI$pvalue <= 0.05),]
Recon_all_NTI_sig <- Recon_all_NTI[which(Recon_all_NTI$pvalue >= 0.95 | Recon_all_NTI$pvalue <= 0.05),]
Recon_all_PD_sig <- Recon_all_PD[which(Recon_all_PD$pvalue >= 0.95 | Recon_all_PD$pvalue <= 0.05),]

#Add overall to each dataset
Ult_all_NRI_sig <- rbind.fill(Ult_all_NRI_sig, full_tree_NRI)
Random_all_NRI_sig <- rbind.fill(Random_all_NRI_sig, full_tree_NRI)
Recon_all_NRI_sig <- rbind.fill(Recon_all_NRI_sig, full_tree_NRI)


Ult_all_NTI_sig <- rbind.fill(Ult_all_NTI_sig, full_tree_NTI)
Random_all_NTI_sig <- rbind.fill(Random_all_NTI_sig, full_tree_NTI)
Recon_all_NTI_sig <- rbind.fill(Recon_all_NTI_sig, full_tree_NTI)


Ult_all_PD_sig <- rbind.fill(Ult_all_PD_sig, full_tree_PD)
Random_all_PD_sig <- rbind.fill(Random_all_PD_sig, full_tree_PD)
Recon_all_PD_sig <- rbind.fill(Recon_all_PD_sig, full_tree_PD)


#Remove pvalue columns
Ult_all_NRI_sig <- Ult_all_NRI_sig[,-5]
Ult_all_NTI_sig <- Ult_all_NTI_sig[,-5]
Ult_all_PD_sig <- Ult_all_PD_sig[,-5]

Random_all_NRI_sig <- Random_all_NRI_sig[,-5]
Random_all_NTI_sig <- Random_all_NTI_sig[,-5]
Random_all_PD_sig <- Random_all_PD_sig[,-5]

Recon_all_NRI_sig <- Recon_all_NRI_sig[,-5]
Recon_all_NTI_sig <- Recon_all_NTI_sig[,-5]
Recon_all_PD_sig <- Recon_all_PD_sig[,-5]

#Get average value for each subset number and each community for ttests
ttest_NRI_values <- data.frame(community = character((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), subset = character((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))))
ttest_NRI_values$community <- c(unique(Ult_all_NRI_sig$variable))
ttest_NRI_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NRI_values) <- paste0(ttest_NRI_values$community, "_", ttest_NRI_values$subset)
ttest_NRI_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NRI_values)){
  j <- ttest_NRI_values$community[k]
  i <- ttest_NRI_values$subset[k]
  data_dist <- Ult_all_NRI_sig[which(Ult_all_NRI_sig$subset == i & Ult_all_NRI_sig$variable == j),4]
  mu <- Ult_all_NRI_sig[which(Ult_all_NRI_sig$subset == "572" & Ult_all_NRI_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NRI_values[k,3] <- t_test_values[1]
  ttest_NRI_values[k,4] <- t_test_values[2]
}

write.csv(ttest_NRI_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NRI_ult.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_NTI_values <- data.frame(community = character((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), subset = character((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))))
ttest_NTI_values$community <- c(unique(Ult_all_NTI_sig$variable))
ttest_NTI_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NTI_values) <- paste0(ttest_NTI_values$community, "_", ttest_NTI_values$subset)
ttest_NTI_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NTI_values)){
  j <- ttest_NTI_values$community[k]
  i <- ttest_NTI_values$subset[k]
  data_dist <- Ult_all_NTI_sig[which(Ult_all_NTI_sig$subset == i & Ult_all_NTI_sig$variable == j),4]
  mu <- Ult_all_NTI_sig[which(Ult_all_NTI_sig$subset == "572" & Ult_all_NTI_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NTI_values[k,3] <- t_test_values[1]
  ttest_NTI_values[k,4] <- t_test_values[2]
}

write.csv(ttest_NTI_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NTI_ult.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_PD_values <- data.frame(community = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), subset = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))))
ttest_PD_values$community <- c(unique(Ult_all_PD_sig$variable))
ttest_PD_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_PD_values) <- paste0(ttest_PD_values$community, "_", ttest_PD_values$subset)
ttest_PD_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_PD_values)){
  j <- ttest_PD_values$community[k]
  i <- ttest_PD_values$subset[k]
  data_dist <- Ult_all_PD_sig[which(Ult_all_PD_sig$subset == i & Ult_all_PD_sig$variable == j),4]
  mu <- Ult_all_PD_sig[which(Ult_all_PD_sig$subset == "572" & Ult_all_PD_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_PD_values[k,3] <- t_test_values[1]
  ttest_PD_values[k,4] <- t_test_values[2]
}

write.csv(ttest_PD_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_PD_ult.csv", row.names = TRUE)

#### Doing ttests for ult and phylo
#Get average value for each subset number and each community for ttests
ttest_NRI_values_pr <- data.frame(community = character((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), subset = character((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_NRI_sig$subset))-1)*length(unique(Ult_all_NRI_sig$variable))))
ttest_NRI_values_pr$community <- c(unique(Ult_all_NRI_sig$variable))
ttest_NRI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NRI_values_pr) <- paste0(ttest_NRI_values_pr$community, "_", ttest_NRI_values_pr$subset)
ttest_NRI_values_pr

#Doing ttest for comparing each subset between types of trees
for (k in 1:nrow(ttest_NRI_values_pr)){
  j <- ttest_NRI_values_pr$community[k]
  i <- ttest_NRI_values_pr$subset[k]
  ult_dist <- Ult_all_NRI_sig[which(Ult_all_NRI_sig$subset == i & Ult_all_NRI_sig$variable == j),4]
  random_dist <- Random_all_NRI_sig[which(Random_all_NRI_sig$subset == i & Random_all_NRI_sig$variable == j),4] 
  if(length(ult_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(ult_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NRI_values_pr[k,3] <- t_test_values[1]
  ttest_NRI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NRI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NRI_ult_random.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_NTI_values_pr <- data.frame(community = character((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), subset = character((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_NTI_sig$subset))-1)*length(unique(Ult_all_NTI_sig$variable))))
ttest_NTI_values_pr$community <- c(unique(Ult_all_NTI_sig$variable))
ttest_NTI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NTI_values_pr) <- paste0(ttest_NTI_values_pr$community, "_", ttest_NTI_values_pr$subset)
ttest_NTI_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NTI_values_pr)){
  j <- ttest_NTI_values_pr$community[k]
  i <- ttest_NTI_values_pr$subset[k]
  ult_dist <- Ult_all_NTI_sig[which(Ult_all_NTI_sig$subset == i & Ult_all_NTI_sig$variable == j),4]
  random_dist <- Random_all_NTI_sig[which(Random_all_NTI_sig$subset == i & Random_all_NTI_sig$variable == j),4] 
  if(length(ult_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(ult_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NTI_values_pr[k,3] <- t_test_values[1]
  ttest_NTI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NTI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NTI_ult_random.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_PD_values_pr <- data.frame(community = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), subset = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))))
ttest_PD_values_pr$community <- c(unique(Ult_all_PD_sig$variable))
ttest_PD_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_PD_values_pr) <- paste0(ttest_PD_values_pr$community, "_", ttest_PD_values_pr$subset)
ttest_PD_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_PD_values_pr)){
  j <- ttest_PD_values_pr$community[k]
  i <- ttest_PD_values_pr$subset[k]
  ult_dist <- Ult_all_PD_sig[which(Ult_all_PD_sig$subset == i & Ult_all_PD_sig$variable == j),4]
  random_dist <- Random_all_PD_sig[which(Random_all_PD_sig$subset == i & Random_all_PD_sig$variable == j),4] 
  if(length(ult_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(ult_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_PD_values_pr[k,3] <- t_test_values[1]
  ttest_PD_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_PD_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_PD_ult_random.csv", row.names = TRUE)

#############
#Doing ttests for recon prune
#Get average value for each subset number and each community for ttests
ttest_NRI_values_pr <- data.frame(community = character((length(unique(Recon_all_NRI_sig$subset))-1)*length(unique(Recon_all_NRI_sig$variable))), subset = character((length(unique(Recon_all_NRI_sig$subset))-1)*length(unique(Recon_all_NRI_sig$variable))), ttest_stat = numeric((length(unique(Recon_all_NRI_sig$subset))-1)*length(unique(Recon_all_NRI_sig$variable))), ttest_pvalue = numeric((length(unique(Recon_all_NRI_sig$subset))-1)*length(unique(Recon_all_NRI_sig$variable))))
ttest_NRI_values_pr$community <- c(unique(Recon_all_NRI_sig$variable))
ttest_NRI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NRI_values_pr) <- paste0(ttest_NRI_values_pr$community, "_", ttest_NRI_values_pr$subset)
ttest_NRI_values_pr

#Doing ttest for comparing each subset between types of trees
for (k in 1:nrow(ttest_NRI_values_pr)){
  j <- ttest_NRI_values_pr$community[k]
  i <- ttest_NRI_values_pr$subset[k]
  recon_dist <- Recon_all_NRI_sig[which(Recon_all_NRI_sig$subset == i & Recon_all_NRI_sig$variable == j),4]
  random_dist <- Random_all_NRI_sig[which(Random_all_NRI_sig$subset == i & Random_all_NRI_sig$variable == j),4] 
  if(length(recon_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(recon_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NRI_values_pr[k,3] <- t_test_values[1]
  ttest_NRI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NRI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NRI_reconprune.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_NTI_values_pr <- data.frame(community = character((length(unique(Recon_all_NTI_sig$subset))-1)*length(unique(Recon_all_NTI_sig$variable))), subset = character((length(unique(Recon_all_NTI_sig$subset))-1)*length(unique(Recon_all_NTI_sig$variable))), ttest_stat = numeric((length(unique(Recon_all_NTI_sig$subset))-1)*length(unique(Recon_all_NTI_sig$variable))), ttest_pvalue = numeric((length(unique(Recon_all_NTI_sig$subset))-1)*length(unique(Recon_all_NTI_sig$variable))))
ttest_NTI_values_pr$community <- c(unique(Recon_all_NTI_sig$variable))
ttest_NTI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NTI_values_pr) <- paste0(ttest_NTI_values_pr$community, "_", ttest_NTI_values_pr$subset)
ttest_NTI_values_pr
unique(Recon_all_NTI_sig$subset)
#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NTI_values_pr)){
  j <- ttest_NTI_values_pr$community[k]
  i <- ttest_NTI_values_pr$subset[k]
  recon_dist <- Recon_all_NTI_sig[which(Recon_all_NTI_sig$subset == i & Recon_all_NTI_sig$variable == j),4]
  random_dist <- Random_all_NTI_sig[which(Random_all_NTI_sig$subset == i & Random_all_NTI_sig$variable == j),4] 
  if(length(recon_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(recon_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NTI_values_pr[k,3] <- t_test_values[1]
  ttest_NTI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NTI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NTI_reconprune.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_PD_values_pr <- data.frame(community = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), subset = character((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_stat = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))), ttest_pvalue = numeric((length(unique(Ult_all_PD_sig$subset))-1)*length(unique(Ult_all_PD_sig$variable))))
ttest_PD_values_pr$community <- c(unique(Ult_all_PD_sig$variable))
ttest_PD_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_PD_values_pr) <- paste0(ttest_PD_values_pr$community, "_", ttest_PD_values_pr$subset)
ttest_PD_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_PD_values_pr)){
  j <- ttest_PD_values_pr$community[k]
  i <- ttest_PD_values_pr$subset[k]
  ult_dist <- Ult_all_PD_sig[which(Ult_all_PD_sig$subset == i & Ult_all_PD_sig$variable == j),4]
  random_dist <- Random_all_PD_sig[which(Random_all_PD_sig$subset == i & Random_all_PD_sig$variable == j),4] 
  if(length(ult_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(ult_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_PD_values_pr[k,3] <- t_test_values[1]
  ttest_PD_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_PD_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_PD_ult_random.csv", row.names = TRUE)
