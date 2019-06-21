#Ttests sampling strategies vs overall

#Read files with dataframes to plot
#####
Prop_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prop_index_pvalue.csv", stringsAsFactors = FALSE)
Prop_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prop_index_pvalue.csv", stringsAsFactors = FALSE)
Prop_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prop_index_pvalue.csv", stringsAsFactors = FALSE)

# Prop_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NRI.csv", stringsAsFactors = FALSE)
# Prop_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NTI.csv", stringsAsFactors = FALSE)
# Prop_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_PD.csv", stringsAsFactors = FALSE)

# Prop_all_NRI <- Prop_all_NRI[,-2]
#Prop_all_NRI <- Prop_all_NRI[,-17]

# Prop_all_NTI <- Prop_all_NTI[,-2]
#Prop_all_NTI <- Prop_all_NTI[,-17]

# Prop_all_PD <- Prop_all_PD[,-2]
#Prop_all_PD <- Prop_all_PD[,-17]

# Set_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_NRI.csv", stringsAsFactors = FALSE)
# Set_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_NTI.csv", stringsAsFactors = FALSE)
# Set_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_PD.csv", stringsAsFactors = FALSE)
# 
# Set_all_NRI <- Set_all_NRI[,-2]
# Set_all_NRI <- Set_all_NRI[,-17]
# 
# Set_all_NTI <- Set_all_NTI[,-2]
# Set_all_NTI <- Set_all_NTI[,-17]
# 
# Set_all_PD <- Set_all_PD[,-2]
# Set_all_PD <- Set_all_PD[,-17]
# 
# Prop_all_NRI <- Prop_all_NRI[grep("NR",(Prop_all_NRI$X)),]
# Prop_all_NTI <- Prop_all_NTI[grep("NT",(Prop_all_NTI$X)),]
# Prop_all_PD <- Prop_all_PD[grep("PD",(Prop_all_PD$X)),]


# Set_all_NRI <- Set_all_NRI[grep("NR",(Set_all_NRI$X)),]
# Set_all_NTI <- Set_all_NTI[grep("NT",(Set_all_NTI$X)),]
# Set_all_PD <- Set_all_PD[grep("PD",(Set_all_PD$X)),]


# Random_prune_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
# Random_prune_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)
# Random_prune_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)

Random_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
Random_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
Random_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

# Random_prune_NRI <- Random_prune_NRI[,-2]
# Random_prune_NRI <- Random_prune_NRI[,-17]
# 
# Random_prune_NTI <- Random_prune_NTI[,-2]
# Random_prune_NTI <- Random_prune_NTI[,-17]
# 
# Random_prune_PD <- Random_prune_PD[,-2]
# Random_prune_PD <- Random_prune_PD[,-17]
# 

#Read community data
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

#Read full tree pd values
clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Make dataframe for indices
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
Prop_all_NRI_sig <- Prop_all_NRI[which(Prop_all_NRI$pvalue >= 0.95 | Prop_all_NRI$pvalue <= 0.05),]
Prop_all_NTI_sig <- Prop_all_NTI[which(Prop_all_NTI$pvalue >= 0.95 | Prop_all_NTI$pvalue <= 0.05),]
Prop_all_PD_sig <- Prop_all_PD[which(Prop_all_PD$pvalue >= 0.95 | Prop_all_PD$pvalue <= 0.05),]

Random_all_NRI_sig <- Random_all_NRI[which(Random_all_NRI$pvalue >= 0.95 | Random_all_NRI$pvalue <= 0.05),]
Random_all_NTI_sig <- Random_all_NTI[which(Random_all_NTI$pvalue >= 0.95 | Random_all_NTI$pvalue <= 0.05),]
Random_all_PD_sig <- Random_all_PD[which(Random_all_PD$pvalue >= 0.95 | Random_all_PD$pvalue <= 0.05),]


#Add overall to each dataset
Prop_all_NRI_sig <- rbind.fill(Prop_all_NRI_sig, full_tree_NRI)
Random_all_NRI_sig <- rbind.fill(Random_all_NRI_sig, full_tree_NRI)
# Prop_all_NRI$subset[nrow(Prop_all_NRI)] <- "572"

Prop_all_NTI_sig <- rbind.fill(Prop_all_NTI_sig, full_tree_NTI)
Random_all_NTI_sig <- rbind.fill(Random_all_NTI_sig, full_tree_NTI)
# Prop_all_NTI$subset[nrow(Prop_all_NTI)] <- "572"

Prop_all_PD_sig <- rbind.fill(Prop_all_PD_sig, full_tree_PD)
Random_all_PD_sig <- rbind.fill(Random_all_PD_sig, full_tree_PD)
# Prop_all_PD$subset[nrow(Prop_all_PD)] <- "572"

#Remove pvalue columns
Prop_all_NRI_sig <- Prop_all_NRI_sig[,-5]
Prop_all_NTI_sig <- Prop_all_NTI_sig[,-5]
Prop_all_PD_sig <- Prop_all_PD_sig[,-5]

Random_all_NRI_sig <- Random_all_NRI_sig[,-5]
Random_all_NTI_sig <- Random_all_NTI_sig[,-5]
Random_all_PD_sig <- Random_all_PD_sig[,-5]

#Get average value for each subset number and each community for ttests
ttest_NRI_values <- data.frame(community = character((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), subset = character((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))))
ttest_NRI_values$community <- c(unique(Prop_all_NRI_sig$variable))
ttest_NRI_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NRI_values) <- paste0(ttest_NRI_values$community, "_", ttest_NRI_values$subset)
ttest_NRI_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NRI_values)){
  j <- ttest_NRI_values$community[k]
  i <- ttest_NRI_values$subset[k]
  data_dist <- Prop_all_NRI_sig[which(Prop_all_NRI_sig$subset == i & Prop_all_NRI_sig$variable == j),4]
  mu <- Prop_all_NRI_sig[which(Prop_all_NRI_sig$subset == "572" & Prop_all_NRI_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NRI_values[k,3] <- t_test_values[1]
  ttest_NRI_values[k,4] <- t_test_values[2]
}

write.csv(ttest_NRI_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NRI_prop.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_NTI_values <- data.frame(community = character((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), subset = character((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))))
ttest_NTI_values$community <- c(unique(Prop_all_NTI_sig$variable))
ttest_NTI_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NTI_values) <- paste0(ttest_NTI_values$community, "_", ttest_NTI_values$subset)
ttest_NTI_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NTI_values)){
  j <- ttest_NTI_values$community[k]
  i <- ttest_NTI_values$subset[k]
  data_dist <- Prop_all_NTI_sig[which(Prop_all_NTI_sig$subset == i & Prop_all_NTI_sig$variable == j),4]
  mu <- Prop_all_NTI_sig[which(Prop_all_NTI_sig$subset == "572" & Prop_all_NTI_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NTI_values[k,3] <- t_test_values[1]
  ttest_NTI_values[k,4] <- t_test_values[2]
}

write.csv(ttest_NTI_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NTI_prop.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_PD_values <- data.frame(community = character((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), subset = character((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))))
ttest_PD_values$community <- c(unique(Prop_all_PD_sig$variable))
ttest_PD_values$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_PD_values) <- paste0(ttest_PD_values$community, "_", ttest_PD_values$subset)
ttest_PD_values

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_PD_values)){
  j <- ttest_PD_values$community[k]
  i <- ttest_PD_values$subset[k]
  data_dist <- Prop_all_PD_sig[which(Prop_all_PD_sig$subset == i & Prop_all_PD_sig$variable == j),4]
  mu <- Prop_all_PD_sig[which(Prop_all_PD_sig$subset == "572" & Prop_all_PD_sig$variable == j),4] 
  if(length(data_dist) >= 2){
    t_test <- t.test(data_dist, mu = mu, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_PD_values[k,3] <- t_test_values[1]
  ttest_PD_values[k,4] <- t_test_values[2]
}

write.csv(ttest_PD_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_PD_prop.csv", row.names = TRUE)


#Get average value for each subset number and each community for ttests
ttest_NRI_values_pr <- data.frame(community = character((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), subset = character((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_NRI_sig$subset))-1)*length(unique(Prop_all_NRI_sig$variable))))
ttest_NRI_values_pr$community <- c(unique(Prop_all_NRI_sig$variable))
ttest_NRI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NRI_values_pr) <- paste0(ttest_NRI_values_pr$community, "_", ttest_NRI_values_pr$subset)
ttest_NRI_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NRI_values_pr)){
  j <- ttest_NRI_values_pr$community[k]
  i <- ttest_NRI_values_pr$subset[k]
  prop_dist <- Prop_all_NRI_sig[which(Prop_all_NRI_sig$subset == i & Prop_all_NRI_sig$variable == j),4]
  random_dist <- Random_all_NRI_sig[which(Random_all_NRI_sig$subset == i & Random_all_NRI_sig$variable == j),4] 
  if(length(prop_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(prop_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NRI_values_pr[k,3] <- t_test_values[1]
  ttest_NRI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NRI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NRI_prop_random.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_NTI_values_pr <- data.frame(community = character((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), subset = character((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_NTI_sig$subset))-1)*length(unique(Prop_all_NTI_sig$variable))))
ttest_NTI_values_pr$community <- c(unique(Prop_all_NTI_sig$variable))
ttest_NTI_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_NTI_values_pr) <- paste0(ttest_NTI_values_pr$community, "_", ttest_NTI_values_pr$subset)
ttest_NTI_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_NTI_values_pr)){
  j <- ttest_NTI_values_pr$community[k]
  i <- ttest_NTI_values_pr$subset[k]
  prop_dist <- Prop_all_NTI_sig[which(Prop_all_NTI_sig$subset == i & Prop_all_NTI_sig$variable == j),4]
  random_dist <- Random_all_NTI_sig[which(Random_all_NTI_sig$subset == i & Random_all_NTI_sig$variable == j),4] 
  if(length(prop_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(prop_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_NTI_values_pr[k,3] <- t_test_values[1]
  ttest_NTI_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_NTI_values_pr, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_NTI_prop_random.csv", row.names = TRUE)

#Get average value for each subset number and each community for ttests
ttest_PD_values_pr <- data.frame(community = character((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), subset = character((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), ttest_stat = numeric((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))), ttest_pvalue = numeric((length(unique(Prop_all_PD_sig$subset))-1)*length(unique(Prop_all_PD_sig$variable))))
ttest_PD_values_pr$community <- c(unique(Prop_all_PD_sig$variable))
ttest_PD_values_pr$subset <- c(rep("100", 14), rep("200", 14), rep("300", 14), rep("400", 14), rep("500", 14))
rownames(ttest_PD_values_pr) <- paste0(ttest_PD_values_pr$community, "_", ttest_PD_values_pr$subset)
ttest_PD_values_pr

#Doing ttest for comparing each subset to overall within a community - proportional
for (k in 1:nrow(ttest_PD_values_pr)){
  j <- ttest_PD_values_pr$community[k]
  i <- ttest_PD_values_pr$subset[k]
  prop_dist <- Prop_all_PD_sig[which(Prop_all_PD_sig$subset == i & Prop_all_PD_sig$variable == j),4]
  random_dist <- Random_all_PD_sig[which(Random_all_PD_sig$subset == i & Random_all_PD_sig$variable == j),4] 
  if(length(prop_dist) >= 2 & length(random_dist) >= 2){
    t_test <- t.test(prop_dist, random_dist, na.rm = TRUE)  
    t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  } else {
    t_test_values <- c(NA, NA)
  }
  ttest_PD_values_pr[k,3] <- t_test_values[1]
  ttest_PD_values_pr[k,4] <- t_test_values[2]
}

write.csv(ttest_PD_values, "./Plots/2017_botany/Files_to_plot/Ttest_sig_only_PD_prop_random.csv", row.names = TRUE)







##############
t_tests_100_572 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  s100 <- Prop_all_NRI %>% 
    filter(subset == "100")
  s572 <- Prop_all_NRI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(Prop_all_NRI[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  s200 <- Prop_all_NRI %>% 
    filter(subset == "200")
  s572 <- Prop_all_NRI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(Prop_all_NRI[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  s300 <- Prop_all_NRI %>% 
    filter(subset == "300")
  s572 <- Prop_all_NRI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(Prop_all_NRI[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  s400 <- Prop_all_NRI %>% 
    filter(subset == "400")
  s572 <- Prop_all_NRI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(Prop_all_NRI[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  s500 <- Prop_all_NRI %>% 
    filter(subset == "500")
  s572 <- Prop_all_NRI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(Prop_all_NRI[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
prop_NRI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(prop_NRI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
prop_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
prop_NRI_ttest$type <- c("stat", "pvalue")
melted_prop_NRI_ttest <- melt(prop_NRI_ttest, id = c("subset", "type"))
melted_prop_NRI_ttest_star <- spread(melted_prop_NRI_ttest, type, value)
#Add column with * for significance
melted_prop_NRI_ttest_star$star <- ""
melted_prop_NRI_ttest_star$star[melted_prop_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_prop_NRI_ttest_star$star[melted_prop_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_prop_NRI_ttest_star$star[melted_prop_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_prop_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_prop_overall_NRI.csv", row.names = TRUE)



#For NTI pruned
t_tests_100_572 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  s100 <- Prop_all_NTI %>% 
    filter(subset == "100")
  s572 <- Prop_all_NTI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(Prop_all_NTI[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  s200 <- Prop_all_NTI %>% 
    filter(subset == "200")
  s572 <- Prop_all_NTI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(Prop_all_NTI[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  s300 <- Prop_all_NTI %>% 
    filter(subset == "300")
  s572 <- Prop_all_NTI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(Prop_all_NTI[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  s400 <- Prop_all_NTI %>% 
    filter(subset == "400")
  s572 <- Prop_all_NTI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(Prop_all_NTI[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  s500 <- Prop_all_NTI %>% 
    filter(subset == "500")
  s572 <- Prop_all_NTI %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(Prop_all_NTI[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
prop_NTI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(prop_NTI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
prop_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
prop_NTI_ttest$type <- c("stat", "pvalue")
melted_prop_NTI_ttest <- melt(prop_NTI_ttest, id = c("subset", "type"))
melted_prop_NTI_ttest_star <- spread(melted_prop_NTI_ttest, type, value)
#Add column with * for significance
melted_prop_NTI_ttest_star$star <- ""
melted_prop_NTI_ttest_star$star[melted_prop_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_prop_NTI_ttest_star$star[melted_prop_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_prop_NTI_ttest_star$star[melted_prop_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_prop_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_prop_overall_NTI.csv", row.names = TRUE)



#For PD pruned
t_tests_100_572 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  s100 <- Prop_all_PD %>% 
    filter(subset == "100")
  s572 <- Prop_all_PD %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(Prop_all_PD[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  s200 <- Prop_all_PD %>% 
    filter(subset == "200")
  s572 <- Prop_all_PD %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(Prop_all_PD[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  s300 <- Prop_all_PD %>% 
    filter(subset == "300")
  s572 <- Prop_all_PD %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(Prop_all_PD[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  s400 <- Prop_all_PD %>% 
    filter(subset == "400")
  s572 <- Prop_all_PD %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(Prop_all_PD[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  s500 <- Prop_all_PD %>% 
    filter(subset == "500")
  s572 <- Prop_all_PD %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(Prop_all_PD[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
prop_PD_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(prop_PD_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
prop_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
prop_PD_ttest$type <- c("stat", "pvalue")
melted_prop_PD_ttest <- melt(prop_PD_ttest, id = c("subset", "type"))
melted_prop_PD_ttest_star <- spread(melted_prop_PD_ttest, type, value)
#Add column with * for significance
melted_prop_PD_ttest_star$star <- ""
melted_prop_PD_ttest_star$star[melted_prop_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_prop_PD_ttest_star$star[melted_prop_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_prop_PD_ttest_star$star[melted_prop_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_prop_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_prop_overall_PD.csv", row.names = TRUE)

