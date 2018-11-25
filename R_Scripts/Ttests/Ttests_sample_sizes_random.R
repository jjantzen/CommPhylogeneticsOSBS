#Ttests sample size random subsets
#Read files with dataframes to plot
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)

clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)
ult_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Ult/PD_tree572_ult.csv", stringsAsFactors = FALSE)

full_tree_values <- as.data.frame(clad_572$mpd.obs.z)
full_tree_values$NTI <- clad_572$mntd.obs.z
full_tree_values$PD <- clad_572$PD
rownames(full_tree_values) <- rownames(comm_dataset)
colnames(full_tree_values) <- c("full_NRI", "full_NTI", "full_PD")
full_tree_values_tr <- as.data.frame(t(full_tree_values))
full_tree_values_tr <- full_tree_values_tr[,-1]
full_tree_values_tr

ult_tree_values <- as.data.frame(ult_572$mpd.obs.z)
ult_tree_values$NTI <- ult_572$mntd.obs.z
ult_tree_values$PD <- ult_572$pd.obs.z
rownames(ult_tree_values) <- rownames(comm_dataset)
colnames(ult_tree_values) <- c("ult_NRI", "ult_NTI", "ult_PD")
ult_tree_values_tr <- as.data.frame(t(ult_tree_values))
ult_tree_values_tr <- ult_tree_values_tr[,-1]
ult_tree_values_tr


prune_NRI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
prune_NRI_random_subsets
#Remove "Taxa" column - should not be relevant for Ordway as a whole
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets[,-2]
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets_comm[,-17]

prune_NRI_random_subsets_comm <- rbind.fill(prune_NRI_random_subsets_comm, full_tree_values_tr["full_NRI",])
prune_NRI_random_subsets_comm$subset[501] <- "572"


prune_NTI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
prune_NTI_random_subsets_comm <- prune_NTI_random_subsets[,-2]
prune_NTI_random_subsets_comm <- prune_NTI_random_subsets_comm[,-17]

prune_NTI_random_subsets_comm <- rbind.fill(prune_NTI_random_subsets_comm, full_tree_values_tr["full_NTI",])
prune_NTI_random_subsets_comm$subset[501] <- "572"


prune_PD_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
prune_PD_random_subsets_comm <- prune_PD_random_subsets[,-2]
prune_PD_random_subsets_comm <- prune_PD_random_subsets_comm[,-17]

prune_PD_random_subsets_comm <- rbind.fill(prune_PD_random_subsets_comm, full_tree_values_tr["full_PD",])
prune_PD_random_subsets_comm$subset[501] <- "572"


recon_NRI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_recon_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
recon_NRI_random_subsets_comm <- recon_NRI_random_subsets[,-2]
recon_NRI_random_subsets_comm <- recon_NRI_random_subsets_comm[,-17]

recon_NRI_random_subsets_comm <- rbind.fill(recon_NRI_random_subsets_comm, full_tree_values_tr["full_NRI",])
recon_NRI_random_subsets_comm$subset[304] <- "572"


recon_NTI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_recon_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
recon_NTI_random_subsets_comm <- recon_NTI_random_subsets[,-2]
recon_NTI_random_subsets_comm <- recon_NTI_random_subsets_comm[,-17]

recon_NTI_random_subsets_comm <- rbind.fill(recon_NTI_random_subsets_comm, full_tree_values_tr["full_NTI",])
recon_NTI_random_subsets_comm$subset[304] <- "572"


recon_PD_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_recon_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
recon_PD_random_subsets_comm <- recon_PD_random_subsets[,-2]
recon_PD_random_subsets_comm <- recon_PD_random_subsets_comm[,-17]

recon_PD_random_subsets_comm <- rbind.fill(recon_PD_random_subsets_comm, full_tree_values_tr["full_PD",])
recon_PD_random_subsets_comm$subset[304] <- "572"


ult_NRI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_ult_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
ult_NRI_random_subsets_comm <- ult_NRI_random_subsets[,-2]
ult_NRI_random_subsets_comm <- ult_NRI_random_subsets_comm[,-17]

ult_NRI_random_subsets_comm <- rbind.fill(ult_NRI_random_subsets_comm, ult_tree_values_tr["ult_NRI",])
ult_NRI_random_subsets_comm$subset[501] <- "572"



ult_NTI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_ult_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
ult_NTI_random_subsets_comm <- ult_NTI_random_subsets[,-2]
ult_NTI_random_subsets_comm <- ult_NTI_random_subsets_comm[,-17]


ult_NTI_random_subsets_comm <- rbind.fill(ult_NTI_random_subsets_comm, ult_tree_values_tr["ult_NTI",])
ult_NTI_random_subsets_comm$subset[501] <- "572"


ult_PD_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_ult_random_subsets.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
ult_PD_random_subsets_comm <- ult_PD_random_subsets[,-2]
ult_PD_random_subsets_comm <- ult_PD_random_subsets_comm[,-17]


ult_PD_random_subsets_comm <- rbind.fill(ult_PD_random_subsets_comm, ult_tree_values_tr["ult_PD",])
ult_PD_random_subsets_comm$subset[501] <- "572"

#######
#For NRI pruned
t_tests_100_572 <- list()
for (i in 2:(ncol(prune_NRI_random_subsets_comm)-1)){
  s100 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NRI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(prune_NRI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(prune_NRI_random_subsets_comm)-1)){
  s200 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NRI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(prune_NRI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(prune_NRI_random_subsets_comm)-1)){
  s300 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NRI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(prune_NRI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(prune_NRI_random_subsets_comm)-1)){
  s400 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NRI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(prune_NRI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(prune_NRI_random_subsets_comm)-1)){
  s500 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- prune_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NRI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(prune_NRI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NRI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NRI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NRI_ttest$type <- c("stat", "pvalue")
melted_ss_NRI_ttest <- melt(ss_NRI_ttest, id = c("subset", "type"))
melted_ss_NRI_ttest_star <- spread(melted_ss_NRI_ttest, type, value)
#Add column with * for significance
melted_ss_NRI_ttest_star$star <- ""
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_NRI.csv", row.names = TRUE)



#For NTI pruned
t_tests_100_572 <- list()
for (i in 2:(ncol(prune_NTI_random_subsets_comm)-1)){
  s100 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NTI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(prune_NTI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(prune_NTI_random_subsets_comm)-1)){
  s200 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NTI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(prune_NTI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(prune_NTI_random_subsets_comm)-1)){
  s300 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NTI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(prune_NTI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(prune_NTI_random_subsets_comm)-1)){
  s400 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NTI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(prune_NTI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(prune_NTI_random_subsets_comm)-1)){
  s500 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- prune_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_NTI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(prune_NTI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NTI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NTI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NTI_ttest$type <- c("stat", "pvalue")
melted_ss_NTI_ttest <- melt(ss_NTI_ttest, id = c("subset", "type"))
melted_ss_NTI_ttest_star <- spread(melted_ss_NTI_ttest, type, value)
#Add column with * for significance
melted_ss_NTI_ttest_star$star <- ""
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_NTI.csv", row.names = TRUE)



#For PD pruned
t_tests_100_572 <- list()
for (i in 2:(ncol(prune_PD_random_subsets_comm)-1)){
  s100 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_PD_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(prune_PD_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(prune_PD_random_subsets_comm)-1)){
  s200 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_PD_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(prune_PD_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(prune_PD_random_subsets_comm)-1)){
  s300 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_PD_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(prune_PD_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(prune_PD_random_subsets_comm)-1)){
  s400 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_PD_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(prune_PD_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(prune_PD_random_subsets_comm)-1)){
  s500 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- prune_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(prune_PD_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(prune_PD_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_PD_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_PD_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_PD_ttest$type <- c("stat", "pvalue")
melted_ss_PD_ttest <- melt(ss_PD_ttest, id = c("subset", "type"))
melted_ss_PD_ttest_star <- spread(melted_ss_PD_ttest, type, value)
#Add column with * for significance
melted_ss_PD_ttest_star$star <- ""
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_PD.csv", row.names = TRUE)


#For NRI recon
t_tests_100_572 <- list()
for (i in 2:(ncol(recon_NRI_random_subsets_comm)-1)){
  s100 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NRI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(recon_NRI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(recon_NRI_random_subsets_comm)-1)){
  s200 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NRI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(recon_NRI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(recon_NRI_random_subsets_comm)-1)){
  s300 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NRI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(recon_NRI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(recon_NRI_random_subsets_comm)-1)){
  s400 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NRI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(recon_NRI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(recon_NRI_random_subsets_comm)-1)){
  s500 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- recon_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NRI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(recon_NRI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NRI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NRI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NRI_ttest$type <- c("stat", "pvalue")
melted_ss_NRI_ttest <- melt(ss_NRI_ttest, id = c("subset", "type"))
melted_ss_NRI_ttest_star <- spread(melted_ss_NRI_ttest, type, value)
#Add column with * for significance
melted_ss_NRI_ttest_star$star <- ""
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_recon_NRI.csv", row.names = TRUE)


#For NTI recon
t_tests_100_572 <- list()
for (i in 2:(ncol(recon_NTI_random_subsets_comm)-1)){
  s100 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NTI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(recon_NTI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(recon_NTI_random_subsets_comm)-1)){
  s200 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NTI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(recon_NTI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(recon_NTI_random_subsets_comm)-1)){
  s300 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NTI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(recon_NTI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(recon_NTI_random_subsets_comm)-1)){
  s400 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NTI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(recon_NTI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(recon_NTI_random_subsets_comm)-1)){
  s500 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- recon_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_NTI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(recon_NTI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NTI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NTI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NTI_ttest$type <- c("stat", "pvalue")
melted_ss_NTI_ttest <- melt(ss_NTI_ttest, id = c("subset", "type"))
melted_ss_NTI_ttest_star <- spread(melted_ss_NTI_ttest, type, value)
#Add column with * for significance
melted_ss_NTI_ttest_star$star <- ""
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_recon_NTI.csv", row.names = TRUE)


#For PD recon
t_tests_100_572 <- list()
for (i in 2:(ncol(recon_PD_random_subsets_comm)-1)){
  s100 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_PD_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(recon_PD_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(recon_PD_random_subsets_comm)-1)){
  s200 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_PD_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(recon_PD_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(recon_PD_random_subsets_comm)-1)){
  s300 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_PD_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(recon_PD_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(recon_PD_random_subsets_comm)-1)){
  s400 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_PD_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(recon_PD_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(recon_PD_random_subsets_comm)-1)){
  s500 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- recon_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(recon_PD_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(recon_PD_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_PD_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_PD_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_PD_ttest$type <- c("stat", "pvalue")
melted_ss_PD_ttest <- melt(ss_PD_ttest, id = c("subset", "type"))
melted_ss_PD_ttest_star <- spread(melted_ss_PD_ttest, type, value)
#Add column with * for significance
melted_ss_PD_ttest_star$star <- ""
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_recon_PD.csv", row.names = TRUE)



#For NRI ult
t_tests_100_572 <- list()
for (i in 2:(ncol(ult_NRI_random_subsets_comm)-1)){
  s100 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NRI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(ult_NRI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(ult_NRI_random_subsets_comm)-1)){
  s200 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NRI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(ult_NRI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(ult_NRI_random_subsets_comm)-1)){
  s300 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NRI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(ult_NRI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(ult_NRI_random_subsets_comm)-1)){
  s400 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NRI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(ult_NRI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(ult_NRI_random_subsets_comm)-1)){
  s500 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- ult_NRI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NRI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(ult_NRI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NRI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NRI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NRI_ttest$type <- c("stat", "pvalue")
melted_ss_NRI_ttest <- melt(ss_NRI_ttest, id = c("subset", "type"))
melted_ss_NRI_ttest_star <- spread(melted_ss_NRI_ttest, type, value)
#Add column with * for significance
melted_ss_NRI_ttest_star$star <- ""
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NRI_ttest_star$star[melted_ss_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_ult_NRI.csv", row.names = TRUE)


#For NTI ult
t_tests_100_572 <- list()
for (i in 2:(ncol(ult_NTI_random_subsets_comm)-1)){
  s100 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NTI_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(ult_NTI_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")
t_tests_100_572


t_tests_200_572 <- list()
for (i in 2:(ncol(ult_NTI_random_subsets_comm)-1)){
  s200 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NTI_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(ult_NTI_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(ult_NTI_random_subsets_comm)-1)){
  s300 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NTI_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(ult_NTI_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(ult_NTI_random_subsets_comm)-1)){
  s400 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NTI_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(ult_NTI_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(ult_NTI_random_subsets_comm)-1)){
  s500 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- ult_NTI_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_NTI_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(ult_NTI_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_NTI_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_NTI_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_NTI_ttest$type <- c("stat", "pvalue")
melted_ss_NTI_ttest <- melt(ss_NTI_ttest, id = c("subset", "type"))
melted_ss_NTI_ttest_star <- spread(melted_ss_NTI_ttest, type, value)
#Add column with * for significance
melted_ss_NTI_ttest_star$star <- ""
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_NTI_ttest_star$star[melted_ss_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_ult_NTI.csv", row.names = TRUE)

#For PD ult
t_tests_100_572 <- list()
for (i in 2:(ncol(ult_PD_random_subsets_comm)-1)){
  s100 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "100")
  s572 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s100[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_PD_random_subsets_comm[,i])
  t_tests_100_572 <- rbind(t_tests_100_572, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100_572) <- colnames(ult_PD_random_subsets_comm[,2:15])
colnames(t_tests_100_572) <- paste0(colnames(t_tests_100_572), "100v572")


t_tests_200_572 <- list()
for (i in 2:(ncol(ult_PD_random_subsets_comm)-1)){
  s200 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "200")
  s572 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s200[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_PD_random_subsets_comm[,i])
  t_tests_200_572 <- rbind(t_tests_200_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_200_572) <- colnames(ult_PD_random_subsets_comm[,2:15])
colnames(t_tests_200_572) <- paste0(colnames(t_tests_200_572), "200v572")

t_tests_300_572 <- list()
for (i in 2:(ncol(ult_PD_random_subsets_comm)-1)){
  s300 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "300")
  s572 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s300[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_PD_random_subsets_comm[,i])
  t_tests_300_572 <- rbind(t_tests_300_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_300_572) <- colnames(ult_PD_random_subsets_comm[,2:15])
colnames(t_tests_300_572) <- paste0(colnames(t_tests_300_572), "300v572")

t_tests_400_572 <- list()
for (i in 2:(ncol(ult_PD_random_subsets_comm)-1)){
  s400 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "400")
  s572 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s400[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_PD_random_subsets_comm[,i])
  t_tests_400_572 <- rbind(t_tests_400_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_400_572) <- colnames(ult_PD_random_subsets_comm[,2:15])
colnames(t_tests_400_572) <- paste0(colnames(t_tests_400_572), "400v572")

t_tests_500_572 <- list()
for (i in 2:(ncol(ult_PD_random_subsets_comm)-1)){
  s500 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "500")
  s572 <- ult_PD_random_subsets_comm %>% 
    filter(subset == "572")
  mu <- s572[,i]
  t_test <- (t.test(s500[,i], mu = mu))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(ult_PD_random_subsets_comm[,i])
  t_tests_500_572 <- rbind(t_tests_500_572, t_test_values, make.row.names = FALSE)
}

rownames(t_tests_500_572) <- colnames(ult_PD_random_subsets_comm[,2:15])
colnames(t_tests_500_572) <- paste0(colnames(t_tests_500_572), "500v572")


#Transpose t tests results
tr_t_tests_100_572 <- as.data.frame(t(t_tests_100_572))
tr_t_tests_200_572 <- as.data.frame(t(t_tests_200_572))
tr_t_tests_300_572 <- as.data.frame(t(t_tests_300_572))
tr_t_tests_400_572 <- as.data.frame(t(t_tests_400_572))
tr_t_tests_500_572 <- as.data.frame(t(t_tests_500_572))

#Make dataframe of all t test results
ss_PD_ttest <- rbind.fill(tr_t_tests_100_572, tr_t_tests_200_572, tr_t_tests_300_572, tr_t_tests_400_572, tr_t_tests_500_572)
rownames(ss_PD_ttest) <- c(rownames(tr_t_tests_100_572), rownames(tr_t_tests_200_572), rownames(tr_t_tests_300_572), rownames(tr_t_tests_400_572), rownames(tr_t_tests_500_572))
ss_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
ss_PD_ttest$type <- c("stat", "pvalue")
melted_ss_PD_ttest <- melt(ss_PD_ttest, id = c("subset", "type"))
melted_ss_PD_ttest_star <- spread(melted_ss_PD_ttest, type, value)
#Add column with * for significance
melted_ss_PD_ttest_star$star <- ""
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_ss_PD_ttest_star$star[melted_ss_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_ss_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_samplesize_ult_PD.csv", row.names = TRUE)

