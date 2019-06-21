#T tests

#Cladult differences by subset and family

#Read files with dataframes to plot
#####
cladult_NRI_all_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_random_subsets_NRI.csv", stringsAsFactors = FALSE)
cladult_NRI_family_df <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_family_subsets_NRI.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
cladult_NRI_comm_diff <- cladult_NRI_all_diff[,-2]
cladult_NRI_comm_family <- cladult_NRI_family_df[,-2]

#t-tests 

#For NRI comms
t_tests_100 <- list()
for (i in 2:(ncol(cladult_NRI_comm_diff)-1)){
  subsetted <- cladult_NRI_comm_diff %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NRI_comm_diff[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100) <- colnames(cladult_NRI_comm_diff[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")


t_tests_200 <- list()
for (i in 2:(ncol(cladult_NRI_comm_diff)-1)){
  subsetted <- cladult_NRI_comm_diff %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NRI_comm_diff[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(cladult_NRI_comm_diff[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(cladult_NRI_comm_diff)-1)){
  subsetted <- cladult_NRI_comm_diff %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NRI_comm_diff[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(cladult_NRI_comm_diff[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(cladult_NRI_comm_diff)-1)){
  subsetted <- cladult_NRI_comm_diff %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NRI_comm_diff[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(cladult_NRI_comm_diff[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(cladult_NRI_comm_diff)-1)){
  subsetted <- cladult_NRI_comm_diff %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NRI_comm_diff[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(cladult_NRI_comm_diff[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")


#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

#Make dataframe of all t test results
cladult_NRI_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(cladult_NRI_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
cladult_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
cladult_NRI_ttest$type <- c("stat", "pvalue")
melted_cladult_NRI_ttest <- melt(cladult_NRI_ttest, id = c("subset", "type"))
melted_cladult_NRI_ttest_star <- spread(melted_cladult_NRI_ttest, type, value)
#Add column with * for significance
melted_cladult_NRI_ttest_star$star <- ""
melted_cladult_NRI_ttest_star$star[melted_cladult_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_cladult_NRI_ttest_star$star[melted_cladult_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_cladult_NRI_ttest_star$star[melted_cladult_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_cladult_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_cladult_NRI.csv", row.names = TRUE)


####
#NRI for reconstructed 
reconprune_all_diff_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NRI.csv", stringsAsFactors = FALSE)
reconprune_comm_diff_NRI <- reconprune_all_diff_NRI[,-2]

t_tests_100 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NRI)-1)){
  subsetted <- reconprune_comm_diff_NRI %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NRI[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
}
rownames(t_tests_100) <- colnames(reconprune_comm_diff_NRI[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")

t_tests_200 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NRI)-1)){
  subsetted <- reconprune_comm_diff_NRI %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NRI[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(reconprune_comm_diff_NRI[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NRI)-1)){
  subsetted <- reconprune_comm_diff_NRI %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NRI[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(reconprune_comm_diff_NRI[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NRI)-1)){
  subsetted <- reconprune_comm_diff_NRI %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NRI[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(reconprune_comm_diff_NRI[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NRI)-1)){
  subsetted <- reconprune_comm_diff_NRI %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NRI[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(reconprune_comm_diff_NRI[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")

#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

#Make dataframe of all t test results
reconprune_NRI_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(reconprune_NRI_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
reconprune_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
reconprune_NRI_ttest$type <- c("stat", "pvalue")
melted_reconprune_NRI_ttest <- melt(reconprune_NRI_ttest, id = c("subset", "type"))
melted_reconprune_NRI_ttest_star <- spread(melted_reconprune_NRI_ttest, type, value)
#Add column with * for significance
melted_reconprune_NRI_ttest_star$star <- ""
melted_reconprune_NRI_ttest_star$star[melted_reconprune_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_reconprune_NRI_ttest_star$star[melted_reconprune_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_reconprune_NRI_ttest_star$star[melted_reconprune_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_reconprune_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_reconprune_NRI.csv", row.names = TRUE)

#####
#
#For NTI comms

cladult_NTI_all_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_random_subsets_NTI.csv", stringsAsFactors = FALSE)
cladult_NTI_family_df <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_family_subsets_NTI.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
cladult_NTI_comm_diff <- cladult_NTI_all_diff[,-2]
cladult_NTI_comm_family <- cladult_NTI_family_df[,-2]

#t-tests 

#For NTI comms
t_tests_100 <- list()
for (i in 2:(ncol(cladult_NTI_comm_diff)-1)){
  subsetted <- cladult_NTI_comm_diff %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NTI_comm_diff[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_100) <- colnames(cladult_NTI_comm_diff[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")


t_tests_200 <- list()
for (i in 2:(ncol(cladult_NTI_comm_diff)-1)){
  subsetted <- cladult_NTI_comm_diff %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NTI_comm_diff[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(cladult_NTI_comm_diff[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(cladult_NTI_comm_diff)-1)){
  subsetted <- cladult_NTI_comm_diff %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NTI_comm_diff[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(cladult_NTI_comm_diff[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(cladult_NTI_comm_diff)-1)){
  subsetted <- cladult_NTI_comm_diff %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NTI_comm_diff[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(cladult_NTI_comm_diff[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(cladult_NTI_comm_diff)-1)){
  subsetted <- cladult_NTI_comm_diff %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_NTI_comm_diff[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(cladult_NTI_comm_diff[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")

#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

tr_t_tests_100
melted_cladult_NTI_ttest_star
#Make dataframe of all t test results
cladult_NTI_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(cladult_NTI_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
cladult_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
cladult_NTI_ttest$type <- c("stat", "pvalue")
melted_cladult_NTI_ttest <- melt(cladult_NTI_ttest, id = c("subset", "type"))
melted_cladult_NTI_ttest_star <- spread(melted_cladult_NTI_ttest, type, value)
melted_cladult_NTI_ttest_star



#Add column with * for significance
melted_cladult_NTI_ttest_star$star <- ""
melted_cladult_NTI_ttest_star$star[melted_cladult_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_cladult_NTI_ttest_star$star[melted_cladult_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_cladult_NTI_ttest_star$star[melted_cladult_NTI_ttest_star$pvalue <= 0.001] <- "***"
melted_cladult_NTI_ttest_star
write.csv(melted_cladult_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_cladult_NTI.csv", row.names = TRUE)


#####
#Reconprune NTI
#t tests
reconprune_all_diff_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NTI.csv", stringsAsFactors = FALSE)

reconprune_comm_diff_NTI <- reconprune_all_diff_NTI[,-2]


t_tests_100 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NTI)-1)){
  subsetted <- reconprune_comm_diff_NTI %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NTI[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
}
rownames(t_tests_100) <- colnames(reconprune_comm_diff_NTI[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")

t_tests_200 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NTI)-1)){
  subsetted <- reconprune_comm_diff_NTI %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NTI[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(reconprune_comm_diff_NTI[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NTI)-1)){
  subsetted <- reconprune_comm_diff_NTI %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NTI[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(reconprune_comm_diff_NTI[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NTI)-1)){
  subsetted <- reconprune_comm_diff_NTI %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NTI[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(reconprune_comm_diff_NTI[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(reconprune_comm_diff_NTI)-1)){
  subsetted <- reconprune_comm_diff_NTI %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_NTI[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(reconprune_comm_diff_NTI[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")

#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

#Make dataframe of all t test results
reconprune_NTI_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(reconprune_NTI_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
reconprune_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
reconprune_NTI_ttest$type <- c("stat", "pvalue")
melted_reconprune_NTI_ttest <- melt(reconprune_NTI_ttest, id = c("subset", "type"))
melted_reconprune_NTI_ttest_star <- spread(melted_reconprune_NTI_ttest, type, value)
#Add column with * for significance
melted_reconprune_NTI_ttest_star$star <- ""
melted_reconprune_NTI_ttest_star$star[melted_reconprune_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_reconprune_NTI_ttest_star$star[melted_reconprune_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_reconprune_NTI_ttest_star$star[melted_reconprune_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_reconprune_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_reconprune_NTI.csv", row.names = TRUE)

#####PD

cladult_PD_all_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_ult_prunephy_random_subsets_PD.csv", stringsAsFactors = FALSE)

#Remove "Taxa" column - should not be relevant for Ordway as a whole
cladult_PD_comm_diff <- cladult_PD_all_diff[,-2]
cladult_PD_comm_diff <- cladult_PD_comm_diff[,-17]

#cladult
t_tests_100 <- list()
for (i in 2:(ncol(cladult_PD_comm_diff)-1)){
  subsetted <- cladult_PD_comm_diff %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_PD_comm_diff[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
  
}

rownames(t_tests_100) <- colnames(cladult_PD_comm_diff[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")


t_tests_200 <- list()
for (i in 2:(ncol(cladult_PD_comm_diff)-1)){
  subsetted <- cladult_PD_comm_diff %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_PD_comm_diff[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(cladult_PD_comm_diff[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(cladult_PD_comm_diff)-1)){
  subsetted <- cladult_PD_comm_diff %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_PD_comm_diff[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(cladult_PD_comm_diff[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(cladult_PD_comm_diff)-1)){
  subsetted <- cladult_PD_comm_diff %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_PD_comm_diff[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(cladult_PD_comm_diff[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(cladult_PD_comm_diff)-1)){
  subsetted <- cladult_PD_comm_diff %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(cladult_PD_comm_diff[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(cladult_PD_comm_diff[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")

#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

#Make dataframe of all t test results
cladult_PD_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(cladult_PD_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
cladult_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
cladult_PD_ttest$type <- c("stat", "pvalue")
melted_cladult_PD_ttest <- melt(cladult_PD_ttest, id = c("subset", "type"))
melted_cladult_PD_ttest_star <- spread(melted_cladult_PD_ttest, type, value)
#Add column with * for significance
melted_cladult_PD_ttest_star$star <- ""
melted_cladult_PD_ttest_star$star[melted_cladult_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_cladult_PD_ttest_star$star[melted_cladult_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_cladult_PD_ttest_star$star[melted_cladult_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_cladult_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_cladult_PD.csv", row.names = TRUE)



#Reconprune PD
reconprune_all_diff_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prune_phylogram_random_subsets_PD.csv", stringsAsFactors = FALSE)

reconprune_comm_diff_PD <- reconprune_all_diff_PD[,-2]
reconprune_comm_diff_PD <- reconprune_comm_diff_PD[,-17]

t_tests_100 <- list()
for (i in 2:(ncol(reconprune_comm_diff_PD)-1)){
  subsetted <- reconprune_comm_diff_PD %>% 
    filter(subset == "100")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_PD[,i])
  t_tests_100 <- rbind(t_tests_100, t_test_values, make.row.names = FALSE)
}
rownames(t_tests_100) <- colnames(reconprune_comm_diff_PD[,2:15])
colnames(t_tests_100) <- paste0(colnames(t_tests_100), "100")

t_tests_200 <- list()
for (i in 2:(ncol(reconprune_comm_diff_PD)-1)){
  subsetted <- reconprune_comm_diff_PD %>% 
    filter(subset == "200")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_PD[,i])
  t_tests_200 <- rbind(t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_200) <- colnames(reconprune_comm_diff_PD[,2:15])
colnames(t_tests_200) <- paste0(colnames(t_tests_200), "200")

t_tests_300 <- list()
for (i in 2:(ncol(reconprune_comm_diff_PD)-1)){
  subsetted <- reconprune_comm_diff_PD %>% 
    filter(subset == "300")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_PD[,i])
  t_tests_300 <- rbind(t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_300) <- colnames(reconprune_comm_diff_PD[,2:15])
colnames(t_tests_300) <- paste0(colnames(t_tests_300), "300")


t_tests_400 <- list()
for (i in 2:(ncol(reconprune_comm_diff_PD)-1)){
  subsetted <- reconprune_comm_diff_PD %>% 
    filter(subset == "400")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_PD[,i])
  t_tests_400 <- rbind(t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_400) <- colnames(reconprune_comm_diff_PD[,2:15])
colnames(t_tests_400) <- paste0(colnames(t_tests_400), "400")


t_tests_500 <- list()
for (i in 2:(ncol(reconprune_comm_diff_PD)-1)){
  subsetted <- reconprune_comm_diff_PD %>% 
    filter(subset == "500")
  t_test <- (t.test(subsetted[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(reconprune_comm_diff_PD[,i])
  t_tests_500 <- rbind(t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(t_tests_500) <- colnames(reconprune_comm_diff_PD[,2:15])
colnames(t_tests_500) <- paste0(colnames(t_tests_500), "500")

#Transpose t tests results
tr_t_tests_100 <- as.data.frame(t(t_tests_100))
tr_t_tests_200 <- as.data.frame(t(t_tests_200))
tr_t_tests_300 <- as.data.frame(t(t_tests_300))
tr_t_tests_400 <- as.data.frame(t(t_tests_400))
tr_t_tests_500 <- as.data.frame(t(t_tests_500))

#Make dataframe of all t test results
reconprune_PD_ttest <- rbind.fill(tr_t_tests_100, tr_t_tests_200, tr_t_tests_300, tr_t_tests_400, tr_t_tests_500)
rownames(reconprune_PD_ttest) <- c(rownames(tr_t_tests_100), rownames(tr_t_tests_200), rownames(tr_t_tests_300), rownames(tr_t_tests_400), rownames(tr_t_tests_500))
reconprune_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
reconprune_PD_ttest$type <- c("stat", "pvalue")
melted_reconprune_PD_ttest <- melt(reconprune_PD_ttest, id = c("subset", "type"))
melted_reconprune_PD_ttest_star <- spread(melted_reconprune_PD_ttest, type, value)
#Add column with * for significance
melted_reconprune_PD_ttest_star$star <- ""
melted_reconprune_PD_ttest_star$star[melted_reconprune_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_reconprune_PD_ttest_star$star[melted_reconprune_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_reconprune_PD_ttest_star$star[melted_reconprune_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_reconprune_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_reconprune_PD.csv", row.names = TRUE)
