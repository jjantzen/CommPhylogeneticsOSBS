#Ttests sampling strategies 

#Read files with dataframes to plot
#####
Prop_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NRI.csv", stringsAsFactors = FALSE)
Prop_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_NTI.csv", stringsAsFactors = FALSE)
Prop_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Prop_all_PD.csv", stringsAsFactors = FALSE)

Prop_all_NRI <- Prop_all_NRI[,-2]
Prop_all_NRI <- Prop_all_NRI[,-17]

Prop_all_NTI <- Prop_all_NTI[,-2]
Prop_all_NTI <- Prop_all_NTI[,-17]

Prop_all_PD <- Prop_all_PD[,-2]
Prop_all_PD <- Prop_all_PD[,-17]

Set_all_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_NRI.csv", stringsAsFactors = FALSE)
Set_all_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_NTI.csv", stringsAsFactors = FALSE)
Set_all_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Set_all_PD.csv", stringsAsFactors = FALSE)

Set_all_NRI <- Set_all_NRI[,-2]
Set_all_NRI <- Set_all_NRI[,-17]

Set_all_NTI <- Set_all_NTI[,-2]
Set_all_NTI <- Set_all_NTI[,-17]

Set_all_PD <- Set_all_PD[,-2]
Set_all_PD <- Set_all_PD[,-17]

Prop_all_NRI <- Prop_all_NRI[grep("NR",(Prop_all_NRI$X)),]
Prop_all_NTI <- Prop_all_NTI[grep("NT",(Prop_all_NTI$X)),]
Prop_all_PD <- Prop_all_PD[grep("PD",(Prop_all_PD$X)),]


Set_all_NRI <- Set_all_NRI[grep("NR",(Set_all_NRI$X)),]
Set_all_NTI <- Set_all_NTI[grep("NT",(Set_all_NTI$X)),]
Set_all_PD <- Set_all_PD[grep("PD",(Set_all_PD$X)),]


Random_prune_NRI <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
Random_prune_NTI <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)
Random_prune_PD <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)

Random_prune_NRI <- Random_prune_NRI[,-2]
Random_prune_NRI <- Random_prune_NRI[,-17]

Random_prune_NTI <- Random_prune_NTI[,-2]
Random_prune_NTI <- Random_prune_NTI[,-17]

Random_prune_PD <- Random_prune_PD[,-2]
Random_prune_PD <- Random_prune_PD[,-17]


#First compare random with proportional

#t-tests 


#For NRI comms
pr_t_tests_100 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  prop <- Prop_all_NRI %>% 
    filter(subset == "100")
  random <- Random_prune_NRI %>% 
    filter(subset == "100")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  pr_t_tests_100 <- rbind(pr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_100) <- colnames(Prop_all_NRI[,2:15])
colnames(pr_t_tests_100) <- paste0(colnames(pr_t_tests_100), "100")

pr_t_tests_200 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  prop <- Prop_all_NRI %>% 
    filter(subset == "200")
  random <- Random_prune_NRI %>% 
    filter(subset == "200")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  pr_t_tests_200 <- rbind(pr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_200) <- colnames(Prop_all_NRI[,2:15])
colnames(pr_t_tests_200) <- paste0(colnames(pr_t_tests_200), "200")

pr_t_tests_300 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  prop <- Prop_all_NRI %>% 
    filter(subset == "300")
  random <- Random_prune_NRI %>% 
    filter(subset == "300")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  pr_t_tests_300 <- rbind(pr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_300) <- colnames(Prop_all_NRI[,2:15])
colnames(pr_t_tests_300) <- paste0(colnames(pr_t_tests_300), "300")


pr_t_tests_400 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  prop <- Prop_all_NRI %>% 
    filter(subset == "400")
  random <- Random_prune_NRI %>% 
    filter(subset == "400")
    t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  pr_t_tests_400 <- rbind(pr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_400) <- colnames(Prop_all_NRI[,2:15])
colnames(pr_t_tests_400) <- paste0(colnames(pr_t_tests_400), "400")
prop
t_test

pr_t_tests_500 <- list()
for (i in 2:(ncol(Prop_all_NRI)-1)){
  prop <- Prop_all_NRI %>% 
    filter(subset == "500")
  random <- Random_prune_NRI %>% 
    filter(subset == "500")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NRI[,i])
  pr_t_tests_500 <- rbind(pr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_500) <- colnames(Prop_all_NRI[,2:15])
colnames(pr_t_tests_500) <- paste0(colnames(pr_t_tests_500), "500")

#Transpose t tests results
tr_pr_t_tests_100 <- as.data.frame(t(pr_t_tests_100))
tr_pr_t_tests_200 <- as.data.frame(t(pr_t_tests_200))
tr_pr_t_tests_300 <- as.data.frame(t(pr_t_tests_300))
tr_pr_t_tests_400 <- as.data.frame(t(pr_t_tests_400))
tr_pr_t_tests_500 <- as.data.frame(t(pr_t_tests_500))

#Make dataframe of all t test results
pr_NRI_ttest <- rbind.fill(tr_pr_t_tests_100, tr_pr_t_tests_200, tr_pr_t_tests_300, tr_pr_t_tests_400, tr_pr_t_tests_500)
rownames(pr_NRI_ttest) <- c(rownames(tr_pr_t_tests_100), rownames(tr_pr_t_tests_200), rownames(tr_pr_t_tests_300), rownames(tr_pr_t_tests_400), rownames(tr_pr_t_tests_500))
pr_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
pr_NRI_ttest$type <- c("stat", "pvalue")
melted_pr_NRI_ttest <- melt(pr_NRI_ttest, id = c("subset", "type"))
melted_pr_NRI_ttest_star <- spread(melted_pr_NRI_ttest, type, value)
#Add column with * for significance
melted_pr_NRI_ttest_star$star <- ""
melted_pr_NRI_ttest_star$star[melted_pr_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_pr_NRI_ttest_star$star[melted_pr_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_pr_NRI_ttest_star$star[melted_pr_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_pr_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_proprandom_NRI.csv", row.names = TRUE)



#For NTI comms
pr_t_tests_100 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  prop <- Prop_all_NTI %>% 
    filter(subset == "100")
  random <- Random_prune_NTI %>% 
    filter(subset == "100")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  pr_t_tests_100 <- rbind(pr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_100) <- colnames(Prop_all_NTI[,2:15])
colnames(pr_t_tests_100) <- paste0(colnames(pr_t_tests_100), "100")


pr_t_tests_200 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  prop <- Prop_all_NTI %>% 
    filter(subset == "200")
  random <- Random_prune_NTI %>% 
    filter(subset == "200")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  pr_t_tests_200 <- rbind(pr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_200) <- colnames(Prop_all_NTI[,2:15])
colnames(pr_t_tests_200) <- paste0(colnames(pr_t_tests_200), "200")

pr_t_tests_300 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  prop <- Prop_all_NTI %>% 
    filter(subset == "300")
  random <- Random_prune_NTI %>% 
    filter(subset == "300")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  pr_t_tests_300 <- rbind(pr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_300) <- colnames(Prop_all_NTI[,2:15])
colnames(pr_t_tests_300) <- paste0(colnames(pr_t_tests_300), "300")


pr_t_tests_400 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  prop <- Prop_all_NTI %>% 
    filter(subset == "400")
  random <- Random_prune_NTI %>% 
    filter(subset == "400")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  pr_t_tests_400 <- rbind(pr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_400) <- colnames(Prop_all_NTI[,2:15])
colnames(pr_t_tests_400) <- paste0(colnames(pr_t_tests_400), "400")



pr_t_tests_500 <- list()
for (i in 2:(ncol(Prop_all_NTI)-1)){
  prop <- Prop_all_NTI %>% 
    filter(subset == "500")
  random <- Random_prune_NTI %>% 
    filter(subset == "500")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_NTI[,i])
  pr_t_tests_500 <- rbind(pr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_500) <- colnames(Prop_all_NTI[,2:15])
colnames(pr_t_tests_500) <- paste0(colnames(pr_t_tests_500), "500")

#Transpose t tests results
tr_pr_t_tests_100 <- as.data.frame(t(pr_t_tests_100))
tr_pr_t_tests_200 <- as.data.frame(t(pr_t_tests_200))
tr_pr_t_tests_300 <- as.data.frame(t(pr_t_tests_300))
tr_pr_t_tests_400 <- as.data.frame(t(pr_t_tests_400))
tr_pr_t_tests_500 <- as.data.frame(t(pr_t_tests_500))

#Make dataframe of all t test results
pr_NTI_ttest <- rbind.fill(tr_pr_t_tests_100, tr_pr_t_tests_200, tr_pr_t_tests_300, tr_pr_t_tests_400, tr_pr_t_tests_500)
rownames(pr_NTI_ttest) <- c(rownames(tr_pr_t_tests_100), rownames(tr_pr_t_tests_200), rownames(tr_pr_t_tests_300), rownames(tr_pr_t_tests_400), rownames(tr_pr_t_tests_500))
pr_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
pr_NTI_ttest$type <- c("stat", "pvalue")
melted_pr_NTI_ttest <- melt(pr_NTI_ttest, id = c("subset", "type"))
melted_pr_NTI_ttest_star <- spread(melted_pr_NTI_ttest, type, value)
#Add column with * for significance
melted_pr_NTI_ttest_star$star <- ""
melted_pr_NTI_ttest_star$star[melted_pr_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_pr_NTI_ttest_star$star[melted_pr_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_pr_NTI_ttest_star$star[melted_pr_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_pr_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_proprandom_NTI.csv", row.names = TRUE)


#For PD
pr_t_tests_100 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  prop <- Prop_all_PD %>% 
    filter(subset == "100")
  random <- Random_prune_PD %>% 
    filter(subset == "100")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  pr_t_tests_100 <- rbind(pr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_100) <- colnames(Prop_all_PD[,2:15])
colnames(pr_t_tests_100) <- paste0(colnames(pr_t_tests_100), "100")


pr_t_tests_200 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  prop <- Prop_all_PD %>% 
    filter(subset == "200")
  random <- Random_prune_PD %>% 
    filter(subset == "200")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  pr_t_tests_200 <- rbind(pr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_200) <- colnames(Prop_all_PD[,2:15])
colnames(pr_t_tests_200) <- paste0(colnames(pr_t_tests_200), "200")

pr_t_tests_300 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  prop <- Prop_all_PD %>% 
    filter(subset == "300")
  random <- Random_prune_PD %>% 
    filter(subset == "300")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  pr_t_tests_300 <- rbind(pr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_300) <- colnames(Prop_all_PD[,2:15])
colnames(pr_t_tests_300) <- paste0(colnames(pr_t_tests_300), "300")


pr_t_tests_400 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  prop <- Prop_all_PD %>% 
    filter(subset == "400")
  random <- Random_prune_PD %>% 
    filter(subset == "400")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  pr_t_tests_400 <- rbind(pr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_400) <- colnames(Prop_all_PD[,2:15])
colnames(pr_t_tests_400) <- paste0(colnames(pr_t_tests_400), "400")



pr_t_tests_500 <- list()
for (i in 2:(ncol(Prop_all_PD)-1)){
  prop <- Prop_all_PD %>% 
    filter(subset == "500")
  random <- Random_prune_PD %>% 
    filter(subset == "500")
  t_test <- (t.test(prop[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Prop_all_PD[,i])
  pr_t_tests_500 <- rbind(pr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(pr_t_tests_500) <- colnames(Prop_all_PD[,2:15])
colnames(pr_t_tests_500) <- paste0(colnames(pr_t_tests_500), "500")

#Transpose t tests results
tr_pr_t_tests_100 <- as.data.frame(t(pr_t_tests_100))
tr_pr_t_tests_200 <- as.data.frame(t(pr_t_tests_200))
tr_pr_t_tests_300 <- as.data.frame(t(pr_t_tests_300))
tr_pr_t_tests_400 <- as.data.frame(t(pr_t_tests_400))
tr_pr_t_tests_500 <- as.data.frame(t(pr_t_tests_500))

#Make dataframe of all t test results
pr_PD_ttest <- rbind.fill(tr_pr_t_tests_100, tr_pr_t_tests_200, tr_pr_t_tests_300, tr_pr_t_tests_400, tr_pr_t_tests_500)
rownames(pr_PD_ttest) <- c(rownames(tr_pr_t_tests_100), rownames(tr_pr_t_tests_200), rownames(tr_pr_t_tests_300), rownames(tr_pr_t_tests_400), rownames(tr_pr_t_tests_500))
pr_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
pr_PD_ttest$type <- c("stat", "pvalue")
melted_pr_PD_ttest <- melt(pr_PD_ttest, id = c("subset", "type"))
melted_pr_PD_ttest_star <- spread(melted_pr_PD_ttest, type, value)
#Add column with * for significance
melted_pr_PD_ttest_star$star <- ""
melted_pr_PD_ttest_star$star[melted_pr_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_pr_PD_ttest_star$star[melted_pr_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_pr_PD_ttest_star$star[melted_pr_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_pr_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_proprandom_PD.csv", row.names = TRUE)


####
#For NRI comms
sr_t_tests_100 <- list()
for (i in 2:(ncol(Set_all_NRI)-1)){
  set <- Set_all_NRI %>% 
    filter(subset == "105")
  random <- Random_prune_NRI %>% 
    filter(subset == "100")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NRI[,i])
  sr_t_tests_100 <- rbind(sr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_100) <- colnames(Set_all_NRI[,2:15])
colnames(sr_t_tests_100) <- paste0(colnames(sr_t_tests_100), "100")


sr_t_tests_200 <- list()
for (i in 2:(ncol(Set_all_NRI)-1)){
  set <- Set_all_NRI %>% 
    filter(subset == "182")
  random <- Random_prune_NRI %>% 
    filter(subset == "200")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NRI[,i])
  sr_t_tests_200 <- rbind(sr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_200) <- colnames(Set_all_NRI[,2:15])
colnames(sr_t_tests_200) <- paste0(colnames(sr_t_tests_200), "200")

sr_t_tests_300 <- list()
for (i in 2:(ncol(Set_all_NRI)-1)){
  set <- Set_all_NRI %>% 
    filter(subset == "292")
  random <- Random_prune_NRI %>% 
    filter(subset == "300")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NRI[,i])
  sr_t_tests_300 <- rbind(sr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_300) <- colnames(Set_all_NRI[,2:15])
colnames(sr_t_tests_300) <- paste0(colnames(sr_t_tests_300), "300")


sr_t_tests_400 <- list()
for (i in 2:(ncol(Set_all_NRI)-1)){
  set <- Set_all_NRI %>% 
    filter(subset == "332")
  random <- Random_prune_NRI %>% 
    filter(subset == "400")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NRI[,i])
  sr_t_tests_400 <- rbind(sr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_400) <- colnames(Set_all_NRI[,2:15])
colnames(sr_t_tests_400) <- paste0(colnames(sr_t_tests_400), "400")



sr_t_tests_500 <- list()
for (i in 2:(ncol(Set_all_NRI)-1)){
  set <- Set_all_NRI %>% 
    filter(subset == "332")
  random <- Random_prune_NRI %>% 
    filter(subset == "500")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NRI[,i])
  sr_t_tests_500 <- rbind(sr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_500) <- colnames(Set_all_NRI[,2:15])
colnames(sr_t_tests_500) <- paste0(colnames(sr_t_tests_500), "500")

#Transpose t tests results
tr_sr_t_tests_100 <- as.data.frame(t(sr_t_tests_100))
tr_sr_t_tests_200 <- as.data.frame(t(sr_t_tests_200))
tr_sr_t_tests_300 <- as.data.frame(t(sr_t_tests_300))
tr_sr_t_tests_400 <- as.data.frame(t(sr_t_tests_400))
tr_sr_t_tests_500 <- as.data.frame(t(sr_t_tests_500))

#Make dataframe of all t test results
sr_NRI_ttest <- rbind.fill(tr_sr_t_tests_100, tr_sr_t_tests_200, tr_sr_t_tests_300, tr_sr_t_tests_400, tr_sr_t_tests_500)
rownames(sr_NRI_ttest) <- c(rownames(tr_sr_t_tests_100), rownames(tr_sr_t_tests_200), rownames(tr_sr_t_tests_300), rownames(tr_sr_t_tests_400), rownames(tr_sr_t_tests_500))
sr_NRI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
sr_NRI_ttest$type <- c("stat", "pvalue")
melted_sr_NRI_ttest <- melt(sr_NRI_ttest, id = c("subset", "type"))
melted_sr_NRI_ttest_star <- spread(melted_sr_NRI_ttest, type, value)
#Add column with * for significance
melted_sr_NRI_ttest_star$star <- ""
melted_sr_NRI_ttest_star$star[melted_sr_NRI_ttest_star$pvalue <= 0.05] <- "*"
melted_sr_NRI_ttest_star$star[melted_sr_NRI_ttest_star$pvalue <= 0.01] <- "**"
melted_sr_NRI_ttest_star$star[melted_sr_NRI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_sr_NRI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_setrandom_NRI.csv", row.names = TRUE)


#For NTI comms
sr_t_tests_100 <- list()
for (i in 2:(ncol(Set_all_NTI)-1)){
  set <- Set_all_NTI %>% 
    filter(subset == "105")
  random <- Random_prune_NTI %>% 
    filter(subset == "100")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NTI[,i])
  sr_t_tests_100 <- rbind(sr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_100) <- colnames(Set_all_NTI[,2:15])
colnames(sr_t_tests_100) <- paste0(colnames(sr_t_tests_100), "100")


sr_t_tests_200 <- list()
for (i in 2:(ncol(Set_all_NTI)-1)){
  set <- Set_all_NTI %>% 
    filter(subset == "182")
  random <- Random_prune_NTI %>% 
    filter(subset == "200")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NTI[,i])
  sr_t_tests_200 <- rbind(sr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_200) <- colnames(Set_all_NTI[,2:15])
colnames(sr_t_tests_200) <- paste0(colnames(sr_t_tests_200), "200")

sr_t_tests_300 <- list()
for (i in 2:(ncol(Set_all_NTI)-1)){
  set <- Set_all_NTI %>% 
    filter(subset == "292")
  random <- Random_prune_NTI %>% 
    filter(subset == "300")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NTI[,i])
  sr_t_tests_300 <- rbind(sr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_300) <- colnames(Set_all_NTI[,2:15])
colnames(sr_t_tests_300) <- paste0(colnames(sr_t_tests_300), "300")


sr_t_tests_400 <- list()
for (i in 2:(ncol(Set_all_NTI)-1)){
  set <- Set_all_NTI %>% 
    filter(subset == "332")
  random <- Random_prune_NTI %>% 
    filter(subset == "400")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NTI[,i])
  sr_t_tests_400 <- rbind(sr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_400) <- colnames(Set_all_NTI[,2:15])
colnames(sr_t_tests_400) <- paste0(colnames(sr_t_tests_400), "400")



sr_t_tests_500 <- list()
for (i in 2:(ncol(Set_all_NTI)-1)){
  set <- Set_all_NTI %>% 
    filter(subset == "332")
  random <- Random_prune_NTI %>% 
    filter(subset == "500")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_NTI[,i])
  sr_t_tests_500 <- rbind(sr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_500) <- colnames(Set_all_NTI[,2:15])
colnames(sr_t_tests_500) <- paste0(colnames(sr_t_tests_500), "500")

#Transpose t tests results
tr_sr_t_tests_100 <- as.data.frame(t(sr_t_tests_100))
tr_sr_t_tests_200 <- as.data.frame(t(sr_t_tests_200))
tr_sr_t_tests_300 <- as.data.frame(t(sr_t_tests_300))
tr_sr_t_tests_400 <- as.data.frame(t(sr_t_tests_400))
tr_sr_t_tests_500 <- as.data.frame(t(sr_t_tests_500))

#Make dataframe of all t test results
sr_NTI_ttest <- rbind.fill(tr_sr_t_tests_100, tr_sr_t_tests_200, tr_sr_t_tests_300, tr_sr_t_tests_400, tr_sr_t_tests_500)
rownames(sr_NTI_ttest) <- c(rownames(tr_sr_t_tests_100), rownames(tr_sr_t_tests_200), rownames(tr_sr_t_tests_300), rownames(tr_sr_t_tests_400), rownames(tr_sr_t_tests_500))
sr_NTI_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
sr_NTI_ttest$type <- c("stat", "pvalue")
melted_sr_NTI_ttest <- melt(sr_NTI_ttest, id = c("subset", "type"))
melted_sr_NTI_ttest_star <- spread(melted_sr_NTI_ttest, type, value)
#Add column with * for significance
melted_sr_NTI_ttest_star$star <- ""
melted_sr_NTI_ttest_star$star[melted_sr_NTI_ttest_star$pvalue <= 0.05] <- "*"
melted_sr_NTI_ttest_star$star[melted_sr_NTI_ttest_star$pvalue <= 0.01] <- "**"
melted_sr_NTI_ttest_star$star[melted_sr_NTI_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_sr_NTI_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_setrandom_NTI.csv", row.names = TRUE)



#For PD comms
sr_t_tests_100 <- list()
for (i in 2:(ncol(Set_all_PD)-1)){
  set <- Set_all_PD %>% 
    filter(subset == "105")
  random <- Random_prune_PD %>% 
    filter(subset == "100")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_PD[,i])
  sr_t_tests_100 <- rbind(sr_t_tests_100, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_100) <- colnames(Set_all_PD[,2:15])
colnames(sr_t_tests_100) <- paste0(colnames(sr_t_tests_100), "100")


sr_t_tests_200 <- list()
for (i in 2:(ncol(Set_all_PD)-1)){
  set <- Set_all_PD %>% 
    filter(subset == "182")
  random <- Random_prune_PD %>% 
    filter(subset == "200")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_PD[,i])
  sr_t_tests_200 <- rbind(sr_t_tests_200, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_200) <- colnames(Set_all_PD[,2:15])
colnames(sr_t_tests_200) <- paste0(colnames(sr_t_tests_200), "200")

sr_t_tests_300 <- list()
for (i in 2:(ncol(Set_all_PD)-1)){
  set <- Set_all_PD %>% 
    filter(subset == "292")
  random <- Random_prune_PD %>% 
    filter(subset == "300")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_PD[,i])
  sr_t_tests_300 <- rbind(sr_t_tests_300, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_300) <- colnames(Set_all_PD[,2:15])
colnames(sr_t_tests_300) <- paste0(colnames(sr_t_tests_300), "300")


sr_t_tests_400 <- list()
for (i in 2:(ncol(Set_all_PD)-1)){
  set <- Set_all_PD %>% 
    filter(subset == "332")
  random <- Random_prune_PD %>% 
    filter(subset == "400")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_PD[,i])
  sr_t_tests_400 <- rbind(sr_t_tests_400, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_400) <- colnames(Set_all_PD[,2:15])
colnames(sr_t_tests_400) <- paste0(colnames(sr_t_tests_400), "400")



sr_t_tests_500 <- list()
for (i in 2:(ncol(Set_all_PD)-1)){
  set <- Set_all_PD %>% 
    filter(subset == "332")
  random <- Random_prune_PD %>% 
    filter(subset == "500")
  t_test <- (t.test(set[,i], random[,i]))
  t_test_values <- as.data.frame(c(t_test[1], t_test[3]))
  rownames(t_test_values) <- colnames(Set_all_PD[,i])
  sr_t_tests_500 <- rbind(sr_t_tests_500, t_test_values, make.row.names = FALSE)
  
}
rownames(sr_t_tests_500) <- colnames(Set_all_PD[,2:15])
colnames(sr_t_tests_500) <- paste0(colnames(sr_t_tests_500), "500")

#Transpose t tests results
tr_sr_t_tests_100 <- as.data.frame(t(sr_t_tests_100))
tr_sr_t_tests_200 <- as.data.frame(t(sr_t_tests_200))
tr_sr_t_tests_300 <- as.data.frame(t(sr_t_tests_300))
tr_sr_t_tests_400 <- as.data.frame(t(sr_t_tests_400))
tr_sr_t_tests_500 <- as.data.frame(t(sr_t_tests_500))

#Make dataframe of all t test results
sr_PD_ttest <- rbind.fill(tr_sr_t_tests_100, tr_sr_t_tests_200, tr_sr_t_tests_300, tr_sr_t_tests_400, tr_sr_t_tests_500)
rownames(sr_PD_ttest) <- c(rownames(tr_sr_t_tests_100), rownames(tr_sr_t_tests_200), rownames(tr_sr_t_tests_300), rownames(tr_sr_t_tests_400), rownames(tr_sr_t_tests_500))
sr_PD_ttest$subset <- as.integer(c("100", "100", "200", "200", "300", "300", "400", "400", "500", "500"))
sr_PD_ttest$type <- c("stat", "pvalue")
melted_sr_PD_ttest <- melt(sr_PD_ttest, id = c("subset", "type"))
melted_sr_PD_ttest_star <- spread(melted_sr_PD_ttest, type, value)
#Add column with * for significance
melted_sr_PD_ttest_star$star <- ""
melted_sr_PD_ttest_star$star[melted_sr_PD_ttest_star$pvalue <= 0.05] <- "*"
melted_sr_PD_ttest_star$star[melted_sr_PD_ttest_star$pvalue <= 0.01] <- "**"
melted_sr_PD_ttest_star$star[melted_sr_PD_ttest_star$pvalue <= 0.001] <- "***"

write.csv(melted_sr_PD_ttest_star, "./Plots/2017_botany/Files_to_plot/Ttest_setrandom_PD.csv", row.names = TRUE)
