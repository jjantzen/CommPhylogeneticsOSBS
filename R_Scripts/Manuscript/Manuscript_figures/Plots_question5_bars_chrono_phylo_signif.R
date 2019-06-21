#Question 3 - plotting by proportion clustered, overdispersed and random
#Aggregate by community to reduce number of plots
#Barplots (sum to 1) with significance coloured
#or points with different colours or shapes for clust/over/random
#for plots- still need to separate communities by patterns for clarity

#Read data with pvalues for pruned subsets and overall dataset, as well as differences
NRIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

NRIp_ult <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_ult <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_ult <- read.csv("./Plots/2017_botany/Files_to_plot/PD_ult_index_pvalue.csv", stringsAsFactors = FALSE)

# NRI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NRI.csv", stringsAsFactors = FALSE)
# NTI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NTI.csv", stringsAsFactors = FALSE)
# PD_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_PD.csv", stringsAsFactors = FALSE)

#Correct spelling
NRIp_prune$variable[which(NRIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_prune$variable[which(NTIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_prune$variable[which(PDp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

NRIp_ult$variable[which(NRIp_ult$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_ult$variable[which(NTIp_ult$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_ult$variable[which(PDp_ult$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

# NRI_diff$variable[which(NRI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# NTI_diff$variable[which(NTI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# PD_diff$variable[which(PD_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

#Make categories for clustered, overdispersed and random
NRIp_prune$category[which(NRIp_prune$pvalue <= 0.05)] <- "Clustered" 
NRIp_prune$category[which(NRIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_prune$category[which(NRIp_prune$pvalue > 0.05 & NRIp_prune$pvalue < 0.95)] <- "Random"

NRIp_ult$category <- NA
NRIp_ult$category[which(NRIp_ult$pvalue <= 0.05)] <- "Clustered" 
NRIp_ult$category[which(NRIp_ult$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_ult$category[which(NRIp_ult$pvalue > 0.05 & NRIp_ult$pvalue < 0.95)] <- "Random"

NTIp_prune$category <- NA
NTIp_prune$category[which(NTIp_prune$pvalue <= 0.05)] <- "Clustered" 
NTIp_prune$category[which(NTIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_prune$category[which(NTIp_prune$pvalue > 0.05 & NTIp_prune$pvalue < 0.95)] <- "Random"

NTIp_ult$category <- NA
NTIp_ult$category[which(NTIp_ult$pvalue <= 0.05)] <- "Clustered" 
NTIp_ult$category[which(NTIp_ult$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_ult$category[which(NTIp_ult$pvalue > 0.05 & NTIp_ult$pvalue < 0.95)] <- "Random"

PDp_prune$category <- NA
PDp_prune$category[which(PDp_prune$pvalue <= 0.05)] <- "Clustered" 
PDp_prune$category[which(PDp_prune$pvalue >= 0.95)] <- "Overdispersed" 
PDp_prune$category[which(PDp_prune$pvalue > 0.05 & PDp_prune$pvalue < 0.95)] <- "Random"

PDp_ult$category <- NA
PDp_ult$category[which(PDp_ult$pvalue <= 0.05)] <- "Clustered" 
PDp_ult$category[which(PDp_ult$pvalue >= 0.95)] <- "Overdispersed" 
PDp_ult$category[which(PDp_ult$pvalue > 0.05 & PDp_ult$pvalue < 0.95)] <- "Random"

#Add column for number of reps
num_reps_prune <- NRIp_prune %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

num_reps_ult <- NRIp_ult %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

#Get proportions clustered
prop_sig_NRI_prune <- NRIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NRI_ult <- NRIp_ult %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_prune <- NTIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_ult <- NTIp_ult %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_prune <- PDp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_ult <- PDp_ult %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

#Change column names
colnames(prop_sig_NRI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_prune) <- c("Community", "Subset", "Category", "Number")

colnames(prop_sig_NRI_ult) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_ult) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_ult) <- c("Community", "Subset", "Category", "Number")

#Calculate proportion (divide by 100)
#Have num_reps object with subset, community and num reps
#Have object with number of significant per subset and community
#for each row, check the community and subset, and divide number by number of replicates and put into proportion_sig for that row

for (i in 1:nrow(prop_sig_NRI_prune)){
  subset <- prop_sig_NRI_prune$Subset[i]
  community <- prop_sig_NRI_prune$Community[i]
  num_reps <- num_reps_prune$n[which(num_reps_prune$subset == subset & num_reps_prune$variable == community)]
  prop_sig <- prop_sig_NRI_prune$Number[i]/num_reps
  prop_sig_NRI_prune$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_NTI_prune)){
  subset <- prop_sig_NTI_prune$Subset[i]
  community <- prop_sig_NTI_prune$Community[i]
  num_reps <- num_reps_prune$n[which(num_reps_prune$subset == subset & num_reps_prune$variable == community)]
  prop_sig <- prop_sig_NTI_prune$Number[i]/num_reps
  prop_sig_NTI_prune$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_PD_prune)){
  subset <- prop_sig_PD_prune$Subset[i]
  community <- prop_sig_PD_prune$Community[i]
  num_reps <- num_reps_prune$n[which(num_reps_prune$subset == subset & num_reps_prune$variable == community)]
  prop_sig <- prop_sig_PD_prune$Number[i]/num_reps
  prop_sig_PD_prune$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_NRI_ult)){
  subset <- prop_sig_NRI_ult$Subset[i]
  community <- prop_sig_NRI_ult$Community[i]
  num_reps <- num_reps_ult$n[which(num_reps_ult$subset == subset & num_reps_ult$variable == community)]
  prop_sig <- prop_sig_NRI_ult$Number[i]/num_reps
  prop_sig_NRI_ult$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_NTI_ult)){
  subset <- prop_sig_NTI_ult$Subset[i]
  community <- prop_sig_NTI_ult$Community[i]
  num_reps <- num_reps_ult$n[which(num_reps_ult$subset == subset & num_reps_ult$variable == community)]
  prop_sig <- prop_sig_NTI_ult$Number[i]/num_reps
  prop_sig_NTI_ult$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_PD_ult)){
  subset <- prop_sig_PD_ult$Subset[i]
  community <- prop_sig_PD_ult$Community[i]
  num_reps <- num_reps_ult$n[which(num_reps_ult$subset == subset & num_reps_ult$variable == community)]
  prop_sig <- prop_sig_PD_ult$Number[i]/num_reps
  prop_sig_PD_ult$Proportion_sig[i] <- prop_sig
}

#Add columm for recon or prune
prop_sig_NRI_prune$type <- "Phylo"
prop_sig_NRI_ult$type <- "Chrono"

prop_sig_NTI_prune$type <- "Phylo"
prop_sig_NTI_ult$type <- "Chrono"

prop_sig_PD_prune$type <- "Phylo"
prop_sig_PD_ult$type <- "Chrono"

# #Combine recon and prune into one dataset - not for barplot
# NRI_ultprune <- rbind(prop_sig_NRI_prune, prop_sig_NRI_ult)
# NTI_ultprune <- rbind(prop_sig_NTI_prune, prop_sig_NTI_ult)
# PD_ultprune <- rbind(prop_sig_PD_prune, prop_sig_PD_ult)
# 
# NRI_ultprune <- NRI_ultprune[which(complete.cases(NRI_ultprune)),]
# NTI_ultprune <- NTI_ultprune[which(complete.cases(NTI_ultprune)),]
# PD_ultprune <- PD_ultprune[which(complete.cases(PD_ultprune)),]
# 
# #Remove NAs - not for barplot
# NRI_ultprune <- NRI_ultprune[which(complete.cases(NRI_ultprune)),]
# NTI_ultprune <- NTI_ultprune[which(complete.cases(NTI_ultprune)),]
# PD_ultprune <- PD_ultprune[which(complete.cases(PD_ultprune)),]

#Make communities factors
prop_sig_NRI_prune$Community <- as.factor(prop_sig_NRI_prune$Community)
prop_sig_NTI_prune$Community <- as.factor(prop_sig_NTI_prune$Community)
prop_sig_PD_prune$Community <- as.factor(prop_sig_PD_prune$Community)

prop_sig_NRI_ult$Community <- as.factor(prop_sig_NRI_ult$Community)
prop_sig_NTI_ult$Community <- as.factor(prop_sig_NTI_ult$Community)
prop_sig_PD_ult$Community <- as.factor(prop_sig_PD_ult$Community)

#Make communities factors
prop_sig_NRI_prune$Category <- as.factor(prop_sig_NRI_prune$Category)
prop_sig_NTI_prune$Category <- as.factor(prop_sig_NTI_prune$Category)
prop_sig_PD_prune$Category <- as.factor(prop_sig_PD_prune$Category)

prop_sig_NRI_ult$Category <- as.factor(prop_sig_NRI_ult$Category)
prop_sig_NTI_ult$Category <- as.factor(prop_sig_NTI_ult$Category)
prop_sig_PD_ult$Category <- as.factor(prop_sig_PD_ult$Category)

#Rename communities to remove . and add space
levels(prop_sig_NRI_prune$Community)
levels(prop_sig_NRI_prune$Community) <- gsub(".", " ", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub(".", " ", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub(".", " ", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_NRI_ult$Community) <- gsub(".", " ", levels(prop_sig_NRI_ult$Community), fixed = TRUE)
levels(prop_sig_NTI_ult$Community) <- gsub(".", " ", levels(prop_sig_NTI_ult$Community), fixed = TRUE)
levels(prop_sig_PD_ult$Community) <- gsub(".", " ", levels(prop_sig_PD_ult$Community), fixed = TRUE)


#Reorder factors of Categories
prop_sig_NRI_prune$Category <- factor(prop_sig_NRI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_prune$Category <- factor(prop_sig_NTI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_prune$Category <- factor(prop_sig_PD_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))

prop_sig_NRI_ult$Category <- factor(prop_sig_NRI_ult$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_ult$Category <- factor(prop_sig_NTI_ult$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_ult$Category <- factor(prop_sig_PD_ult$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))

#Make barplot of all categories together by community and subset 
#need some formatting before including in resubmission

#Get colours for plotting
mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"

#Adding line break to names of Abandoned field 
levels(prop_sig_NRI_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NRI_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NRI_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NRI_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NRI_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NRI_prune$Community), fixed = TRUE)

levels(prop_sig_NRI_ult$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NRI_ult$Community), fixed = TRUE)
levels(prop_sig_NRI_ult$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NRI_ult$Community), fixed = TRUE)
levels(prop_sig_NRI_ult$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NRI_ult$Community), fixed = TRUE)
levels(prop_sig_NRI_ult$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NRI_ult$Community), fixed = TRUE)
levels(prop_sig_NRI_ult$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NRI_ult$Community), fixed = TRUE)

levels(prop_sig_NTI_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_prune$Community), fixed = TRUE)

levels(prop_sig_NTI_ult$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_ult$Community), fixed = TRUE)
levels(prop_sig_NTI_ult$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_ult$Community), fixed = TRUE)
levels(prop_sig_NTI_ult$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_ult$Community), fixed = TRUE)
levels(prop_sig_NTI_ult$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_ult$Community), fixed = TRUE)
levels(prop_sig_NTI_ult$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_ult$Community), fixed = TRUE)

levels(prop_sig_PD_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_PD_ult$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_ult$Community), fixed = TRUE)
levels(prop_sig_PD_ult$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_ult$Community), fixed = TRUE)
levels(prop_sig_PD_ult$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_ult$Community), fixed = TRUE)
levels(prop_sig_PD_ult$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_ult$Community), fixed = TRUE)
levels(prop_sig_PD_ult$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_ult$Community), fixed = TRUE)

#Make barplot of all categories together by community and subset


### for final plots (june 2019)
pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NRI_bar_pruneult_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Phylogram")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"), plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), plot.title = element_text(face = "bold"), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NRI_ult, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  #guides(fill = FALSE)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Chronogram")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0.25), "cm"), 
        legend.justification = c(1.05, 0), legend.position = c(1.05, 0), 
        legend.direction = "vertical", panel.background = element_blank(), 
        strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), axis.ticks.x = element_blank(), plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
  #guides(fill = guide_legend(nrow = 2))

#plot two plots one on top of each other - need to save this one
#two_plots <- (p1 | p2) / legend #+ plot_layout(ncol = 1)
two_plots <- (p1 | p2)
plot(two_plots)
ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NRI_bar_prune_ult_signif_final_plot.pdf", two_plots, dpi = 1000, height = 180, width = 180, units = "mm")


dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NTI_bar_pruneult_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Phylogram")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NTI_ult, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Chronogram")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/PD_bar_pruneult_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Phylogram")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_PD_ult, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Chronogram")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2
#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()


####After this not useful

#Make barplot of all categories together by community and subset
pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/NRI_bar_proportions_significance.pdf")
ggplot(NRI_ultprune, aes(x = Subset, y = Proportion_sig, fill = interaction(Category, type)))+
  geom_bar(stat = "identity")+
  facet_grid(~ Community)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/NTI_bar_proportions_significance.pdf")
ggplot(NTI_ultprune, aes(x = Subset, y = Proportion_sig, fill = interaction(Category,type)))+
  geom_bar(stat = "identity")+
  facet_grid(~ Community)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/PD_bar_proportions_significance.pdf")
ggplot(PD_ultprune, aes(x = Subset, y = Proportion_sig, fill = interaction(Category,type)))+
  geom_bar(stat = "identity")+
  facet_grid(~ Community)
dev.off()


NRIa <- c("Basin.Swamp", "Pine.Plantation","Xeric.Hammock", "Scrubby.Flatwoods", "Successional.Hardwood.Forest", "Clastic.Upland.Lake", "Mesic.Hammock", "Baygall")
NRIb <- c("Basin.Marsh", "Abandoned.Field.Pasture", "Sandhill", "Sandhill.Upland.Lake")

for (i in 1:nrow(NRI_ultprune)){
  if (NRI_ultprune$Community[i] %in% NRIa){
    NRI_ultprune$plot_group[i] <- "Different"
  } else {
    NRI_ultprune$plot_group[i] <- "Similar"
  } 
}


comm_names <- unique(NRI_ultprune$Community)
str(comm_names)
levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
levels(comm_names)

levels(NRI_ultprune$Community) <- levels(comm_names)

#Make scatterplot with aggregates of communtiies and with coloured by category
pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/NRI_points_proportions_significance_formatted.pdf")
ggplot(NRI_ultprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset", linetype = "Phylogeny Type")+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Chronogram", "Phylogram"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))

dev.off()

####Plot single frame
pdf("./Plots/2017_botany/Paper_plots/single_ultprune.pdf")
ggplot(NRI_ultprune[which(NRI_ultprune$Community == "Mesic Hammock"),], aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset", linetype = "Phylogeny Type")+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Chronogram", "Phylogram"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))
dev.off()




NTIa <- c("Basin.Marsh","Basin.Swamp", "Mesic.Hammock", "Improved","Baygall")
NTIb <- c("Xeric.Hammock", "Successional.Hardwood.Forest", "Sandhill.Upland.Lake", "Pine.Plantation", "Scrubby.Flatwoods", "Abandoned.Field.Pasture", "Sandhill", "Clastic.Upland.Lake",  "Lake.Bottom")

for (i in 1:nrow(NTI_ultprune)){
  if (NTI_ultprune$Community[i] %in% NTIa){
    NTI_ultprune$plot_group[i] <- "Different"
  } else {
    NTI_ultprune$plot_group[i] <- "Similar"
  } 
}


pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/NTI_points_proportions_significance_formatted.pdf")
ggplot(NTI_ultprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ interaction(plot_group,Category), ncol = 2)+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 3. Proportion of significantly non-random patterns of NTI \ncan diverge between chronograms and phylograms for different \ncommunities across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Phylogeny Type")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Chronogram", "Phylogram"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))
dev.off()

PDa <- c("Basin.Swamp", "Improved", "Scrubby.Flatwoods", "Clastic.Upland.Lake", "Mesic.Hammock", "Baygall")
PDb <- c("Basin.Marsh", "Pine.Plantation","Xeric.Hammock", "Abandoned.Field.Pasture", "Successional.Hardwood.Forest", "Sandhill", "Sandhill.Upland.Lake", "Lake.Bottom")

for (i in 1:nrow(PD_ultprune)){
  if (PD_ultprune$Community[i] %in% PDa){
    PD_ultprune$plot_group[i] <- "Different"
  } else {
    PD_ultprune$plot_group[i] <- "Similar"
  } 
}

#Need different categories for these following plots based on actual patterns not NRI patterns
pdf("./Plots/2017_botany/Paper_plots/Question3/Barplots/PD_points_proportions_significance_formatted.pdf")
ggplot(PD_ultprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ interaction(plot_group, Category), ncol = 2)+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 3. Proportion of significantly non-random patterns of PD \ncan diverge between chronograms and phylograms for different \ncommunities across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Phylogeny Type")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Chronogram", "Phylogram"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))
dev.off()

###
