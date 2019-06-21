#Question  - plotting by proportion clustered, overdispersed and random
#Aggregate by community to reduce number of plots
#Barplots (sum to 1) first - coloured by significance
#or points with different colours or shapes for clust/over/random
#for plots- still need to separate communities by patterns for clarity

#Read data with pvalues for pruned subsets and overall dataset, as well as differences
NRIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

NRIp_recon <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_recon <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_recon <- read.csv("./Plots/2017_botany/Files_to_plot/PD_recon_index_pvalue.csv", stringsAsFactors = FALSE)

# NRI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NRI.csv", stringsAsFactors = FALSE)
# NTI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NTI.csv", stringsAsFactors = FALSE)
# PD_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_PD.csv", stringsAsFactors = FALSE)

#Correct spelling
NRIp_prune$variable[which(NRIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_prune$variable[which(NTIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_prune$variable[which(PDp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

NRIp_recon$variable[which(NRIp_recon$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_recon$variable[which(NTIp_recon$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_recon$variable[which(PDp_recon$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

# NRI_diff$variable[which(NRI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# NTI_diff$variable[which(NTI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# PD_diff$variable[which(PD_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

#Make categories for clustered, overdispersed and random
NRIp_prune$category[which(NRIp_prune$pvalue <= 0.05)] <- "Clustered" 
NRIp_prune$category[which(NRIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_prune$category[which(NRIp_prune$pvalue > 0.05 & NRIp_prune$pvalue < 0.95)] <- "Random"

NRIp_recon$category[which(NRIp_recon$pvalue <= 0.05)] <- "Clustered" 
NRIp_recon$category[which(NRIp_recon$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_recon$category[which(NRIp_recon$pvalue > 0.05 & NRIp_recon$pvalue < 0.95)] <- "Random"

NTIp_prune$category <- NA
NTIp_prune$category[which(NTIp_prune$pvalue <= 0.05)] <- "Clustered" 
NTIp_prune$category[which(NTIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_prune$category[which(NTIp_prune$pvalue > 0.05 & NTIp_prune$pvalue < 0.95)] <- "Random"

NTIp_recon$category <- NA
NTIp_recon$category[which(NTIp_recon$pvalue <= 0.05)] <- "Clustered" 
NTIp_recon$category[which(NTIp_recon$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_recon$category[which(NTIp_recon$pvalue > 0.05 & NTIp_recon$pvalue < 0.95)] <- "Random"

PDp_prune$category <- NA
PDp_prune$category[which(PDp_prune$pvalue <= 0.05)] <- "Clustered" 
PDp_prune$category[which(PDp_prune$pvalue >= 0.95)] <- "Overdispersed" 
PDp_prune$category[which(PDp_prune$pvalue > 0.05 & PDp_prune$pvalue < 0.95)] <- "Random"

PDp_recon$category <- NA
PDp_recon$category[which(PDp_recon$pvalue <= 0.05)] <- "Clustered" 
PDp_recon$category[which(PDp_recon$pvalue >= 0.95)] <- "Overdispersed" 
PDp_recon$category[which(PDp_recon$pvalue > 0.05 & PDp_recon$pvalue < 0.95)] <- "Random"

#Add column for number of reps
num_reps_prune <- NRIp_prune %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

num_reps_recon <- NRIp_recon %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

#Get proportions clustered
prop_sig_NRI_prune <- NRIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NRI_recon <- NRIp_recon %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_prune <- NTIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_recon <- NTIp_recon %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_prune <- PDp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_recon <- PDp_recon %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

#Change column names
colnames(prop_sig_NRI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_prune) <- c("Community", "Subset", "Category", "Number")

colnames(prop_sig_NRI_recon) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_recon) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_recon) <- c("Community", "Subset", "Category", "Number")


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

for (i in 1:nrow(prop_sig_NRI_recon)){
  subset <- prop_sig_NRI_recon$Subset[i]
  community <- prop_sig_NRI_recon$Community[i]
  num_reps <- num_reps_recon$n[which(num_reps_recon$subset == subset & num_reps_recon$variable == community)]
  prop_sig <- prop_sig_NRI_recon$Number[i]/num_reps
  prop_sig_NRI_recon$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_NTI_recon)){
  subset <- prop_sig_NTI_recon$Subset[i]
  community <- prop_sig_NTI_recon$Community[i]
  num_reps <- num_reps_recon$n[which(num_reps_recon$subset == subset & num_reps_recon$variable == community)]
  prop_sig <- prop_sig_NTI_recon$Number[i]/num_reps
  prop_sig_NTI_recon$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_PD_recon)){
  subset <- prop_sig_PD_recon$Subset[i]
  community <- prop_sig_PD_recon$Community[i]
  num_reps <- num_reps_recon$n[which(num_reps_recon$subset == subset & num_reps_recon$variable == community)]
  prop_sig <- prop_sig_PD_recon$Number[i]/num_reps
  prop_sig_PD_recon$Proportion_sig[i] <- prop_sig
}

#Add columm for recon or prune
prop_sig_NRI_prune$type <- "Prune"
prop_sig_NRI_recon$type <- "Recon"

prop_sig_NTI_prune$type <- "Prune"
prop_sig_NTI_recon$type <- "Recon"

prop_sig_PD_prune$type <- "Prune"
prop_sig_PD_recon$type <- "Recon"

# #Combine recon and prune into one dataset - not for barplots
# NRI_reconprune <- rbind(prop_sig_NRI_prune, prop_sig_NRI_recon)
# NTI_reconprune <- rbind(prop_sig_NTI_prune, prop_sig_NTI_recon)
# PD_reconprune <- rbind(prop_sig_PD_prune, prop_sig_PD_recon)
# 
# NRI_reconprune <- NRI_reconprune[which(complete.cases(NRI_reconprune)),]
# NTI_reconprune <- NTI_reconprune[which(complete.cases(NTI_reconprune)),]
# PD_reconprune <- PD_reconprune[which(complete.cases(PD_reconprune)),]

# #Remove NAs - don't remove for barplots
# NRI_reconprune <- NRI_reconprune[which(complete.cases(NRI_reconprune)),]
# NTI_reconprune <- NTI_reconprune[which(complete.cases(NTI_reconprune)),]
# PD_reconprune <- PD_reconprune[which(complete.cases(PD_reconprune)),]

#Make communities factors
prop_sig_NRI_prune$Community <- as.factor(prop_sig_NRI_prune$Community)
prop_sig_NTI_prune$Community <- as.factor(prop_sig_NTI_prune$Community)
prop_sig_PD_prune$Community <- as.factor(prop_sig_PD_prune$Community)

prop_sig_NRI_recon$Community <- as.factor(prop_sig_NRI_recon$Community)
prop_sig_NTI_recon$Community <- as.factor(prop_sig_NTI_recon$Community)
prop_sig_PD_recon$Community <- as.factor(prop_sig_PD_recon$Community)

#Make communities factors
prop_sig_NRI_prune$Category <- as.factor(prop_sig_NRI_prune$Category)
prop_sig_NTI_prune$Category <- as.factor(prop_sig_NTI_prune$Category)
prop_sig_PD_prune$Category <- as.factor(prop_sig_PD_prune$Category)

prop_sig_NRI_recon$Category <- as.factor(prop_sig_NRI_recon$Category)
prop_sig_NTI_recon$Category <- as.factor(prop_sig_NTI_recon$Category)
prop_sig_PD_recon$Category <- as.factor(prop_sig_PD_recon$Category)

#Rename communities to remove . and add space
levels(prop_sig_NRI_prune$Community)
levels(prop_sig_NRI_prune$Community) <- gsub(".", " ", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub(".", " ", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub(".", " ", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_NRI_recon$Community) <- gsub(".", " ", levels(prop_sig_NRI_recon$Community), fixed = TRUE)
levels(prop_sig_NTI_recon$Community) <- gsub(".", " ", levels(prop_sig_NTI_recon$Community), fixed = TRUE)
levels(prop_sig_PD_recon$Community) <- gsub(".", " ", levels(prop_sig_PD_recon$Community), fixed = TRUE)


#Reorder factors of Categories
prop_sig_NRI_prune$Category <- factor(prop_sig_NRI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_prune$Category <- factor(prop_sig_NTI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_prune$Category <- factor(prop_sig_PD_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))

prop_sig_NRI_recon$Category <- factor(prop_sig_NRI_recon$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_recon$Category <- factor(prop_sig_NTI_recon$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_recon$Category <- factor(prop_sig_PD_recon$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))

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

levels(prop_sig_NRI_recon$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NRI_recon$Community), fixed = TRUE)
levels(prop_sig_NRI_recon$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NRI_recon$Community), fixed = TRUE)
levels(prop_sig_NRI_recon$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NRI_recon$Community), fixed = TRUE)
levels(prop_sig_NRI_recon$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NRI_recon$Community), fixed = TRUE)
levels(prop_sig_NRI_recon$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NRI_recon$Community), fixed = TRUE)

levels(prop_sig_NTI_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_prune$Community), fixed = TRUE)

levels(prop_sig_NTI_recon$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_recon$Community), fixed = TRUE)
levels(prop_sig_NTI_recon$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_recon$Community), fixed = TRUE)
levels(prop_sig_NTI_recon$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_recon$Community), fixed = TRUE)
levels(prop_sig_NTI_recon$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_recon$Community), fixed = TRUE)
levels(prop_sig_NTI_recon$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_recon$Community), fixed = TRUE)

levels(prop_sig_PD_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_PD_recon$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_recon$Community), fixed = TRUE)
levels(prop_sig_PD_recon$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_recon$Community), fixed = TRUE)
levels(prop_sig_PD_recon$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_recon$Community), fixed = TRUE)
levels(prop_sig_PD_recon$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_recon$Community), fixed = TRUE)
levels(prop_sig_PD_recon$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_recon$Community), fixed = TRUE)

#Make barplot of all categories together by community and subset
pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question4/NRI_bar_reconprune_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Pruned phylogeny")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0.25,0), "cm"), plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), axis.ticks.x = element_blank(), plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NRI_recon, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Reconstructed phylogeny")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0.25,0.25), "cm"), 
        legend.justification = c(1.05, 0), legend.position = c(1.05, 0), 
        legend.direction = "vertical", panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), axis.ticks.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5), plot.title = element_text(face = "bold"))
p2

#plot two plots one on top of each other - need to save this one
two_plots <- p1 + p2 #+ plot_layout(ncol = 1)


ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question4/NRI_bar_recon_prune_signif_final_plot.pdf", two_plots, dpi = 1000, height = 180, width = 180, units = "mm")



dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question4/NTI_bar_reconprune_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Pruned phylogeny")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NTI_recon, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Reconstructed phylogeny")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question4/PD_bar_reconprune_proportions_significance.pdf", height = 9, width = 19)
p1 <- ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Pruned phylogeny")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_PD_recon, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Reconstructed phylogeny")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()


##########
#After here, not useful - separated into multiple groups

lista <- c("Xeric.Hammock", "Scrubby.Flatwoods", "Clastic.Upland.Lake", "Baygall", "Sandhill", "Basin.Swamp", "Sandhill.Upland.Lake")
listc <- c("Basin.Marsh", "Pine.Plantation", "Abandoned.Field.Pasture", "Mesic.Hammock", "Successional.Hardwood.Forest")

for (i in 1:nrow(NRI_reconprune)){
  if (NRI_reconprune$Community[i] %in% lista){
    NRI_reconprune$plot_group[i] <- "Increasing"
  } else {
    NRI_reconprune$plot_group[i] <- "Random"
  }
}

for (i in 1:nrow(NTI_reconprune)){
  if (NTI_reconprune$Community[i] %in% lista){
    NTI_reconprune$plot_group[i] <- "Increasing"
  } else {
    NTI_reconprune$plot_group[i] <- "Random"
  }
}

for (i in 1:nrow(PD_reconprune)){
  if (PD_reconprune$Community[i] %in% lista){
    PD_reconprune$plot_group[i] <- "Increasing"
  } else {
    PD_reconprune$plot_group[i] <- "Random"
  }
}

#Categories not consistent across indices - redo lists

#Make scatterplot with aggregates of communtiies and with coloured by category
#interaction(plot_group, ) - add this to facet if I need to group them differently if too cluttered

comm_names <- unique(NRI_reconprune$Community)
str(comm_names)
levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
levels(comm_names)

levels(NRI_reconprune$Community) <- levels(comm_names)

###latest plot april 23, 2018
pdf("./Plots/2017_botany/Paper_plots/Question2/Barplots/NRI_points_proportions_significance_formatted.pdf")
ggplot(NRI_reconprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Reconstruction Method")+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pruned phylogeny", "Reconstructed phylogeny"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))
dev.off()


#Plot single panel
pdf("./Plots/2017_botany/Paper_plots/single_reconprune.pdf")
ggplot(NRI_reconprune[which(NRI_reconprune$Community == "Baygall"),], aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Reconstruction Method")+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pruned phylogeny", "Reconstructed phylogeny"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))
dev.off()


#(title = "Figure 2. Proportion of significantly non-random patterns of NRI are consistent\nbetween pruned and reconstructed phylogenies across different numbers of taxa")

pdf("./Plots/2017_botany/Paper_plots/Question2/Barplots/NTI_points_proportions_significance_formatted.pdf")
ggplot(NTI_reconprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 2. Proportion of significantly non-random patterns of NTI are consistent\nbetween pruned and reconstructed phylogenies across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Reconstruction Method")+
  theme(plot.title = element_text(size = 14, face = "bold"))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pruned phylogeny", "Reconstructed phylogeny"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question2/Barplots/PD_points_proportions_significance_formatted.pdf")
ggplot(PD_reconprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 2. Proportion of significantly non-random patterns of PD are consistent\nbetween pruned and reconstructed phylogenies across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Reconstruction Method")+
  theme(plot.title = element_text(size = 14, face = "bold"))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pruned phylogeny", "Reconstructed phylogeny"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))
dev.off()

###
