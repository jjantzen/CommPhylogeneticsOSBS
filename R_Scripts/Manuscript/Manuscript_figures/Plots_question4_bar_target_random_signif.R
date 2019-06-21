#Question 4 - plotting by proportion clustered, overdispersed and random
#Aggregate by community to reduce number of plots
#Barplots (sum to 1) to save - two plots on top of each other
#or points with different colours or shapes for clust/over/random
#for plots- still need to separate communities by patterns for clarity

#Read data with pvalues for pruned subsets and overall dataset, as well as differences
NRIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

NRIp_prop <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prop_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_prop <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prop_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_prop <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prop_index_pvalue.csv", stringsAsFactors = FALSE)

# NRI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NRI.csv", stringsAsFactors = FALSE)
# NTI_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_NTI.csv", stringsAsFactors = FALSE)
# PD_diff <- read.csv("./Plots/2017_botany/Files_to_plot/Diff_recon_prunephy_random_subsets_PD.csv", stringsAsFactors = FALSE)

#Correct spelling
NRIp_prune$variable[which(NRIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_prune$variable[which(NTIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_prune$variable[which(PDp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

NRIp_prop$variable[which(NRIp_prop$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_prop$variable[which(NTIp_prop$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_prop$variable[which(PDp_prop$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

# NRI_diff$variable[which(NRI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# NTI_diff$variable[which(NTI_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
# PD_diff$variable[which(PD_diff$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

#Make categories for clustered, overdispersed and random
NRIp_prune$category[which(NRIp_prune$pvalue <= 0.05)] <- "Clustered" 
NRIp_prune$category[which(NRIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_prune$category[which(NRIp_prune$pvalue > 0.05 & NRIp_prune$pvalue < 0.95)] <- "Random"

NRIp_prop$category <- NA
NRIp_prop$category[which(NRIp_prop$pvalue <= 0.05)] <- "Clustered" 
NRIp_prop$category[which(NRIp_prop$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_prop$category[which(NRIp_prop$pvalue > 0.05 & NRIp_prop$pvalue < 0.95)] <- "Random"

NTIp_prune$category <- NA
NTIp_prune$category[which(NTIp_prune$pvalue <= 0.05)] <- "Clustered" 
NTIp_prune$category[which(NTIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_prune$category[which(NTIp_prune$pvalue > 0.05 & NTIp_prune$pvalue < 0.95)] <- "Random"

NTIp_prop$category <- NA
NTIp_prop$category[which(NTIp_prop$pvalue <= 0.05)] <- "Clustered" 
NTIp_prop$category[which(NTIp_prop$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_prop$category[which(NTIp_prop$pvalue > 0.05 & NTIp_prop$pvalue < 0.95)] <- "Random"

PDp_prune$category <- NA
PDp_prune$category[which(PDp_prune$pvalue <= 0.05)] <- "Clustered" 
PDp_prune$category[which(PDp_prune$pvalue >= 0.95)] <- "Overdispersed" 
PDp_prune$category[which(PDp_prune$pvalue > 0.05 & PDp_prune$pvalue < 0.95)] <- "Random"

PDp_prop$category <- NA
PDp_prop$category[which(PDp_prop$pvalue <= 0.05)] <- "Clustered" 
PDp_prop$category[which(PDp_prop$pvalue >= 0.95)] <- "Overdispersed" 
PDp_prop$category[which(PDp_prop$pvalue > 0.05 & PDp_prop$pvalue < 0.95)] <- "Random"

#Add column for number of reps
num_reps_prune <- NRIp_prune %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

num_reps_prop <- NRIp_prop %>% 
  group_by(subset, variable) %>% 
  summarize(n = n())

#Get proportions clustered
prop_sig_NRI_prune <- NRIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NRI_prop <- NRIp_prop %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_prune <- NTIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_prop <- NTIp_prop %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_prune <- PDp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_prop <- PDp_prop %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

#Change column names
colnames(prop_sig_NRI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_prune) <- c("Community", "Subset", "Category", "Number")

colnames(prop_sig_NRI_prop) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_prop) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_prop) <- c("Community", "Subset", "Category", "Number")

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

for (i in 1:nrow(prop_sig_NRI_prop)){
  subset <- prop_sig_NRI_prop$Subset[i]
  community <- prop_sig_NRI_prop$Community[i]
  num_reps <- num_reps_prop$n[which(num_reps_prop$subset == subset & num_reps_prop$variable == community)]
  prop_sig <- prop_sig_NRI_prop$Number[i]/num_reps
  prop_sig_NRI_prop$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_NTI_prop)){
  subset <- prop_sig_NTI_prop$Subset[i]
  community <- prop_sig_NTI_prop$Community[i]
  num_reps <- num_reps_prop$n[which(num_reps_prop$subset == subset & num_reps_prop$variable == community)]
  prop_sig <- prop_sig_NTI_prop$Number[i]/num_reps
  prop_sig_NTI_prop$Proportion_sig[i] <- prop_sig
}

for (i in 1:nrow(prop_sig_PD_prop)){
  subset <- prop_sig_PD_prop$Subset[i]
  community <- prop_sig_PD_prop$Community[i]
  num_reps <- num_reps_prop$n[which(num_reps_prop$subset == subset & num_reps_prop$variable == community)]
  prop_sig <- prop_sig_PD_prop$Number[i]/num_reps
  prop_sig_PD_prop$Proportion_sig[i] <- prop_sig
}

#Add columm for recon or prune
prop_sig_NRI_prune$type <- "Random"
prop_sig_NRI_prop$type <- "Prop"

prop_sig_NTI_prune$type <- "Random"
prop_sig_NTI_prop$type <- "Prop"

prop_sig_PD_prune$type <- "Random"
prop_sig_PD_prop$type <- "Prop"

# #Combine recon and prune into one dataset - don't combine for barplots
# NRI_propprune <- rbind(prop_sig_NRI_prune, prop_sig_NRI_prop)
# NTI_propprune <- rbind(prop_sig_NTI_prune, prop_sig_NTI_prop)
# PD_propprune <- rbind(prop_sig_PD_prune, prop_sig_PD_prop)
# 
# NRI_propprune <- NRI_propprune[which(complete.cases(NRI_propprune)),]
# NTI_propprune <- NTI_propprune[which(complete.cases(NTI_propprune)),]
# PD_propprune <- PD_propprune[which(complete.cases(PD_propprune)),]

# #Remove NAs - don't remove NAs for barplots
# NRI_propprune <- NRI_propprune[which(complete.cases(NRI_propprune)),]
# NTI_propprune <- NTI_propprune[which(complete.cases(NTI_propprune)),]
# PD_propprune <- PD_propprune[which(complete.cases(PD_propprune)),]

#Make communities factors
prop_sig_NRI_prune$Community <- as.factor(prop_sig_NRI_prune$Community)
prop_sig_NTI_prune$Community <- as.factor(prop_sig_NTI_prune$Community)
prop_sig_PD_prune$Community <- as.factor(prop_sig_PD_prune$Community)

prop_sig_NRI_prop$Community <- as.factor(prop_sig_NRI_prop$Community)
prop_sig_NTI_prop$Community <- as.factor(prop_sig_NTI_prop$Community)
prop_sig_PD_prop$Community <- as.factor(prop_sig_PD_prop$Community)

#Make Categories factors
prop_sig_NRI_prune$Category <- as.factor(prop_sig_NRI_prune$Category)
prop_sig_NTI_prune$Category <- as.factor(prop_sig_NTI_prune$Category)
prop_sig_PD_prune$Category <- as.factor(prop_sig_PD_prune$Category)

prop_sig_NRI_prop$Category <- as.factor(prop_sig_NRI_prop$Category)
prop_sig_NTI_prop$Category <- as.factor(prop_sig_NTI_prop$Category)
prop_sig_PD_prop$Category <- as.factor(prop_sig_PD_prop$Category)

#Rename communities to remove . and add space
levels(prop_sig_NRI_prune$Community)
levels(prop_sig_NRI_prune$Community) <- gsub(".", " ", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub(".", " ", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub(".", " ", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_NRI_prop$Community) <- gsub(".", " ", levels(prop_sig_NRI_prop$Community), fixed = TRUE)
levels(prop_sig_NTI_prop$Community) <- gsub(".", " ", levels(prop_sig_NTI_prop$Community), fixed = TRUE)
levels(prop_sig_PD_prop$Community) <- gsub(".", " ", levels(prop_sig_PD_prop$Community), fixed = TRUE)

#Reorder factors of Categories
prop_sig_NRI_prune$Category <- factor(prop_sig_NRI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_prune$Category <- factor(prop_sig_NTI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_prune$Category <- factor(prop_sig_PD_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))

prop_sig_NRI_prop$Category <- factor(prop_sig_NRI_prop$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_prop$Category <- factor(prop_sig_NTI_prop$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_prop$Category <- factor(prop_sig_PD_prop$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))


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

levels(prop_sig_NRI_prop$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NRI_prop$Community), fixed = TRUE)
levels(prop_sig_NRI_prop$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NRI_prop$Community), fixed = TRUE)
levels(prop_sig_NRI_prop$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NRI_prop$Community), fixed = TRUE)
levels(prop_sig_NRI_prop$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NRI_prop$Community), fixed = TRUE)
levels(prop_sig_NRI_prop$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NRI_prop$Community), fixed = TRUE)

levels(prop_sig_NTI_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_prune$Community), fixed = TRUE)

levels(prop_sig_NTI_prop$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_NTI_prop$Community), fixed = TRUE)
levels(prop_sig_NTI_prop$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_NTI_prop$Community), fixed = TRUE)
levels(prop_sig_NTI_prop$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_NTI_prop$Community), fixed = TRUE)
levels(prop_sig_NTI_prop$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_NTI_prop$Community), fixed = TRUE)
levels(prop_sig_NTI_prop$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_NTI_prop$Community), fixed = TRUE)

levels(prop_sig_PD_prune$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_prune$Community), fixed = TRUE)

levels(prop_sig_PD_prop$Community) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(prop_sig_PD_prop$Community), fixed = TRUE)
levels(prop_sig_PD_prop$Community) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(prop_sig_PD_prop$Community), fixed = TRUE)
levels(prop_sig_PD_prop$Community) <- gsub("Successional Hardwood Forest", "Successional \nHardwood Forest", levels(prop_sig_PD_prop$Community), fixed = TRUE)
levels(prop_sig_PD_prop$Community) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(prop_sig_PD_prop$Community), fixed = TRUE)
levels(prop_sig_PD_prop$Community) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(prop_sig_PD_prop$Community), fixed = TRUE)

#Make barplot of all categories together by community and subset
pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question2/NRI_bar_randomprop_significance.pdf", height = 7, width = 19)
p1 <- ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Random sampling")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"), plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), axis.ticks.x = element_blank(), plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NRI_prop, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Targeted sampling")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0.25), "cm"), 
        legend.justification = c(1.05, 0), legend.position = c(1.05, 0), 
        legend.direction = "vertical", panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10, face = "bold"), 
        strip.text = element_text(size = 6), plot.title = element_text(face = "bold"), axis.ticks.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
two_plots <- p1 + p2 #+ plot_layout(ncol = 1)


ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question2/NRI_bar_target_random_signif_final_plot.pdf", two_plots, dpi = 1000, height = 180, width = 180, units = "mm")





dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question2/NTI_bar_randomprop_significance.pdf", height = 7, width = 19)

p1 <- ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Random sampling")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_NTI_prop, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Targeted sampling")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question2/PD_bar_randomprune_significance.pdf", height = 7, width = 19)
p1 <- ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "", y = "Proportion of replicates", title = "Random sampling")+
  guides(fill=FALSE)+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.background = element_rect(colour = mydark))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p1
p2 <- ggplot(prop_sig_PD_prop, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 14)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Targeted sampling")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

#plot two plots one on top of each other - need to save this one
p1 + p2 + plot_layout(ncol = 1)
dev.off()

#########
#After here not keepers?
#Separating communities into groups
NRIa <- c("Successional.Hardwood.Forest", "Sandhill", "Scrubby.Flatwoods", "Clastic.Upland.Lake", "Pine.Plantation")
NRIb <- c( "Improved", "Lake.Bottom", "Mesic.Hammock", "Baygall")
NRIc <- c("Basin.Swamp", "Sandhill.Upland.Lake","Basin.Marsh", "Abandoned.Field.Pasture", "Xeric.Hammock")
comm4 <- c("Baygall", "Mesic Hammock", "Scrubby Flatwoods", "Successional Hardwood Forest", "Sandhill Upland Lake", "Basin Swamp")


for (i in 1:nrow(NRI_propprune)){
  if (NRI_propprune$Community[i] %in% NRIa){
    NRI_propprune$plot_group[i] <- "Similar"
  } else if (NRI_propprune$Community[i] %in% NRIb){
    NRI_propprune$plot_group[i] <- "Random"
  } else {
    NRI_propprune$plot_group[i] <- "Different"
  }
}

for (i in 1:nrow(NTI_propprune)){
  if (NTI_propprune$Community[i] %in% lista){
    NTI_propprune$plot_group[i] <- "Divergent"
  } else if (NTI_propprune$Community[i] %in% listb){
    NTI_propprune$plot_group[i] <- "Parallel"
  } else {
    NTI_propprune$plot_group[i] <- "Random"
  }
}

for (i in 1:nrow(PD_propprune)){
  if (PD_propprune$Community[i] %in% lista){
    PD_propprune$plot_group[i] <- "Divergent"
  } else if (PD_propprune$Community[i] %in% listb){
    PD_propprune$plot_group[i] <- "Parallel"
  } else {
    PD_propprune$plot_group[i] <- "Random"
  }
}

#Read files with community richness
clad_ntax <- read.csv("./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_pruned_phylograms.csv", stringsAsFactors = FALSE)
prop_ntax <- read.csv("./PD_files/R_calc_picante/Significance_values/Ntaxa_avgs_prop_subsets.csv", stringsAsFactors = FALSE)

#un melt data
clad_ntax_avg <- dcast(clad_ntax, community + subset ~ variable, fun.aggregate = sum, value.var = "value")
prop_ntax_avg <- dcast(prop_ntax, community + subset ~ variable, fun.aggregate = sum, value.var = "value")

#Sort by number of taxa
clad_ntax_avg <- clad_ntax_avg[with(clad_ntax_avg, order(avgtax)), ]
prop_ntax_avg <- prop_ntax_avg[with(prop_ntax_avg, order(avgtax)), ]

#Testing num taxa and pattern between pairs with dip in prop
ggplot()+
  geom_point(data = clad_ntax_avg[which(clad_ntax_avg$community == "Abandoned.Field.Pasture" | clad_ntax_avg$community == "Basin.Marsh" | clad_ntax_avg$community == "Sandhill"),], aes(x = subset, y = avgtax, colour = community))
  
ggplot()+
  geom_point(data = clad_ntax_avg[which(clad_ntax_avg$community == "Xeric.Hammock" | clad_ntax_avg$community == "Baygall" | clad_ntax_avg$community == "Clastic.Upland.Lake"),], aes(x = subset, y = avgtax, colour = community))

ggplot()+
  geom_point(data = clad_ntax_avg[which(clad_ntax_avg$community == "Basin.Swamp" | clad_ntax_avg$community == "Sandhill.Upland.Lake" | clad_ntax_avg$community == "Sandhill"),], aes(x = subset, y = avgtax, colour = community))


ntax_ordered_prop <- prop_ntax_avg %>% 
  group_by(community) %>% 
  summarize(mean = mean(avgtax)) %>% 
  arrange(mean)
  
ntax_ordered_clad <- clad_ntax_avg %>% 
  group_by(community) %>% 
  summarize(mean = mean(avgtax)) %>% 
  arrange(mean)

group1 <- c(ntax_ordered_prop[c(1:7),1])
group2 <- c(ntax_ordered_prop[c(8:14),1])



ntax_ordered_clad

for (i in 1:nrow(NRI_propprune)){
  if (NRI_propprune$Community[i] %in% group1$community){
    NRI_propprune$plot_group[i] <- "Few"
  } else {
    NRI_propprune$plot_group[i] <- "Many"
  } 
}

#Make scatterplot with aggregates of communtiies and with coloured by category
pdf("./Plots/2017_botany/Paper_plots/Question4/Barplots/NRI_points_proportions_significance_formatted.pdf")
ggplot(NRI_propprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  #facet_wrap(~ interaction(Category, plot_group))+
  facet_wrap(~ interaction(plot_group, Category), ncol = 2)+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 4. Proportion of significantly non-random patterns of NRI can differ\n between proportionally and randomly sampled subsets in some \ncommunities especially with intermediate numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Sampling Strategy")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Proportional", "Random"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))

dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question4/Barplots/NTI_points_proportions_significance_formatted.pdf")
ggplot(NTI_propprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ interaction(plot_group, Category))+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 4. Proportion of significantly non-random patterns of NTI \ncan diverge between chronograms and phylograms for different \ncommunities across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Sampling Strategy")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Proportional", "Random"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))

dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question4/Barplots/PD_points_proportions_significance_formatted.pdf")
ggplot(PD_propprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ interaction(plot_group, Category))+
  #facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(title = "Figure 4. Proportion of significantly non-random patterns of PD \ncan diverge between chronograms and phylograms for different \ncommunities across different numbers of taxa", y = "Proportion of replicates",
       x = "Number of taxa in regional dataset", linetype = "Sampling Strategy")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Proportional", "Random"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))

dev.off()


###Plot just four communities - no categories
comm_names <- unique(NRI_propprune$Community)
str(comm_names)
levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
levels(comm_names)

levels(NRI_propprune$Community) <- levels(comm_names)

pdf("./Plots/2017_botany/Paper_plots/Question4/NRI_propprune_sig_notitle.pdf")
ggplot(NRI_propprune[which(NRI_propprune$Community %in% comm4),], aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset", linetype = "Sampling Strategy")+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Proportional", "Random"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))

#ggsave("plot.pdf", width=4, height=4, dpi=100)

#plot for paper July 13, 2018

NRI_propprune$Community <- gsub("\\.", " ", NRI_propprune$Community)
NRI_propprune$Community


#, height = 12, width = 12, res = 600
pdf("./Plots/2017_botany/Paper_plots/Question4/NRI_propprune_sig_notitle_all_600_july2018_take3.pdf")
ggplot(NRI_propprune, aes(x = Subset, y = Proportion_sig, group = interaction(type,Community, Category)))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community, linetype = type))+
  facet_wrap(~ Category, ncol = 1)+
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset", linetype = "Sampling Strategy")+
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Targeted", "Random"))+
  guides(color = guide_legend(order=2),linetype = guide_legend(order=1))+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))

dev.off()
NRI_propprune
###

