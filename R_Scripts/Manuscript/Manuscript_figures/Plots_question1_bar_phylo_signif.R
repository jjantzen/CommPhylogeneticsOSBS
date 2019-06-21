#Question 1 - plotting by proportion clustered, overdispersed and random
#First barplots (sum to 1) then line plots with different colours for each community and faceted by type of pattern
#Aggregate by community to reduce number of plots?
#Need to save barplots with final formatting   

#Read data with pvalues for pruned subsets and overall dataset
NRIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
NTIp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
PDp_prune <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)

#Correct spelling
NRIp_prune$variable[which(NRIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
NTIp_prune$variable[which(NTIp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
PDp_prune$variable[which(PDp_prune$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

#Make categories for clustered, overdispersed and random
NRIp_prune$category[which(NRIp_prune$pvalue <= 0.05)] <- "Clustered" 
NRIp_prune$category[which(NRIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NRIp_prune$category[which(NRIp_prune$pvalue > 0.05 & NRIp_prune$pvalue < 0.95)] <- "Random"

NTIp_prune$category <- NA
NTIp_prune$category[which(NTIp_prune$pvalue <= 0.05)] <- "Clustered" 
NTIp_prune$category[which(NTIp_prune$pvalue >= 0.95)] <- "Overdispersed" 
NTIp_prune$category[which(NTIp_prune$pvalue > 0.05 & NTIp_prune$pvalue < 0.95)] <- "Random"

PDp_prune$category <- NA
PDp_prune$category[which(PDp_prune$pvalue <= 0.05)] <- "Clustered" 
PDp_prune$category[which(PDp_prune$pvalue >= 0.95)] <- "Overdispersed" 
PDp_prune$category[which(PDp_prune$pvalue > 0.05 & PDp_prune$pvalue < 0.95)] <- "Random"

#Get proportions clustered - first summarize by number of replicates
prop_sig_NRI_prune <- NRIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_NTI_prune <- NTIp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

prop_sig_PD_prune <- PDp_prune %>% 
  group_by(variable, subset, category) %>% 
  summarize(n = n()) %>% 
  as.data.frame()

#Change column names
colnames(prop_sig_NRI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_NTI_prune) <- c("Community", "Subset", "Category", "Number")
colnames(prop_sig_PD_prune) <- c("Community", "Subset", "Category", "Number")

#Calculate proportion (divide by 100)
prop_sig_NRI_prune$Proportion_sig <- prop_sig_NRI_prune$Number/100
prop_sig_NTI_prune$Proportion_sig <- prop_sig_NTI_prune$Number/100
prop_sig_PD_prune$Proportion_sig <- prop_sig_PD_prune$Number/100

# #Remove NAs - don't remove NAs if we want barplots to reach total of 1
# prop_sig_NRI_prune <- prop_sig_NRI_prune[which(complete.cases(prop_sig_NRI_prune)),]
# prop_sig_NTI_prune <- prop_sig_NTI_prune[which(complete.cases(prop_sig_NTI_prune)),]
# prop_sig_PD_prune <- prop_sig_PD_prune[which(complete.cases(prop_sig_PD_prune)),]

#Make communities factors
prop_sig_NRI_prune$Community <- as.factor(prop_sig_NRI_prune$Community)
prop_sig_NTI_prune$Community <- as.factor(prop_sig_NTI_prune$Community)
prop_sig_PD_prune$Community <- as.factor(prop_sig_PD_prune$Community)

#Make communities factors
prop_sig_NRI_prune$Category <- as.factor(prop_sig_NRI_prune$Category)
prop_sig_NTI_prune$Category <- as.factor(prop_sig_NTI_prune$Category)
prop_sig_PD_prune$Category <- as.factor(prop_sig_PD_prune$Category)

#Rename communities to remove . and add space
levels(prop_sig_NRI_prune$Community)
levels(prop_sig_NRI_prune$Community) <- gsub(".", " ", levels(prop_sig_NRI_prune$Community), fixed = TRUE)
levels(prop_sig_NTI_prune$Community) <- gsub(".", " ", levels(prop_sig_NTI_prune$Community), fixed = TRUE)
levels(prop_sig_PD_prune$Community) <- gsub(".", " ", levels(prop_sig_PD_prune$Community), fixed = TRUE)


#Reorder factors of Categories
prop_sig_NRI_prune$Category <- factor(prop_sig_NRI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_NTI_prune$Category <- factor(prop_sig_NTI_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))
prop_sig_PD_prune$Category <- factor(prop_sig_PD_prune$Category, levels = c("Clustered", "Random", "Overdispersed", "NA"))


#Make barplot of all categories together by community and subset 
#need some formatting before including in resubmission

#Get colours for plotting
mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question1/NRI_bar_numtaxa_proportions_significance.pdf", height = 7, width = 15)

p2 <- ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 7, labeller = label_wrap_gen(width = 15))+
  labs(x = "Subset size", y = "Proportion of replicates")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(legend.position = "bottom", panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 8), plot.title = element_text(face = "bold"), axis.title = element_text(size = 10, face = "bold"), strip.text = element_text(size = 8), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2

# ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
#   geom_bar(stat = "identity")+
#   facet_wrap(~ Community, ncol = 7)+
#   labs(x = "Subset size", y = "Proportion of replicates")+
#   scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
#   theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
#   theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8))
dev.off()


ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question1/NRI_bar_prune_signif_final_plot.pdf", p2, dpi = 1000, height = 180, width = 180, units = "mm")



pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question1/NTI_bar_numtaxa_proportions_significance.pdf", height = 7, width = 15)
p2 <- ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 7)+
  labs(x = "Subset size", y = "Proportion of replicates")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2
# 
# ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
#   geom_bar(stat = "identity")+
#   facet_wrap(~ Community, ncol = 7)+
#   labs(x = "Subset size", y = "Proportion of replicates")+
#   scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
#   theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
#   theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8))
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question1/PD_bar_numtaxa_proportions_significance.pdf", height = 7, width = 15)

p2 <- ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 7)+
  labs(x = "Subset size", y = "Proportion of replicates")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), strip.text = element_text(size = 7), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))
p2


# ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig, fill = Category))+
#   geom_bar(stat = "identity")+
#   facet_wrap(~ Community, ncol = 7)+
#   scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
#   theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
#   theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8))+
#   labs(x = "Subset size", y = "Proportion of replicates")
dev.off()

#Make scatterplot with aggregates of communtiies and with coloured by category
####this is latest plot april 23, 2018 - for first submission

# #Already fixed community names above
# comm_names <- unique(prop_sig_NRI_prune$Community)
# str(comm_names)
# levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
# levels(comm_names)
# 
# levels(prop_sig_NRI_prune$Community) <- levels(comm_names)

#Plot for initial submission - line plot
pdf("../Plots/2017_botany/Paper_plots/Question1/NRI_proportions_significance_notitle.pdf")
ggplot(prop_sig_NRI_prune, aes(x = Subset, y = Proportion_sig))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community))+
  facet_wrap(~ Category, ncol = 1)+
  #theme(legend.position = c(0.8, 0.2), legend.box = "horizontal")+
  #guides(colour = guide_legend(order=1, ncol = 2), shape = guide_legend(order=2))
  guides(shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset")+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))
dev.off()

####Only graph no legend
pdf("../Plots/2017_botany/Paper_plots/single_comm_example.pdf")
ggplot(prop_sig_NRI_prune[which(prop_sig_NRI_prune$Community == "Baygall"),], aes(x = Subset, y = Proportion_sig))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community))+
  facet_wrap(~ Category, ncol = 1)+
  #theme(legend.position = c(0.8, 0.2), legend.box = "horizontal")+
  #guides(colour = guide_legend(order=1, ncol = 2), shape = guide_legend(order=2))
  guides(colour = FALSE, shape = FALSE)+
  labs(y = "Proportion of replicates", x = "Number of taxa in regional dataset")+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12))
dev.off()

#unique(prop_sig_NRI_prune$Community)

#Earlier versions of lineplots - including NAs and titles
pdf("../Plots/2017_botany/Paper_plots/Question1/Barplots/NTI_points_proportions_significance_formatted.pdf")
ggplot(prop_sig_NTI_prune, aes(x = Subset, y = Proportion_sig))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community))+
  facet_wrap(~ Category, ncol = 1)+
  #theme(legend.position = c(0.8, 0.2), legend.box = "horizontal")+
  #guides(colour = guide_legend(order=1, ncol = 2), shape = guide_legend(order=2))
  guides(shape = FALSE)+
  labs(title = "Figure 1. Proportion of significantly non-random patterns of NTI increase\n with increasing number of taxa for some communities", y = "Proportion of replicates", x = "Number of taxa in regional dataset")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
dev.off()

pdf("../Plots/2017_botany/Paper_plots/Question1/Barplots/PD_points_proportions_significance_formatted.pdf")
ggplot(prop_sig_PD_prune, aes(x = Subset, y = Proportion_sig))+
  geom_point(aes(colour = Community, shape = Category))+
  geom_line(aes(colour = Community))+
  facet_wrap(~ Category, ncol = 1)+
  #theme(legend.position = c(0.8, 0.2), legend.box = "horizontal")+
  #guides(colour = guide_legend(order=1, ncol = 2), shape = guide_legend(order=2))
  guides(shape = FALSE)+
  labs(title = "Figure 1. Proportion of significantly non-random patterns of PD increase \n with increasing number of taxa for some communities", y = "Proportion of replicates", x = "Number of taxa in regional dataset")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
dev.off()

###
