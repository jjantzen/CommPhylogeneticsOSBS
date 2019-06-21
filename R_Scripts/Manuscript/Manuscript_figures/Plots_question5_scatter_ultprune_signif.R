#Question 3 - chrono phylogram colour by significance scatterplots
#need to save scatterplots

#Read spread data 
spread_NRIp_prune_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NRIp_recon_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NRIp_ult_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NRIp_prop_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NRI_prop_index_pvalue.csv", stringsAsFactors = FALSE)

spread_NTIp_prune_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prune_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NTIp_recon_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_recon_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NTIp_ult_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_ult_index_pvalue.csv", stringsAsFactors = FALSE)
spread_NTIp_prop_comb <- read.csv("./Plots/2017_botany/Files_to_plot/NTI_prop_index_pvalue.csv", stringsAsFactors = FALSE)

spread_PDp_prune_comb <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prune_index_pvalue.csv", stringsAsFactors = FALSE)
spread_PDp_recon_comb <- read.csv("./Plots/2017_botany/Files_to_plot/PD_recon_index_pvalue.csv", stringsAsFactors = FALSE)
spread_PDp_ult_comb <- read.csv("./Plots/2017_botany/Files_to_plot/PD_ult_index_pvalue.csv", stringsAsFactors = FALSE)
spread_PDp_prop_comb <- read.csv("./Plots/2017_botany/Files_to_plot/PD_prop_index_pvalue.csv", stringsAsFactors = FALSE)

#Get colours for plotting
mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"

#Rename communities to remove . and add space

spread_NRIp_prune_comb$variable <- as.factor(spread_NRIp_prune_comb$variable)
spread_NTIp_prune_comb$variable <- as.factor(spread_NTIp_prune_comb$variable)
spread_PDp_prune_comb$variable <- as.factor(spread_PDp_prune_comb$variable)

spread_NRIp_ult_comb$variable <- as.factor(spread_NRIp_ult_comb$variable)
spread_NTIp_ult_comb$variable <- as.factor(spread_NTIp_ult_comb$variable)
spread_PDp_ult_comb$variable <- as.factor(spread_PDp_ult_comb$variable)

levels(spread_NRIp_prune_comb$variable) <- gsub(".", " ", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)
levels(spread_NTIp_prune_comb$variable) <- gsub(".", " ", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)
levels(spread_PDp_prune_comb$variable) <- gsub(".", " ", levels(spread_PDp_prune_comb$variable), fixed = TRUE)

levels(spread_NRIp_ult_comb$variable) <- gsub(".", " ", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)
levels(spread_NTIp_ult_comb$variable) <- gsub(".", " ", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)
levels(spread_PDp_ult_comb$variable) <- gsub(".", " ", levels(spread_PDp_ult_comb$variable), fixed = TRUE)

#make category for signif and not
spread_NRIp_prune_comb$signif <- NA
spread_NRIp_prune_comb$signif[which(spread_NRIp_prune_comb$pvalue <= 0.05 | spread_NRIp_prune_comb$pvalue >= 0.95)] <- "Significant"
spread_NRIp_prune_comb$signif[which(spread_NRIp_prune_comb$pvalue > 0.05 & spread_NRIp_prune_comb$pvalue < 0.95)] <- "Random"
spread_NRIp_ult_comb$signif <- NA
spread_NRIp_ult_comb$signif[which(spread_NRIp_ult_comb$pvalue <= 0.05 | spread_NRIp_ult_comb$pvalue >= 0.95)] <- "Significant"
spread_NRIp_ult_comb$signif[which(spread_NRIp_ult_comb$pvalue > 0.05 & spread_NRIp_ult_comb$pvalue < 0.95)] <- "Random"
spread_NTIp_prune_comb$signif <- NA
spread_NTIp_prune_comb$signif[which(spread_NTIp_prune_comb$pvalue <= 0.05 | spread_NTIp_prune_comb$pvalue >= 0.95)] <- "Significant"
spread_NTIp_prune_comb$signif[which(spread_NTIp_prune_comb$pvalue > 0.05 & spread_NTIp_prune_comb$pvalue < 0.95)] <- "Random"
spread_NTIp_ult_comb$signif <- NA
spread_NTIp_ult_comb$signif[which(spread_NTIp_ult_comb$pvalue <= 0.05 | spread_NTIp_ult_comb$pvalue >= 0.95)] <- "Significant"
spread_NTIp_ult_comb$signif[which(spread_NTIp_ult_comb$pvalue > 0.05 & spread_NTIp_ult_comb$pvalue < 0.95)] <- "Random"
spread_PDp_prune_comb$signif <- NA
spread_PDp_prune_comb$signif[which(spread_PDp_prune_comb$pvalue <= 0.05 | spread_PDp_prune_comb$pvalue >= 0.95)] <- "Significant"
spread_PDp_prune_comb$signif[which(spread_PDp_prune_comb$pvalue > 0.05 & spread_PDp_prune_comb$pvalue < 0.95)] <- "Random"
spread_PDp_ult_comb$signif <- NA
spread_PDp_ult_comb$signif[which(spread_PDp_ult_comb$pvalue <= 0.05 | spread_PDp_ult_comb$pvalue >= 0.95)] <- "Significant"
spread_PDp_ult_comb$signif[which(spread_PDp_ult_comb$pvalue > 0.05 & spread_PDp_ult_comb$pvalue < 0.95)] <- "Random"

#Make significance as factors and rearrange
spread_NRIp_ult_comb$signif <- as.factor(spread_NRIp_ult_comb$signif)
spread_NTIp_ult_comb$signif <- as.factor(spread_NTIp_ult_comb$signif)
spread_PDp_ult_comb$signif <- as.factor(spread_PDp_ult_comb$signif)

spread_NRIp_prune_comb$signif <- as.factor(spread_NRIp_prune_comb$signif)
spread_NTIp_prune_comb$signif <- as.factor(spread_NTIp_prune_comb$signif)
spread_PDp_prune_comb$signif <- as.factor(spread_PDp_prune_comb$signif)

levels(spread_NRIp_prune_comb$signif)

levels(spread_NRIp_prune_comb$signif) <- factor(spread_NRIp_prune_comb$signif, levels = c("Significant", "Random"))
levels(spread_NTIp_prune_comb$signif) <- factor(spread_NTIp_prune_comb$signif, levels = c("Significant", "Random", "NA"))
levels(spread_PDp_prune_comb$signif) <- factor(spread_PDp_prune_comb$signif, levels = c("Significant", "Random", "NA"))


#Adding line break to names of Abandoned field 
levels(spread_NRIp_prune_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)
levels(spread_NRIp_prune_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)
levels(spread_NRIp_prune_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)
levels(spread_NRIp_prune_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)
levels(spread_NRIp_prune_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_NRIp_prune_comb$variable), fixed = TRUE)

levels(spread_NRIp_ult_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)
levels(spread_NRIp_ult_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)
levels(spread_NRIp_ult_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)
levels(spread_NRIp_ult_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)
levels(spread_NRIp_ult_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_NRIp_ult_comb$variable), fixed = TRUE)

levels(spread_NTIp_prune_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)
levels(spread_NTIp_prune_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)
levels(spread_NTIp_prune_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)
levels(spread_NTIp_prune_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)
levels(spread_NTIp_prune_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_NTIp_prune_comb$variable), fixed = TRUE)

levels(spread_NTIp_ult_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)
levels(spread_NTIp_ult_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)
levels(spread_NTIp_ult_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)
levels(spread_NTIp_ult_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)
levels(spread_NTIp_ult_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_NTIp_ult_comb$variable), fixed = TRUE)

levels(spread_PDp_prune_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_PDp_prune_comb$variable), fixed = TRUE)
levels(spread_PDp_prune_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_PDp_prune_comb$variable), fixed = TRUE)
levels(spread_PDp_prune_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_PDp_prune_comb$variable), fixed = TRUE)
levels(spread_PDp_prune_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_PDp_prune_comb$variable), fixed = TRUE)
levels(spread_PDp_prune_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_PDp_prune_comb$variable), fixed = TRUE)

levels(spread_PDp_ult_comb$variable) <- gsub("Abandoned Field Pasture", "Abandoned Field \n Pasture", levels(spread_PDp_ult_comb$variable), fixed = TRUE)
levels(spread_PDp_ult_comb$variable) <- gsub("Sandhill Upland Lake", "Sandhill Upland \n Lake", levels(spread_PDp_ult_comb$variable), fixed = TRUE)
levels(spread_PDp_ult_comb$variable) <- gsub("Succesional Hardwood Forest", "Successional \nHardwood Forest", levels(spread_PDp_ult_comb$variable), fixed = TRUE)
levels(spread_PDp_ult_comb$variable) <- gsub("Clastic Upland Lake", "Clastic Upland \n Lake", levels(spread_PDp_ult_comb$variable), fixed = TRUE)
levels(spread_PDp_ult_comb$variable) <- gsub("Scrubby Flatwoods", "Scrubby \n Flatwoods", levels(spread_PDp_ult_comb$variable), fixed = TRUE)



spread_NRIp_prune_comb$pattern <- NA
spread_NRIp_prune_comb$pattern[which(spread_NRIp_prune_comb$pvalue <= 0.05)] <- "Clustered"
spread_NRIp_prune_comb$pattern[which(spread_NRIp_prune_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_NRIp_prune_comb$pattern[which(spread_NRIp_prune_comb$pvalue > 0.05 & spread_NRIp_prune_comb$pvalue < 0.95)] <- "Random"

spread_NTIp_prune_comb$pattern <- NA
spread_NTIp_prune_comb$pattern[which(spread_NTIp_prune_comb$pvalue <= 0.05)] <- "Clustered"
spread_NTIp_prune_comb$pattern[which(spread_NTIp_prune_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_NTIp_prune_comb$pattern[which(spread_NTIp_prune_comb$pvalue > 0.05 & spread_NTIp_prune_comb$pvalue < 0.95)] <- "Random"

spread_PDp_prune_comb$pattern <- NA
spread_PDp_prune_comb$pattern[which(spread_PDp_prune_comb$pvalue <= 0.05)] <- "Clustered"
spread_PDp_prune_comb$pattern[which(spread_PDp_prune_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_PDp_prune_comb$pattern[which(spread_PDp_prune_comb$pvalue > 0.05 & spread_PDp_prune_comb$pvalue < 0.95)] <- "Random"

spread_NRIp_ult_comb$pattern <- NA
spread_NRIp_ult_comb$pattern[which(spread_NRIp_ult_comb$pvalue <= 0.05)] <- "Clustered"
spread_NRIp_ult_comb$pattern[which(spread_NRIp_ult_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_NRIp_ult_comb$pattern[which(spread_NRIp_ult_comb$pvalue > 0.05 & spread_NRIp_ult_comb$pvalue < 0.95)] <- "Random"

spread_NTIp_ult_comb$pattern <- NA
spread_NTIp_ult_comb$pattern[which(spread_NTIp_ult_comb$pvalue <= 0.05)] <- "Clustered"
spread_NTIp_ult_comb$pattern[which(spread_NTIp_ult_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_NTIp_ult_comb$pattern[which(spread_NTIp_ult_comb$pvalue > 0.05 & spread_NTIp_ult_comb$pvalue < 0.95)] <- "Random"

spread_PDp_ult_comb$pattern <- NA
spread_PDp_ult_comb$pattern[which(spread_PDp_ult_comb$pvalue <= 0.05)] <- "Clustered"
spread_PDp_ult_comb$pattern[which(spread_PDp_ult_comb$pvalue >= 0.95)] <- "Overdispersed"
spread_PDp_ult_comb$pattern[which(spread_PDp_ult_comb$pvalue > 0.05 & spread_PDp_ult_comb$pvalue < 0.95)] <- "Random"


#Make factors and rearrange
spread_NRIp_prune_comb$pattern <- as.factor(spread_NRIp_prune_comb$pattern)
spread_NTIp_prune_comb$pattern <- as.factor(spread_NTIp_prune_comb$pattern)
spread_PDp_prune_comb$pattern <- as.factor(spread_PDp_prune_comb$pattern)

spread_NRIp_ult_comb$pattern <- as.factor(spread_NRIp_ult_comb$pattern)
spread_NTIp_ult_comb$pattern <- as.factor(spread_NTIp_ult_comb$pattern)
spread_PDp_ult_comb$pattern <- as.factor(spread_PDp_ult_comb$pattern)


spread_NRIp_prune_comb$pattern <- factor(spread_NRIp_prune_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))
spread_NTIp_prune_comb$pattern <- factor(spread_NTIp_prune_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))
spread_PDp_prune_comb$pattern <- factor(spread_PDp_prune_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))

spread_NRIp_ult_comb$pattern <- factor(spread_NRIp_ult_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))
spread_NTIp_ult_comb$pattern <- factor(spread_NTIp_ult_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))
spread_PDp_ult_comb$pattern <- factor(spread_PDp_ult_comb$pattern, levels = c("Clustered", "Random", "Overdispersed", "NA"))


#Plot scattered points coloured by significance
pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NRI_pruneult_scatter_colour_significance.pdf", height = 9, width = 19)
p1 <- ggplot()+
  geom_jitter(data = spread_NRIp_prune_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 3)+
  guides(colour = FALSE)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.text = element_text(size = 8), plot.title = element_text(face = "bold"), axis.title = element_text(size = 10, face = "bold"), strip.text = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "Subset size", y = "-NRI", title = "Phylogram")+
  #guides(colour = element_blank())+
  geom_hline(yintercept = 0, colour = mymidgrey)
p1
p2 <- ggplot()+
  geom_jitter(data = spread_NRIp_ult_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 3)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0.25), "cm"), 
        legend.justification = c(1, -0.05), legend.position = c(1, -0.05), legend.text = element_text(size = 6),
        legend.direction = "vertical", panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.text = element_text(size = 8), plot.title = element_text(face = "bold"), axis.title = element_text(size = 10, face = "bold"), strip.text = element_text(size = 8),  axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "Subset size", y = "-NRI", title = "Chronogram")+
  geom_hline(yintercept = 0, colour = mymidgrey)
p2


two_plots <- (p1 | p2)

ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NRI_scatter_prune_ult_signif_final_plot.pdf", two_plots, dpi = 1000, height = 220, width = 180, units = "mm")

ggplot(prop_sig_NRI_ult, aes(x = Subset, y = Proportion_sig, fill = Category))+
  geom_bar(stat = "identity")+
  facet_wrap(~ Community, ncol = 4)+
  #guides(fill = FALSE)+
  labs(x = "Subset size", y = "Proportion of replicates", title = "Chronogram")+
  scale_fill_manual("Phylodiversity Pattern", values = c("Overdispersed" = myotherblue, "Clustered" = myred, "Random" = mydark), na.value = mymidgrey)+
  theme(plot.margin = unit(c(0,0,0,0.25), "cm"), 
        legend.justification = c(1.05, 0), legend.position = c(1.05, 0), 
        legend.direction = "vertical", panel.background = element_blank(), 
        strip.background = element_rect(fill=mygrey))+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 10), 
        strip.text = element_text(size = 6), axis.ticks.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))


dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/NTI_pruneult_scatter_colour_significance.pdf", height = 9, width = 19)
p1 <- ggplot()+
  geom_jitter(data = spread_NTIp_prune_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 14)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "", y = "NTI", title = "Phylogram")+
  guides(colour = element_blank())+
  geom_hline(yintercept = 0, colour = mymidgrey)
p1
p2 <- ggplot()+
  geom_jitter(data = spread_NTIp_ult_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 14)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8),  axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "Subset size", y = "NTI", title = "Chronogram")+
  geom_hline(yintercept = 0, colour = mymidgrey)
p2
spread_NTIp_recon_comb
p1 + p2 + plot_layout(ncol = 1)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question5/PD_pruneult_scatter_colour_significance.pdf", height = 9, width = 19)
p1 <- ggplot()+
  geom_jitter(data = spread_PDp_prune_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 14)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8), axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "", y = "PD", title = "Phylogram")+
  guides(colour = element_blank())+
  geom_hline(yintercept = 0, colour = mymidgrey)
p1
p2 <- ggplot()+
  geom_jitter(data = spread_PDp_ult_comb, aes(x = subset, y = index, colour = pattern))+
  facet_wrap(~ variable, ncol = 14)+
  scale_colour_manual("Significance", values = c("Random" = mydark, "Clustered" = myred, "Overdispersed" = myotherblue), na.value = mymidgrey)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 14), strip.text = element_text(size = 8),  axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+
  labs(x = "Subset size", y = "PD", title = "Chronogram")+
  geom_hline(yintercept = 0, colour = mymidgrey)
p2

p1 + p2 + plot_layout(ncol = 1)
dev.off()
