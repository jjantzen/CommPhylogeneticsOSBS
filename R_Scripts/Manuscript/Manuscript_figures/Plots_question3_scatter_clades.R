#Making plots for papers
#Question 7

#Make files of just correct PDs for families
PD_Asteraceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv", stringsAsFactors = FALSE)
PD_Cyperaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv", stringsAsFactors = FALSE)
PD_Ericaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fabaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fagaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv", stringsAsFactors = FALSE)
PD_Poaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv", stringsAsFactors = FALSE)
PD_rosids <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv", stringsAsFactors = FALSE)
PD_overall <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_tree572_clad.csv", stringsAsFactors = FALSE)

#Set rownames to communities
rownames(PD_Asteraceae) <- PD_overall$X
rownames(PD_Cyperaceae) <- PD_overall$X
rownames(PD_Ericaceae) <- PD_overall$X
rownames(PD_Fabaceae) <- PD_overall$X
rownames(PD_Fagaceae) <- PD_overall$X
rownames(PD_Poaceae) <- PD_overall$X
rownames(PD_rosids) <- PD_overall$X
rownames(PD_overall) <- PD_overall$X

#Transpose datasets
PD_aster_tr <- t(PD_Asteraceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_cyper_tr <- t(PD_Cyperaceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_eric_tr <- t(PD_Ericaceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_fab_tr <- t(PD_Fabaceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_fag_tr <- t(PD_Fagaceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_po_tr <- t(PD_Poaceae) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_rosids_tr <- t(PD_rosids) %>% 
  as.data.frame(stringsAsFactors = FALSE)
PD_overall_tr <- t(PD_overall) %>% 
  as.data.frame(stringsAsFactors = FALSE)

#Correct misspelling
colnames(PD_aster_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_cyper_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_eric_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_fab_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_fag_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_po_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_rosids_tr)[10] <- "Successional.Hardwood.Forest"
colnames(PD_overall_tr)[10] <- "Successional.Hardwood.Forest"

#Remove Ordway as whole
PD_aster_tr <- PD_aster_tr[,-1]
PD_cyper_tr <- PD_cyper_tr[,-1]
PD_eric_tr <- PD_eric_tr[,-1]
PD_fab_tr <- PD_fab_tr[,-1]
PD_fag_tr <- PD_fag_tr[,-1]
PD_po_tr <- PD_po_tr[,-1]
PD_rosids_tr <- PD_rosids_tr[,-1]
PD_overall_tr <- PD_overall_tr[,-1]

PD_aster_tr[10,]
#Make new dataframe with families etc in same dataframe and make column for family
family_PD <- rbind(PD_overall_tr[9,], PD_overall_tr[3,], PD_overall_tr[10,],PD_aster_tr[9,], PD_aster_tr[3,], PD_aster_tr[10,], PD_cyper_tr[9,], PD_cyper_tr[3,], PD_cyper_tr[10,],PD_eric_tr[9,], PD_eric_tr[3,], PD_eric_tr[10,],PD_fab_tr[9,], PD_fab_tr[3,], PD_fab_tr[10,],PD_fag_tr[9,], PD_fag_tr[3,], PD_fag_tr[10,],PD_po_tr[9,], PD_po_tr[3,], PD_po_tr[10,], PD_rosids_tr[9,], PD_rosids_tr[3,], PD_rosids_tr[10,], PD_overall_tr[9,], PD_overall_tr[3,], PD_overall_tr[10,])
family_PD$family <- c("Overall", "Overall", "Overall", "Aster", "Aster", "Aster", "Cyper", "Cyper", "Cyper", "Eric", "Eric", "Eric", "Fab", "Fab", "Fab", "Fag", "Fag", "Fag", "Po","Po", "Po", "Rosids","Rosids", "Rosids", "Total", "Total", "Total")

family_NRI <- rbind(PD_overall_tr[17:18,], PD_overall_tr[3,], PD_aster_tr[17:18,], PD_aster_tr[3,], PD_cyper_tr[17:18,], PD_cyper_tr[3,], PD_eric_tr[17:18,], PD_eric_tr[3,], PD_fab_tr[17:18,], PD_fab_tr[3,], PD_fag_tr[17:18,], PD_fag_tr[3,], PD_po_tr[17:18,], PD_po_tr[3,], PD_rosids_tr[17:18,], PD_rosids_tr[3,], PD_overall_tr[17:18,], PD_overall_tr[3,])
family_NRI$family <- c("Overall", "Overall", "Overall", "Aster", "Aster", "Aster", "Cyper", "Cyper", "Cyper", "Eric", "Eric", "Eric", "Fab", "Fab", "Fab", "Fag", "Fag", "Fag", "Po", "Po","Po", "Rosids", "Rosids", "Rosids", "Total", "Total", "Total")

family_NTI <- rbind(PD_overall_tr[25:26,], PD_overall_tr[3,],PD_aster_tr[25:26,], PD_aster_tr[3,], PD_cyper_tr[25:26,], PD_cyper_tr[3,], PD_eric_tr[25:26,], PD_eric_tr[3,], PD_fab_tr[25:26,], PD_fab_tr[3,], PD_fag_tr[25:26,], PD_fag_tr[3,], PD_po_tr[25:26,], PD_po_tr[3,], PD_rosids_tr[25:26,], PD_rosids_tr[3,], PD_overall_tr[25:26,], PD_overall_tr[3,])
family_NTI$family <- c("Overall", "Overall", "Overall", "Aster", "Aster", "Aster", "Cyper", "Cyper", "Cyper", "Eric", "Eric", "Eric", "Fab", "Fab", "Fab", "Fag", "Fag", "Fag", "Po", "Po","Po", "Rosids", "Rosids", "Rosids", "Total", "Total", "Total")

#Make column for type of index
family_PD$index <- c("PD", "SR", "pvalue")
family_NRI$index <- c("NRI", "pvalue", "SR")
family_NTI$index <- c("NTI", "pvalue", "SR")

#Melt datasets for plotting
melted_PD_family <- melt(family_PD, id = c("family", "index"))
melted_NRI_family <- melt(family_NRI, id = c("family", "index"))
melted_NTI_family <- melt(family_NTI, id = c("family", "index"))

#Make values numeric 
class(melted_PD_family$value) <- "numeric"
class(melted_NRI_family$value) <- "numeric"
class(melted_NTI_family$value) <- "numeric"

unique(melted_NRI_family$family)
#Get numbers of taxa in communities
num_tax <- read.csv("./../../../Writing/Tables/Q6_num_taxa_family.csv", stringsAsFactors = FALSE)
num_tax
num_tax <- num_tax[,-1]
num_tax <- melt(num_tax)
colnames(num_tax) <- c("variable", "subset", "value")

num_tax_nooverall <- num_tax[-which(num_tax$variable == "Taxa"),]
#num_tax_nooverall <- num_tax_nooverall[-which(num_tax_nooverall$subset == "Overall"),]
num_tax_nooverall
#Get SR and NRI in different columns
spread_NRI_family <- spread(melted_NRI_family, index, value)
spread_NTI_family <- spread(melted_NTI_family, index, value)
spread_PD_family <- spread(melted_PD_family, index, value)
spread_NRI_family[which(spread_NRI_family$family == "Overall"),]

reduced_NRI_family <- spread_NRI_family[-which(spread_NRI_family$family == "Total"),]
reduced_NRI_family

colnames(spread_NRI_family)
unique(spread_NRI_family$family)

# #Plot number of taxa in community by subsets
# pdf("./Plots/2017_botany/Paper_plots/Question6/Comparison_numtax_NTI_family.pdf")
# ggplot()+
#   geom_point(data = num_tax_nooverall, aes(subset, value))+
#   geom_point(data = test_melted_NTI_family[which(test_melted_NTI_family$index == "NTI"),], aes(x = family, y = value, color = family))+
#   facet_wrap(~ variable, scales = "free", ncol = 10)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# dev.off()
# num_tax
# test_melted_PD_family
# #Testing plotting both num and pd together
# test_melted_NTI_family <- melted_NTI_family
# test_melted_NTI_family$variable <- paste0(test_melted_NTI_family$variable, "2")


# #Plot raw index values for families
# #Not this first plot
# #PD
# pdf("./Plots/2017_botany/Paper_plots/Question6/Faceted_community_PD_family_by_ntax.pdf")
# ggplot()+
#   geom_point(data = spread_PD_family[-which(spread_PD_family$family == "Total"),], aes(x = SR, y = PD, color = family), size = 2)+
#   facet_wrap(~ variable, ncol = 5, scales = "free")+
#   xlab("Number of taxa in subset")+
#   ggtitle("PD for families")+
#   ylab("PD")+
#   theme(text = element_text(size=7))+
#   geom_hline(yintercept = 0)
#   #geom_text(data = melted_PD_family[which(melted_PD_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
# dev.off()

#NRI
###Latest plot april 23, 2018
comm_names <- reduced_NRI_family$variable[which(reduced_NRI_family$family == "Aster")]
str(comm_names)
levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
levels(comm_names)

levels(reduced_NRI_family$variable) <- levels(comm_names)
fam_names <- c()
sp_names <- reduced_NRI_family$family[which(reduced_NRI_family$variable == "Baygall")]
#sp_names <- gsub("Overall", "All taxa", sp_names) %>% 
#  sort()
sp_names

for (i in 1:(length(sp_names)-2)){
  name <- paste(sp_names[i], "aceae", sep = "")
  fam_names <- c(fam_names, name)
}
levels(reduced_NRI_family$variable)
sp_names
fam_names                  
labels <- c(fam_names, "Rosids")

labels <- gsub("Overallaceae", "All taxa", labels)
labels
reduced_NRI_family[which(reduced_NRI_family$family == "Overall" | reduced_NRI_family$family == "Total"),]

#reduced_NRI_family <- reduced_NRI_family[-which(reduced_NRI_family$family == "Total"),]
reduced_NRI_family

unique(reduced_NRI_family$family)
str(reduced_NRI_family)
labels

reduced_NRI_family$signif <- "ns"
reduced_NRI_family$signif[which(reduced_NRI_family$pvalue >= 0.95)] <- "*"
reduced_NRI_family$signif[which(reduced_NRI_family$pvalue <= 0.05)] <- "*"
reduced_NRI_family$signif 

#Not a useful plot
pdf("./Plots/2017_botany/Paper_plots/Question6/NRI_family_by_ntax_points.pdf", height = 7, width = 9)
ggplot()+
  geom_point(data = reduced_NRI_family[-which(reduced_NRI_family$variable == "Basin Swamp" | reduced_NRI_family$variable == "Baygall" | is.na(spread_NRI_family$NRI | is.na(spread_NRI_family$family))),], aes(x = SR, y = NRI, color = family), size = 2.5)+
  facet_wrap(~ variable, ncol = 4, scales = "free_x")+
  xlab("Species richness of taxonomic group")+
  ylab("-NRI")+
  theme_bw()+
  theme(text = element_text(size=9), axis.title=element_text(size=10,face="bold"))+
  guides(color=guide_legend("Taxonomic Group"))+
  geom_hline(yintercept = 0)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  scale_color_hue(labels = sort(labels))
  #geom_text(data = melted_NRI_family[which(melted_NRI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)

#Not a useful plot
#plot for only a single panel

ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$variable == "Basin Marsh" | is.na(spread_NRI_family$NRI | is.na(spread_NRI_family$family))),], aes(x = SR, y = NRI, color = family), size = 6)+
  facet_wrap(~ variable, ncol = 4, scales = "free_x")+
  xlab("Species richness of taxonomic group")+
  ylab("-NRI")+
  theme_bw()+
  theme(text = element_text(size=15), axis.title=element_text(size=20))+
  guides(color=guide_legend("Taxonomic Group"))+
  geom_hline(yintercept = 0)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  scale_color_hue(labels = sort(labels))
#geom_text(data = melted_NRI_family[which(melted_NRI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)


reduced_NRI_family[which(reduced_NRI_family$variable == "Basin Marsh"),]
sort(reduced_NRI_family$family)
labels
str(reduced_NRI_family$family)

reduced_NRI_family$family <- as.factor(reduced_NRI_family$family)
#labels <- as.factor(labels)
levels(reduced_NRI_family$family) <- c("Asteraceae", "Cyperaceae", "Ericaceae", "Fabaceae", "Fagaceae", "All taxa", "Poaceae", "Rosids")
levels(reduced_NRI_family$family)
#reduced_NRI_family$family_fac

#Sort the families by level? - new function
sortLvls.fnc <- function(oldFactor, levelOrder) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  
  if(!is.numeric(levelOrder)) stop("'order' should be a numeric vector.")
  
  if(max(levelOrder) > length(levels(oldFactor))) stop("The largest number in 'order' can't be larger than the number of levels in the factor.")
  
  if(length(levelOrder) > length(levels(oldFactor))) stop("You can't have more elements in 'order' than there are levels in the factor.")
  
  if(length(levelOrder) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrder])
  }
  
  if(length(levelOrder) < length(levels(oldFactor))) {
    levelOrderAll <- c(levelOrder, (1:length(levels(oldFactor)))[-levelOrder])
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderAll])
  }
  
  return(reorderedFactor)
}

#Sort families
levels(reduced_NRI_family$family)
reduced_NRI_family$family <- sortLvls.fnc(reduced_NRI_family$family, c(6))
levels(reduced_NRI_family$family)

reduced_NRI_family$signif <- as.factor(reduced_NRI_family$signif)
levels(reduced_NRI_family$signif) <- gsub("ns", "Random", levels(reduced_NRI_family$signif), fixed = TRUE)
levels(reduced_NRI_family$signif) <- gsub("*", "Significant", levels(reduced_NRI_family$signif), fixed = TRUE)
levels(reduced_NRI_family$signif)

#This is the useful plot - added significance definition but not sure how else to clarify
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question3/NRI_family_by_ntax_points_signif.pdf", height = 7, width = 9)
plot.fam <- ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Random"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 2.5, stroke = 1.5)+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Significant"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 2.5, stroke = 1.5)+
  facet_wrap(~ variable, ncol = 4, scales = "free_x")+
  xlab("Species Richness of Clade")+
  ylab("-NRI")+
  theme_bw()+
  theme(text = element_text(size=9), axis.title=element_text(size=10,face="bold"))+
  guides(color=guide_legend("Clade"))+
  guides(shape = guide_legend("Significance", override.aes = list(shape = c(1,16))), text = c("ns" = "Random", "*" = "Significant"))+
  geom_hline(yintercept = 0)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  #scale_color_hue(labels = levels(reduced_NRI_family$family))+
  scale_shape_manual(values = c("Random" = 1, "Significant" = 16))+
  scale_colour_manual(values=cbPalette, labels = levels(reduced_NRI_family$family))+
  #scale_color_brewer(palette="Dark2", labels = levels(reduced_NRI_family$family))+
  scale_y_continuous(limits = c(-5.75, 3.5))
#geom_text(data = melted_NRI_family[which(melted_NRI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
plot.fam
#Plot just legend - need function to get legend first - not needed now
#legend <- get_legend(plot.fam)
dev.off()
labels <- as.data.frame(unique(reduced_NRI_family$variable))
labels$`unique(reduced_NRI_family$variable)` <- as.character(labels$`unique(reduced_NRI_family$variable)`)
labels$`unique(reduced_NRI_family$variable)`[9] <- "Successional Hardwood\n Forest"
labels$`unique(reduced_NRI_family$variable)`[12] <- "Abandoned Field \n Pasture"

colnames(labels)[1] <- "variable"

plot.fam2 <- ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Random"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 0.5, stroke = 0.5)+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Significant"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 0.5, stroke = 0.5)+
  facet_wrap(~ variable, ncol = 5, scales = "free_x", labeller = label_wrap_gen(width = 15))+
  xlab("Species Richness of Clade")+
  ylab("-NRI")+
  #theme_classic()+
  theme(panel.background = element_blank(), panel.border = element_rect(size = 0.5, fill = NA), text = element_text(size=4), axis.text.x = element_text(size = 4), axis.text.y = element_text(size = 4), axis.title=element_text(size=6,face="bold"), legend.position = "bottom")+
  guides(color=guide_legend("Clade", keyheight = 1, direction = "horizontal"))+
  guides(shape = guide_legend("Significance", override.aes = list(shape = c(2,3)), keyheight = 1, direction = "vertical"), text = c("ns" = "Random", "*" = "Significant"))+
  geom_hline(yintercept = 0, size = 0.25)+
  theme(legend.justification = 'left', 
        legend.position = 'bottom',  
        legend.box.just = 'left', legend.margin=margin(t = 0, b = 0, r = 0, l = 0, unit='cm'), panel.background = element_blank(), 
        strip.text.x = element_text(margin = margin(0,0,0,0, "cm")), 
        strip.background = element_rect(linetype = "solid", fill=rgb(236,240,241, maxColorValue = 255)))+ #panel.spacing.y = unit(-0.1, "lines"),legend.box = 'vertical',
  #scale_color_hue(labels = levels(reduced_NRI_family$family))+
  scale_shape_manual(values = c("Random" = 2, "Significant" = 3))+
  scale_colour_manual(values=cbPalette, labels = levels(reduced_NRI_family$family))+
  #scale_color_brewer(palette="Dark2", labels = levels(reduced_NRI_family$family))+
  scale_y_continuous(limits = c(-5.75, 3.5))
  #scale_x_continuous(limits = c(0, max(reduced_NRI_family$SR)))


#this is the one with best resolution and sizing etc (final plot)
plot.fam3 <- ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Random"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 0.5, stroke = 0.5)+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Significant"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 0.5, stroke = 0.5)+
  facet_wrap(~ variable, ncol = 5, scales = "free_x", labeller = label_wrap_gen(width = 15))+
  xlab("Species Richness of Clade")+
  ylab("-NRI")+
  theme(panel.border = element_rect(size = 0.5, fill = NA), text = element_text(size=4), 
        axis.text.x = element_text(size = 4), axis.text.y = element_text(size = 4), axis.title=element_text(size=6,face="bold"),
        axis.ticks = element_line(size = 0.25), axis.line = element_blank(),
        legend.position = "bottom", legend.box = 'vertical', legend.box.just = 'left', legend.justification = "top",
        legend.margin=margin(t = -0.25,  r = 0, b = 0, l = 0, unit = "cm"),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.key.size = unit(0.5, "line"),
        panel.background = element_blank(), 
        strip.text.x = element_text(margin = margin(t = 0,0,0,0, "cm")), 
        strip.background = element_rect(linetype = "solid", fill=rgb(236,240,241, maxColorValue = 255)))+ #panel.spacing.y = unit(-0.1, "lines"),  legend.justification = 'left', 
  guides(color=guide_legend("Clade", nrow = 1))+#, direction = "horizontal"
  guides(shape = guide_legend("Significance", override.aes = list(shape = c(2,3))), text = c("ns" = "Random", "*" = "Significant"))+#, direction = "horizontal"
  geom_hline(yintercept = 0, size = 0.25)+
  scale_shape_manual(values = c("Random" = 2, "Significant" = 3))+
  scale_colour_manual(values=cbPalette, labels = levels(reduced_NRI_family$family))+
  scale_y_continuous(limits = c(-5.75, 3.5))
#scale_color_hue(labels = levels(reduced_NRI_family$family))
#scale_color_brewer(palette="Dark2", labels = levels(reduced_NRI_family$family))
#scale_x_continuous(limits = c(0, max(reduced_NRI_family$SR)))
ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question3/NRI_family_by_ntax_points_signif_final_plot.pdf", plot.fam3, dpi = 1000, height = 80, width = 80, units = "mm")

plot.fam3_big <- ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Random"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 3, stroke = 0.75)+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "Significant"),], aes(x = SR, y = NRI, color = family, shape = signif), size = 3, stroke = 0.75)+
  facet_wrap(~ variable, ncol = 5, scales = "free", labeller = label_wrap_gen(width = 15))+
  geom_hline(yintercept = 0, color = mydark, size = 0.3)+
  xlab("Species Richness of Clade")+
  ylab("-NRI")+
  theme(panel.border = element_rect(size = 1, fill = NA), text = element_text(size=8), 
        axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title=element_text(size=10,face="bold"),
        axis.ticks = element_line(size = 1), axis.line = element_blank(),
        legend.position = "bottom",  legend.box.just = 'left', legend.justification = "top", #legend.box = 'vertical',
        legend.margin=margin(t = 0,  r = 0, b = 0, l = 0.5, unit = "cm"),
        legend.spacing.y = unit(0.1, "cm"), legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(2, "line"), legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        panel.background = element_blank(), 
        strip.text.x = element_text(size = 8, margin = margin(t = 0,0,0,0, "cm")), 
        strip.background = element_rect(linetype = "solid", fill=rgb(236,240,241, maxColorValue = 255)))+ #panel.spacing.y = unit(-0.1, "lines"),  legend.justification = 'left', 
  guides(color=guide_legend("Clade", nrow = 2))+#, direction = "horizontal"
  guides(shape = guide_legend("Significance", nrow = 2, override.aes = list(shape = c(2,3))), text = c("ns" = "Random", "*" = "Significant"))+#, direction = "horizontal"
  scale_shape_manual(values = c("Random" = 2, "Significant" = 3))+
  scale_colour_manual(values=cbPalette, labels = levels(reduced_NRI_family$family))+
  scale_y_continuous(limits = c(-5.75, 3.5))
#for final plot for pub

ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question3/NRI_family_by_ntax_points_signif_final_plot_big.eps", plot.fam3_big, dpi = 1000, height = 180, width = 180, units = "mm")

library(ggplot2)



tiff("./Plots/2017_botany/Paper_plots/Resubmission/Question3/Testing_resolution.tiff", width = 4, height = 4, pointsize = 1/300, units = 'in', res = 300)


pdf("./Plots/2017_botany/Paper_plots/Fam_legend.pdf")
ggdraw(plot_grid(legend))
dev.off()
colnames(spread_NRI_family)
spread_NRI_family[which(is.na(spread_NRI_family$NRI)),]


#### Not useful after this but OK


#####single frame
pdf("./Plots/2017_botany/Paper_plots/single_comm_familyNRI.pdf")
ggplot()+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "" & reduced_NRI_family$variable == "Sandhill"),], shape = 4, aes(x = SR, y = NRI, color = family), size = 2.5, stroke = 1.5)+
  geom_point(data = reduced_NRI_family[which(reduced_NRI_family$signif == "*" & reduced_NRI_family$variable == "Sandhill"),], shape = 8, aes(x = SR, y = NRI, color = family), size = 2.5, stroke = 1.5)+
  #facet_wrap(~ variable, ncol = 4, scales = "free_x")+
  xlab("Species richness of taxonomic group")+
  ylab("-NRI")+
  theme_bw()+
  theme(text = element_text(size=15), axis.title=element_text(size=20))+
  guides(color=guide_legend("Taxonomic Group"))+
  geom_hline(yintercept = 0)+
  theme(panel.background = element_blank(), strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  scale_color_hue(labels = levels(reduced_NRI_family$family))+
  scale_y_continuous(limits = c(-5.75, 3.5))
dev.off()



#NTI
pdf("./Plots/2017_botany/Paper_plots/Question6/Faceted_community_NTI_family_by_ntax.pdf")
ggplot()+
  geom_point(data = spread_NTI_family[-which(spread_NTI_family$family == "Total"),], aes(x = SR, y = NTI, color = family), size = 2)+
  facet_wrap(~ variable, ncol = 5, scales = "free")+
  xlab("Number of taxa in subset")+
  ggtitle("NTI for families")+
  ylab("NTI")+
  theme(text = element_text(size=7))+
  geom_hline(yintercept = 0)
  #geom_text(data = melted_NTI_family[which(melted_NTI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
dev.off()

#Including totals
pdf("./Plots/2017_botany/Paper_plots/Question6/Faceted_community_PD_family_by_ntax_inctotal.pdf")
ggplot()+
  geom_point(data = spread_PD_family[which(spread_PD_family$pvalue <= 0.05 | spread_PD_family$pvalue >= 0.95),], aes(x = SR, y = PD, color = family), size = 2)+
  facet_wrap(~ variable, ncol = 5, scales = "free")+
  xlab("Number of taxa in subset")+
  ggtitle("PD for families")+
  ylab("PD")+
  theme(text = element_text(size=7))+
  geom_hline(yintercept = 0)
#geom_text(data = melted_PD_family[which(melted_PD_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
dev.off()

#NRI
pdf("./Plots/2017_botany/Paper_plots/Question6/Faceted_community_NRI_family_by_ntax_inctotal.pdf")
ggplot()+
  geom_point(data = spread_NRI_family[which(spread_NRI_family$pvalue <= 0.05 | spread_NRI_family$pvalue >= 0.95),], aes(x = SR, y = NRI, color = family), size = 2)+
  facet_wrap(~ variable, ncol = 5, scales = "free")+
  xlab("Number of taxa in subset")+
  ggtitle("NRI for families")+
  ylab("NRI")+
  theme(text = element_text(size=7))+
  geom_hline(yintercept = 0)
#geom_text(data = melted_NRI_family[which(melted_NRI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
dev.off()

#NTI
pdf("./Plots/2017_botany/Paper_plots/Question6/Faceted_community_NTI_family_by_ntax_inctotal.pdf")
ggplot()+
  geom_point(data = spread_NTI_family[which(spread_NTI_family$pvalue <= 0.05 | spread_NTI_family$pvalue >= 0.95),], aes(x = SR, y = NTI, color = family), size = 2)+
  #Add black for insignificant results
  geom_point(data = spread_NTI_family[which(spread_NTI_family$pvalue > 0.05 & spread_NTI_family$pvalue < 0.95),], aes(x = SR, y = NTI), size = 2)+
  facet_wrap(~ variable, ncol = 5, scales = "free")+
  xlab("Number of taxa in subset")+
  ggtitle("NTI for families")+
  ylab("NTI")+
  theme(text = element_text(size=7))+
  geom_hline(yintercept = 0)
#geom_text(data = melted_NTI_family[which(melted_NTI_family$index == "SR"),], aes(x = family, y = 4, label = round(value)), show.legend = FALSE, size = 3)
dev.off()

