#Plots for question 1 - plots number of taxa in community by subset - not raw values successfully yet
#See plots for paper only signif script for colouring by significance - and significance coloured for latest version

#Plot number of taxa in community by number of taxa in subset
#Plot subset index values for pruned phylogenies for each community and anova significance - failed

#Plotting raw values NRI NTI PD random subsets 
#Read files with dataframes to plot
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)
rownames(comm_dataset)[10] <- "Successional.Hardwood.Forest"

# Read in pruned subset data
prune_NRI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
prune_NTI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)
prune_PD_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)

#Correct spelling 
colnames(prune_NRI_random_subsets)[11] <- "Successional.Hardwood.Forest"
colnames(prune_NTI_random_subsets)[11] <- "Successional.Hardwood.Forest"
colnames(prune_PD_random_subsets)[11] <- "Successional.Hardwood.Forest"

#Remove "Taxa" column - should not be relevant for Ordway as a whole
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets[,-2]
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets_comm[,-17]

prune_NTI_random_subsets_comm <- prune_NTI_random_subsets[,-2]
prune_NTI_random_subsets_comm <- prune_NTI_random_subsets_comm[,-17]

prune_PD_random_subsets_comm <- prune_PD_random_subsets[,-2]
prune_PD_random_subsets_comm <- prune_PD_random_subsets_comm[,-17]

#Read overall dataset in and correct spelling
clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)
clad_572[10,1] <- "Successional.Hardwood.Forest"

#Get value from overall tree for each index 
full_tree_NRI <- as.data.frame(clad_572$mpd.obs.z)
rownames(full_tree_NRI) <- rownames(comm_dataset)
colnames(full_tree_NRI) <- "full"
full_tree_NRI_tr <- as.data.frame(t(full_tree_NRI))
full_tree_NRI_tr <- full_tree_NRI_tr[,-1]

full_tree_NTI <- as.data.frame(clad_572$mntd.obs.z)
rownames(full_tree_NTI) <- rownames(comm_dataset)
colnames(full_tree_NTI) <- "full"
full_tree_NTI_tr <- as.data.frame(t(full_tree_NTI))
full_tree_NTI_tr <- full_tree_NTI_tr[,-1]

full_tree_PD <- as.data.frame(clad_572$pd.obs.z)
rownames(full_tree_PD) <- rownames(comm_dataset)
colnames(full_tree_PD) <- "full"
full_tree_PD_tr <- as.data.frame(t(full_tree_PD))
full_tree_PD_tr <- full_tree_PD_tr[,-1]

#Add full data value to subsets
prune_NRI_random_subsets_comm <- bind_rows(prune_NRI_random_subsets_comm, full_tree_NRI_tr)
prune_NRI_random_subsets_comm$subset[501] <- "572"

prune_NTI_random_subsets_comm <- bind_rows(prune_NTI_random_subsets_comm, full_tree_NTI_tr)
prune_NTI_random_subsets_comm$subset[501] <- "572"

prune_PD_random_subsets_comm <- bind_rows(prune_PD_random_subsets_comm, full_tree_PD_tr)
prune_PD_random_subsets_comm$subset[501] <- "572"

#Get melted dataframe for only communities by subset
melted_prune_NRI_random_subsets <- melt(prune_NRI_random_subsets_comm, id = c("subset", "X"))
melted_prune_NTI_random_subsets <- melt(prune_NTI_random_subsets_comm, id = c("subset", "X"))
melted_prune_PD_random_subsets <- melt(prune_PD_random_subsets_comm, id = c("subset", "X"))

#Read anova results star files
prune_NRI_tukey_star <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_NRI_random_tukey.csv", stringsAsFactors = FALSE)
prune_NTI_tukey_star <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_NTI_random_tukey.csv", stringsAsFactors = FALSE)
prune_PD_tukey_star <- read.csv("./PD_files/R_calc_picante/Significance_values/ANOVAs_num_taxa_random/prune_PD_random_tukey.csv", stringsAsFactors = FALSE)

#Divide subsets column into two subset options
prune_NRI_tukey_star$subsets <- str_split_fixed(prune_NRI_tukey_star$subsets, "-", 2)
prune_NTI_tukey_star$subsets <- str_split_fixed(prune_NTI_tukey_star$subsets, "-", 2)
prune_PD_tukey_star$subsets <- str_split_fixed(prune_PD_tukey_star$subsets, "-", 2)

#Correct spelling
prune_NRI_tukey_star$comm <- gsub('Succesional', 'Successional', prune_NRI_tukey_star$comm)
prune_NTI_tukey_star$comm <- gsub('Succesional', 'Successional', prune_NTI_tukey_star$comm)
prune_PD_tukey_star$comm <- gsub('Succesional', 'Successional', prune_PD_tukey_star$comm)

#Make annotations for plotting NRI
annotations_to_plot_NRI <- as.data.frame(cbind(prune_NRI_tukey_star$comm, prune_NRI_tukey_star$subsets, prune_NRI_tukey_star$star, prune_NRI_tukey_star$p.adj))
colnames(annotations_to_plot_NRI) <- c("variable", "sub1", "sub2", "signif", "pvalue")
annotations_to_plot_NRI$xmin <- c(1, 1, 1, 1, 2.1, 2.1, 2.1, 3.1, 3.1, 4.1)
annotations_to_plot_NRI$xmax <- c(1.9, 2.9, 3.9, 5, 2.9, 3.9, 5, 3.9, 5, 5)
annotations_to_plot_NRI$y_position <- c(0, 1, 3, 5, 0, 2, 4, 0, 1, 0)
#annotations_to_plot_NRI$y_position <- c(annotations_to_plot_NRI$y_position + 2)
annotations_to_plot_NRI <- annotations_to_plot_NRI[which(annotations_to_plot_NRI$signif != ""),]


#Make annotations for plotting NTI
annotations_to_plot_NTI <- as.data.frame(cbind(prune_NTI_tukey_star$comm, prune_NTI_tukey_star$subsets, prune_NTI_tukey_star$star, prune_NTI_tukey_star$p.adj))
colnames(annotations_to_plot_NTI) <- c("variable", "sub1", "sub2", "signif", "pvalue")
annotations_to_plot_NTI$xmin <- c(1, 1, 1, 1, 2.1, 2.1, 2.1, 3.1, 3.1, 4.1)
annotations_to_plot_NTI$xmax <- c(1.9, 2.9, 3.9, 5, 2.9, 3.9, 5, 3.9, 5, 5)
annotations_to_plot_NTI$y_position <- c(0, 1, 3, 5, 0, 2, 4, 0, 1, 0)
#annotations_to_plot_NTI$y_position <- c(annotations_to_plot_NTI$y_position + 2)
annotations_to_plot_NTI <- annotations_to_plot_NTI[which(annotations_to_plot_NTI$signif != ""),]

#Make annotations for plotting PD
annotations_to_plot_PD <- as.data.frame(cbind(prune_PD_tukey_star$comm, prune_PD_tukey_star$subsets, prune_PD_tukey_star$star, prune_PD_tukey_star$p.adj))
colnames(annotations_to_plot_PD) <- c("variable", "sub1", "sub2", "signif", "pvalue")
annotations_to_plot_PD$xmin <- c(1, 1, 1, 1, 2.1, 2.1, 2.1, 3.1, 3.1, 4.1)
annotations_to_plot_PD$xmax <- c(1.9, 2.9, 3.9, 5, 2.9, 3.9, 5, 3.9, 5, 5)
annotations_to_plot_PD$y_position <- c(0, 1, 3, 5, 0, 2, 4, 0, 1, 0)
#annotations_to_plot_PD$y_position <- c(annotations_to_plot_PD$y_position + 2)
annotations_to_plot_PD <- annotations_to_plot_PD[which(annotations_to_plot_PD$signif != ""),]

#Get numbers of taxa in communities
num_prune <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_prune.csv", stringsAsFactors = FALSE)
num_prop <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_prop.csv", stringsAsFactors = FALSE)
num_ult <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_ult.csv", stringsAsFactors = FALSE)
num_recon <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_recon.csv", stringsAsFactors = FALSE)

# #Getting total number of taxa in entire dataset per community?
# num_tax <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_q1.csv", stringsAsFactors = FALSE)
# colnames(num_tax) <- c("comm","100", "200", "300", "400", "500")
# melt_num_tax <- melt(num_tax)
# colnames(melt_num_tax) <- c("variable", "subset", "value")
# num_tax_nooverall <- melt_num_tax[-which(melt_num_tax$variable == "Taxa"),]

#Change class of subset to numeric not factor
#class(num_tax_nooverall$subset) <- "numeric"
class(num_prune$subset) <- "numeric"
class(num_ult$subset) <- "numeric"
class(num_recon$subset) <- "numeric"
class(num_prop$subset) <- "numeric"

#Remove overall data from each number of taxa dataset
num_prune_nooverall <- num_prune[-which(num_prune$variable == "Taxa"),]
num_ult_nooverall <- num_ult[-which(num_ult$variable == "Taxa"),]
num_recon_nooverall <- num_recon[-which(num_recon$variable == "Taxa"),]
num_prop_nooverall <- num_prop[-which(num_prop$variable == "Taxa"),]


#Get models for R squared and slopes
#Run lm model for all communities to create list of results
models <- dlply(num_prune_nooverall, "variable", function(df) lm(value ~ subset, data = df))
models_ult <- dlply(num_ult_nooverall, "variable", function(df) lm(value ~ subset, data = df))
models_recon <- dlply(num_recon_nooverall, "variable", function(df) lm(value ~ subset, data = df))
models_prop <- dlply(num_prop_nooverall, "variable", function(df) lm(value ~ subset, data = df))

#Get summary stats for models - get coefficients
summary_lms <- ldply(models, coef)
summary_lms_ult <- ldply(models_ult, coef)
summary_lms_recon <- ldply(models_recon, coef)
summary_lms_prop <- ldply(models_prop, coef)

#Get summary stats for models - get r squared values
rsq_df <- data.frame(rsq = numeric())
for (i in 1:length(models)){
  rsq <- summary(models[[i]])$adj.r.squared
  rsq_df[i,1] <- rsq
}

# #print rsquared values
# summary(models[[14]])$adj.r.squared
# summary(models_ult[[14]])$adj.r.squared
# summary(models_prop[[14]])$adj.r.squared
# summary(models_recon[[14]])$adj.r.squared
# 

rsq_df_recon <- data.frame(rsq_recon = numeric())
for (i in 1:length(models_recon)){
  rsq_recon <- summary(models_recon[[i]])$adj.r.squared
  rsq_df_recon[i,1] <- rsq_recon
}

rsq_df_prop <- data.frame(rsq_prop = numeric())
for (i in 1:length(models_prop)){
  rsq_prop <- summary(models_prop[[i]])$adj.r.squared
  rsq_df_prop[i,1] <- rsq_prop
}
 summary_lms
 
#Add community column to rsq table and slope m
rsq_df_comm <- cbind("variable" = summary_lms$variable, rsq_df, "m" = round(summary_lms$subset, 3))
rsq_df_comm_recon <- cbind("variable" = summary_lms_recon$variable, rsq_df_recon, "m" = round(summary_lms_recon$subset, 3))
rsq_df_comm_prop <- cbind("variable" = summary_lms_prop$variable, rsq_df_prop, "m" = round(summary_lms_prop$subset, 3))


#Add max value for community
max_values <- num_prune_nooverall %>%
  group_by(variable) %>%
  summarize(max(value))
rsq_df_comm <- cbind(rsq_df_comm, "max_val" = max_values$`max(value)`)

max_values_recon <- num_recon_nooverall %>%
  group_by(variable) %>%
  summarize(max(value))
rsq_df_comm_recon <- cbind(rsq_df_comm_recon, "max_val" = max_values_recon$`max(value)`)

max_values_prop <- num_prop_nooverall %>%
  group_by(variable) %>%
  summarize(max(value))
rsq_df_comm_prop <- cbind(rsq_df_comm_prop, "max_val" = max_values_prop$`max(value)`)

# # Print the summary of each model
# l_ply(models, summary, .print = TRUE)
# l_ply(models_recon, summary, .print = TRUE)
# l_ply(models_prop, summary, .print = TRUE)

# Apply coef to each model and return a data frame
coef_table <- ldply(models, coef)
coef_table_recon <- ldply(models_recon, coef)
coef_table_prop <- ldply(models_prop, coef)

#Replace names with dots with names without dots
#But first make factor
num_prune_nooverall$variable <- as.factor(num_prune_nooverall$variable)
levels(num_prune_nooverall$variable) <- gsub(".", " ", levels(num_prune_nooverall$variable), fixed = TRUE)
class(num_prune_nooverall$variable)
rsq_df_comm$variable <- gsub(".", " ", rsq_df_comm$variable, fixed = TRUE)

#Get colours for plotting
mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"


#Plot number of taxa in community by number of taxa in subset
#With R squared and slope of linear models for each community
pdf("./Plots/2017_botany/Paper_plots/Question1/Faceted_community_num_taxa_per_subset_labelled.pdf", height = 7, width = 9)
p2 <- ggplot(num_prune_nooverall, aes(factor(subset), value))+
  geom_boxplot(fill = myblue, colour = mydark, size = 0.05, weight = 0.05, outlier.size = 0.05)+
  stat_summary(fun.y=mean, geom="line", aes(group=1), size = 0.1)  + 
  labs(x = "Subset size", y = "Number of taxa in community")+
  #stat_summary(fun.y=mean, geom="point")+
  #geom_smooth(method = "lm")+
  facet_wrap(~ variable, scales = "free_y", labeller = label_wrap_gen(width = 15))+
  guides(colour = element_blank())+
  geom_hline(yintercept = 0, colour = mymidgrey)+
  theme(panel.background = element_blank(), 
        strip.background = element_rect(fill=rgb(236,240,241, maxColorValue = 255)))+
  theme(axis.title = element_text(size = 6), strip.text = element_text(size = 4, margin = margin(t = 0,0,0,0, "cm")), 
        axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size = 4), 
        axis.ticks = element_blank())+
  geom_text(data = rsq_df_comm, x = 1.5, y = rsq_df_comm$max_val, label = paste0("m=", rsq_df_comm$m), size = 1)+
  geom_text(data = rsq_df_comm, x = 1.5, y = (rsq_df_comm$max_val-(rsq_df_comm$max_val/8)), label = paste0("R2=", (round(rsq_df_comm$rsq, 3))), size = 1)
  #annotate("text", data = rsq_df_comm, x = 2, y = rsq_df_comm$max_val, label = paste0("m=", rsq_df_comm$m), size = 3)
  #annotate("text", data = rsq_df_comm, x = 2, y = (rsq_df_comm$max_val+5), label = paste0("R2=", (round(rsq_df_comm$rsq, 3))), size = 3)
dev.off()

ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question1/App1_num_tax_num_sub_final_plot.pdf", p2, dpi = 1000, height = 80, width = 80, units = "mm")


pdf("../Plots/2017_botany/Paper_plots/Question4/Faceted_community_num_taxa_per_subset_prop.pdf")
ggplot(num_prop_nooverall, aes(factor(subset), value))+
  geom_boxplot()+
  labs(x = "Subset size", y = "Number of taxa")+
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  #stat_summary(fun.y=mean, geom="point")+
  #geom_smooth(method = "lm")+
  facet_wrap(~ variable)+
  geom_text(data = rsq_df_comm_prop, x = 1.5, y = rsq_df_comm_prop$max_val, label = paste0("m=", rsq_df_comm_prop$m), size = 3)+
  geom_text(data = rsq_df_comm_prop, x = 1.5, y = (rsq_df_comm_prop$max_val-(rsq_df_comm_prop$max_val/8)), label = paste0("R2=", (round(rsq_df_comm_prop$rsq, 3))), size = 3)
dev.off()

pdf("../Plots/2017_botany/Paper_plots/Question2/Faceted_community_num_taxa_per_subset_recon.pdf")
ggplot(num_recon_nooverall, aes(factor(subset), value))+
  geom_boxplot()+
  labs(x = "Subset size", y = "Number of taxa")+
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  #stat_summary(fun.y=mean, geom="point")+
  #geom_smooth(method = "lm")+
  facet_wrap(~ variable)+
  geom_text(data = rsq_df_comm_recon, x = 1.5, y = rsq_df_comm_recon$max_val, label = paste0("m=", rsq_df_comm_recon$m), size = 3)+
  geom_text(data = rsq_df_comm_recon, x = 1.5, y = (rsq_df_comm_recon$max_val-(rsq_df_comm_recon$max_val/8)), label = paste0("R2=", (round(rsq_df_comm_recon$rsq, 3))), size = 3)
dev.off()


#For plotting index values with ttests of significance ---- still not working
#Unable to plot significance with boxplots - come back to this

#Read file for ttest of subset to overall
ttest_NRI <- read.csv("../Plots/2017_botany/Files_to_plot/Ttest_samplesize_NRI.csv", stringsAsFactors = FALSE)
ttest_NTI <- read.csv("../Plots/2017_botany/Files_to_plot/Ttest_samplesize_NTI.csv", stringsAsFactors = FALSE)
ttest_PD <- read.csv("../Plots/2017_botany/Files_to_plot/Ttest_samplesize_PD.csv", stringsAsFactors = FALSE)
ttest_NTI$variable[which(ttest_NTI$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"
ttest_PD$variable[which(ttest_PD$variable == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

class(ttest_NRI$subset) <- "character"
class(ttest_NTI$subset) <- "character"
class(ttest_PD$subset) <- "character"

#Plot faceted melted dataset - geom_signif not working, y-axis weird

pdf("./Plots/2017_botany/Paper_plots/Question1/Faceted_community_pruned_NRI_random_subsets.pdf")
ggplot(melted_prune_NRI_random_subsets, aes(subset, value))+
  geom_boxplot() +
  geom_signif(data=annotations_to_plot_NRI, aes(xmin=xmin, xmax=xmax, annotations="", y_position=(y_position + max(melted_prune_NRI_random_subsets$value, na.rm = TRUE))), textsize = 3, vjust = -0.2, tip_length = 0, manual=TRUE) +
  facet_wrap(~ variable, scales = "free")+
  geom_hline(yintercept = 0, color= "red")+
  geom_text(data = ttest_NRI, aes(subset, -5), label = ttest_NRI$star)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question1/Faceted_community_pruned_NTI_random_subsets.pdf")
ggplot(melted_prune_NTI_random_subsets, aes(subset, value))+
  geom_boxplot() +
  geom_signif(data=annotations_to_plot_NTI, aes(xmin=xmin, xmax=xmax, annotations="", y_position=(y_position + max(melted_prune_NTI_random_subsets$value, na.rm = TRUE))), textsize = 3, vjust = -0.2, tip_length = 0, manual=TRUE) +
  facet_wrap(~ variable, scales = "free")+
  geom_hline(yintercept = 0, color = "red")+
  geom_text(data = ttest_NTI, aes(subset, -5), label = ttest_NTI$star)
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question1/Faceted_community_pruned_PD_random_subsets.pdf")
ggplot(melted_prune_PD_random_subsets, aes(subset, value))+
  geom_boxplot() +
  geom_signif(data=annotations_to_plot_PD, aes(xmin=xmin, xmax=xmax, annotations="", y_position=(y_position + max(melted_prune_PD_random_subsets$value, na.rm = TRUE))), textsize = 3, vjust = -0.2, tip_length = 0, manual=TRUE) +
  facet_wrap(~ variable, scales = "free")+
  geom_hline(yintercept = 0, color = "red")+
  geom_text(data = ttest_PD, aes(subset, -5), label = ttest_PD$star)
dev.off()

# ggplot(melted_prune_NRI_random_subsets, aes(x = factor(subset), y = value))+
#   geom_boxplot()+
#   #geom_jitter(data = melted_prune_NRI_random_subsets, aes(x = subset, y = value, group = subset))+
#   facet_wrap(~ variable, ncol = 5, scales = "free_y")+
#   xlab("Number of taxa in subset")+
#   ggtitle("-NRI for pruned phylogenies by number of taxa in subset")+
#   ylab("-NRI")+
#   theme(text = element_text(size=10), strip.text.x = element_text(size = 7))+
#   ggsignif()
#   geom_text(data = prune_NRI_tukey_star, aes(x = factor(subsets[,1]), y = 6, label = star), size = 3)+
#   geom_text(data = prune_NRI_tukey_star, aes(x = factor(subsets[,2]), y = 6, label = star), size = 3)
# dev.off()



#See if geom_signif can work for this - doesn't work, y axis weird
ggplot(melted_prune_NRI_random_subsets, aes(subset, value)) +
  geom_boxplot()+
  labs(x = "Subset size", y = "NRI")+
  facet_wrap(~ variable, ncol = 4)+
  geom_signif(annotations = annotations_to_plot_NRI$signif, y_position = c(annotations_to_plot_NRI$y_position), xmin = c(annotations_to_plot_NRI$xmin), xmax = c(annotations_to_plot_NRI$xmax), tip_length=0, vjust = 0.1, textsize = 3)

#Best effort so far - plotting index values by subset size but significance not plotted
ggplot(melted_prune_NRI_random_subsets, aes(subset, value))+
  geom_boxplot() +
  labs(x = "Subset size", y = "NRI")+
  geom_signif(data=annotations_to_plot_NRI, aes(xmin=xmin, xmax=xmax, annotations=signif, y_position=(y_position + max(melted_prune_NRI_random_subsets$value, na.rm = TRUE))), textsize = 3, vjust = -0.2, tip_length = 0, manual=TRUE) +
  facet_wrap(~ variable)+
  ylim(NA, 13)


#Work in progress

annotations_to_plot_NRI$signif %>% 
  group_by(variable)
class(annotations_to_plot_NRI$variable)
class(melted_prune_NRI_random_subsets$variable)
annotations_to_plot_NRI

melted_prune_NRI_random_subsets %>% 
  group_by(variable) %>% 
  max(value, na.rm = TRUE)

head(melted_prune_NRI_random_subsets)

#Testing facet wrapping - try using ggpubr functions
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "npg")+
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)  

ggplot(melted_prune_NRI_random_subsets, aes(subset, value))+
  geom_boxplot() +
  geom_signif(data=test_annotations, aes(xmin=xmin, xmax=xmax, annotations = signif, y_position=(y_position + max(melted_prune_NRI_random_subsets$value, na.rm = TRUE))), textsize = 3, vjust = -0.2, tip_length = 0, manual = TRUE) +
  facet_wrap(~ variable)+
  ylim(NA, 13)

test_annotations <- annotations_to_plot_NRI[which(annotations_to_plot_NRI$variable == "Clastic.Upland.Lake"),]
test_annotations


class(melted_prune_NRI_random_subsets$value)

