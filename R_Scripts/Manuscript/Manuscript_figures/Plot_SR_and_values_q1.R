#Plot species richness and values of NRI and such to test for correlation

#Read species richness files
#Read values of NRI etc
#Plot for 14 communities with species richness (for 1 subset) for one index (or three and facet or something)
#Facet by subset

#Read community data
comm_dataset <- read.csv("./Basedata_Prep/R_community_designations2.csv", stringsAsFactors = FALSE, row.names = 1)
rownames(comm_dataset)[10] <- "Successional.Hardwood.Forest"

# Read in pruned subset data
prune_NRI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NRI_prune_random_subsets.csv", stringsAsFactors = FALSE)
prune_NTI_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_NTI_prune_random_subsets.csv", stringsAsFactors = FALSE)
prune_PD_random_subsets <- read.csv("./Plots/2017_botany/Files_to_plot/Raw_PD_prune_random_subsets.csv", stringsAsFactors = FALSE)

#Correct spelling 
colnames(prune_NRI_random_subsets)[11] <- "Successional.Hardwood.Forest"
#prune_NRI_random_subsets[which(prune_NRI_random_subsets$variable == "Succesional.Hardwood.Forest"), 3] <- "Successional.Hardwood.Forest"

colnames(prune_NTI_random_subsets)[11] <- "Successional.Hardwood.Forest"
colnames(prune_PD_random_subsets)[11] <- "Successional.Hardwood.Forest"

#Remove "Taxa" column - should not be relevant for Ordway as a whole
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets[,-2]
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets_comm[,-17]
prune_NRI_random_subsets_comm <- prune_NRI_random_subsets_comm[,-18]

prune_NTI_random_subsets_comm <- prune_NTI_random_subsets[,-2]
prune_NTI_random_subsets_comm <- prune_NTI_random_subsets_comm[,-17]
prune_NTI_random_subsets_comm <- prune_NTI_random_subsets_comm[,-18]

prune_PD_random_subsets_comm <- prune_PD_random_subsets[,-2]
prune_PD_random_subsets_comm <- prune_PD_random_subsets_comm[,-17]
prune_PD_random_subsets_comm <- prune_PD_random_subsets_comm[,-18]

prune_PD_random_subsets_comm
#Read overall dataset in and correct spelling
clad_572 <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_tree572_clad.csv", stringsAsFactors = FALSE)
clad_572[10,1] <- "Successional.Hardwood.Forest"

#Get value from overall tree for each index 
full_tree_NRI <- as.data.frame(cbind(clad_572$mpd.obs.z, clad_572$SR), stringsAsFactors = FALSE)

rownames(full_tree_NRI) <- rownames(comm_dataset)
colnames(full_tree_NRI) <- c("NRI", "SR")
full_tree_NRI_tr <- as.data.frame(t(full_tree_NRI))
full_tree_NRI_tr <- full_tree_NRI_tr[,-1]
#full_tree_NRI <- full_tree_NRI[-which(is.na(full_tree_NRI$full)), , drop = FALSE]
full_tree_NRI_tr

full_tree_NTI <- as.data.frame(cbind(clad_572$mntd.obs.z, clad_572$SR), stringsAsFactors = FALSE)
rownames(full_tree_NTI) <- rownames(comm_dataset)
colnames(full_tree_NTI) <- c("NTI", "SR")
full_tree_NTI_tr <- as.data.frame(t(full_tree_NTI))
full_tree_NTI_tr <- full_tree_NTI_tr[,-1]

full_tree_PD <- as.data.frame(cbind(clad_572$pd.obs.z, clad_572$SR), stringsAsFactors = FALSE)
rownames(full_tree_PD) <- rownames(comm_dataset)
colnames(full_tree_PD) <- c("PD", "SR")
full_tree_PD_tr <- as.data.frame(t(full_tree_PD))
full_tree_PD_tr <- full_tree_PD_tr[,-1]



# #Add full data value to subsets
# colnames(prune_NRI_random_subsets)
# full_df <- as.data.frame(cbind(index = full_tree_NRI$full, variable = rownames(full_tree_NRI)), stringsAsFactors = FALSE)
# str(full_df)
# full_df$subset <- "572"
# full_df$pvalue <- NA
# full_df$reps <- "1"
# 
# tail(prune_NRI_random_subsets_comm)
# 

colnames(full_tree_NRI_tr)
colnames(prune_NRI_random_subsets_comm)
full_tree_NRI_tr$subset <- "572"
full_tree_NTI_tr$subset <- "572"
full_tree_PD_tr$subset <- "572"
full_tree_NRI_tr$X <- c("NRI1", "SR1")
full_tree_NTI_tr$X <- c("NTI1", "SR1")
full_tree_PD_tr$X <- c("PD1", "SR1")




prune_NRI_random_subsets_comm <- rbind.fill(prune_NRI_random_subsets_comm, full_tree_NRI_tr)
#prune_NRI_random_subsets_comm$subset[501] <- "572"

prune_NTI_random_subsets_comm <- rbind.fill(prune_NTI_random_subsets_comm, full_tree_NTI_tr)
#prune_NTI_random_subsets_comm$subset[501] <- "572"

prune_PD_random_subsets_comm <- rbind.fill(prune_PD_random_subsets_comm, full_tree_PD_tr)
#prune_PD_random_subsets_comm$subset[501] <- "572"
tail(prune_PD_random_subsets_comm)

full_tree_NRI_tr
#Get melted dataframe for only communities by subset
melted_prune_NRI_random_subsets <- melt(prune_NRI_random_subsets_comm, id = c("subset", "X"))
melted_prune_NTI_random_subsets <- melt(prune_NTI_random_subsets_comm, id = c("subset", "X"))
melted_prune_PD_random_subsets <- melt(prune_PD_random_subsets_comm, id = c("subset", "X"))
melted_prune_PD_random_subsets

#Remove "Taxa" rows
noTaxa_NRI_prune <- melted_prune_NRI_random_subsets[which(melted_prune_NRI_random_subsets$variable != "Taxa"),]
noTaxa_NTI_prune <- melted_prune_NTI_random_subsets[which(melted_prune_NTI_random_subsets$variable != "Taxa"),]
noTaxa_PD_prune <- melted_prune_PD_random_subsets[which(melted_prune_PD_random_subsets$variable != "Taxa"),]

noTaxa_PD_prune
#Spread into NRI Pvalue and SR columns
head(noTaxa_NRI_prune)
testing <- noTaxa_NRI_prune

#Get rid of extra characters
testing$reps <- testing$X
noTaxa_NTI_prune$reps <- noTaxa_NTI_prune$X
noTaxa_PD_prune$reps <- noTaxa_PD_prune$X


testing$X <- gsub("X\\d+\\.", "", testing$X)
noTaxa_NTI_prune$X <- gsub("X\\d+\\.", "", noTaxa_NTI_prune$X)
noTaxa_PD_prune$X <- gsub("X\\d+\\.", "", noTaxa_PD_prune$X)
noTaxa_PD_prune$X <- gsub("raw_", "", noTaxa_PD_prune$X)
noTaxa_PD_prune

testing$X <- gsub("\\d+", "", testing$X)
noTaxa_NTI_prune$X <- gsub("\\d+", "", noTaxa_NTI_prune$X)
noTaxa_PD_prune$X <- gsub("\\d+", "", noTaxa_PD_prune$X)
noTaxa_PD_prune

tail(noTaxa_PD_prune)

testing$reps
testing$reps<- gsub("\\.\\w+", "", testing$reps)
noTaxa_NTI_prune$reps <- gsub("\\.\\w+", "", noTaxa_NTI_prune$reps)
noTaxa_PD_prune$reps <- gsub("\\.\\w+", "", noTaxa_PD_prune$reps)
noTaxa_PD_prune



testing$reps <- gsub("X", "", testing$reps)
noTaxa_NTI_prune$reps <- gsub("X", "", noTaxa_NTI_prune$reps)
noTaxa_PD_prune$reps <- gsub("X", "", noTaxa_PD_prune$reps)

testing$reps <- gsub("\\D", "", testing$reps)
noTaxa_NTI_prune$reps <- gsub("\\D", "", noTaxa_NTI_prune$reps)
noTaxa_PD_prune$reps <- gsub("\\D", "", noTaxa_PD_prune$reps)

colnames(testing)
testing$reps
tail(testing$reps)
head(testing)
tail(testing)
tail(noTaxa_PD_prune)

#Spread data
spread <- spread(testing, X, value)
spread_NTI <- spread(noTaxa_NTI_prune, X, value)
spread_PD <- spread(noTaxa_PD_prune, X, value)

tail(noTaxa_NTI_prune)

tail(spread)
#testing[which(testing$subset == "572"),]

#Plot data 
library(devtools)

ggplot(data = spread, aes(x = SR, y = NRI))+
  geom_point(aes(colour = variable))+
  facet_wrap(~ subset, scales = "free")+
  geom_smooth(method='lm')

#ggplot(data = spread[which(spread$subset == "500"),], aes(x = SR, y = NRI))+
#  geom_point(aes(colour = subset))+
#  facet_wrap(~ variable, scales = "free")+
#  geom_smooth(method='lm')

ggplot(data = spread, aes(x = SR, y = NRI))+
  geom_point(aes(colour = subset))+
  facet_wrap(~ variable, scales = "free")+
  geom_smooth(method='lm')

ggplot(data = spread, aes(x = SR, y = NRI))+
  geom_point(aes(colour = variable))+
  facet_grid(variable ~ subset, scales = "free")+
  geom_smooth(method='lm')


ggplot(data = spread_NTI, aes(x = SR, y = NTI))+
  geom_point(aes(colour = variable))+
  facet_wrap(~ subset, scales = "free")+
  geom_smooth(method='lm',formula=y~x)

#ggplot(data = spread_NTI[which(spread_NTI$subset == "500"),], aes(x = SR, y = NTI))+
#  geom_point(aes(colour = subset))+
#  facet_wrap(~ variable, scales = "free")+
#  geom_smooth(method='lm',formula=y~x)

ggplot(data = spread_NTI, aes(x = SR, y = NTI))+
  geom_point(aes(colour = subset))+
  facet_wrap(~ variable, scales = "free")+
  geom_smooth(method='lm',formula=y~x)

ggplot(data = spread_NTI, aes(x = SR, y = NTI))+
  geom_point(aes(colour = variable))+
  facet_grid(variable ~ subset, scales = "free")+
  geom_smooth(method='lm',formula=y~x)

ggplot(data = spread_PD, aes(x = SR, y = PD))+
  geom_point(aes(colour = variable))+
  facet_wrap(~ subset, scales = "free")+
  geom_smooth(method='lm',formula=y~x)

#ggplot(data = spread_PD[which(spread_PD$subset == "500"),], aes(x = SR, y = PD))+
#  geom_point(aes(colour = subset))+
#  facet_wrap(~ variable, scales = "free")+
#  geom_smooth(method='lm',formula=y~x)

ggplot(data = spread_PD, aes(x = SR, y = PD))+
  geom_point(aes(colour = subset))+
  facet_wrap(~ variable, scales = "free")+
  geom_smooth(method='lm',formula=y~x)

ggplot(data = spread_PD, aes(x = SR, y = PD))+
  geom_point(aes(colour = variable))+
  facet_grid(variable ~ subset, scales = "free")+
  geom_smooth(method='lm',formula=y~x)


#ends here
######################################
spread_PD

#Get numbers of taxa in communities
num_prune <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_prune.csv", stringsAsFactors = FALSE)
# num_prop <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_prop.csv", stringsAsFactors = FALSE)
# num_ult <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_ult.csv", stringsAsFactors = FALSE)
# num_recon <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_all_recon.csv", stringsAsFactors = FALSE)

# num_tax <- read.csv("./Plots/2017_botany/Files_to_plot/Num_taxa_q1.csv", stringsAsFactors = FALSE)
# colnames(num_tax) <- c("comm","100", "200", "300", "400", "500")
# melt_num_tax <- melt(num_tax)
# colnames(melt_num_tax) <- c("variable", "subset", "value")
# num_tax_nooverall <- melt_num_tax[-which(melt_num_tax$variable == "Taxa"),]

#Change class of subset to numeric not factor
#class(num_tax_nooverall$subset) <- "numeric"
class(num_prune$subset) <- "numeric"
# class(num_ult$subset) <- "numeric"
# class(num_recon$subset) <- "numeric"
# class(num_prop$subset) <- "numeric"

#Remove overall data from each number of taxa dataset
num_prune_nooverall <- num_prune[-which(num_prune$variable == "Taxa"),]
# num_ult_nooverall <- num_ult[-which(num_ult$variable == "Taxa"),]
# num_recon_nooverall <- num_recon[-which(num_recon$variable == "Taxa"),]
# num_prop_nooverall <- num_prop[-which(num_prop$variable == "Taxa"),]

num_prune_nooverall$reps <- c(1:100)
tail(num_prune_nooverall)

nrow(prune_NRI_random_subsets_comm)
nrow(num_prune_nooverall)
tail(prune_NRI_random_subsets_comm)

tail(num_prune)

full_nsamp <- as.data.frame(cbind(variable = clad_572$X, value = clad_572$SR), stringsAsFactors = FALSE)
colnames(full_nsamp)
colnames(num_prune_nooverall)

full_nsamp$X <- NA
full_nsamp$subset <- "572"
full_nsamp$reps <- "1"
colnames(full_nsamp)

full_nsamp <- full_nsamp[-1,]

prune_NRI_random_subsets_comm

num_prune_nooverall_combo <- rbind(num_prune_nooverall, full_nsamp)
tail(num_prune_nooverall_combo)
nrow(num_prune_nooverall_combo)

str(prune_NRI_random_subsets_comm)
str(num_prune_nooverall_combo)
prune_plus_SR_NRI <- as.data.frame(cbind(prune_NRI_random_subsets_comm, SR = num_prune_nooverall_combo[order(num_prune_nooverall_combo$subset, num_prune_nooverall_combo$reps),4]))
prune_plus_SR_NRI
#Name columns
colnames(num_prune_nooverall) <- c("Number", "Subset", "Community", "SR")

str(prune_plus_SR_NRI)
class(prune_plus_SR_NRI$index) <- "numeric"
class(prune_plus_SR_NRI$SR) <- "numeric"
prune_plus_SR_NRI[which(prune_plus_SR_NRI$subset == "500"),]


ggplot(data = prune_plus_SR_NRI[which(prune_plus_SR_NRI$variable == "Pine.Plantation"),], aes(x = SR, y = index))+
  geom_point(aes(colour = variable))+
  facet_wrap(~ subset)
