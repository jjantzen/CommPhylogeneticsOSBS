#Constraint script to get reduced constraint depending on taxa in subset
#Run in hipergator as:
#Rscript Constraint_script.R subset1.fasta subset1_constraint.nex


#load libraries
library(picante)
library(ape)
library(picante)
library(geiger)
library(seqinr)

#set command args (not sure why)
args <- commandArgs(trailingOnly = TRUE)

#read full 572 constraint tree
constraint <- read.nexus(file="Constraints572.nex")

#read subset fasta file
subset <- read.fasta(args[1])

#get names for subset taxa
names<-getName(subset)

#match names between constraint tree and names(subset)
match<-name.check(constraint, names, data.names=names)

#drop tips off constraint not in names(subset)
prunedconstraint<-drop.tip(constraint, match$tree_not_data)

#write new nexus file for constraint now matched to subset
write.tree(prunedconstraint, file=args[2])
