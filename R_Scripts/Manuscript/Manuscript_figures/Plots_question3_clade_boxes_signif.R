#Question 5 - plotting squares of difference of family from overall with species richness
#One axis - family by number of species
#Other axis - community by number of species

#Read family data and correct spelling
PD_Asteraceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Asteraceae_clad.csv", stringsAsFactors = FALSE)
PD_Cyperaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Cyperaceae_clad.csv", stringsAsFactors = FALSE)
PD_Ericaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Ericaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fabaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fabaceae_clad.csv", stringsAsFactors = FALSE)
PD_Fagaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Fagaceae_clad.csv", stringsAsFactors = FALSE)
PD_Poaceae <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_Poaceae_clad.csv", stringsAsFactors = FALSE)
PD_rosids <- read.csv("./PD_files/R_calc_picante/Ult_vs_clad/Clad/PD_rosid_clad.csv", stringsAsFactors = FALSE)
PD_overall <- read.csv("./PD_files/R_calc_picante/Growth_form/PD_tree572_clad.csv", stringsAsFactors = FALSE)

PD_overall$X[which(PD_overall$X == "Succesional.Hardwood.Forest")] <- "Successional.Hardwood.Forest"

#Set rownames to communities
rownames(PD_Asteraceae) <- PD_overall$X
rownames(PD_Cyperaceae) <- PD_overall$X
rownames(PD_Ericaceae) <- PD_overall$X
rownames(PD_Fabaceae) <- PD_overall$X
rownames(PD_Fagaceae) <- PD_overall$X
rownames(PD_Poaceae) <- PD_overall$X
rownames(PD_rosids) <- PD_overall$X
rownames(PD_overall) <- PD_overall$X

#Get indices into separate datasets with pvalues and SR
Aster_NRI <- as.data.frame(cbind(Community = rownames(PD_Asteraceae), NRI = PD_Asteraceae$mpd.obs.z, pvalue = PD_Asteraceae$mpd.obs.p, SR = PD_Asteraceae$SR), stringsAsFactors = FALSE)
Aster_NTI <- as.data.frame(cbind(Community = rownames(PD_Asteraceae), NTI = PD_Asteraceae$mntd.obs.z, pvalue = PD_Asteraceae$mntd.obs.p, SR = PD_Asteraceae$SR), stringsAsFactors = FALSE)
Aster_PD <- as.data.frame(cbind(Community = rownames(PD_Asteraceae), PD = PD_Asteraceae$pd.obs.z, pvalue = PD_Asteraceae$pd.obs.p, SR = PD_Asteraceae$SR), stringsAsFactors = FALSE)

Eric_NRI <- as.data.frame(cbind(Community = rownames(PD_Ericaceae), NRI = PD_Ericaceae$mpd.obs.z, pvalue = PD_Ericaceae$mpd.obs.p, SR = PD_Ericaceae$SR), stringsAsFactors = FALSE)
Eric_NTI <- as.data.frame(cbind(Community = rownames(PD_Ericaceae), NTI = PD_Ericaceae$mntd.obs.z, pvalue = PD_Ericaceae$mntd.obs.p, SR = PD_Ericaceae$SR), stringsAsFactors = FALSE)
Eric_PD <- as.data.frame(cbind(Community = rownames(PD_Ericaceae), PD = PD_Ericaceae$pd.obs.z, pvalue = PD_Ericaceae$pd.obs.p, SR = PD_Ericaceae$SR), stringsAsFactors = FALSE)

Cyper_NRI <- as.data.frame(cbind(Community = rownames(PD_Cyperaceae), NRI = PD_Cyperaceae$mpd.obs.z, pvalue = PD_Cyperaceae$mpd.obs.p, SR = PD_Cyperaceae$SR), stringsAsFactors = FALSE)
Cyper_NTI <- as.data.frame(cbind(Community = rownames(PD_Cyperaceae), NTI = PD_Cyperaceae$mntd.obs.z, pvalue = PD_Cyperaceae$mntd.obs.p, SR = PD_Cyperaceae$SR), stringsAsFactors = FALSE)
Cyper_PD <- as.data.frame(cbind(Community = rownames(PD_Cyperaceae), PD = PD_Cyperaceae$pd.obs.z, pvalue = PD_Cyperaceae$pd.obs.p, SR = PD_Cyperaceae$SR), stringsAsFactors = FALSE)

Fab_NRI <- as.data.frame(cbind(Community = rownames(PD_Fabaceae), NRI = PD_Fabaceae$mpd.obs.z, pvalue = PD_Fabaceae$mpd.obs.p, SR = PD_Fabaceae$SR), stringsAsFactors = FALSE)
Fab_NTI <- as.data.frame(cbind(Community = rownames(PD_Fabaceae), NTI = PD_Fabaceae$mntd.obs.z, pvalue = PD_Fabaceae$mntd.obs.p, SR = PD_Fabaceae$SR), stringsAsFactors = FALSE)
Fab_PD <- as.data.frame(cbind(Community = rownames(PD_Fabaceae), PD = PD_Fabaceae$pd.obs.z, pvalue = PD_Fabaceae$pd.obs.p, SR = PD_Fabaceae$SR), stringsAsFactors = FALSE)

Fag_NRI <- as.data.frame(cbind(Community = rownames(PD_Fagaceae), NRI = PD_Fagaceae$mpd.obs.z, pvalue = PD_Fagaceae$mpd.obs.p, SR = PD_Fagaceae$SR), stringsAsFactors = FALSE)
Fag_NTI <- as.data.frame(cbind(Community = rownames(PD_Fagaceae), NTI = PD_Fagaceae$mntd.obs.z, pvalue = PD_Fagaceae$mntd.obs.p, SR = PD_Fagaceae$SR), stringsAsFactors = FALSE)
Fag_PD <- as.data.frame(cbind(Community = rownames(PD_Fagaceae), PD = PD_Fagaceae$pd.obs.z, pvalue = PD_Fagaceae$pd.obs.p, SR = PD_Fagaceae$SR), stringsAsFactors = FALSE)

Po_NRI <- as.data.frame(cbind(Community = rownames(PD_Poaceae), NRI = PD_Poaceae$mpd.obs.z, pvalue = PD_Poaceae$mpd.obs.p, SR = PD_Poaceae$SR), stringsAsFactors = FALSE)
Po_NTI <- as.data.frame(cbind(Community = rownames(PD_Poaceae), NTI = PD_Poaceae$mntd.obs.z, pvalue = PD_Poaceae$mntd.obs.p, SR = PD_Poaceae$SR), stringsAsFactors = FALSE)
Po_PD <- as.data.frame(cbind(Community = rownames(PD_Poaceae), PD = PD_Poaceae$pd.obs.z, pvalue = PD_Poaceae$pd.obs.p, SR = PD_Poaceae$SR), stringsAsFactors = FALSE)

Rosid_NRI <- as.data.frame(cbind(Community = rownames(PD_rosids), NRI = PD_rosids$mpd.obs.z, pvalue = PD_rosids$mpd.obs.p, SR = PD_rosids$SR), stringsAsFactors = FALSE)
Rosid_NTI <- as.data.frame(cbind(Community = rownames(PD_rosids), NTI = PD_rosids$mntd.obs.z, pvalue = PD_rosids$mntd.obs.p, SR = PD_rosids$SR), stringsAsFactors = FALSE)
Rosid_PD <- as.data.frame(cbind(Community = rownames(PD_rosids), PD = PD_rosids$pd.obs.z, pvalue = PD_rosids$pd.obs.p, SR = PD_rosids$SR), stringsAsFactors = FALSE)

Overall_NRI <- as.data.frame(cbind(Community = rownames(PD_overall), NRI = PD_overall$mpd.obs.z, pvalue = PD_overall$mpd.obs.p, SR = PD_overall$SR), stringsAsFactors = FALSE)
Overall_NTI <- as.data.frame(cbind(Community = rownames(PD_overall), NTI = PD_overall$mntd.obs.z, pvalue = PD_overall$mntd.obs.p, SR = PD_overall$SR), stringsAsFactors = FALSE)
Overall_PD <- as.data.frame(cbind(Community = rownames(PD_overall), PD = PD_overall$pd.obs.z, pvalue = PD_overall$pd.obs.p, SR = PD_overall$SR), stringsAsFactors = FALSE)

#Add column for significant conclusions - need to do for other indices
Aster_NRI$Category <- ""
Aster_NRI$Category[which(Aster_NRI$pvalue <= 0.05)] <- "Clustered"
Aster_NRI$Category[which(Aster_NRI$pvalue >= 0.95)] <- "Overdispersed"
Aster_NRI$Category[which(Aster_NRI$pvalue > 0.05 & Aster_NRI$pvalue < 0.95)] <- "Random"
Aster_NRI$Category[is.na(Aster_NRI$pvalue)] <- NA

Cyper_NRI$Category <- ""
Cyper_NRI$Category[which(Cyper_NRI$pvalue <= 0.05)] <- "Clustered"
Cyper_NRI$Category[which(Cyper_NRI$pvalue >= 0.95)] <- "Overdispersed"
Cyper_NRI$Category[which(Cyper_NRI$pvalue > 0.05 & Cyper_NRI$pvalue < 0.95)] <- "Random"
Cyper_NRI$Category[is.na(Cyper_NRI$pvalue)] <- NA

Eric_NRI$Category <- ""
Eric_NRI$Category[which(Eric_NRI$pvalue <= 0.05)] <- "Clustered"
Eric_NRI$Category[which(Eric_NRI$pvalue >= 0.95)] <- "Overdispersed"
Eric_NRI$Category[which(Eric_NRI$pvalue > 0.05 & Eric_NRI$pvalue < 0.95)] <- "Random"
Eric_NRI$Category[is.na(Eric_NRI$pvalue)] <- NA

Fab_NRI$Category <- ""
Fab_NRI$Category[which(Fab_NRI$pvalue <= 0.05)] <- "Clustered"
Fab_NRI$Category[which(Fab_NRI$pvalue >= 0.95)] <- "Overdispersed"
Fab_NRI$Category[which(Fab_NRI$pvalue > 0.05 & Fab_NRI$pvalue < 0.95)] <- "Random"
Fab_NRI$Category[is.na(Fab_NRI$pvalue)] <- NA

Fag_NRI$Category <- ""
Fag_NRI$Category[which(Fag_NRI$pvalue <= 0.05)] <- "Clustered"
Fag_NRI$Category[which(Fag_NRI$pvalue >= 0.95)] <- "Overdispersed"
Fag_NRI$Category[which(Fag_NRI$pvalue > 0.05 & Fag_NRI$pvalue < 0.95)] <- "Random"
Fag_NRI$Category[is.na(Fag_NRI$pvalue)] <- NA

Po_NRI$Category <- ""
Po_NRI$Category[which(Po_NRI$pvalue <= 0.05)] <- "Clustered"
Po_NRI$Category[which(Po_NRI$pvalue >= 0.95)] <- "Overdispersed"
Po_NRI$Category[which(Po_NRI$pvalue > 0.05 & Po_NRI$pvalue < 0.95)] <- "Random"
Po_NRI$Category[is.na(Po_NRI$pvalue)] <- NA

Rosid_NRI$Category <- ""
Rosid_NRI$Category[which(Rosid_NRI$pvalue <= 0.05)] <- "Clustered"
Rosid_NRI$Category[which(Rosid_NRI$pvalue >= 0.95)] <- "Overdispersed"
Rosid_NRI$Category[which(Rosid_NRI$pvalue > 0.05 & Rosid_NRI$pvalue < 0.95)] <- "Random"
Rosid_NRI$Category[is.na(Rosid_NRI$pvalue)] <- NA

Overall_NRI$Category <- ""
Overall_NRI$Category[which(Overall_NRI$pvalue <= 0.05)] <- "Clustered"
Overall_NRI$Category[which(Overall_NRI$pvalue >= 0.95)] <- "Overdispersed"
Overall_NRI$Category[which(Overall_NRI$pvalue > 0.05 & Overall_NRI$pvalue < 0.95)] <- "Random"
Overall_NRI$Category[is.na(Overall_NRI$pvalue)] <- NA

#Make column for same as overall or not - compare family value with overall to get binary value (or NA)
Aster_NRI$Comparison <- ""
Aster_NRI$Comparison[which(Aster_NRI$Category == Overall_NRI$Category)] <- 1
Aster_NRI$Comparison[which(Aster_NRI$Category != Overall_NRI$Category)] <- 0
Aster_NRI$Comparison[is.na(Aster_NRI$Category)] <- NA

Eric_NRI$Comparison <- ""
Eric_NRI$Comparison[which(Eric_NRI$Category == Overall_NRI$Category)] <- 1
Eric_NRI$Comparison[which(Eric_NRI$Category != Overall_NRI$Category)] <- 0
Eric_NRI$Comparison[is.na(Eric_NRI$Category)] <- NA

Cyper_NRI$Comparison <- ""
Cyper_NRI$Comparison[which(Cyper_NRI$Category == Overall_NRI$Category)] <- 1
Cyper_NRI$Comparison[which(Cyper_NRI$Category != Overall_NRI$Category)] <- 0
Cyper_NRI$Comparison[is.na(Cyper_NRI$Category)] <- NA

Fab_NRI$Comparison <- ""
Fab_NRI$Comparison[which(Fab_NRI$Category == Overall_NRI$Category)] <- 1
Fab_NRI$Comparison[which(Fab_NRI$Category != Overall_NRI$Category)] <- 0
Fab_NRI$Comparison[is.na(Fab_NRI$Category)] <- NA

Fag_NRI$Comparison <- ""
Fag_NRI$Comparison[which(Fag_NRI$Category == Overall_NRI$Category)] <- 1
Fag_NRI$Comparison[which(Fag_NRI$Category != Overall_NRI$Category)] <- 0
Fag_NRI$Comparison[is.na(Fag_NRI$Category)] <- NA

Po_NRI$Comparison <- ""
Po_NRI$Comparison[which(Po_NRI$Category == Overall_NRI$Category)] <- 1
Po_NRI$Comparison[which(Po_NRI$Category != Overall_NRI$Category)] <- 0
Po_NRI$Comparison[is.na(Po_NRI$Category)] <- NA

Rosid_NRI$Comparison <- ""
Rosid_NRI$Comparison[which(Rosid_NRI$Category == Overall_NRI$Category)] <- 1
Rosid_NRI$Comparison[which(Rosid_NRI$Category != Overall_NRI$Category)] <- 0
Rosid_NRI$Comparison[is.na(Rosid_NRI$Category)] <- NA


#Make column for specific comparisons - significant and nonsignificant mismatches 
Aster_NRI$Pattern_match <- ""
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Aster_NRI$Pattern_match[which(Aster_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Aster_NRI$Pattern_match[which(is.na(Aster_NRI$Category) | is.na(Overall_NRI$Category))] <- 10


Eric_NRI$Pattern_match <- ""
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Eric_NRI$Pattern_match[which(Eric_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Eric_NRI$Pattern_match[which(is.na(Eric_NRI$Category) | is.na(Overall_NRI$Category))] <- 10

Cyper_NRI$Pattern_match <- ""
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Cyper_NRI$Pattern_match[which(Cyper_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Cyper_NRI$Pattern_match[which(is.na(Cyper_NRI$Category) | is.na(Overall_NRI$Category))] <- 10


Fab_NRI$Pattern_match <- ""
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Fab_NRI$Pattern_match[which(Fab_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Fab_NRI$Pattern_match[which(is.na(Fab_NRI$Category) | is.na(Overall_NRI$Category))] <- 10

Fag_NRI$Pattern_match <- ""
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Fag_NRI$Pattern_match[which(Fag_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Fag_NRI$Pattern_match[which(is.na(Fag_NRI$Category) | is.na(Overall_NRI$Category))] <- 10

Po_NRI$Pattern_match <- ""
Po_NRI$Pattern_match[which(Po_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Po_NRI$Pattern_match[which(Po_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Po_NRI$Pattern_match[which(Po_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Po_NRI$Pattern_match[which(Po_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Po_NRI$Pattern_match[which(Po_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Po_NRI$Pattern_match[which(Po_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Po_NRI$Pattern_match[which(Po_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Po_NRI$Pattern_match[which(Po_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Po_NRI$Pattern_match[which(Po_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Po_NRI$Pattern_match[which(is.na(Po_NRI$Category) | is.na(Overall_NRI$Category))] <- 10

Rosid_NRI$Pattern_match <- ""
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Random" & Overall_NRI$Category == "Random")] <- 1
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Clustered" & Overall_NRI$Category == "Clustered")] <- 2
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Overdispersed")] <- 3
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Random" & Overall_NRI$Category == "Overdispersed")] <- 4
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Random" & Overall_NRI$Category == "Clustered")] <- 5
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Random")] <- 6
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Clustered" & Overall_NRI$Category == "Random")] <- 7
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Overdispersed" & Overall_NRI$Category == "Clustered")] <- 8
Rosid_NRI$Pattern_match[which(Rosid_NRI$Category == "Clustered" & Overall_NRI$Category == "Overdispersed")] <- 9
Rosid_NRI$Pattern_match[which(is.na(Rosid_NRI$Category) | is.na(Overall_NRI$Category))] <- 10

#Add column for family
Aster_NRI$Family <- "Asteraceae"
Cyper_NRI$Family <- "Cyperaceae"
Eric_NRI$Family <- "Ericaceae"
Fab_NRI$Family <- "Fabaceae"
Fag_NRI$Family <- "Fagaceae"
Po_NRI$Family <- "Poaceae"
Rosid_NRI$Family <- "Rosids"

#Get all families into single dataset
Family_NRI <- rbind(Aster_NRI, Cyper_NRI, Eric_NRI, Fab_NRI, Fag_NRI, Po_NRI, Rosid_NRI)

#Get species richness for families and communities
totals_for_families <- as.data.frame(cbind(Family = Family_NRI$Family[which(Family_NRI$Community == "Taxa")], SR = as.numeric(Family_NRI$SR[which(Family_NRI$Community == "Taxa")])), stringsAsFactors = FALSE)
totals_for_communities <- as.data.frame(cbind(Community = Overall_NRI$Community, SR = as.numeric(Overall_NRI$SR)), stringsAsFactors = FALSE) 
totals_for_communities$SR <- as.numeric(totals_for_communities$SR )
totals_for_families$SR <- as.numeric(totals_for_families$SR )

#Using for loop, add column of species richness for each value in the dataset
for (i in 1:nrow(Aster_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Aster_NRI$Community[i])]
  Aster_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Asteraceae")]
  Aster_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Cyper_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Cyper_NRI$Community[i])]
  Cyper_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Cyperaceae")]
  Cyper_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Eric_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Eric_NRI$Community[i])]
  Eric_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Ericaceae")]
  Eric_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fab_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fab_NRI$Community[i])]
  Fab_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fabaceae")]
  Fab_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fag_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fag_NRI$Community[i])]
  Fag_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fagaceae")]
  Fag_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Po_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Po_NRI$Community[i])]
  Po_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Poaceae")]
  Po_NRI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Rosid_NRI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Rosid_NRI$Community[i])]
  Rosid_NRI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Rosids")]
  Rosid_NRI$Fam_SR[i] <- SR_fam
}

#Get all families into single dataset
Family_NRI <- rbind(Aster_NRI, Cyper_NRI, Eric_NRI, Fab_NRI, Fag_NRI, Po_NRI, Rosid_NRI)

#Sort data by community species richness, community and family species richness
sorted_Family_NRI <- Family_NRI %>% 
  arrange(Comm_SR, Community, Fam_SR)
sorted_Family_NRI[,c(1,5,7)]
Overall_NRI[,c(1,5)]
#Make character columns into factors that are ordered
sorted_Family_NRI$Community <- factor(sorted_Family_NRI$Community, levels = unique(sorted_Family_NRI$Community), ordered = TRUE)
sorted_Family_NRI$Family <- factor(sorted_Family_NRI$Family, levels = unique(sorted_Family_NRI$Family), ordered = TRUE)

# #Plot grids sorted by species richness in total community and in total family
# pdf("./Plots/2017_botany/Paper_plots/Question6/NRI_family_v_overall_boxes.pdf")
# ggplot(sorted_Family_NRI, aes(x = Family, y = Community, fill = Comparison)) +
#   geom_raster()
# dev.off()



###NTI
#Add column for significant conclusions - need to do for other indices
Aster_NTI$Category <- ""
Aster_NTI$Category[which(Aster_NTI$pvalue <= 0.05)] <- "Clustered"
Aster_NTI$Category[which(Aster_NTI$pvalue >= 0.95)] <- "Overdispersed"
Aster_NTI$Category[which(Aster_NTI$pvalue > 0.05 & Aster_NTI$pvalue < 0.95)] <- "Random"
Aster_NTI$Category[is.na(Aster_NTI$pvalue)] <- NA

Cyper_NTI$Category <- ""
Cyper_NTI$Category[which(Cyper_NTI$pvalue <= 0.05)] <- "Clustered"
Cyper_NTI$Category[which(Cyper_NTI$pvalue >= 0.95)] <- "Overdispersed"
Cyper_NTI$Category[which(Cyper_NTI$pvalue > 0.05 & Cyper_NTI$pvalue < 0.95)] <- "Random"
Cyper_NTI$Category[is.na(Cyper_NTI$pvalue)] <- NA

Eric_NTI$Category <- ""
Eric_NTI$Category[which(Eric_NTI$pvalue <= 0.05)] <- "Clustered"
Eric_NTI$Category[which(Eric_NTI$pvalue >= 0.95)] <- "Overdispersed"
Eric_NTI$Category[which(Eric_NTI$pvalue > 0.05 & Eric_NTI$pvalue < 0.95)] <- "Random"
Eric_NTI$Category[is.na(Eric_NTI$pvalue)] <- NA

Fab_NTI$Category <- ""
Fab_NTI$Category[which(Fab_NTI$pvalue <= 0.05)] <- "Clustered"
Fab_NTI$Category[which(Fab_NTI$pvalue >= 0.95)] <- "Overdispersed"
Fab_NTI$Category[which(Fab_NTI$pvalue > 0.05 & Fab_NTI$pvalue < 0.95)] <- "Random"
Fab_NTI$Category[is.na(Fab_NTI$pvalue)] <- NA

Fag_NTI$Category <- ""
Fag_NTI$Category[which(Fag_NTI$pvalue <= 0.05)] <- "Clustered"
Fag_NTI$Category[which(Fag_NTI$pvalue >= 0.95)] <- "Overdispersed"
Fag_NTI$Category[which(Fag_NTI$pvalue > 0.05 & Fag_NTI$pvalue < 0.95)] <- "Random"
Fag_NTI$Category[is.na(Fag_NTI$pvalue)] <- NA

Po_NTI$Category <- ""
Po_NTI$Category[which(Po_NTI$pvalue <= 0.05)] <- "Clustered"
Po_NTI$Category[which(Po_NTI$pvalue >= 0.95)] <- "Overdispersed"
Po_NTI$Category[which(Po_NTI$pvalue > 0.05 & Po_NTI$pvalue < 0.95)] <- "Random"
Po_NTI$Category[is.na(Po_NTI$pvalue)] <- NA

Rosid_NTI$Category <- ""
Rosid_NTI$Category[which(Rosid_NTI$pvalue <= 0.05)] <- "Clustered"
Rosid_NTI$Category[which(Rosid_NTI$pvalue >= 0.95)] <- "Overdispersed"
Rosid_NTI$Category[which(Rosid_NTI$pvalue > 0.05 & Rosid_NTI$pvalue < 0.95)] <- "Random"
Rosid_NTI$Category[is.na(Rosid_NTI$pvalue)] <- NA

Overall_NTI$Category <- ""
Overall_NTI$Category[which(Overall_NTI$pvalue <= 0.05)] <- "Clustered"
Overall_NTI$Category[which(Overall_NTI$pvalue >= 0.95)] <- "Overdispersed"
Overall_NTI$Category[which(Overall_NTI$pvalue > 0.05 & Overall_NTI$pvalue < 0.95)] <- "Random"
Overall_NTI$Category[is.na(Overall_NTI$pvalue)] <- NA



#Make column for same as overall or not - compare family value with overall to get binary value (or NA)
Aster_NTI$Comparison <- ""
Aster_NTI$Comparison[which(Aster_NTI$Category == Overall_NTI$Category)] <- 1
Aster_NTI$Comparison[which(Aster_NTI$Category != Overall_NTI$Category)] <- 0
Aster_NTI$Comparison[is.na(Aster_NTI$Category)] <- NA

Eric_NTI$Comparison <- ""
Eric_NTI$Comparison[which(Eric_NTI$Category == Overall_NTI$Category)] <- 1
Eric_NTI$Comparison[which(Eric_NTI$Category != Overall_NTI$Category)] <- 0
Eric_NTI$Comparison[is.na(Eric_NTI$Category)] <- NA

Cyper_NTI$Comparison <- ""
Cyper_NTI$Comparison[which(Cyper_NTI$Category == Overall_NTI$Category)] <- 1
Cyper_NTI$Comparison[which(Cyper_NTI$Category != Overall_NTI$Category)] <- 0
Cyper_NTI$Comparison[is.na(Cyper_NTI$Category)] <- NA

Fab_NTI$Comparison <- ""
Fab_NTI$Comparison[which(Fab_NTI$Category == Overall_NTI$Category)] <- 1
Fab_NTI$Comparison[which(Fab_NTI$Category != Overall_NTI$Category)] <- 0
Fab_NTI$Comparison[is.na(Fab_NTI$Category)] <- NA

Fag_NTI$Comparison <- ""
Fag_NTI$Comparison[which(Fag_NTI$Category == Overall_NTI$Category)] <- 1
Fag_NTI$Comparison[which(Fag_NTI$Category != Overall_NTI$Category)] <- 0
Fag_NTI$Comparison[is.na(Fag_NTI$Category)] <- NA

Po_NTI$Comparison <- ""
Po_NTI$Comparison[which(Po_NTI$Category == Overall_NTI$Category)] <- 1
Po_NTI$Comparison[which(Po_NTI$Category != Overall_NTI$Category)] <- 0
Po_NTI$Comparison[is.na(Po_NTI$Category)] <- NA

Rosid_NTI$Comparison <- ""
Rosid_NTI$Comparison[which(Rosid_NTI$Category == Overall_NTI$Category)] <- 1
Rosid_NTI$Comparison[which(Rosid_NTI$Category != Overall_NTI$Category)] <- 0
Rosid_NTI$Comparison[is.na(Rosid_NTI$Category)] <- NA

#Make column for specific comparisons - significant and nonsignificant mismatches 
Aster_NTI$Pattern_match <- ""
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Aster_NTI$Pattern_match[which(Aster_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Aster_NTI$Pattern_match[which(is.na(Aster_NTI$Category) | is.na(Overall_NTI$Category))] <- 10


Eric_NTI$Pattern_match <- ""
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Eric_NTI$Pattern_match[which(Eric_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Eric_NTI$Pattern_match[which(is.na(Eric_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

Cyper_NTI$Pattern_match <- ""
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Cyper_NTI$Pattern_match[which(Cyper_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Cyper_NTI$Pattern_match[which(is.na(Cyper_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

Fab_NTI$Pattern_match <- ""
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Fab_NTI$Pattern_match[which(Fab_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Fab_NTI$Pattern_match[which(is.na(Fab_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

Fag_NTI$Pattern_match <- ""
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Fag_NTI$Pattern_match[which(Fag_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Fag_NTI$Pattern_match[which(is.na(Fag_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

Po_NTI$Pattern_match <- ""
Po_NTI$Pattern_match[which(Po_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Po_NTI$Pattern_match[which(Po_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Po_NTI$Pattern_match[which(Po_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Po_NTI$Pattern_match[which(Po_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Po_NTI$Pattern_match[which(Po_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Po_NTI$Pattern_match[which(Po_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Po_NTI$Pattern_match[which(Po_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Po_NTI$Pattern_match[which(Po_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Po_NTI$Pattern_match[which(Po_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Po_NTI$Pattern_match[which(is.na(Po_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

Rosid_NTI$Pattern_match <- ""
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Random" & Overall_NTI$Category == "Random")] <- 1
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Clustered" & Overall_NTI$Category == "Clustered")] <- 2
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Overdispersed")] <- 3
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Random" & Overall_NTI$Category == "Overdispersed")] <- 4
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Random" & Overall_NTI$Category == "Clustered")] <- 5
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Random")] <- 6
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Clustered" & Overall_NTI$Category == "Random")] <- 7
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Overdispersed" & Overall_NTI$Category == "Clustered")] <- 8
Rosid_NTI$Pattern_match[which(Rosid_NTI$Category == "Clustered" & Overall_NTI$Category == "Overdispersed")] <- 9
Rosid_NTI$Pattern_match[which(is.na(Rosid_NTI$Category) | is.na(Overall_NTI$Category))] <- 10

#Add column for family
Aster_NTI$Family <- "Asteraceae"
Cyper_NTI$Family <- "Cyperaceae"
Eric_NTI$Family <- "Ericaceae"
Fab_NTI$Family <- "Fabaceae"
Fag_NTI$Family <- "Fagaceae"
Po_NTI$Family <- "Poaceae"
Rosid_NTI$Family <- "Rosids"

#Get all families into single dataset
Family_NTI <- rbind(Aster_NTI, Cyper_NTI, Eric_NTI, Fab_NTI, Fag_NTI, Po_NTI, Rosid_NTI)

#Get species richness for families and communities
totals_for_families <- as.data.frame(cbind(Family = Family_NTI$Family[which(Family_NTI$Community == "Taxa")], SR = as.numeric(Family_NTI$SR[which(Family_NTI$Community == "Taxa")])), stringsAsFactors = FALSE)
totals_for_communities <- as.data.frame(cbind(Community = Overall_NTI$Community, SR = as.numeric(Overall_NTI$SR)), stringsAsFactors = FALSE) 
totals_for_communities$SR <- as.numeric(totals_for_communities$SR )
totals_for_families$SR <- as.numeric(totals_for_families$SR )

#Using for loop, add column of species richness for each value in the dataset
for (i in 1:nrow(Aster_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Aster_NTI$Community[i])]
  Aster_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Asteraceae")]
  Aster_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Cyper_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Cyper_NTI$Community[i])]
  Cyper_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Cyperaceae")]
  Cyper_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Eric_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Eric_NTI$Community[i])]
  Eric_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Ericaceae")]
  Eric_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fab_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fab_NTI$Community[i])]
  Fab_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fabaceae")]
  Fab_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fag_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fag_NTI$Community[i])]
  Fag_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fagaceae")]
  Fag_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Po_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Po_NTI$Community[i])]
  Po_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Poaceae")]
  Po_NTI$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Rosid_NTI)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Rosid_NTI$Community[i])]
  Rosid_NTI$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Rosids")]
  Rosid_NTI$Fam_SR[i] <- SR_fam
}

#Get all families into single dataset
Family_NTI <- rbind(Aster_NTI, Cyper_NTI, Eric_NTI, Fab_NTI, Fag_NTI, Po_NTI, Rosid_NTI)

#Sort data by community species richness, community and family species richness
sorted_Family_NTI <- Family_NTI %>% 
  arrange(Comm_SR, Community, Fam_SR)

#Make character columns into factors that are ordered
sorted_Family_NTI$Community <- factor(sorted_Family_NTI$Community, levels = unique(sorted_Family_NTI$Community), ordered = TRUE)
sorted_Family_NTI$Family <- factor(sorted_Family_NTI$Family, levels = unique(sorted_Family_NTI$Family), ordered = TRUE)

#Plot grids sorted by species richness in total community and in total family
# pdf("./Plots/2017_botany/Paper_plots/Question6/NTI_family_v_overall_boxes.pdf")
# ggplot(sorted_Family_NTI, aes(x = Family, y = Community, fill = Comparison)) +
#   geom_raster()
# dev.off()

####PD
#Add column for significant conclusions - need to do for other indices
Aster_PD$Category <- ""
Aster_PD$Category[which(Aster_PD$pvalue <= 0.05)] <- "Clustered"
Aster_PD$Category[which(Aster_PD$pvalue >= 0.95)] <- "Overdispersed"
Aster_PD$Category[which(Aster_PD$pvalue > 0.05 & Aster_PD$pvalue < 0.95)] <- "Random"
Aster_PD$Category[is.na(Aster_PD$pvalue)] <- NA

Cyper_PD$Category <- ""
Cyper_PD$Category[which(Cyper_PD$pvalue <= 0.05)] <- "Clustered"
Cyper_PD$Category[which(Cyper_PD$pvalue >= 0.95)] <- "Overdispersed"
Cyper_PD$Category[which(Cyper_PD$pvalue > 0.05 & Cyper_PD$pvalue < 0.95)] <- "Random"
Cyper_PD$Category[is.na(Cyper_PD$pvalue)] <- NA

Eric_PD$Category <- ""
Eric_PD$Category[which(Eric_PD$pvalue <= 0.05)] <- "Clustered"
Eric_PD$Category[which(Eric_PD$pvalue >= 0.95)] <- "Overdispersed"
Eric_PD$Category[which(Eric_PD$pvalue > 0.05 & Eric_PD$pvalue < 0.95)] <- "Random"
Eric_PD$Category[is.na(Eric_PD$pvalue)] <- NA

Fab_PD$Category <- ""
Fab_PD$Category[which(Fab_PD$pvalue <= 0.05)] <- "Clustered"
Fab_PD$Category[which(Fab_PD$pvalue >= 0.95)] <- "Overdispersed"
Fab_PD$Category[which(Fab_PD$pvalue > 0.05 & Fab_PD$pvalue < 0.95)] <- "Random"
Fab_PD$Category[is.na(Fab_PD$pvalue)] <- NA

Fag_PD$Category <- ""
Fag_PD$Category[which(Fag_PD$pvalue <= 0.05)] <- "Clustered"
Fag_PD$Category[which(Fag_PD$pvalue >= 0.95)] <- "Overdispersed"
Fag_PD$Category[which(Fag_PD$pvalue > 0.05 & Fag_PD$pvalue < 0.95)] <- "Random"
Fag_PD$Category[is.na(Fag_PD$pvalue)] <- NA

Po_PD$Category <- ""
Po_PD$Category[which(Po_PD$pvalue <= 0.05)] <- "Clustered"
Po_PD$Category[which(Po_PD$pvalue >= 0.95)] <- "Overdispersed"
Po_PD$Category[which(Po_PD$pvalue > 0.05 & Po_PD$pvalue < 0.95)] <- "Random"
Po_PD$Category[is.na(Po_PD$pvalue)] <- NA

Rosid_PD$Category <- ""
Rosid_PD$Category[which(Rosid_PD$pvalue <= 0.05)] <- "Clustered"
Rosid_PD$Category[which(Rosid_PD$pvalue >= 0.95)] <- "Overdispersed"
Rosid_PD$Category[which(Rosid_PD$pvalue > 0.05 & Rosid_PD$pvalue < 0.95)] <- "Random"
Rosid_PD$Category[is.na(Rosid_PD$pvalue)] <- NA

Overall_PD$Category <- ""
Overall_PD$Category[which(Overall_PD$pvalue <= 0.05)] <- "Clustered"
Overall_PD$Category[which(Overall_PD$pvalue >= 0.95)] <- "Overdispersed"
Overall_PD$Category[which(Overall_PD$pvalue > 0.05 & Overall_PD$pvalue < 0.95)] <- "Random"
Overall_PD$Category[is.na(Overall_PD$pvalue)] <- NA



#Make column for specific comparisons - significant and nonsignificant mismatches 
Aster_PD$Pattern_match <- ""
Aster_PD$Pattern_match[which(Aster_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Aster_PD$Pattern_match[which(Aster_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Aster_PD$Pattern_match[which(Aster_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Aster_PD$Pattern_match[which(Aster_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Aster_PD$Pattern_match[which(Aster_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Aster_PD$Pattern_match[which(Aster_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Aster_PD$Pattern_match[which(Aster_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Aster_PD$Pattern_match[which(Aster_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Aster_PD$Pattern_match[which(Aster_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Aster_PD$Pattern_match[which(is.na(Aster_PD$Category) | is.na(Overall_PD$Category))] <- 10

Eric_PD$Pattern_match <- ""
Eric_PD$Pattern_match[which(Eric_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Eric_PD$Pattern_match[which(Eric_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Eric_PD$Pattern_match[which(Eric_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Eric_PD$Pattern_match[which(Eric_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Eric_PD$Pattern_match[which(Eric_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Eric_PD$Pattern_match[which(Eric_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Eric_PD$Pattern_match[which(Eric_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Eric_PD$Pattern_match[which(Eric_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Eric_PD$Pattern_match[which(Eric_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Eric_PD$Pattern_match[which(is.na(Eric_PD$Category) | is.na(Overall_PD$Category))] <- 10

Cyper_PD$Pattern_match <- ""
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Cyper_PD$Pattern_match[which(Cyper_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Cyper_PD$Pattern_match[which(is.na(Cyper_PD$Category) | is.na(Overall_PD$Category))] <- 10

Fab_PD$Pattern_match <- ""
Fab_PD$Pattern_match[which(Fab_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Fab_PD$Pattern_match[which(Fab_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Fab_PD$Pattern_match[which(Fab_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Fab_PD$Pattern_match[which(Fab_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Fab_PD$Pattern_match[which(Fab_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Fab_PD$Pattern_match[which(Fab_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Fab_PD$Pattern_match[which(Fab_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Fab_PD$Pattern_match[which(Fab_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Fab_PD$Pattern_match[which(Fab_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Fab_PD$Pattern_match[which(is.na(Fab_PD$Category) | is.na(Overall_PD$Category))] <- 10

Fag_PD$Pattern_match <- ""
Fag_PD$Pattern_match[which(Fag_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Fag_PD$Pattern_match[which(Fag_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Fag_PD$Pattern_match[which(Fag_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Fag_PD$Pattern_match[which(Fag_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Fag_PD$Pattern_match[which(Fag_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Fag_PD$Pattern_match[which(Fag_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Fag_PD$Pattern_match[which(Fag_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Fag_PD$Pattern_match[which(Fag_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Fag_PD$Pattern_match[which(Fag_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Fag_PD$Pattern_match[which(is.na(Fag_PD$Category) | is.na(Overall_PD$Category))] <- 10

Po_PD$Pattern_match <- ""
Po_PD$Pattern_match[which(Po_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Po_PD$Pattern_match[which(Po_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Po_PD$Pattern_match[which(Po_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Po_PD$Pattern_match[which(Po_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Po_PD$Pattern_match[which(Po_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Po_PD$Pattern_match[which(Po_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Po_PD$Pattern_match[which(Po_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Po_PD$Pattern_match[which(Po_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Po_PD$Pattern_match[which(Po_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Po_PD$Pattern_match[which(is.na(Po_PD$Category) | is.na(Overall_PD$Category))] <- 10

Rosid_PD$Pattern_match <- ""
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
Rosid_PD$Pattern_match[which(is.na(Rosid_PD$Category) | is.na(Overall_PD$Category))] <- 10

#Make column for same as overall or not - compare family value with overall to get binary value (or NA)
Aster_PD$Comparison <- ""
Aster_PD$Comparison[which(Aster_PD$Category == Overall_PD$Category)] <- 1
Aster_PD$Comparison[which(Aster_PD$Category != Overall_PD$Category)] <- 0
Aster_PD$Comparison[is.na(Aster_PD$Category)] <- NA

Eric_PD$Comparison <- ""
Eric_PD$Comparison[which(Eric_PD$Category == Overall_PD$Category)] <- 1
Eric_PD$Comparison[which(Eric_PD$Category != Overall_PD$Category)] <- 0
Eric_PD$Comparison[is.na(Eric_PD$Category)] <- NA

Cyper_PD$Comparison <- ""
Cyper_PD$Comparison[which(Cyper_PD$Category == Overall_PD$Category)] <- 1
Cyper_PD$Comparison[which(Cyper_PD$Category != Overall_PD$Category)] <- 0
Cyper_PD$Comparison[is.na(Cyper_PD$Category)] <- NA

Fab_PD$Comparison <- ""
Fab_PD$Comparison[which(Fab_PD$Category == Overall_PD$Category)] <- 1
Fab_PD$Comparison[which(Fab_PD$Category != Overall_PD$Category)] <- 0
Fab_PD$Comparison[is.na(Fab_PD$Category)] <- NA

Fag_PD$Comparison <- ""
Fag_PD$Comparison[which(Fag_PD$Category == Overall_PD$Category)] <- 1
Fag_PD$Comparison[which(Fag_PD$Category != Overall_PD$Category)] <- 0
Fag_PD$Comparison[is.na(Fag_PD$Category)] <- NA

Po_PD$Comparison <- ""
Po_PD$Comparison[which(Po_PD$Category == Overall_PD$Category)] <- 1
Po_PD$Comparison[which(Po_PD$Category != Overall_PD$Category)] <- 0
Po_PD$Comparison[is.na(Po_PD$Category)] <- NA

Rosid_PD$Comparison <- ""
Rosid_PD$Comparison[which(Rosid_PD$Category == Overall_PD$Category)] <- 1
Rosid_PD$Comparison[which(Rosid_PD$Category != Overall_PD$Category)] <- 0
Rosid_PD$Comparison[is.na(Rosid_PD$Category)] <- NA

#Add column for family
Aster_PD$Family <- "Asteraceae"
Cyper_PD$Family <- "Cyperaceae"
Eric_PD$Family <- "Ericaceae"
Fab_PD$Family <- "Fabaceae"
Fag_PD$Family <- "Fagaceae"
Po_PD$Family <- "Poaceae"
Rosid_PD$Family <- "Rosids"

#Get all families into single dataset
Family_PD <- rbind(Aster_PD, Cyper_PD, Eric_PD, Fab_PD, Fag_PD, Po_PD, Rosid_PD)

#Get species richness for families and communities
totals_for_families <- as.data.frame(cbind(Family = Family_PD$Family[which(Family_PD$Community == "Taxa")], SR = as.numeric(Family_PD$SR[which(Family_PD$Community == "Taxa")])), stringsAsFactors = FALSE)
totals_for_communities <- as.data.frame(cbind(Community = Overall_PD$Community, SR = as.numeric(Overall_PD$SR)), stringsAsFactors = FALSE) 
totals_for_communities$SR <- as.numeric(totals_for_communities$SR )
totals_for_families$SR <- as.numeric(totals_for_families$SR )

#Using for loop, add column of species richness for each value in the dataset
for (i in 1:nrow(Aster_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Aster_PD$Community[i])]
  Aster_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Asteraceae")]
  Aster_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Cyper_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Cyper_PD$Community[i])]
  Cyper_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Cyperaceae")]
  Cyper_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Eric_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Eric_PD$Community[i])]
  Eric_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Ericaceae")]
  Eric_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fab_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fab_PD$Community[i])]
  Fab_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fabaceae")]
  Fab_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Fag_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Fag_PD$Community[i])]
  Fag_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Fagaceae")]
  Fag_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Po_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Po_PD$Community[i])]
  Po_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Poaceae")]
  Po_PD$Fam_SR[i] <- SR_fam
}
for (i in 1:nrow(Rosid_PD)){
  SR_comm <- totals_for_communities$SR[which(totals_for_communities$Community == Rosid_PD$Community[i])]
  Rosid_PD$Comm_SR[i] <- SR_comm
  SR_fam <- totals_for_families$SR[which(totals_for_families$Family == "Rosids")]
  Rosid_PD$Fam_SR[i] <- SR_fam
}

#Get all families into single dataset
Family_PD <- rbind(Aster_PD, Cyper_PD, Eric_PD, Fab_PD, Fag_PD, Po_PD, Rosid_PD)

#Sort data by community species richness, community and family species richness
sorted_Family_PD <- Family_PD %>% 
  arrange(Comm_SR, Community, Fam_SR)

#Make character columns into factors that are ordered
sorted_Family_PD$Community <- factor(sorted_Family_PD$Community, levels = unique(sorted_Family_PD$Community), ordered = TRUE)
sorted_Family_PD$Family <- factor(sorted_Family_PD$Family, levels = unique(sorted_Family_PD$Family), ordered = TRUE)

# #Plot grids sorted by species richness in total community and in total family
# pdf("./Plots/2017_botany/Paper_plots/Question6/PD_family_v_overall_boxes.pdf")
# ggplot(sorted_Family_PD, aes(x = Family, y = Community, fill = Comparison)) +
#   geom_raster()
# dev.off()


#Plot species richness vs community richness and plot richness of family richness
sorted_Family_NRI$SR <- as.numeric(sorted_Family_NRI$SR)
sorted_Family_NTI$SR <- as.numeric(sorted_Family_NTI$SR)
sorted_Family_PD$SR <- as.numeric(sorted_Family_PD$SR)

sorted_Family_NRI$Comparison <- as.numeric(sorted_Family_NRI$Comparison)
sorted_Family_NTI$Comparison <- as.numeric(sorted_Family_NTI$Comparison)
sorted_Family_PD$Comparison <- as.numeric(sorted_Family_PD$Comparison)

sorted_Family_NRI$Pattern_match <- as.numeric(sorted_Family_NRI$Pattern_match)
sorted_Family_NTI$Pattern_match <- as.numeric(sorted_Family_NTI$Pattern_match)
sorted_Family_PD$Pattern_match <- as.numeric(sorted_Family_PD$Pattern_match)

# #skip this part to avoid having match become nas later when converted to numeric
# for (i in 1:nrow(sorted_Family_NRI)){
#   if (sorted_Family_NRI$Comparison[i] == 1 & !is.na(sorted_Family_NRI$Comparison[i])){
#     sorted_Family_NRI$Comparison[i] <- "Match"
#   } 
# }
# 
# for (i in 1:nrow(sorted_Family_NTI)){
#   if (sorted_Family_NTI$Comparison[i] == 1 & !is.na(sorted_Family_NTI$Comparison[i])){
#     sorted_Family_NTI$Comparison[i] <- "Match"
#   } 
# }
# 
# for (i in 1:nrow(sorted_Family_PD)){
#   if (sorted_Family_PD$Comparison[i] == 1 & !is.na(sorted_Family_PD$Comparison[i])){
#     sorted_Family_PD$Comparison[i] <- "Match"
#   } 
# }

#But need to run this for comparison, but change values for pattern match 

#Change value of 1 in comparison to value of 20 with title "match" - set to factor
sorted_Family_NRI$Comparison <- factor(sorted_Family_NRI$Comparison, levels=c("1", "0"), labels=c("Match", "Mismatch"))

sorted_Family_NRI$Pattern_class <- ""
sorted_Family_NRI$Pattern_class[which(sorted_Family_NRI$Pattern_match == 1)] <- "Nonsignificant Match"
sorted_Family_NRI$Pattern_class[which(sorted_Family_NRI$Pattern_match == 2 | sorted_Family_NRI$Pattern_match == 3)] <- "Significant Match"
sorted_Family_NRI$Pattern_class[which(sorted_Family_NRI$Pattern_match == 4 | sorted_Family_NRI$Pattern_match == 5 | sorted_Family_NRI$Pattern_match == 6 | sorted_Family_NRI$Pattern_match == 7)] <- "Nonsignificant Mismatch"
sorted_Family_NRI$Pattern_class[which(sorted_Family_NRI$Pattern_match == 8 | sorted_Family_NRI$Pattern_match == 9 )] <- "Significant Mismatch"
sorted_Family_NRI$Pattern_class[which(sorted_Family_NRI$Pattern_match == 10)] <- "NA"
tail(sorted_Family_NRI)
sorted_Family_NRI$Pattern_class <- as.factor(sorted_Family_NRI$Pattern_class)
levels(sorted_Family_NRI$Pattern_class)

# 
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Random")] <- 1
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Clustered")] <- 2
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Overdispersed")] <- 3
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Overdispersed")] <- 4
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Random" & Overall_PD$Category == "Clustered")] <- 5
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Random")] <- 6
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Random")] <- 7
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Overdispersed" & Overall_PD$Category == "Clustered")] <- 8
# Rosid_PD$Pattern_match[which(Rosid_PD$Category == "Clustered" & Overall_PD$Category == "Overdispersed")] <- 9
# Rosid_PD$Pattern_match[which(is.na(Rosid_PD$Category) | is.na(Overall_PD$Category))] <- 10

sorted_Family_NRI

sorted_Family_NRI

str(sorted_Family_NRI)

sorted_Family_NRI_melted <- melt(sorted_Family_NRI, id = c("Community", "NRI", "pvalue", "Category", "Pattern_match", "Family", "Comm_SR", "Fam_SR", "Pattern_class"))
sorted_Family_NRI_melted$value <- as.numeric(sorted_Family_NRI_melted$value)
sorted_Family_NRI_melted



# sorted_Family_NTI_melted <- melt(sorted_Family_NTI, id = c("Community", "NTI", "pvalue", "Category", "Pattern_match", "Family", "Comm_SR", "Fam_SR", "Pattern_class"))
# sorted_Family_NTI_melted$value <- as.numeric(sorted_Family_NTI_melted$value)
# 
# sorted_Family_PD_melted <- melt(sorted_Family_PD, id = c("Community", "PD", "pvalue", "Category", "Pattern_match", "Family", "Comm_SR", "Fam_SR","Comparison", "Pattern_class"))
# sorted_Family_PD_melted$value <- as.numeric(sorted_Family_PD_melted$value)
# 
#Get colours for plotting
mygrey <- "#ECF0F1"
myred <- "#E74C3C"
myblue <- "#3498DB"
mydark <- "#2C3E50"
myotherblue <- "#2980B9"
mymidgrey <- "#AAB1B9"
mylightred <- "#F0938A"
mylightblue <- "#94BFDC"

pdf("./Plots/2017_botany/Paper_plots/Question6/NRI_Comm_v_family_SR_both.pdf")
ggplot(sorted_Family_NRI_melted, aes(x = Family, y = Community)) +
  geom_raster(aes(fill = factor(value)))+
  facet_wrap(~ variable, labeller = labeller(yfacet = c(`0` = "an y label", `1` = "another y label"),
    xfacet = c(`10` = "an x label", `20` = "another x label")))+
  scale_colour_manual()
dev.off()

#Plot separately rather than in facet_wrap to get two legends
comm_names <- sorted_Family_NRI$Community[which(sorted_Family_NRI$Family == "Rosids")]
levels(comm_names) <- gsub(".", " ", levels(comm_names), fixed = TRUE)
levels(comm_names)

levels(comm_names) <- gsub("Taxa", "Ordway Swisher", levels(comm_names))


levels(comm_names) <- gsub("Abandoned Field Pasture", "Abandoned Field\n Pasture", levels(comm_names))
levels(comm_names) <- gsub("Successional Hardwood Forest", "Successional\nHardwood Forest", levels(comm_names))
levels(comm_names)

#Remove OS from figure - why?
sorted_Family_NRI[,c(1,2,4,6:9)]
sorted_Family_NRI_red <- sorted_Family_NRI[-which(sorted_Family_NRI$Community == "Taxa"),]

sorted_Family_NRI_red

#Replace NA with NA
sorted_Family_NRI_red$Pattern_class[which(sorted_Family_NRI_red$Pattern_class == "NA")] <- NA

pdf("./Plots/2017_botany/Paper_plots/Resubmission/Question3/NRI_familyvsoverall_boxes_coloured_signif_clades.pdf")
p1 <- ggplot(sorted_Family_NRI_red, aes(x = Family, y = Community, fill = Pattern_class)) +
  geom_tile(color = rgb(44,62,80, maxColorValue = 255))+
  labs(x = "Clade")+
  theme_bw()+
  theme(text = element_text(size=5))+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(-0.5, -0.15), 
        legend.key.size = unit(0.25, "line"), legend.justification = "left", 
        legend.margin=margin(t = -0.25,  r = 0, b = 0, l = 0, unit = "cm"))+
  scale_fill_manual(values = c(mylightblue, mylightred, myotherblue, myred), na.value= mymidgrey)+
  guides(fill=guide_legend("All Taxa vs Clade"))+
  theme(legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8, face="bold"))+
  theme(panel.border = element_blank())+
  scale_y_discrete(labels=comm_names)


ggsave("./Plots/2017_botany/Paper_plots/Resubmission/Question3/NRI_familyvsoverall_boxes_coloured_signif_clades_final_plot.pdf", p1, dpi = 1000, height = 80, width = 80, units = "mm")
# ggplot(sorted_Family_NRI_red, aes(x = Family, y = Community, fill = Pattern_match, order = -as.numeric(Pattern_match))) +
#   geom_tile(color = rgb(44,62,80, maxColorValue = 255))+
#   labs(title = "Comparison", x = "Taxonomic Group")+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   scale_fill_manual(values = c(rgb(41,128,185, maxColorValue = 255), rgb(231,76,60, maxColorValue = 255)), na.value= rgb(44,62,80, maxColorValue = 255))+
#   guides(fill=guide_legend("All Taxa vs\nTaxonomic Group"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(panel.border = element_blank())+
#   scale_y_discrete(labels=comm_names)
dev.off()



pdf("./Plots/2017_botany/Paper_plots/Question6/SR_familyvsoverall_boxes_title.pdf")
ggplot(sorted_Family_NRI, aes(x = Family, y = Community, fill = SR)) +
  geom_tile(color = rgb(44,62,80, maxColorValue = 255))+
  labs(title = "Species Richness", x = "Taxonomic Group")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  #scale_fill_continuous(low = rgb(41,128,185, maxColorValue = 255), high = rgb(231,76,60, maxColorValue = 255))+
  #scale_fill_manual(values = c(rgb(41,128,185, maxColorValue = 255), rgb(231,76,60, maxColorValue = 255)), na.value= rgb(44,62,80, maxColorValue = 255))+
  #guides(fill=guide_colorbar("Species Richness", nbin = 20))+
  scale_fill_gradient2(midpoint = 50, low = rgb(41,128,185, maxColorValue = 255), mid = rgb(236, 240, 241, maxColorValue = 255), high = rgb(231,76,60, maxColorValue = 255), breaks = c(0, 30, 60, 90, 120, 150), guide = guide_colorbar("Species Richness", nbin = 20, draw.ulim = FALSE, draw.llim = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(panel.border = element_blank())+
  scale_y_discrete(labels=comm_names)
dev.off()





################

pdf("./Plots/2017_botany/Paper_plots/Question6/NRI_family_v_overall_boxes_title.pdf")
ggplot(sorted_Family_NRI, aes(x = Family, y = Community, fill = SR)) +
  geom_raster()+
  labs(plot.title = element_text("Species Richness"))+
  theme(plot.background = element_rect(colour = NULL))
dev.off()



pdf("./Plots/2017_botany/Paper_plots/Question6/NTI_Comm_v_family_SR_both.pdf")
ggplot(sorted_Family_NTI_melted, aes(x = Family, y = Community)) +
  geom_raster(aes(fill = factor(value)))+
  facet_wrap(~ variable)+
  scale_colour_manual()
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question6/PD_Comm_v_family_SR_both.pdf")
ggplot(sorted_Family_PD_melted, aes(x = Family, y = Community)) +
  geom_raster(aes(fill = factor(value)))+
  facet_wrap(~ variable)+
  scale_colour_manual()
dev.off()




pdf("./Plots/2017_botany/Paper_plots/Question6/NTI_Comm_v_family_SR.pdf")
ggplot(sorted_Family_NTI, aes(x = Family, y = Community, fill = factor(SR))) +
  geom_raster()+
  geom_text(aes(label = SR))
dev.off()

pdf("./Plots/2017_botany/Paper_plots/Question6/PD_Comm_v_family_SR.pdf")
ggplot(sorted_Family_PD, aes(x = Family, y = Community, fill = factor(SR))) +
  geom_raster()+
  geom_text(aes(label = SR))
dev.off()
