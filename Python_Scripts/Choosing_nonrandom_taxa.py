#I am skeptical that I actually used this script to generate the lists of non-random taxa
#I think I used an R script instead


#! /usr/bin/env python

#import libraries
import argparse
from Bio import SeqIO
from Bio.SeqRecord import SeqRecord
from random import sample
import pandas

#old version?
#for number in xrange(1,101):
#	output_handle = open("subset"+str(number)+".fasta", "w") 
#	with open("572taxa.fas") as f:
#		seqs = SeqIO.parse(f, "fasta")
#		samps = ((seq.name, seq.seq) for seq in sample(list(seqs),300))
#		for samp in samps:
#			output_handle.write(">{}\n{}\n".format(*samp))
#	output_handle.close()

#for each file in the folder, print name of file
for file in *
	print "This is", file.name $file

#Assign names to infiles and outfiles
#Describe format of files
InFileName = 'Family_output.csv'
InFile2Name = 'Names_short.csv'
OutFile1Name = '100_taxa.csv'
OutFile2Name = '200_taxa.csv'
OutFile3Name = '300_taxa.csv'
OutFile4Name = '400_taxa.csv'
OutFile5Name = '500_taxa.csv'

#Open infile and try to open outfile. If outfile fails, print error code
InFile = open(InFileName, 'r')
InFile2 = open(InFile2Name, 'r')
try:
  OutFile1 = open(OutFile1Name, 'w')
  #print OutFileName
except:
  print "Could not open file" 
try:
  OutFile2 = open(OutFile2Name, 'w')
  #print OutFileName
except:
  print "Could not open file" 
try:
  OutFile3 = open(OutFile3Name, 'w')
  #print OutFileName
except:
  print "Could not open file" 
try:
  OutFile4 = open(OutFile4Name, 'w')
  #print OutFileName
except:
  print "Could not open file" 
try:
  OutFile5 = open(OutFile5Name, 'w')
  #print OutFileName
except:
  print "Could not open file" 
  
#Set LineNumber to zero to allow for loops to work and terminate at the right time
LineNumber = 0

#Define lists
Family_number = []
Family_list = []
Taxa_list = []

#For loop to read through all the lines in family and number of samples per family
#For each line in the file
for Line in InFile:
#If the line number is greater than zero
	if LineNumber > 0:
		#Remove the line end character
		Line = Line.strip('\n').strip('\r')
		#Split the line by commas
		Family_parts = Line.split(',')
		#Set object Family to the first column separated by underscores to get the Family name
		Family = Family_parts[0]
		#To take second part of line to get number of taxa for each family
		Number_of_taxa_100 = [(Family_parts[2])]
		Family_list.append(Family)
		Family_number.append(Family_parts)
	#Add one to line number to proceed to next line - necessary to skip first line (header)
	LineNumber = LineNumber + 1
	
#For loop to read list of taxa and choose random subset for each family for each subset number
for Line in InFile2:
#If the line number is greater than zero
	if LineNumber > 0:
		#Remove the line end character
		Line = Line.strip('\n').strip('\r')
		#Split the line by commas
		Taxa_parts = Line.split(',')
		#Set taxa list and family list
		Taxa = Taxa_parts[1]
		Family_tax = Taxa_parts[0]
		Taxa_list.append(Taxa)
	#Add one to line number to proceed to next line
	LineNumber = LineNumber + 1

print Taxa_list
print Family_number
print Family_list


##Additional maybe useful code
##Sort the list of family to order (sorts by family since it's first)
#sorted_by_family = sorted(Family_to_order) 
##Count families in family_list 
#wordfreq = [Family_list.count(w) for w in Family_list]
##Combine the family list and the count for the family into list
#pairs = (list(zip(Family_list, wordfreq)))
##Take unique values for family and counts to remove duplicate counts
#unique_counts = set(pairs)
##For each line in the sorted by family object (including genus and species)
#for line in sorted_by_family:
##Write to file and add line end symbol
#  OutFile1.write("%s\n" % line)
##For each line in unique counts
#for line in unique_counts:
 ##Separate tuple line into two components
#  Family, Count = (line)
#  print (Family)
#  print (Count)
##Write Family and Count to outfile2, separated by a comma
#  OutFile2.write("%s,%d\n" % (Family, Count))
##Close infile and outfile
#InFile.close()
#OutFile1.close()


