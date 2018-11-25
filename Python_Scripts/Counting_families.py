#! /usr/bin/env python

#Open file, parse input, reorganize bits to get only part of the input, sort it, count by one variable, then write to a new file

import pandas

#Assign names to infile and outfile
InFileName = 'Names_short.csv'
OutFile1Name = 'Name_output.csv'
OutFile2Name = 'Family_output.csv'
#Open infile and try to open outfile. If outfile fails, print error code
InFile = open(InFileName, 'r')
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
#Set LineNumber to zero to allow for for loops to work and terminate at the right time
LineNumber = 0
#Define lists
Family_to_order = []
Family_list = []
#For loop to read through all the lines in infile
#For each line in the file
for Line in InFile:
#If the line number is greater than zero
  if LineNumber > 0:
 #Remove the line end character
   Line = Line.strip('\n').strip('\r')
   #Split the line by commas
   Name_columns = Line.split(',')
   #Set object Name_bits to the first column separated by underscores to get the individual parts of name
   Name_bits = Name_columns[0].split('_')
   #To take different parts of the name, change the number of column in the []
   #Here, we are taking column 3(genus) and column 4 (species) and joining them, separated by _
   Name_output = [(Name_bits[2] + "_" + Name_bits[3])]
#Set family to the 2nd column in the list
   Family = Name_bits[1]
 #Family output is the combination of Family object and name output (genus_species) separted by comma
   Family_output = Family + "," + (Name_output[0])
   #Add Familyoutput object to list (happens with each iteration to add to the list)
   Family_to_order.append(Family_output)
   #Add family to the list of families (happens with each iteration)
   Family_list.append(Family)
    #Add one to line number to proceed to next line - necessary to skip first line (header)
  LineNumber = LineNumber + 1

#Sort the list of family to order (sorts by family since it's first)
sorted_by_family = sorted(Family_to_order) 
#Count families in family_list 
wordfreq = [Family_list.count(w) for w in Family_list]
#Combine the family list and the count for the family into list
pairs = (list(zip(Family_list, wordfreq)))
#Take unique values for family and counts to remove duplicate counts
unique_counts = set(pairs)
#For each line in the sorted by family object (including genus and species)
for line in sorted_by_family:
#Write to file and add line end symbol
  OutFile1.write("%s\n" % line)
#For each line in unique counts
for line in unique_counts:
  #Separate tuple line into two components
  Family, Count = (line)
  print (Family)
  print (Count)
#Write Family and Count to outfile2, separated by a comma
  OutFile2.write("%s,%d\n" % (Family, Count))
#Close infile and outfile
InFile.close()
OutFile1.close()
