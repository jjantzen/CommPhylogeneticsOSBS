#Python script for randomly subsetting fasta file
#Run for each subset number
#Does 100 replicates of random taxon selection

#import libraries
import argparse
from Bio import SeqIO
from Bio.SeqRecord import SeqRecord
from random import sample

#Run for each number of taxa in subset (here is example for 300 taxa in subset)
for number in xrange(1,101):
output_handle = open("subset"+str(number)+".fasta", "w")
with open("572taxa.fas") as f:
	seqs = SeqIO.parse(f, "fasta")
	samps = ((seq.name, seq.seq) for seq in sample(list(seqs),300))
	for samp in samps:
		output_handle.write(">{}\n{}\n".format(*samp))
output_handle.close()
