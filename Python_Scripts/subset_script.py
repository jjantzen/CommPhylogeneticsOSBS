#Did I actually use this to generate subsamples?
#Not the original script, but should work with modifications

#import libraries
import argparse
from Bio import SeqIO
from Bio.SeqRecord import SeqRecord
from random import sample

#create output handle
output_handle = open("subset5.fasta", "w")

#parse input fastas file to get seqs
with open("Combined_01202016_community.fas") as f:
    seqs = SeqIO.parse(f,"fasta")
    #from all seqs, get random 300
    samps = ((seq.name, seq.seq) for seq in  sample(list(seqs),300))
    for samp in samps:
#        print(">{}\n{}".format(*samp))
           
        output_handle.write(">{}\n{}\n".format(*samp))
output_handle.close()

