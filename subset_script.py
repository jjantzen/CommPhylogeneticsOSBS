import argparse
from Bio import SeqIO
from Bio.SeqRecord import SeqRecord
from random import sample

output_handle = open("subset5.fasta", "w")

with open("Combined_01202016_community.fas") as f:
    seqs = SeqIO.parse(f,"fasta")
    samps = ((seq.name, seq.seq) for seq in  sample(list(seqs),300))
    for samp in samps:
#        print(">{}\n{}".format(*samp))
           
        output_handle.write(">{}\n{}\n".format(*samp))
output_handle.close()

