#!/bin/bash

# Kill script if any commands fail
set -e
echo "Job Start at `date`"

##############ARGs annotation###########################

conda activate ~/miniconda3/envs/rgi/
#bulid local database
rgi load --card_json /hl/zhouyunyan/Softwares/rgi-5.2.0/card_data/card.json --local
rgi card_annotation -i /hl/zhouyunyan/Softwares/rgi-5.2.0/card_data/card.json > card_annotation.log 2>&1
rgi main \
-i  total.protein.faa.filter.deal.90\
-o catalog90.CARD.anno.txt \
-t protein -a DIAMOND -n 8 --clean \
--local 

##############MGEs annotation###########################

#convert nucleic acid sequence to amino acid sequence
perl ./cds2aa.pl -check  MGEs_FINAL_99perc_trim.fasta >MGEs_FINAL_99perc_trim.check.protein.fasta

#build index
diamond makedb --in MGEs_FINAL_99perc_trim.protein.fasta -d MGE -p 8

#blastp
diamond blastp \
-q  total.protein.faa.filter.90\
-d /hl/zhouyunyan/Databases/MGE/MobileGeneticElementDatabase/MGE.dmnd \
-p 12 -e 1e-5 -k 1 --id 80 --query-cover 80 --sensitive \
-o All_gene_MGE.besthit.dmnd.txt


##############VFGs annotation###########################

#build index
makeblastdb -in  VFDB_setB_pro.fas   -parse_seqids -hash_index -dbtype prot -out  VFDB

#blastp
blastp -query total.protein.faa.filter.90\ 
-db ~/db/VFDB/VFDB \
-out PRGC90.vfdb.tab \
-evalue 1e-5 --id 80 -outfmt 6 -num_threads 16 -num_alignments 5

#extract best hit 
python  blast_best.py  PRGC90.vfdb.tab   PRGC90.vfdb.best.tab

#get time end the job
echo "Job finished at:" `date`