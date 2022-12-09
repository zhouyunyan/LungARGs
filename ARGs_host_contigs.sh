#!/bin/bash

# Kill script if any commands fail
set -e
echo "Job Start at `date`"

##############host of ARG based on contigs###########################

conda activate metawrap
 
export PATH=~/Softs/metaWRAP/bin/:$PATH
kraken2 ARG_contig_1209.fa \
--db ~/KRAKEN2/ \
--threads 8 \
--use-names --use-mpa-style --report-zero-counts \
--output ARG_contig_1209.Kraken2.txt \
--report ARG_contig_1209.Kraken2.report

#get time end the job
echo "Job finished at:" `date`