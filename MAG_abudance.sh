#!/bin/bash

# Kill script if any commands fail
set -e
echo "Job Start at `date`"

##############The relative abundance of MAGs###########################


conda activate coverm

INPUT=~/cleandata
genomes=~/genomes

coverm genome -1 ${INPUT}/*1.fastq -2 ${INPUT}/*2.fastq \
-d ${genomes}/ -m relative_abundance -o coverm_reltative.tab -t 36 -x fa

#get time end the job
echo "Job finished at:" `date`