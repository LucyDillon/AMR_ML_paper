#!/bin/bash

#SBATCH --time=24:00:00
#SBATCH --partition=medpri
#SBATCH --job-name=WGS_sequences_gff
#SBATCH --mail-user=ldillon05@qub.ac.uk
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
cd /mnt/scratch2/users/40309916
for i in `cat genomes.txt`; do wget -qN "ftp://ftp.patricbrc.org/genomes/$i/$i.*gff";
done
