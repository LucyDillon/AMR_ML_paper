#!/bin/sh
#SBATCH --time=7-23:59:00
#SBATCH --partition=bio-compute
#SBATCH --mem=50G
#SBATCH --cpus-per-task=16
#SBATCH --ntasks=1
#SBATCH --job-name=eggnog
#SBATCH --error=diamond_RGI-%A-%a.err
echo $SLURM_JOB_ID

module load apps/anaconda3
module add  diamond/0.9
cd /mnt/scratch2/users/40309916/PATRIC_Genomes/gff_files

diamond blastx -d /mnt/scratch2/igfs-anaconda/conda-envs/eggnog/lib/python3.9/site-packages/data/eggnog_proteins.dmnd -q /mnt/scratch2/users/40309916/PATRIC_Genomes/gff_files/RGI_sequences_diamond_input_final_edited.txt --more-sensitive --threads 2 -e 0.001000 -o /mnt/scratch2/users/40309916/PATRIC_Genomes/gff_files/Diamond_RGI_sequences.txt --top 3 --query-cover 0 --subject-cover 0
