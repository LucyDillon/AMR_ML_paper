#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --partition=medpri
#SBATCH --job-name=gene_count_RGI
#SBATCH --array=1-167%100
echo $SLURM_ARRAY_TASK_ID
echo $SLURM_JOB_ID
cd /mnt/scratch2/users/40309916/resfinder


for i in `ls *.fna | sed -n $(expr $(expr ${SLURM_ARRAY_TASK_ID} \* 100) - 99),$(expr ${SLURM_ARRAY_TASK_ID} \* 100)p`;
do python3 run_resfinder.py -o /mnt/scratch2/users/40309916/resfinder/Resfinder_output/$i -l 0.6 -t 0.8 --acquired -ifa $i; done
