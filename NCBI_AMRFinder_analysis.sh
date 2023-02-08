#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --partition=medpri
#SBATCH --job-name=RGI_WGS_sequences
#SBATCH --array=1-167%100
echo $SLURM_ARRAY_TASK_ID
echo $SLURM_JOB_ID
module load apps/anaconda3
source ~/miniconda3/bin/activate
cd /mnt/scratch2/users/40309916
#file_list=$(ls *.fna | sed -n ${SLURM_ARRAY_TASK_ID}p)
for file_list in `ls *.fna | sed -n $(expr $(expr ${SLURM_ARRAY_TASK_ID} \* 100) - 99),$(expr ${SLURM_ARRAY_TASK_ID} \* 100)p`;
do amrfinder -n $file_list > AMRFinderResults_$file_list.txt;
done
