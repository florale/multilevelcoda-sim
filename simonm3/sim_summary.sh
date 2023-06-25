#!/bin/bash
#SBATCH --job-name=sim_summary
#SBATCH --time=2-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --cpus-per-task=2
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript sim_summary.R

echo 'Completed by' $USER