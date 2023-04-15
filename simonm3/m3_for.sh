#!/bin/bash
#SBATCH --job-name=m3_for
#SBATCH --time=1-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=20GB
#SBATCH --cpus-per-task=5
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript m3_for.R

echo 'Completed by' $USER