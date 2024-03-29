#!/bin/bash
#SBATCH --job-name=simsum_sub_test
#SBATCH --time=0-0:30:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --cpus-per-task=1
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript simsum_sub.R

echo 'Completed by' $USER