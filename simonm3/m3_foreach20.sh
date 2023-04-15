#!/bin/bash
#SBATCH --job-name=m3_foreach10_ll_brms
#SBATCH --time=1-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10GB
#SBATCH --cpus-per-task=20
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript m3_foreach20_ll_brms.R

echo 'Completed by' $USER