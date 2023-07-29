#!/bin/bash
#SBATCH --job-name=simsum_brmcoda_16
#SBATCH --time=0-1:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=50GB
#SBATCH --cpus-per-task=1
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript simsum_brmcoda_16.R

echo 'Completed by' $USER