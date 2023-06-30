#!/bin/bash
#SBATCH --job-name=simmodel_11
#SBATCH --time=3-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=5GB
#SBATCH --cpus-per-task=20
#SBATCH --partition=comp

cd /fs04/ft29/simonm3

module load R/4.2.2-mkl

Rscript simmodel_11.R

echo 'Completed by' $USER