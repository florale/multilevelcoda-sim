#!/bin/bash
#SBATCH --job-name=testmodel
#SBATCH --time=2-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10GB
#SBATCH --cpus-per-task=10
#SBATCH --partition=comp

cd /fs04/ft29/multilevelcoda

module load R/4.2-mkl

Rscript testmodel.R

echo 'Completed by' $USER