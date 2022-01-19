#!/bin/bash
#SBATCH
#SBATCH --time=20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=60000
#SBATCH --job-name=meriam_prob-model-run
#SBATCH --mail-type=ALL
#SBATCH --mail-user=s.van.der.pol@rug.nl
#SBATCH --output=job-%j.log
#SBATCH --partition=regular

module load R/4.1.0-foss-2021a

Rscript model_runs_scripts/crp_nl.R