library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)

library(doFuture)
library(foreach)
library(doRNG)
library(parallel)

library(rsimsum) # https://cran.r-project.org/web/packages/rsimsum/vignettes/A-introduction.html
library(heatmaply)
library(ggthemes)
library(bayesplot)

library(hrbrthemes)
library(wesanderson)
library(ggplot2)
library(ggsci)
library(ggpubr)

data.table::setDTthreads(10)

input <- readRDS("input.RDS")
meanscovs <- input$meanscovs
prefit5 <- input$prefit5
prefit4 <- input$prefit4
prefit3 <- input$prefit3

source("input.R") # groundtruth, conditions and functions
source("functions.R") # functions for plots
substutitution_gt <- readRDS("substutitution_gt.RDS")
brmcoda_gt <- readRDS("brmcoda_gt.RDS")

# simsum_sub <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_.RDS")
simsum_sub_1 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_1.RDS")
simsum_sub_2 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_2.RDS")
simsum_sub_3 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_3.RDS")
simsum_sub_4 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_4.RDS")
simsum_sub_5 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_5.RDS")
simsum_sub_6 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_6.RDS")
simsum_sub_7 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_7.RDS")
simsum_sub_8 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_8.RDS")
simsum_sub_9 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_9.RDS")
simsum_sub_10 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_10.RDS")
simsum_sub_11 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_11.RDS")
simsum_sub_12 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_12.RDS")
simsum_sub_13 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_13.RDS")
simsum_sub_14 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_14.RDS")
simsum_sub_15 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_15.RDS")
simsum_sub_16 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_16.RDS")
simsum_sub_17 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_17.RDS")
simsum_sub_18 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_18.RDS")
simsum_sub_19 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_19.RDS")
simsum_sub_20 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_20.RDS")
simsum_sub_21 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_21.RDS")
simsum_sub_22 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_22.RDS")
simsum_sub_23 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_23.RDS")
simsum_sub_24 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub_24.RDS")

## separate by parts and clean -----------------
simsum_sub_d3 <- list()
simsum_sub_d4 <- list()
simsum_sub_d5 <- list()

for (j in c(1:22)) {
  simsum_sub_d3[[j]] <- get(paste0("simsum_sub_", j))[[1]]
  simsum_sub_d4[[j]] <- get(paste0("simsum_sub_", j))[[2]]
  simsum_sub_d5[[j]] <- get(paste0("simsum_sub_", j))[[3]]
}

simsum_sub_d3 <- as.data.table(do.call(rbind, simsum_sub_d3))
simsum_sub_d4 <- as.data.table(do.call(rbind, simsum_sub_d4))
simsum_sub_d5 <- as.data.table(do.call(rbind, simsum_sub_d5))

rm(simsum_sub_1, simsum_sub_2, simsum_sub_3, simsum_sub_4, simsum_sub_5, simsum_sub_6,
   simsum_sub_7, simsum_sub_8, simsum_sub_9, simsum_sub_10, simsum_sub_11, simsum_sub_12,
   simsum_sub_13, simsum_sub_14, simsum_sub_15, simsum_sub_16, simsum_sub_17, simsum_sub_18,
   simsum_sub_19, simsum_sub_20, simsum_sub_21, simsum_sub_22, simsum_sub_23, simsum_sub_24)

# add condition
# 3 parts
simsum_sub_d3[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_sub_d3[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_sub_d3[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_sub_d3[, Level := factor(Level, levels = c("between", "within"))]
simsum_sub_d3[, n_parts := NULL]

simsum_sub_d3[, condition := NA]
simsum_sub_d3[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_sub_d3[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_sub_d3[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_sub_d3[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_sub_d3[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_sub_d3[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

# 4 parts
simsum_sub_d4[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_sub_d4[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_sub_d4[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_sub_d4[, Level := factor(Level, levels = c("between", "within"))]
simsum_sub_d4[, n_parts := NULL]

simsum_sub_d4[, condition := NA]
simsum_sub_d4[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_sub_d4[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_sub_d4[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_sub_d4[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_sub_d4[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_sub_d4[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

# 5 parts
simsum_sub_d5[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_sub_d5[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_sub_d5[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_sub_d5[, Level := factor(Level, levels = c("between", "within"))]
simsum_sub_d5[, n_parts := NULL]

simsum_sub_d5[, condition := NA]
simsum_sub_d5[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_sub_d5[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_sub_d5[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_sub_d5[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_sub_d5[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_sub_d5[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

substutitution_gt_d3 <- substutitution_gt[[1]]
substutitution_gt_d4 <- substutitution_gt[[2]]
substutitution_gt_d5 <- substutitution_gt[[3]]

substutitution_gt_d3[, To := NA]
substutitution_gt_d3[, To := ifelse(Sleep == 30, "Sleep", To)]
substutitution_gt_d3[, To := ifelse(PA == 30, "PA", To)]
substutitution_gt_d3[, To := ifelse(SB == 30, "SB", To)]

substutitution_gt_d3[, From := NA]
substutitution_gt_d3[, From := ifelse(Sleep == -30, "Sleep", From)]
substutitution_gt_d3[, From := ifelse(PA == -30, "PA", From)]
substutitution_gt_d3[, From := ifelse(SB == -30, "SB", From)]

substutitution_gt_d4[, To := NA]
substutitution_gt_d4[, To := ifelse(Sleep == 30, "Sleep", To)]
substutitution_gt_d4[, To := ifelse(MVPA == 30, "MVPA", To)]
substutitution_gt_d4[, To := ifelse(LPA == 30, "LPA", To)]
substutitution_gt_d4[, To := ifelse(SB == 30, "SB", To)]

substutitution_gt_d4[, From := NA]
substutitution_gt_d4[, From := ifelse(Sleep == -30, "Sleep", From)]
substutitution_gt_d4[, From := ifelse(MVPA == -30, "MVPA", From)]
substutitution_gt_d4[, From := ifelse(LPA == -30, "LPA", From)]
substutitution_gt_d4[, From := ifelse(SB == -30, "SB", From)]

substutitution_gt_d5[, To := NA]
substutitution_gt_d5[, To := ifelse(TST == 30, "TST", To)]
substutitution_gt_d5[, To := ifelse(WAKE == 30, "WAKE", To)]
substutitution_gt_d5[, To := ifelse(MVPA == 30, "MVPA", To)]
substutitution_gt_d5[, To := ifelse(LPA == 30, "LPA", To)]
substutitution_gt_d5[, To := ifelse(SB == 30, "SB", To)]

substutitution_gt_d5[, From := NA]
substutitution_gt_d5[, From := ifelse(TST == -30, "TST", From)]
substutitution_gt_d5[, From := ifelse(WAKE == -30, "WAKE", From)]
substutitution_gt_d5[, From := ifelse(MVPA == -30, "MVPA", From)]
substutitution_gt_d5[, From := ifelse(LPA == -30, "LPA", From)]
substutitution_gt_d5[, From := ifelse(SB == -30, "SB", From)]

# set non-convergence/divergent transition/ problematic ESS to NA
colnames(simsum_sub_d3)
colnames(simsum_sub_d4)
colnames(simsum_sub_d5)

estnames <- c("Mean", "CI_low", "CI_high")

simsum_sub_d3 <- simsum_sub_d3[ndt != 0, (estnames) := NA]
simsum_sub_d4 <- simsum_sub_d4[ndt != 0, (estnames) := NA]
simsum_sub_d5 <- simsum_sub_d5[ndt != 0, (estnames) := NA]

## bsub d3 ----------------
s_bsub_sleep_pa_d3 <- simsum(
  simsum_sub_d3[To == "Sleep" & From == "PA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "PA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sleep_pa_d3 <- .get_cov(
  s_bsub_sleep_pa_d3,
  simsum_sub_d3[To == "Sleep" & From == "PA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "PA"]$diff_delta_y_b
)

s_bsub_sleep_sb_d3 <- simsum(
  simsum_sub_d3[To == "Sleep" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sleep_sb_d3 <- .get_cov(
  s_bsub_sleep_sb_d3,
  simsum_sub_d3[To == "Sleep" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "SB"]$diff_delta_y_b
)

s_bsub_pa_sleep_d3 <- simsum(
  simsum_sub_d3[To == "PA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "Sleep"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_pa_sleep_d3 <- .get_cov(
  s_bsub_pa_sleep_d3,
  simsum_sub_d3[To == "PA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "Sleep"]$diff_delta_y_b
)

s_bsub_pa_sb_d3 <- simsum(
  simsum_sub_d3[To == "PA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_pa_sb_d3 <- .get_cov(
  s_bsub_pa_sb_d3,
  simsum_sub_d3[To == "PA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "SB"]$diff_delta_y_b
)

s_bsub_sb_sleep_d3 <- simsum(
  simsum_sub_d3[To == "SB" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "Sleep"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_sleep_d3 <- .get_cov(
  s_bsub_sb_sleep_d3,
  simsum_sub_d3[To == "SB" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "Sleep"]$diff_delta_y_b
)

s_bsub_sb_pa_d3 <- simsum(
  simsum_sub_d3[To == "SB" & From == "PA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "PA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_pa_d3 <- .get_cov(
  s_bsub_sb_pa_d3,
  simsum_sub_d3[To == "SB" & From == "PA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "PA"]$diff_delta_y_b
)

## bsub d4 ----------------
s_bsub_sleep_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sleep_mvpa_d4 <- .get_cov(
  s_bsub_sleep_mvpa_d4,
  simsum_sub_d4[To == "Sleep" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_sleep_lpa_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sleep_lpa_d4 <- .get_cov(
  s_bsub_sleep_lpa_d4,
  simsum_sub_d4[To == "Sleep" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "LPA"]$diff_delta_y_b
)

s_bsub_sleep_sb_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sleep_sb_d4 <- .get_cov(
  s_bsub_sleep_sb_d4,
  simsum_sub_d4[To == "Sleep" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_b
)

s_bsub_mvpa_sleep_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "Sleep"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_sleep_d4 <- .get_cov(
  s_bsub_mvpa_sleep_d4,
  simsum_sub_d4[To == "MVPA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "Sleep"]$diff_delta_y_b
)

s_bsub_mvpa_lpa_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_lpa_d4 <- .get_cov(
  s_bsub_mvpa_lpa_d4,
  simsum_sub_d4[To == "MVPA" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "LPA"]$diff_delta_y_b
)

s_bsub_mvpa_sb_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_sb_d4 <- .get_cov(
  s_bsub_mvpa_sb_d4,
  simsum_sub_d4[To == "MVPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "SB"]$diff_delta_y_b
)

s_bsub_lpa_sleep_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "Sleep"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_sleep_d4 <- .get_cov(
  s_bsub_lpa_sleep_d4,
  simsum_sub_d4[To == "LPA" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "Sleep"]$diff_delta_y_b
)

s_bsub_lpa_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_mvpa_d4 <- .get_cov(
  s_bsub_lpa_mvpa_d4,
  simsum_sub_d4[To == "LPA" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_lpa_sb_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_sb_d4 <- .get_cov(
  s_bsub_lpa_sb_d4,
  simsum_sub_d4[To == "LPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "SB"]$diff_delta_y_b
)

s_bsub_sb_sleep_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_sleep_d4 <- .get_cov(
  s_bsub_sb_sleep_d4,
  simsum_sub_d4[To == "SB" & From == "Sleep" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_b
)

s_bsub_sb_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_mvpa_d4 <- .get_cov(
  s_bsub_sb_mvpa_d4,
  simsum_sub_d4[To == "SB" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_sb_lpa_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_lpa_d4 <- .get_cov(
  s_bsub_sb_lpa_d4,
  simsum_sub_d4[To == "SB" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "LPA"]$diff_delta_y_b
)

## bsub d5 ----------------
s_bsub_tst_wake_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "WAKE"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_tst_wake_d5 <- .get_cov(
  s_bsub_tst_wake_d5,
  simsum_sub_d5[To == "TST" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "WAKE"]$diff_delta_y_b
)

s_bsub_tst_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_tst_mvpa_d5 <- .get_cov(
  s_bsub_tst_mvpa_d5,
  simsum_sub_d5[To == "TST" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_tst_lpa_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_tst_lpa_d5 <- .get_cov(
  s_bsub_tst_lpa_d5,
  simsum_sub_d5[To == "TST" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "LPA"]$diff_delta_y_b
)

s_bsub_tst_sb_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_tst_sb_d5 <- .get_cov(
  s_bsub_tst_sb_d5,
  simsum_sub_d5[To == "TST" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "SB"]$diff_delta_y_b
)

s_bsub_wake_tst_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "TST"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_wake_tst_d5 <- .get_cov(
  s_bsub_wake_tst_d5,
  simsum_sub_d5[To == "WAKE" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "TST"]$diff_delta_y_b
)

s_bsub_wake_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_wake_mvpa_d5 <- .get_cov(
  s_bsub_wake_mvpa_d5,
  simsum_sub_d5[To == "WAKE" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_wake_lpa_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_wake_lpa_d5 <- .get_cov(
  s_bsub_wake_lpa_d5,
  simsum_sub_d5[To == "WAKE" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "LPA"]$diff_delta_y_b
)

s_bsub_wake_sb_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_wake_sb_d5 <- .get_cov(
  s_bsub_wake_sb_d5,
  simsum_sub_d5[To == "WAKE" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "SB"]$diff_delta_y_b
)

s_bsub_mvpa_tst_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "TST"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_tst_d5 <- .get_cov(
  s_bsub_mvpa_tst_d5,
  simsum_sub_d5[To == "MVPA" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "TST"]$diff_delta_y_b
)

s_bsub_mvpa_wake_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "WAKE"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_wake_d5 <- .get_cov(
  s_bsub_mvpa_wake_d5,
  simsum_sub_d5[To == "MVPA" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "WAKE"]$diff_delta_y_b
)

s_bsub_mvpa_lpa_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_lpa_d5 <- .get_cov(
  s_bsub_mvpa_lpa_d5,
  simsum_sub_d5[To == "MVPA" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "LPA"]$diff_delta_y_b
)

s_bsub_mvpa_sb_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_mvpa_sb_d5 <- .get_cov(
  s_bsub_mvpa_sb_d5,
  simsum_sub_d5[To == "MVPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "SB"]$diff_delta_y_b
)

s_bsub_lpa_tst_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "TST"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_tst_d5 <- .get_cov(
  s_bsub_lpa_tst_d5,
  simsum_sub_d5[To == "LPA" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "TST"]$diff_delta_y_b
)

s_bsub_lpa_wake_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "WAKE"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_wake_d5 <- .get_cov(
  s_bsub_lpa_wake_d5,
  simsum_sub_d5[To == "LPA" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "WAKE"]$diff_delta_y_b
)

s_bsub_lpa_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_mvpa_d5 <- .get_cov(
  s_bsub_lpa_mvpa_d5,
  simsum_sub_d5[To == "LPA" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_lpa_sb_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "SB"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_lpa_sb_d5 <- .get_cov(
  s_bsub_lpa_sb_d5,
  simsum_sub_d5[To == "LPA" & From == "SB" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "SB"]$diff_delta_y_b
)

s_bsub_sb_tst_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "TST"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_tst_d5 <- .get_cov(
  s_bsub_sb_tst_d5,
  simsum_sub_d5[To == "SB" & From == "TST" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "TST"]$diff_delta_y_b
)

s_bsub_sb_wake_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "WAKE"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_wake_d5 <- .get_cov(
  s_bsub_sb_wake_d5,
  simsum_sub_d5[To == "SB" & From == "WAKE" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "WAKE"]$diff_delta_y_b
)

s_bsub_sb_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "MVPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_mvpa_d5 <- .get_cov(
  s_bsub_sb_mvpa_d5,
  simsum_sub_d5[To == "SB" & From == "MVPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "MVPA"]$diff_delta_y_b
)

s_bsub_sb_lpa_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "LPA"]$diff_delta_y_b,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_bsub_sb_lpa_d5 <- .get_cov(
  s_bsub_sb_lpa_d5,
  simsum_sub_d5[To == "SB" & From == "LPA" & Level == "between"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "LPA"]$diff_delta_y_b
)

## wsub d3 ----------------
s_wsub_sleep_pa_d3 <- simsum(
  simsum_sub_d3[To == "Sleep" & From == "PA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "PA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sleep_pa_d3 <- .get_cov(
  s_wsub_sleep_pa_d3,
  simsum_sub_d3[To == "Sleep" & From == "PA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "PA"]$diff_delta_y_w
)

s_wsub_sleep_sb_d3 <- simsum(
  simsum_sub_d3[To == "Sleep" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sleep_sb_d3 <- .get_cov(
  s_wsub_sleep_sb_d3,
  simsum_sub_d3[To == "Sleep" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "Sleep" & From == "SB"]$diff_delta_y_w
)

s_wsub_pa_sleep_d3 <- simsum(
  simsum_sub_d3[To == "PA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "Sleep"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_pa_sleep_d3 <- .get_cov(
  s_wsub_pa_sleep_d3,
  simsum_sub_d3[To == "PA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "Sleep"]$diff_delta_y_w
)

s_wsub_pa_sb_d3 <- simsum(
  simsum_sub_d3[To == "PA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_pa_sb_d3 <- .get_cov(
  s_wsub_pa_sb_d3,
  simsum_sub_d3[To == "PA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "PA" & From == "SB"]$diff_delta_y_w
)

s_wsub_sb_sleep_d3 <- simsum(
  simsum_sub_d3[To == "SB" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "Sleep"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_sleep_d3 <- .get_cov(
  s_wsub_sb_sleep_d3,
  simsum_sub_d3[To == "SB" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "Sleep"]$diff_delta_y_w
)

s_wsub_sb_pa_d3 <- simsum(
  simsum_sub_d3[To == "SB" & From == "PA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "PA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_pa_d3 <- .get_cov(
  s_wsub_sb_pa_d3,
  simsum_sub_d3[To == "SB" & From == "PA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d3[To == "SB" & From == "PA"]$diff_delta_y_w
)

## wsub d4 ----------------
s_wsub_sleep_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sleep_mvpa_d4 <- .get_cov(
  s_wsub_sleep_mvpa_d4,
  simsum_sub_d4[To == "Sleep" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_sleep_lpa_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sleep_lpa_d4 <- .get_cov(
  s_wsub_sleep_lpa_d4,
  simsum_sub_d4[To == "Sleep" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "LPA"]$diff_delta_y_w
)

s_wsub_sleep_sb_d4 <- simsum(
  simsum_sub_d4[To == "Sleep" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sleep_sb_d4 <- .get_cov(
  s_wsub_sleep_sb_d4,
  simsum_sub_d4[To == "Sleep" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_w
)

s_wsub_mvpa_sleep_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "Sleep"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_sleep_d4 <- .get_cov(
  s_wsub_mvpa_sleep_d4,
  simsum_sub_d4[To == "MVPA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "Sleep"]$diff_delta_y_w
)

s_wsub_mvpa_lpa_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_lpa_d4 <- .get_cov(
  s_wsub_mvpa_lpa_d4,
  simsum_sub_d4[To == "MVPA" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "LPA"]$diff_delta_y_w
)

s_wsub_mvpa_sb_d4 <- simsum(
  simsum_sub_d4[To == "MVPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_sb_d4 <- .get_cov(
  s_wsub_mvpa_sb_d4,
  simsum_sub_d4[To == "MVPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "MVPA" & From == "SB"]$diff_delta_y_w
)

s_wsub_lpa_sleep_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "Sleep"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_sleep_d4 <- .get_cov(
  s_wsub_lpa_sleep_d4,
  simsum_sub_d4[To == "LPA" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "Sleep"]$diff_delta_y_w
)

s_wsub_lpa_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_mvpa_d4 <- .get_cov(
  s_wsub_lpa_mvpa_d4,
  simsum_sub_d4[To == "LPA" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_lpa_sb_d4 <- simsum(
  simsum_sub_d4[To == "LPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_sb_d4 <- .get_cov(
  s_wsub_lpa_sb_d4,
  simsum_sub_d4[To == "LPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "LPA" & From == "SB"]$diff_delta_y_w
)

s_wsub_sb_sleep_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_sleep_d4 <- .get_cov(
  s_wsub_sb_sleep_d4,
  simsum_sub_d4[To == "SB" & From == "Sleep" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_w
)

s_wsub_sb_mvpa_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_mvpa_d4 <- .get_cov(
  s_wsub_sb_mvpa_d4,
  simsum_sub_d4[To == "SB" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_sb_lpa_d4 <- simsum(
  simsum_sub_d4[To == "SB" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_lpa_d4 <- .get_cov(
  s_wsub_sb_lpa_d4,
  simsum_sub_d4[To == "SB" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d4[To == "SB" & From == "LPA"]$diff_delta_y_w
)

## wsub d5 ----------------
s_wsub_tst_wake_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "WAKE"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_tst_wake_d5 <- .get_cov(
  s_wsub_tst_wake_d5,
  simsum_sub_d5[To == "TST" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "WAKE"]$diff_delta_y_w
)

s_wsub_tst_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_tst_mvpa_d5 <- .get_cov(
  s_wsub_tst_mvpa_d5,
  simsum_sub_d5[To == "TST" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_tst_lpa_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_tst_lpa_d5 <- .get_cov(
  s_wsub_tst_lpa_d5,
  simsum_sub_d5[To == "TST" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "LPA"]$diff_delta_y_w
)

s_wsub_tst_sb_d5 <- simsum(
  simsum_sub_d5[To == "TST" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_tst_sb_d5 <- .get_cov(
  s_wsub_tst_sb_d5,
  simsum_sub_d5[To == "TST" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "TST" & From == "SB"]$diff_delta_y_w
)

s_wsub_wake_tst_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "TST"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_wake_tst_d5 <- .get_cov(
  s_wsub_wake_tst_d5,
  simsum_sub_d5[To == "WAKE" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "TST"]$diff_delta_y_w
)

s_wsub_wake_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_wake_mvpa_d5 <- .get_cov(
  s_wsub_wake_mvpa_d5,
  simsum_sub_d5[To == "WAKE" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_wake_lpa_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_wake_lpa_d5 <- .get_cov(
  s_wsub_wake_lpa_d5,
  simsum_sub_d5[To == "WAKE" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "LPA"]$diff_delta_y_w
)

s_wsub_wake_sb_d5 <- simsum(
  simsum_sub_d5[To == "WAKE" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_wake_sb_d5 <- .get_cov(
  s_wsub_wake_sb_d5,
  simsum_sub_d5[To == "WAKE" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "WAKE" & From == "SB"]$diff_delta_y_w
)

s_wsub_mvpa_tst_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "TST"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_tst_d5 <- .get_cov(
  s_wsub_mvpa_tst_d5,
  simsum_sub_d5[To == "MVPA" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "TST"]$diff_delta_y_w
)

s_wsub_mvpa_wake_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "WAKE"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_wake_d5 <- .get_cov(
  s_wsub_mvpa_wake_d5,
  simsum_sub_d5[To == "MVPA" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "WAKE"]$diff_delta_y_w
)

s_wsub_mvpa_lpa_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_lpa_d5 <- .get_cov(
  s_wsub_mvpa_lpa_d5,
  simsum_sub_d5[To == "MVPA" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "LPA"]$diff_delta_y_w
)

s_wsub_mvpa_sb_d5 <- simsum(
  simsum_sub_d5[To == "MVPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_mvpa_sb_d5 <- .get_cov(
  s_wsub_mvpa_sb_d5,
  simsum_sub_d5[To == "MVPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "MVPA" & From == "SB"]$diff_delta_y_w
)

s_wsub_lpa_tst_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "TST"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_tst_d5 <- .get_cov(
  s_wsub_lpa_tst_d5,
  simsum_sub_d5[To == "LPA" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "TST"]$diff_delta_y_w
)

s_wsub_lpa_wake_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "WAKE"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_wake_d5 <- .get_cov(
  s_wsub_lpa_wake_d5,
  simsum_sub_d5[To == "LPA" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "WAKE"]$diff_delta_y_w
)

s_wsub_lpa_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_mvpa_d5 <- .get_cov(
  s_wsub_lpa_mvpa_d5,
  simsum_sub_d5[To == "LPA" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_lpa_sb_d5 <- simsum(
  simsum_sub_d5[To == "LPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "SB"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_lpa_sb_d5 <- .get_cov(
  s_wsub_lpa_sb_d5,
  simsum_sub_d5[To == "LPA" & From == "SB" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "LPA" & From == "SB"]$diff_delta_y_w
)

s_wsub_sb_tst_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "TST"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_tst_d5 <- .get_cov(
  s_wsub_sb_tst_d5,
  simsum_sub_d5[To == "SB" & From == "TST" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "TST"]$diff_delta_y_w
)

s_wsub_sb_wake_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "WAKE"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_wake_d5 <- .get_cov(
  s_wsub_sb_wake_d5,
  simsum_sub_d5[To == "SB" & From == "WAKE" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "WAKE"]$diff_delta_y_w
)

s_wsub_sb_mvpa_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "MVPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_mvpa_d5 <- .get_cov(
  s_wsub_sb_mvpa_d5,
  simsum_sub_d5[To == "SB" & From == "MVPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "MVPA"]$diff_delta_y_w
)

s_wsub_sb_lpa_d5 <- simsum(
  simsum_sub_d5[To == "SB" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "LPA"]$diff_delta_y_w,
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("CI_low", "CI_high")
)
s_wsub_sb_lpa_d5 <- .get_cov(
  s_wsub_sb_lpa_d5,
  simsum_sub_d5[To == "SB" & From == "LPA" & Level == "within"],
  estvarname = "Mean",
  true = substutitution_gt_d5[To == "SB" & From == "LPA"]$diff_delta_y_w
)
