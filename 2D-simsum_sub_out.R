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

source("1C-simmodel_input.R") # groundtruth, conditions and functions
source("2B-simsum_diag.R") # check diag and set problematic runs to missing
source("0A-functions.R")

substutitution_gt <- readRDS("substutitution_gt.RDS")
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
