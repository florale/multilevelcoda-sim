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

## d3 -------------------
s_b0_d3 <- simsum(
  simsum_brmcoda_d3,
  estvarname = "b_Intercept",
  true = groundtruth3$b_Intercept,
  se = "se_Intercept",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_Intercept", "ul_Intercept")
)

s_bilr1_d3 <- simsum(
  simsum_brmcoda_d3,
  estvarname = "b_bilr1",
  true = groundtruth3$b_bilr1,
  se = "se_bilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr1", "ul_bilr1")
)

s_bilr2_d3 <- simsum(
  simsum_brmcoda_d3,
  estvarname = "b_bilr2",
  true = groundtruth3$b_bilr2,
  se = "se_bilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr2", "ul_bilr2")
)

s_wilr1_d3 <- simsum(
  simsum_brmcoda_d3,
  estvarname = "b_wilr1",
  true = groundtruth3$b_wilr1,
  se = "se_wilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr1", "ul_wilr1")
)

s_wilr2_d3 <- simsum(
  simsum_brmcoda_d3,
  estvarname = "b_wilr2",
  true = groundtruth3$b_wilr2,
  se = "se_wilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr2", "ul_wilr2")
)

s_u0_base_d3 <- simsum(
  simsum_brmcoda_d3[u0_condition == "base"],
  estvarname = "u0",
  true = groundtruth3$u0,
  se = "se_u0",
  methodvar = "sigma_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_small_d3 <- simsum(
  simsum_brmcoda_d3[u0_condition == "small"],
  estvarname = "u0",
  true = groundtruth3$u0_small,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_large_d3 <- simsum(
  simsum_brmcoda_d3[u0_condition == "large"],
  estvarname = "u0",
  true = groundtruth3$u0_large,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_sigma_base_d3 <- simsum(
  simsum_brmcoda_d3[sigma_condition == "base"],
  estvarname = "sigma",
  true = groundtruth3$sigma,
  se = "se_sigma",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_small_d3 <- simsum(
  simsum_brmcoda_d3[sigma_condition == "small"][, u0_condition := factor(u0_condition, levels = c("base", "large"))],
  estvarname = "sigma",
  true = groundtruth3$sigma_small,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_large_d3 <- simsum(
  simsum_brmcoda_d3[sigma_condition == "large"][, u0_condition := factor(u0_condition, levels = c("base", "small"))],
  estvarname = "sigma",
  true = groundtruth3$sigma_large,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

## d4 -----------------
s_b0_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_Intercept",
  true = groundtruth4$b_Intercept,
  se = "se_Intercept",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_Intercept", "ul_Intercept")
)

s_bilr1_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_bilr1",
  true = groundtruth4$b_bilr1,
  se = "se_bilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr1", "ul_bilr1")
)

s_bilr2_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_bilr2",
  true = groundtruth4$b_bilr2,
  se = "se_bilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr2", "ul_bilr2")
)

s_bilr3_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_bilr3",
  true = groundtruth4$b_bilr3,
  se = "se_bilr3",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr3", "ul_bilr3")
)

s_wilr1_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_wilr1",
  true = groundtruth4$b_wilr1,
  se = "se_wilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr1", "ul_wilr1")
)

s_wilr2_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_wilr2",
  true = groundtruth4$b_wilr2,
  se = "se_wilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr2", "ul_wilr2")
)

s_wilr3_d4 <- simsum(
  simsum_brmcoda_d4,
  estvarname = "b_wilr3",
  true = groundtruth4$b_wilr3,
  se = "se_wilr3",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr3", "ul_wilr3")
)

s_u0_base_d4 <- simsum(
  simsum_brmcoda_d4[u0_condition == "base"],
  estvarname = "u0",
  true = groundtruth4$u0,
  se = "se_u0",
  methodvar = "sigma_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_small_d4 <- simsum(
  simsum_brmcoda_d4[u0_condition == "small"],
  estvarname = "u0",
  true = groundtruth4$u0_small,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_large_d4 <- simsum(
  simsum_brmcoda_d4[u0_condition == "large"],
  estvarname = "u0",
  true = groundtruth4$u0_large,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_sigma_base_d4 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "base"],
  estvarname = "sigma",
  true = groundtruth4$sigma,
  se = "se_sigma",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_small_d4 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "small"][, u0_condition := factor(u0_condition, levels = c("base", "large"))],
  estvarname = "sigma",
  true = groundtruth4$sigma_small,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_large_d4 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "large"][, u0_condition := factor(u0_condition, levels = c("base", "small"))],
  estvarname = "sigma",
  true = groundtruth4$sigma_large,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

## d5 ------------------
s_b0_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_Intercept",
  true = groundtruth5$b_Intercept,
  se = "se_Intercept",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_Intercept", "ul_Intercept")
)

s_bilr1_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_bilr1",
  true = groundtruth5$b_bilr1,
  se = "se_bilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr1", "ul_bilr1")
)

s_bilr2_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_bilr2",
  true = groundtruth5$b_bilr2,
  se = "se_bilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr2", "ul_bilr2")
)

s_bilr3_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_bilr3",
  true = groundtruth5$b_bilr3,
  se = "se_bilr3",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr3", "ul_bilr3")
)

s_bilr4_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_bilr4",
  true = groundtruth5$b_bilr4,
  se = "se_bilr4",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_bilr4", "ul_bilr4")
)

s_wilr1_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_wilr1",
  true = groundtruth5$b_wilr1,
  se = "se_wilr1",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr1", "ul_wilr1")
)

s_wilr2_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_wilr2",
  true = groundtruth5$b_wilr2,
  se = "se_wilr2",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr2", "ul_wilr2")
)

s_wilr3_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_wilr3",
  true = groundtruth5$b_wilr3,
  se = "se_wilr3",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr3", "ul_wilr3")
)

s_wilr4_d5 <- simsum(
  simsum_brmcoda_d5,
  estvarname = "b_wilr4",
  true = groundtruth5$b_wilr4,
  se = "se_wilr4",
  methodvar = "condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_wilr4", "ul_wilr4")
)

s_u0_base_d5 <- simsum(
  simsum_brmcoda_d4[u0_condition == "base"],
  estvarname = "u0",
  true = groundtruth5$u0,
  se = "se_u0",
  methodvar = "sigma_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_small_d5 <- simsum(
  simsum_brmcoda_d4[u0_condition == "small"],
  estvarname = "u0",
  true = groundtruth5$u0_small,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_u0_large_d5 <- simsum(
  simsum_brmcoda_d4[u0_condition == "large"],
  estvarname = "u0",
  true = groundtruth5$u0_large,
  se = "se_u0",
  by = c("N", "K"),
  ci.limits = c("ll_u0", "ul_u0")
)

s_sigma_base_d5 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "base"],
  estvarname = "sigma",
  true = groundtruth5$sigma,
  se = "se_sigma",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_small_d5 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "small"][, u0_condition := factor(u0_condition, levels = c("base", "large"))],
  estvarname = "sigma",
  true = groundtruth5$sigma_small,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)

s_sigma_large_d5 <- simsum(
  simsum_brmcoda_d4[sigma_condition == "large"][, u0_condition := factor(u0_condition, levels = c("base", "small"))],
  estvarname = "sigma",
  true = groundtruth5$sigma_large,
  se = "se_sigma",
  methodvar = "u0_condition",
  ref = "base",
  by = c("N", "K"),
  ci.limits = c("ll_sigma", "ul_sigma")
)


