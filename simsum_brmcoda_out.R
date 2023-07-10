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

simsum_brmcoda_1 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_1.RDS")
simsum_brmcoda_2 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_2.RDS")
simsum_brmcoda_3 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_3.RDS")
simsum_brmcoda_4 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_4.RDS")
simsum_brmcoda_5 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_5.RDS")
simsum_brmcoda_6 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_6.RDS")
simsum_brmcoda_7 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_7.RDS")
simsum_brmcoda_8 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_8.RDS")
simsum_brmcoda_9 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_9.RDS")
simsum_brmcoda_10 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_10.RDS")
simsum_brmcoda_11 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_11.RDS")
simsum_brmcoda_12 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_12.RDS")
simsum_brmcoda_13 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_13.RDS")
simsum_brmcoda_14 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_14.RDS")
simsum_brmcoda_15 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_15.RDS")
simsum_brmcoda_16 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_16.RDS")
simsum_brmcoda_17 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_17.RDS")
simsum_brmcoda_18 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_18.RDS")
simsum_brmcoda_19 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_19.RDS")
simsum_brmcoda_20 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_20.RDS")
simsum_brmcoda_21 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_21.RDS")
simsum_brmcoda_22 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_22.RDS")
simsum_brmcoda_23 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_23.RDS")
simsum_brmcoda_24 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_24.RDS")

## separate by parts and clean -----------------
simsum_brmcoda_d3 <- list()
simsum_brmcoda_d4 <- list()
simsum_brmcoda_d5 <- list()

for (j in c(1:22)) {
  simsum_brmcoda_d3[[j]] <- get(paste0("simsum_brmcoda_", j))[["out3"]]
  simsum_brmcoda_d4[[j]] <- get(paste0("simsum_brmcoda_", j))[["out4"]]
  simsum_brmcoda_d5[[j]] <- get(paste0("simsum_brmcoda_", j))[["out5"]]
}

simsum_brmcoda_d3 <- as.data.table(do.call(rbind, simsum_brmcoda_d3))
simsum_brmcoda_d4 <- as.data.table(do.call(rbind, simsum_brmcoda_d4))
simsum_brmcoda_d5 <- as.data.table(do.call(rbind, simsum_brmcoda_d5))

rm(simsum_brmcoda_1, simsum_brmcoda_2, simsum_brmcoda_3, simsum_brmcoda_4, 
   simsum_brmcoda_5, simsum_brmcoda_6, simsum_brmcoda_7, simsum_brmcoda_8,
   simsum_brmcoda_9, simsum_brmcoda_10, simsum_brmcoda_11, simsum_brmcoda_12,
   simsum_brmcoda_13, simsum_brmcoda_14, simsum_brmcoda_15, simsum_brmcoda_16,
   simsum_brmcoda_17, simsum_brmcoda_18, simsum_brmcoda_19, simsum_brmcoda_20,
   simsum_brmcoda_21, simsum_brmcoda_22, simsum_brmcoda_23, simsum_brmcoda_24)

# add condition
# 3 parts
simsum_brmcoda_d3[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_brmcoda_d3[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_brmcoda_d3[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_brmcoda_d3[, n_parts := NULL]

simsum_brmcoda_d3[, condition := NA]
simsum_brmcoda_d3[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_brmcoda_d3[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_brmcoda_d3[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_brmcoda_d3[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_brmcoda_d3[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_brmcoda_d3[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

simsum_brmcoda_d3[, sigma_condition := NA]
simsum_brmcoda_d3[, sigma_condition := ifelse(res_sd == 1, "base",  sigma_condition)]
simsum_brmcoda_d3[, sigma_condition := ifelse(res_sd == sqrt(1.5), "large",  sigma_condition)]
simsum_brmcoda_d3[, sigma_condition := ifelse(res_sd == sqrt(.5), "small",  sigma_condition)]
simsum_brmcoda_d3[, sigma_condition := factor(sigma_condition, levels = c(
  "base",
  "small",
  "large"
))]

simsum_brmcoda_d3[, u0_condition := NA]
simsum_brmcoda_d3[, u0_condition := ifelse(rint_sd == 1, "base",  u0_condition)]
simsum_brmcoda_d3[, u0_condition := ifelse(rint_sd == sqrt(1.5), "large",  u0_condition)]
simsum_brmcoda_d3[, u0_condition := ifelse(rint_sd == sqrt(.5), "small",  u0_condition)]
simsum_brmcoda_d3[, u0_condition := factor(u0_condition, levels = c(
  "base",
  "small",
  "large"
))]

# 4 parts
simsum_brmcoda_d4[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_brmcoda_d4[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_brmcoda_d4[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_brmcoda_d4[, n_parts := NULL]

simsum_brmcoda_d4[, condition := NA]
simsum_brmcoda_d4[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_brmcoda_d4[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_brmcoda_d4[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_brmcoda_d4[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_brmcoda_d4[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_brmcoda_d4[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

simsum_brmcoda_d4[, sigma_condition := NA]
simsum_brmcoda_d4[, sigma_condition := ifelse(res_sd == 1, "base",  sigma_condition)]
simsum_brmcoda_d4[, sigma_condition := ifelse(res_sd == sqrt(1.5), "large",  sigma_condition)]
simsum_brmcoda_d4[, sigma_condition := ifelse(res_sd == sqrt(.5), "small",  sigma_condition)]
simsum_brmcoda_d4[, sigma_condition := factor(sigma_condition, levels = c(
  "base",
  "small",
  "large"
))]

simsum_brmcoda_d4[, u0_condition := NA]
simsum_brmcoda_d4[, u0_condition := ifelse(rint_sd == 1, "base",  u0_condition)]
simsum_brmcoda_d4[, u0_condition := ifelse(rint_sd == sqrt(1.5), "large",  u0_condition)]
simsum_brmcoda_d4[, u0_condition := ifelse(rint_sd == sqrt(.5), "small",  u0_condition)]
simsum_brmcoda_d4[, u0_condition := factor(u0_condition, levels = c(
  "base",
  "small",
  "large"
))]

# 5 parts
simsum_brmcoda_d5[, N := factor(N, levels = c("30", "50", "360", "1200"))]
simsum_brmcoda_d5[, K := factor(K, levels = c("3", "5", "7", "14"))]
simsum_brmcoda_d5[, D := factor(n_parts, levels = c("3", "4", "5"))]
simsum_brmcoda_d5[, n_parts := NULL]

simsum_brmcoda_d5[, condition := NA]
simsum_brmcoda_d5[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
simsum_brmcoda_d5[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
simsum_brmcoda_d5[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
simsum_brmcoda_d5[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
simsum_brmcoda_d5[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
simsum_brmcoda_d5[, condition := factor(condition, levels = c(
  "base",
  "REsmall_RESlarge",
  "RElarge_RESsmall",
  "REbase_RESlarge",
  "REbase_RESsmall"
))]

simsum_brmcoda_d5[, sigma_condition := NA]
simsum_brmcoda_d5[, sigma_condition := ifelse(res_sd == 1, "base",  sigma_condition)]
simsum_brmcoda_d5[, sigma_condition := ifelse(res_sd == sqrt(1.5), "large",  sigma_condition)]
simsum_brmcoda_d5[, sigma_condition := ifelse(res_sd == sqrt(.5), "small",  sigma_condition)]
simsum_brmcoda_d5[, sigma_condition := factor(sigma_condition, levels = c(
  "base",
  "small",
  "large"
))]

simsum_brmcoda_d5[, u0_condition := NA]
simsum_brmcoda_d5[, u0_condition := ifelse(rint_sd == 1, "base",  u0_condition)]
simsum_brmcoda_d5[, u0_condition := ifelse(rint_sd == sqrt(1.5), "large",  u0_condition)]
simsum_brmcoda_d5[, u0_condition := ifelse(rint_sd == sqrt(.5), "small",  u0_condition)]
simsum_brmcoda_d5[, u0_condition := factor(u0_condition, levels = c(
  "base",
  "small",
  "large"
))]

# set non-convergence/divergent transition/ problematic ESS to NA
colnames(simsum_brmcoda_d3)
colnames(simsum_brmcoda_d4)
colnames(simsum_brmcoda_d5)

estnames <-
  colnames(simsum_brmcoda_d3) %snin% c("N", "K", "D", "rint_sd", "res_sd", "run", 
                                       "ndt", "zero",
                                       "condition", "sigma_condition", "u0_condition")

simsum_brmcoda_d3 <- simsum_brmcoda_d3[ndt != 0 | zero != 0, (estnames) := NA]
simsum_brmcoda_d4 <- simsum_brmcoda_d4[ndt != 0 | zero != 0, (estnames) := NA]
simsum_brmcoda_d5 <- simsum_brmcoda_d5[ndt != 0 | zero != 0, (estnames) := NA]

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


