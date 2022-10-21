library(MASS)
library(data.table)
library(multilevelTools)
library(extraoperators)
#library(tidyverse)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)
library(multilevelcoda)
library(lme4)
library(doFuture)
library(foreach)
library(doRNG)
library(parallel)

library(ggplot2)
library(ggsci)

if (Sys.info()[["sysname"]] %in% "Windows") {
  loc.base <- "g:"
} else if (Sys.info()[["sysname"]] %in% "Darwin") {
  loc.base <- "/Volumes/GoogleDrive"
}


# sbp
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol=5, byrow=TRUE)

sbp1 <- matrix(c(
  1, -1, -1,-1, -1,
  0, 1, -1, -1, -1,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol=5, byrow=TRUE)
