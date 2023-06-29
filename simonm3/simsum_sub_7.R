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

library(ggplot2)
library(ggsci)
library(rsimsum) # https://cran.r-project.org/web/packages/rsimsum/vignettes/A-introduction.html

input <- readRDS("input.RDS")
meanscovs <- input$meanscovs
prefit5 <- input$prefit5
prefit4 <- input$prefit4
prefit3 <- input$prefit3

source("input.R") # groundtruth, conditions and functions

# out <- readRDS("/fs04/ft29/simonm3/out1.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out2.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out3.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out4.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out5.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out6.RDS")
out <- readRDS("/fs04/ft29/simonm3/out7.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out8.RDS")

## extract -------------------
# registerDoFuture()
# plan(multisession, workers = 8)

out3 <- list()
out4 <- list()
out5 <- list()

for (i in 1:length(out)) {
  if (identical(out[[i]][[8]], 3)) out3[[i]] <- out[[i]]
  if (identical(out[[i]][[8]], 4)) out4[[i]] <- out[[i]]
  if (identical(out[[i]][[8]], 5)) out5[[i]] <- out[[i]]
}
allout <- list(out3, out4, out5)
names(allout) <- c("out3", "out4", "out5")
allout <- lapply(allout, function(x) {
  x[!sapply(x, is.null)]})

rm(out, out3, out4, out5)

sub_sum3 <- do.call(rbind, lapply(allout[["out3"]], function(x) {
  # summary
  sum <- summary(x$Result$Substitution, delta = 30)
  
  ### conditions
  cond_results <- cbind(x$N, x$K, x$rint_sd, x$res_sd, x$run, x$n_parts)
  cond_results <- cond_results[rep(seq_len(nrow(cond_results)), nrow(sum)), ]
  
  colnames(cond_results) <- c("N", "K", "rint_sd", "res_sd", "run", "n_parts")
  
  cbind(cond_results, sum)
}))

sub_sum4 <- do.call(rbind, lapply(allout[["out4"]], function(x) {
  # summary
  sum <- summary(x$Result$Substitution, delta = 30)
  
  ### conditions
  cond_results <- cbind(x$N, x$K, x$rint_sd, x$res_sd, x$run, x$n_parts)
  cond_results <- cond_results[rep(seq_len(nrow(cond_results)), nrow(sum)), ]
  
  colnames(cond_results) <- c("N", "K", "rint_sd", "res_sd", "run", "n_parts")
  
  cbind(cond_results, sum)
}))

sub_sum5 <- do.call(rbind, lapply(allout[["out5"]], function(x) {
  # summary
  sum <- summary(x$Result$Substitution, delta = 30)
  
  ### conditions
  cond_results <- cbind(x$N, x$K, x$rint_sd, x$res_sd, x$run, x$n_parts)
  cond_results <- cond_results[rep(seq_len(nrow(cond_results)), nrow(sum)), ]
  
  colnames(cond_results) <- c("N", "K", "rint_sd", "res_sd", "run", "n_parts")
  
  cbind(cond_results, sum)
}))

simsum_sub_7 <- list(sub_sum3, sub_sum4, sub_sum5)
names(simsum_sub_7) <- c("out3", "out4", "out5")
saveRDS(simsum_sub_7, "simsum_sub_7.RDS")