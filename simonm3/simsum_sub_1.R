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

out <- readRDS("/fs04/ft29/simonm3/out1.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out2.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out3.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out4.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out5.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out6.RDS")
# out <- readRDS("/fs04/ft29/simonm3/out7.RDS")
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

simsum_sub_1 <- lapply(allout, function(y) {
  
  ### substitution ###
  sub_sum <- do.call(rbind, lapply(y, function(x) {
    sum <- summary(x$Result$Substitution)
    
    ### conditions
    cond_results <- cbind(
      sapply(y, function(x) {x$N}),
      sapply(y, function(x) {x$K}),
      sapply(y, function(x) {x$rint_sd}),
      sapply(y, function(x) {x$res_sd}),
      sapply(y, function(x) {x$run}),
      sapply(y, function(x) {x$n_parts})
    )
    colnames(cond_results) <- c("N", "K", "rint_sd", "res_sd", "run", "n_parts")
    
    cond_results <- cond_results[rep(seq_len(nrow(cond_results)), nrow(sum)), ]
    
    ### putting it all together
    cbind(cond_results, sum)
  }))
})

saveRDS(simsum_sub_1, "simsum_sub_1.RDS")