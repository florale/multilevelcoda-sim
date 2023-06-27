library(knitr)

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

out1 <- readRDS("/fs04/ft29/simonm3/out1.RDS")
out2 <- readRDS("/fs04/ft29/simonm3/out2.RDS")
out3 <- readRDS("/fs04/ft29/simonm3/out3.RDS")
out4 <- readRDS("/fs04/ft29/simonm3/out4.RDS")
out5 <- readRDS("/fs04/ft29/simonm3/out5.RDS")
out6 <- readRDS("/fs04/ft29/simonm3/out6.RDS")
out7 <- readRDS("/fs04/ft29/simonm3/out7.RDS")
out8 <- readRDS("/fs04/ft29/simonm3/out8.RDS")

## extract -------------------
registerDoFuture()
plan(multisession, workers = 8)
results <- foreach (j = 1:8, .combine = c) %dopar% {
  out <- get(paste0("out", j))
  
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
  
  # results
  lapply(allout, function(y) {
    
    ### estimates ###
    est <- do.call(rbind, lapply(y, function(x) {
      x$Result$ModelSummary$fixed$Estimate
    }))
    colnames(est) <- paste0("b_", rownames(y[[1]]$Result$ModelSummary$fixed))
    
    ### SE ###
    se <- lci <- do.call(rbind, lapply(y, function(x) {
      x$Result$ModelSummary$fixed[["Est.Error"]]}))
    colnames(se) <- paste0("se_", rownames(y[[1]]$Result$ModelSummary$fixed))
    
    ### CIs ###
    lci <- do.call(rbind, lapply(y, function(x) {
      x$Result$ModelSummary$fixed[["l-95% CI"]]
    }))
    colnames(lci) <- paste0("ll_", rownames(y[[1]]$Result$ModelSummary$fixed))
    
    uci <- do.call(rbind, lapply(y, function(x) {
      x$Result$ModelSummary$fixed[["u-95% CI"]]
    }))
    colnames(uci) <- paste0("ul_", rownames(y[[1]]$Result$ModelSummary$fixed))
    
    fe_results <- cbind(est, se, lci, uci)
    
    ### random ###
    re_results <- cbind(
      sapply(y, function(x) {
        x$Result$ModelSummary$random$ID$Estimate
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$random$ID[["Est.Error"]]
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$random$ID[["l-95% CI"]]
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$random$ID[["u-95% CI"]]
      })
    )
    colnames(re_results) <- c("u0", "se_u0", "ll_u0", "ul_u0")
    
    ### residuals ###
    res_results <- cbind(
      sapply(y, function(x) {
        x$Result$ModelSummary$spec_pars$Estimate
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$spec_pars[["Est.Error"]]
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$spec_pars[["l-95% CI"]]
      }),
      sapply(y, function(x) {
        x$Result$ModelSummary$spec_pars[["u-95% CI"]]
      })
    )
    colnames(res_results) <- c("sigma", "se_sigma", "ll_sigma", "ul_sigma")
    
    ### putting it all together
    cbind(cond[n_parts == length(y[[1]][["parts"]])], fe_results, re_results, res_results)
  })
  
}
saveRDS(results, "results.RDS", compress = "xz")