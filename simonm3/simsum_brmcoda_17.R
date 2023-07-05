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

out <- readRDS("/fs04/ft29/simonm3/out17.RDS")

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

simsum_brmcoda_17 <- lapply(allout, function(y) {
  
  ### estimates ###
  est <- do.call(rbind, lapply(y, function(x) {
    x$Result$ModelSummary$fixed$Estimate
  }))
  colnames(est) <- paste0("b_", rownames(y[[1]]$Result$ModelSummary$fixed))
  
  ### SE ###
  se <- do.call(rbind, lapply(y, function(x) {
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
  
  ### others 
  r_hat <- do.call(rbind, lapply(y, function(x) {
    x$Result$ModelSummary$fixed$Rhat
  }))
  colnames(r_hat) <- paste0("rhat_", rownames(y[[1]]$Result$ModelSummary$fixed))
  
  b_ess <- do.call(rbind, lapply(y, function(x) {
    x$Result$ModelSummary$fixed$Bulk_ESS
  }))
  colnames(b_ess) <- paste0("bess_", rownames(y[[1]]$Result$ModelSummary$fixed))
  
  t_ess <- do.call(rbind, lapply(y, function(x) {
    x$Result$ModelSummary$fixed$Tail_ESS
  }))
  colnames(t_ess) <- paste0("tess_", rownames(y[[1]]$Result$ModelSummary$fixed))
  
  others <- cbind(r_hat, b_ess, t_ess)
  
  ### conditions
  cond_results <- cbind(
    sapply(y, function(x) {x$N}),
    sapply(y, function(x) {x$K}),
    sapply(y, function(x) {x$rint_sd}),
    sapply(y, function(x) {x$res_sd}),
    sapply(y, function(x) {x$run}),
    sapply(y, function(x) {x$n_parts}),
    sapply(y, function(x) {x$Result$ndt}),
    sapply(y, function(x) {any(apply(x$CompILR$data[, x$parts, with = FALSE], 2, function(z) z == 0))})
  )
  
  colnames(cond_results) <- c("N", "K", "rint_sd", "res_sd", "run", "n_parts", "ndt", "zero")
  
  ### putting it all together
  cbind(cond_results, fe_results, re_results, res_results, others)
})

saveRDS(simsum_brmcoda_17, "simsum_brmcoda_17.RDS")