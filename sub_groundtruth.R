library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)
library(insight)
library(MASS)

library(doFuture)
library(foreach)
library(parallel)
library(doRNG)
library(future)
library(multilevelTools)
library(JWileymisc)

library("magrittr")
library("dplyr")
library("purrr")
# data ----------------------------------------------------------------------------------------
brmcoda_gt <- readRDS("brmcoda_gt.RDS")
n_parts <- c(3:5)

# ground truth -----------------------------
### between ###
substutitution_gt_b <- list()

for (i in seq_along(n_parts)) {
  n <- n_parts[i]
  
  if (n == 3) {
    parts <- c("TIBg", "PAg", "SBg")
  } else if (n == 4) {
    parts <- c("TIBg", "MVPAg", "LPAg", "SBg")
  } else if (n == 5) {
    parts <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")
  }
  
  object <- brmcoda_gt[[paste0("m", n)]]
  no_pairsub <- n * (n - 1)
  
  out <- list()
  
  for (j in 1:no_pairsub) {
    pairsub <- basesub(parts)
    pairsub <- pairsub * 30
    
    # V matrix
    V <- object$CompILR$psi
    V_t <- t(V)
    
    # reference bcomp
    x_b_dotdot <-
      cbind(object$CompILR$BetweenComp, object$CompILR$data[, .(ID)])
    x_b_dotdot <- x_b_dotdot[, head(.SD, 1), by = "ID"]
    x_b_dotdot <-
      acomp(x_b_dotdot[, colnames(object$CompILR$BetweenComp), with = FALSE], total = object$CompILR$total)
    dim(x_b_dotdot)
    x_b_dotdot <- mean.acomp(x_b_dotdot)
    
    # reference bilr
    z_b_dotdot <- V_t %*% matrix(log(x_b_dotdot), ncol = 1) %>% c(.)
    names(z_b_dotdot) <- paste0("bilr", 1:length(z_b_dotdot))
    
    # reallocated bcomp
    x_b_dash <- unclass(x_b_dotdot)
    x_b_dash <- x_b_dash + pairsub[j] / 1440
    
    # reallocated bilr
    z_b_dash <-
      V_t %*% matrix(log(as.matrix(x_b_dash)), ncol = 1) %>% c(.)
    names(z_b_dash) <- paste0("bilr", 1:length(z_b_dash))
    
    # ref wilrs = reallocated wilrs
    z_w_dotdot <- rep(0, ncol(object$CompILR$WithinILR))
    names(z_w_dotdot) <- paste0("wilr", 1:length(z_w_dotdot))
    z_w_dotdot <- rmult(z_w_dotdot)
    
    # prediction
    pred_y_dotdot <- t(as.matrix(c(z_b_dotdot, z_w_dotdot)))
    pred_y_b_dash <- t(as.matrix(c(z_b_dash, z_w_dotdot)))
    
    pred_newdat <- rbind(pred_y_dotdot, pred_y_b_dash)
    
    gamma1_hat <- as.matrix(fixef(object$Model)[, "Estimate"])
    z_0 <- cbind(Intercept = 1, pred_newdat)
    # all(colnames(z_0) == rownames(gamma1_hat))
    delta_y_b_manual <- z_0 %*% gamma1_hat
    diff_delta_y_b_manual <-
      delta_y_b_manual[2] - delta_y_b_manual[1]
    
    delta_y_b <-
      fitted(object$Model,
             newdata = pred_newdat,
             re_formula = NA)
    
    diff_delta_y_b <- diff(delta_y_b[, "Estimate"])
    
    out[[j]] <-
      c(diff_delta_y_b_manual,
        diff_delta_y_b)
  }
  substutitution_gt_b[[i]] <- out
}
str(substutitution_gt_b)
