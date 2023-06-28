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
source("input.R") # groundtruth, conditions and functions
n_parts <- c(3:5)

parts3 <- c("TIBg", "PAg", "SBg")
parts4 <- c("TIBg", "MVPAg", "LPAg", "SBg")
parts5 <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")

# ground truth -----------------------------
substutitution_gt <- list()

for (i in seq_along(n_parts)) {
  n <- n_parts[i]
  
  if (n == 3) {
    parts <- parts3
    groundtruth <- data.table(
      Intercept = 1,
      bilr1 = 0.15,
      bilr2 = 0.10,
      wilr1 = -0.8,
      wilr2 = -0.25
    )
    
  } else if (n == 4) {
    parts <- parts4
    groundtruth <- data.table(
      Intercept = 1,
      bilr1 = 0.15,
      bilr2 = 0.15,
      bilr3 = 0.02,
      wilr1 = -0.75,
      wilr2 = -0.3,
      wilr3 = -0.2
    )
    
  } else if (n == 5) {
    parts <- parts5
    groundtruth <- data.table(
      Intercept = 1,
      bilr1 = 0.15,
      bilr2 = -0.01,
      bilr3 = 0.15,
      bilr4 = 0.05,
      wilr1 = -0.6,
      wilr2 = -0.45,
      wilr3 = -0.3,
      wilr4 = -0.2
    )
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
    z_b_dotdot <- V_t %*% matrix(log(x_b_dotdot), ncol = 1) %>% t(.)
    colnames(z_b_dotdot) <- paste0("bilr", 1:length(z_b_dotdot))
    
    # reallocated bcomp
    x_b_dash <- unclass(x_b_dotdot)
    x_b_dash <- x_b_dash + as.matrix(pairsub[j]) / 1440
    
    # reallocated bilr
    z_b_dash <-
      V_t %*% matrix(log(x_b_dash), ncol = 1) %>% t(.)
    colnames(z_b_dash) <- paste0("bilr", 1:length(z_b_dash))
    
    # ref wilrs 
    z_w_dotdot <- matrix(0, nrow = 1, ncol = ncol(object$CompILR$WithinILR))
    colnames(z_w_dotdot) <- paste0("wilr", 1:length(z_w_dotdot))
    
    # reallocated wilrs
    z_w_dash <- z_b_dash - z_b_dotdot
    colnames(z_w_dash) <- paste0("wilr", 1:length(z_w_dash))
    
    # prediction
    pred_y_dotdot <- cbind(z_b_dotdot, z_w_dotdot)
    
    pred_y_b_dash <- cbind(z_b_dash, z_w_dotdot)
    pred_y_w_dash <- cbind(z_b_dotdot, z_w_dash)
    
    pred_y_b <- rbind(pred_y_dotdot, pred_y_b_dash)
    pred_y_w <- rbind(pred_y_dotdot, pred_y_w_dash)
    
    gamma1_hat <- t(groundtruth)
    
    z_b_0 <- cbind(Intercept = 1, pred_y_b)
    z_w_0 <- cbind(Intercept = 1, pred_y_w)
    
    # all(colnames(z_b_0) == rownames(gamma1_hat))
    # all(colnames(z_w_0) == rownames(gamma1_hat))
    
    delta_y_b_manual <- z_b_0 %*% gamma1_hat
    delta_y_w_manual <- z_w_0 %*% gamma1_hat
    
    diff_delta_y_b_manual <- delta_y_b_manual[2] - delta_y_b_manual[1]
    diff_delta_y_w_manual <- delta_y_w_manual[2] - delta_y_w_manual[1]
    
    # delta_y_b <-
    #   fitted(object$Model,
    #          newdata = pred_newdat,
    #          re_formula = NA)
    # 
    # diff_delta_y_b <- diff(delta_y_b[, "Estimate"])
    
    out[[j]] <- list(diff_delta_y_b_manual,
                     diff_delta_y_w_manual
    )
  }
  out <- rbindlist(out)
  colnames(out) <- c("diff_delta_y_b", "diff_delta_y_w")
  out <- cbind(out, pairsub)
  
  substutitution_gt[[i]] <- out
}

str(substutitution_gt)