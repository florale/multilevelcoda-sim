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

# data ----------------------------------------------------------------------------------------
d <- as.data.table(readRDS("/Volumes/shared/Behavioral-med-lab/StressHealthStudy/SHS Research Interns/Data/shs_daily_ggir.RDS"))

# make composition
sbp5 <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)

# check NA and 0
# d[which(is.na(d$Sleepg)), "ID"]
# d[which(is.na(d$WAKEg)), "ID"]
# d[which(is.na(d$SBg)), "ID"]
# d[which(is.na(d$LPAg)), "ID"]
# d[which(is.na(d$MVPAg)), "ID"]
# d[which(is.na(d$SLEEPY)), "ID"]

parts5 <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")
any(apply(d[, parts, with = FALSE], 2, function(x) x == 0))

# d <- d[!(TST == 0 | WAKE == 0 | MVPA == 0 | LPA == 0 | SB == 0) & !is.na(SLEEPY)]

d <- d[!is.na(Sleepg) & !is.na(WAKEg) & !is.na(MVPAg) & !is.na(LPAg) & !is.na(SBg)]

# tilr <- ilr(acomp(d[, .(TST, WAKE, MVPA, LPA, SB)]), V = psi)
# bilr <- ilr(acomp(d[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi)
# wilr <- ilr(acomp(d[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi)
# 
# colnames(tilr) <- c("ilr1", "ilr2", "ilr3", "ilr4")
# colnames(bilr) <- c("bilr1", "bilr2", "bilr3", "bilr4")
# colnames(wilr) <- c("wilr1", "wilr2", "wilr3", "wilr4")

### 5 class ----------
# multilevelcoda
cilr5 <- compilr(d, sbp = sbp5,
                 parts = parts5, idvar = "ID")
m5 <- brmcoda(cilr5,
              SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + bilr4 +
               wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
)
summary(m5$Model)

registerDoFuture()
plan(multisession, workers = 5)
submodel5 <- substitution(
  m5,
  delta = c(1:10),
  level = c("between", "within"),
  ref = "conditional")
registerDoSEQ()

plotsub(submodel5$BetweenpersonSub$Sleepg, x = "Sleep", y = "Sleepy")
plotsub(submodel5$BetweenpersonSub$WAKEg, x = "AWAKE", y = "Sleepy")
plotsub(submodel5$BetweenpersonSub$MVPAg, x = "MVPA", y = "Sleepy")
plotsub(submodel5$BetweenpersonSub$LPAg, x = "LPA", y = "Sleepy")
plotsub(submodel5$BetweenpersonSub$SBg, x = "SB", y = "Sleepy")

plotsub(submodel5$WithinpersonSub$Sleepg, x = "Sleep", y = "Sleepy")
plotsub(submodel5$WithinpersonSub$WAKEg, x = "AWAKE", y = "Sleepy")
plotsub(submodel5$WithinpersonSub$MVPAg, x = "MVPA", y = "Sleepy")
plotsub(submodel5$WithinpersonSub$LPAg, x = "LPA", y = "Sleepy")
plotsub(submodel5$WithinpersonSub$SBg, x = "SB", y = "Sleepy")

plotsub(submodel5$BetweenpersonSubMargin$Sleepg, x = "Sleep", y = "Sleepy")
plotsub(submodel5$BetweenpersonSubMargin$WAKEg, x = "AWAKE", y = "Sleepy")
plotsub(submodel5$BetweenpersonSubMargin$MVPAg, x = "MVPA", y = "Sleepy")
plotsub(submodel5$BetweenpersonSubMargin$LPAg, x = "LPA", y = "Sleepy")
plotsub(submodel5$BetweenpersonSubMargin$SBg, x = "SB", y = "Sleepy")

plotsub(submodel5$WithinpersonSubMargin$Sleepg, x = "Sleep", y = "Sleepy")
plotsub(submodel5$WithinpersonSubMargin$WAKEg, x = "AWAKE", y = "Sleepy")
plotsub(submodel5$WithinpersonSubMargin$MVPAg, x = "MVPA", y = "Sleepy")
plotsub(submodel5$WithinpersonSubMargin$LPAg, x = "LPA", y = "Sleepy")
plotsub(submodel5$WithinpersonSubMargin$SBg, x = "SB", y = "Sleepy")
# # manual coding
# dd <- cbind(
#   tilr,
#   bilr,
#   wilr,
#   d[, .(COPEExpZ, COPEPrcZ, COPEMenZ, COPEEACZ, COPE_APZ, COPE_ApprZ, COPE_AvoiZ, 
#         SLEEPY, ID)])
# 
# # descriptives
# egltable(c("SLEEPY", "TST", "WAKE", "MVPA", "LPA", "SB"), data = d)
# d[, index := 1:.N, by = ID]
# table(dd$SLEEPY) # 1-5
# 
# # models - COPEEACZ COPEMenZ SLEEPY
# # mt <- brm(COPE_ApprZ ~ ilr1 + ilr2 + ilr3 + ilr4 + (1 | ID), data = dd)
# # summary(mt)
# 
# m0 <- brm(SLEEPY ~ bilr1 + bilr2 + bilr3 + bilr4 +
#             wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID), data = dd,
#           cores = 4, seed = 123)
# summary(m0)

### 4 class ----------
sbp4 <- matrix(c(
  1, -1, -1,-1,
  0, 1, -1, -1,
  0, 0, 1, -1), ncol = 4, byrow = TRUE)

cilr4 <- compilr(d, sbp = sbp4, 
                parts = c("TIBg", "MVPAg", "LPAg", "SBg"), idvar = "ID")

m4 <- brmcoda(cilr4,
              SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + 
               wilr1 + wilr2 + wilr3 + (1 | ID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
             )
summary(m4$Model)

submodel4 <- substitution(
  m4,
  delta = c(30),
  level = c("between", "within"),
  type = "conditional")

### 3 class ----------
sbp3 <- matrix(c(
  1, -1,-1, 
  0, 1, -1), 
  ncol = 3, byrow = TRUE)

d[, PAg := MVPAg + LPAg]
cilr3 <- compilr(d, sbp = sbp3, 
                parts = c("TIBg", "PAg", "SBg"), idvar = "ID")

m3 <- brmcoda(cilr3,
              SLEEPYNextDay ~ bilr1 + bilr2 + wilr1 + wilr2 + (1 | ID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
)
summary(m3$Model)

submodel3 <- substitution(
  m3,
  delta = c(30),
  level = c("between", "within"),
  type = "conditional")

# library(doFuture)
# registerDoFuture()
# plan(multisession, workers = 5)
# 
# subm <- substitution(m, delta = 1:60, 
#                      level = c("between", "within"), type = c("conditional", "marginal"))
# 
# registerDoSEQ()
brmcoda_gt <- list(
  m5 = m5,
  m4 = m4,
  m3 = m3
)
saveRDS(brmcoda_gt, "brmcoda_gt.RDS", compress = "xz")