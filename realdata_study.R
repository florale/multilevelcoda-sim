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
d <- as.data.table(readRDS("/Users/florale/Library/CloudStorage/GoogleDrive-flora.le@monash.edu/Shared drives/EMA_Studies/ema_studies.RDS"))

setnames(d, "TSTacti", "TST")
setnames(d, "MVPAPERCacti", "MVPA")
setnames(d, "LIGHTPERCacti", "LPA")
setnames(d, "SEDPERCacti", "SB")

d <- d[!is.na(TST) & !is.na(MVPA) & !is.na(SOLRAWacti)] ## exclude surveys without sleep/acti data

d[, TST := TST * 60]
d[, WAKE := SOLRAWacti + WASORAWacti]
d[, timeawake := (24 * 60) - TST - WAKE]
d[, SB   := (SB   / 100) * timeawake]
d[, LPA  := (LPA  / 100) * timeawake]
d[, MVPA := (MVPA / 100) * timeawake]
d[, totalhours := TST + WAKE + SB + LPA + MVPA]

d[, c("BMVPA", "WMVPA") := meanDeviations(MVPA), by = UID]
d[, c("BLPA", "WLPA") := meanDeviations(LPA), by = UID]
d[, c("BSB", "WSB") := meanDeviations(SB), by = UID]
d[, c("BWAKE", "WWAKE") := meanDeviations(WAKE), by = UID]
d[, c("BTST", "WTST") := meanDeviations(TST), by = UID]
d[, c("BSTRESS", "WSTRESS") := meanDeviations(STRESS), by = UID]

## make within variable ratio of total to between
d[, WMVPA := MVPA / BMVPA]
d[, WLPA := LPA / BLPA]
d[, WSB := SB / BSB]
d[, WWAKE := WAKE / BWAKE]
d[, WTST := TST / BTST]

# make composition
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
psi <- gsi.buildilrBase(t(sbp))

# check NA and 0
d[which(is.na(d$TST)), "UID"]
d[which(is.na(d$WAKE)), "UID"]
d[which(is.na(d$SB)), "UID"]
d[which(is.na(d$LPA)), "UID"]
d[which(is.na(d$MVPA)), "UID"]
d[which(is.na(d$SLEEPY)), "UID"]

parts = c("TST", "WAKE", "MVPA", "LPA", "SB")
any(apply(d[, parts, with = FALSE], 2, function(x) x == 0))

d <- d[!(TST == 0 | WAKE == 0 | MVPA == 0 | LPA == 0 | SB == 0) & !is.na(SLEEPY)]

tilr <- ilr(acomp(d[, .(TST, WAKE, MVPA, LPA, SB)]), V = psi)
bilr <- ilr(acomp(d[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi)
wilr <- ilr(acomp(d[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi)

colnames(tilr) <- c("ilr1", "ilr2", "ilr3", "ilr4")
colnames(bilr) <- c("bilr1", "bilr2", "bilr3", "bilr4")
colnames(wilr) <- c("wilr1", "wilr2", "wilr3", "wilr4")

### 5 class ----------
# sbp & psi
sbp5 <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
psi5 <- gsi.buildilrBase(t(sbp5))

# multilevelcoda
cilr5 <- compilr(d, sbp = sbp5,
                 parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "UID")
m5 <- brmcoda(cilr5,
             SLEEPY ~ bilr1 + bilr2 + bilr3 + bilr4 +
               wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
)
summary(m5$Model)

submodel5 <- substitution(
  m5,
  delta = c(30),
  level = c("between", "within"),
  type = "conditional")

# # manual coding
# dd <- cbind(
#   tilr,
#   bilr,
#   wilr,
#   d[, .(COPEExpZ, COPEPrcZ, COPEMenZ, COPEEACZ, COPE_APZ, COPE_ApprZ, COPE_AvoiZ, 
#         SLEEPY, UID)])
# 
# # descriptives
# egltable(c("SLEEPY", "TST", "WAKE", "MVPA", "LPA", "SB"), data = d)
# d[, index := 1:.N, by = UID]
# table(dd$SLEEPY) # 1-5
# 
# # models - COPEEACZ COPEMenZ SLEEPY
# # mt <- brm(COPE_ApprZ ~ ilr1 + ilr2 + ilr3 + ilr4 + (1 | UID), data = dd)
# # summary(mt)
# 
# m0 <- brm(SLEEPY ~ bilr1 + bilr2 + bilr3 + bilr4 +
#             wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), data = dd,
#           cores = 4, seed = 123)
# summary(m0)

### 4 class ----------
sbp4 <- matrix(c(
  1, -1, -1,-1,
  0, 1, -1, -1,
  0, 0, 1, -1), ncol = 4, byrow = TRUE)
psi4 <- gsi.buildilrBase(t(sbp4))

d[, Sleep := TST + WAKE]
cilr4 <- compilr(d, sbp = sbp4, 
                parts = c("Sleep", "MVPA", "LPA", "SB"), idvar = "UID")
m4 <- brmcoda(cilr4,
             SLEEPY ~ bilr1 + bilr2 + bilr3 + 
               wilr1 + wilr2 + wilr3 + (1 | UID), 
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
psi3 <- gsi.buildilrBase(t(sbp3))

d[, PA := MVPA + LPA]
cilr3 <- compilr(d, sbp = sbp3, 
                parts = c("Sleep", "PA", "SB"), idvar = "UID")
m3 <- brmcoda(cilr3,
             SLEEPY ~ bilr1 + bilr2 + wilr1 + wilr2 + (1 | UID), 
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