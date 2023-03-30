library(data.table)
library(compositions)
library(multilevelTools)
library(multilevelcoda)
library(lme4)
library(brms)
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
             cores = 8, seed = 123)
summary(m5$Model)

# manual coding
dd <- cbind(
  tilr,
  bilr,
  wilr,
  d[, .(COPEExpZ, COPEPrcZ, COPEMenZ, COPEEACZ, COPE_APZ, COPE_ApprZ, COPE_AvoiZ, 
        SLEEPY, UID)])

# descriptives

# egltable(c("SLEEPY", "TST", "WAKE", "MVPA", "LPA", "SB"), data = dd)
table(dd$SLEEPY) # 1-5

# models - COPEEACZ COPEMenZ SLEEPY
# mt <- brm(COPE_ApprZ ~ ilr1 + ilr2 + ilr3 + ilr4 + (1 | UID), data = dd)
# summary(mt)

m0 <- brm(SLEEPY ~ bilr1 + bilr2 + bilr3 + bilr4 +
            wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), data = dd,
          cores = 8, seed = 123)
summary(m0)

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
             cores = 8, seed = 123)
summary(m4$Model)

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
             cores = 8, seed = 123)
summary(m3$Model)

# library(doFuture)
# registerDoFuture()
# plan(multisession, workers = 5)
# 
# subm <- substitution(m, delta = 1:60, 
#                      level = c("between", "within"), type = c("conditional", "marginal"))
# 
# registerDoSEQ()

# save results to guide sim study ------------

class5_result <- data.table(
  b0      = summary(m5)$fixed[1, 1],
  b_bilr1 = summary(m5)$fixed[2, 1],
  b_bilr2 = summary(m5)$fixed[3, 1],
  b_bilr3 = summary(m5)$fixed[4, 1],
  b_bilr4 = summary(m5)$fixed[5, 1],
  b_wilr1 = summary(m5)$fixed[6, 1],
  b_wilr2 = summary(m5)$fixed[7, 1],
  b_wilr3 = summary(m5)$fixed[8, 1],
  b_wilr4 = summary(m5)$fixed[9, 1],
  
  ll_b0    = summary(m5)$fixed[1, 3],
  ll_bilr1 = summary(m5 )$fixed[2, 3],
  ll_bilr2 = summary(m5)$fixed[3, 3],
  ll_bilr3 = summary(m5)$fixed[4, 3],
  ll_bilr4 = summary(m5)$fixed[5, 3],
  ll_wilr1 = summary(m5)$fixed[6, 3],
  ll_wilr2 = summary(m5)$fixed[7, 3],
  ll_wilr3 = summary(m5)$fixed[8, 3],
  ll_wilr4 = summary(m5)$fixed[9, 3],
  
  ul_b0    = summary(m5)$fixed[1, 4],
  ul_bilr1 = summary(m5)$fixed[2, 4],
  ul_bilr2 = summary(m5)$fixed[3, 4],
  ul_bilr3 = summary(m5)$fixed[4, 4],
  ul_bilr4 = summary(m5)$fixed[5, 4],
  ul_wilr1 = summary(m5)$fixed[6, 4],
  ul_wilr2 = summary(m5)$fixed[7, 4],
  ul_wilr3 = summary(m5)$fixed[8, 4],
  ul_wilr4 = summary(m5)$fixed[9, 4],
  
  u0    = summary(m5)$random$UID[1, 1],
  ll_u0 = summary(m5)$random$UID[1, 3],
  ul_u0 = summary(m5)$random$UID[1, 4],
  
  sigma = summary(m5)$spec_pars[[1]],
  ll_sigma = summary(m5)$spec_pars[[3]],
  ul_sigma = summary(m5)$spec_pars[[4]]
  )

# colMeans( as.data.frame(ranef(m0)$UID)[ , 1, drop = FALSE])
# ranef <- as.data.frame(ranef(m0)$UID)
# resid <- residuals(m0)
# resid_m <- colMeans(resid[, 1, drop = FALSE], )
# resid_sd <- sd(resid[, 1])
