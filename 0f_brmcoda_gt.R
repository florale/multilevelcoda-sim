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
shs <- as.data.table(readRDS("/Volumes/shared/Behavioral-med-lab/StressHealthStudy/SHS Research Interns/Data/shs_daily_ggir.RDS"))
destress <- readRDS("/Volumes/shared/Behavioral-med-lab/DESTRESSStudy/Data/destress_daily_ggir.RDS")
aces <- readRDS("/Volumes/shared/Behavioral-med-lab/ACESStudy/Data/aces_daily_ggir.RDS")

d <- rbind(shs[, .(ID, UID = paste0("S", ID), SurveyDay, Survey, USURVEYID,
                   COPE_Appr, SLEEPY, SLEEPYNextDay, 
                   TIBg, Sleepg, WAKEg, MVPAg, LPAg, SBg)],
           destress[, .(ID, UID = paste0("D", ID), SurveyDay, Survey, USURVEYID,
                        COPE_Appr, SLEEPY, SLEEPYNextDay,
                        TIBg, Sleepg, WAKEg, MVPAg, LPAg, SBg)],
           aces[, .(ID, UID = paste0("A", ID), SurveyDay, Survey, USURVEYID,
                    COPE_Appr, SLEEPY, SLEEPYNextDay,
                    TIBg, Sleepg, WAKEg, MVPAg, LPAg, SBg)])

d <- d[complete.cases(d[, .(Sleepg, WAKEg, MVPAg, LPAg, SBg)])]
d <- d[WAKEg > 0] # make sure there is no 0s
d <- d[Survey == "Evening"] # select only when SLEEPYNextDay is avail

parts <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")

# remove NAs and 0s
any(apply(d[, parts, with = FALSE], 2, function(x) x == 0))
any(apply(d[, parts, with = FALSE], 2, function(x) is.na(x)))
# make composition
sbp5 <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)


parts5 <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")
any(apply(d[, parts, with = FALSE], 2, function(x) x == 0))

d <- d[!is.na(Sleepg) & !is.na(WAKEg) & !is.na(MVPAg) & !is.na(LPAg) & !is.na(SBg)]


### 5 class ----------
# multilevelcoda
cilr5 <- compilr(d, sbp = sbp5,
                 parts = parts5, idvar = "UID", total = 1440)
m5 <- brmcoda(cilr5,
              SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + bilr4 +
               wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
)
summary(m5)

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
                parts = c("TIBg", "MVPAg", "LPAg", "SBg"), idvar = "UID", total = 1440)

m4 <- brmcoda(cilr4,
              SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + 
               wilr1 + wilr2 + wilr3 + (1 | UID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
             )
summary(m4)

### 3 class ----------
sbp3 <- matrix(c(
  1, -1,-1, 
  0, 1, -1), 
  ncol = 3, byrow = TRUE)

d[, PAg := MVPAg + LPAg]
cilr3 <- compilr(d, sbp = sbp3, 
                parts = c("TIBg", "PAg", "SBg"), idvar = "UID", total = 1440)

m3 <- brmcoda(cilr3,
              SLEEPYNextDay ~ bilr1 + bilr2 + wilr1 + wilr2 + (1 | UID), 
             cores = 4,
             chains = 4,
             iter = 3000,
             warmup = 500,
             seed = 123,
             backend = "cmdstanr"
)
summary(m3)

## save ------------------
brmcoda_gt <- list(
  m5 = m5,
  m4 = m4,
  m3 = m3
)
# saveRDS(brmcoda_gt, "brmcoda_gt.RDS")
