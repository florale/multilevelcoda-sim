#### Calculate Means & Covariance Matrix for multilevel compositional data simulation ####
library(data.table)
library(compositions)
library(multilevelTools)
library(MASS)

# sbp & psi
# sbp --------------------
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
psi <- gsi.buildilrBase(t(sbp))

sbp5 <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)

sbp4 <- matrix(c(
  1, -1, -1,-1,
  0, 1, -1, -1,
  0, 0, 1, -1), ncol = 4, byrow = TRUE)

sbp3 <- matrix(c(
  1, -1,-1, 
  0, 1, -1),ncol = 3, byrow = TRUE)

# read data ----------------------------------------------------------------------------------------
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

parts <- c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg")

d <- d[complete.cases(d[, .(Sleepg, WAKEg, MVPAg, LPAg, SBg)])]
d <- d[WAKEg > 0]

d[, c("BMVPAg") := mean(MVPAg, na.rm = TRUE), by = UID]
d[, c("BLPAg") := mean(LPAg, na.rm = TRUE), by = UID]
d[, c("BSBg") := mean(SBg, na.rm = TRUE), by = UID]
d[, c("BWAKEg") := mean(WAKEg, na.rm = TRUE), by = UID]
d[, c("BSleepg") := mean(Sleepg, na.rm = TRUE), by = UID]
d[, c("BTIBg") := mean(TIBg, na.rm = TRUE), by = UID]

## make within variable ratio of total to between
d[, WMVPAg := MVPAg / BMVPAg]
d[, WLPAg := LPAg / BLPAg]
d[, WSBg := SBg / BSBg]
d[, WWAKEg := WAKEg / BWAKEg]
d[, WSleepg := Sleepg / BSleepg]
d[, WTIBg := TIBg / BTIBg]

## hist(d[!duplicated(UID)]$Age)
## table(d[!duplicated(UID)]$Female)
## between sim comp  -------------------------------------------------------------------------------

bd_ilr <- ilr(acomp(d[, .(BSleepg, BWAKEg, BMVPAg, BLPAg, BSBg)]), V = psi)
means.b <- colMeans(bd_ilr, na.rm = TRUE)
cov.b <- cov(bd_ilr, use = "complete.obs")

# # sim comp
# bd <- acomp(bd)
# (m_bd <- mean.acomp(bd))
# (v_bd <- var.acomp(bd))

## within sim comp  --------------------------------------------------------------------------------
# sim ilr
wd_ilr <- ilr(acomp(d[, .(WSleepg, WWAKEg, WMVPAg, WLPAg, WSBg)]), V = psi)
means.w <- colMeans(wd_ilr, na.rm = TRUE)
cov.w <- cov(as.matrix(wd_ilr), use = "complete.obs")

# # sim comp
# td <- acomp(d[, .(TST, WAKE, MVPA, LPA, SB)])
# wd <- td - bd
# # wd <- acomp(wd)
# (m_wd <- mean.acomp(wd))
# (v_wd <- var.acomp(wd))

# save and load data --------
meanscovs <- list(
  BMeans = means.b,
  BCov = cov.b,
  WMeans = means.w,
  WCov = cov.w,
  compvars =  c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg"),
  sbp = sbp,
  psi = psi,
  sbp = sbp,
  sbp5 = sbp5,
  sbp4 = sbp4,
  sbp3 = sbp3
)

# meanscovs <- list(
#   BMeans = m_bd,
#   BCov = v_bd,
#   WMeans = m_wd,
#   WCov = v_wd,
#   compvars = c("Sleepg", "WAKEg", "MVPAg", "LPAg", "SBg"),
#   sbp = sbp,
#   psi = psi)

saveRDS(meanscovs, file = "meanscovs.RDS")

meanscovs <- readRDS("meanscovs.RDS")

# # check ------------------
# library(data.table)
# library(compositions)
# library(multilevelTools)
# library(MASS)
# 
# set.seed(1234)
# test <- with(meanscovs, rbind(
#   simulateData(
#     bm = BMeans, wm = WMeans,
#     bcov = BCov, wcov = WCov,
#     n = 2000, k = 50, psi = psi)))
#   
# plot(
#   quantile(d$Age, probs = seq(.01, .99, .01), na.rm = TRUE),
#   quantile(test$Age, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$TST, probs = seq(.01, .99, .01)),
#   quantile(test$TST, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$MVPA, probs = seq(.01, .99, .01)),
#   quantile(test$MVPA, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$WAKE, probs = seq(.01, .99, .01)),
#   quantile(test$WAKE, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# 
# # check compositional normality
# qqnorm(acomp(test[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]))
# qqnorm(acomp(test[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]))
# qqnorm(acomp(test[, .(TST, WAKE, MVPA, LPA, SB)]))
# 