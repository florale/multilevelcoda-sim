source("2C-simsum_brmcoda_out.R")
source("2D-simsum_sub_out.R")

## d3 fixed ----------------
b0_d3    <- as.data.table(tidy(summary(s_b0_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr1_d3 <- as.data.table(tidy(summary(s_bilr1_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr2_d3 <- as.data.table(tidy(summary(s_bilr2_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr1_d3 <- as.data.table(tidy(summary(s_wilr1_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr2_d3 <- as.data.table(tidy(summary(s_wilr2_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

b0_d3[, par := "b0"]
b0_d3[, Predictor := ""]
b0_d3[, Level := ""]

bilr1_d3[, par := "beta"]
bilr1_d3[, Predictor := "ilr1"]
bilr1_d3[, Level := "between"]

bilr2_d3[, par := "beta"]
bilr2_d3[, Predictor := "ilr2"]
bilr2_d3[, Level := "between"]

wilr1_d3[, par := "beta"]
wilr1_d3[, Predictor := "ilr1"]
wilr1_d3[, Level := "within"]

wilr2_d3[, par := "beta"]
wilr2_d3[, Predictor := "ilr2"]
wilr2_d3[, Level := "within"]

## d3 random ----------------
u0_base_d3  <- as.data.table(tidy(summary(s_u0_base_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_small_d3 <- as.data.table(tidy(summary(s_u0_small_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_large_d3 <- as.data.table(tidy(summary(s_u0_large_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

u0_base_d3[, par := "u0"]
u0_base_d3[, Predictor := ""]
u0_base_d3[, Level := ""]
u0_base_d3[, condition := NA]
u0_base_d3[, condition := ifelse(sigma_condition == "base", "base", condition)]
u0_base_d3[, condition := ifelse(sigma_condition == "small", "REbase_RESsmall", condition)]
u0_base_d3[, condition := ifelse(sigma_condition == "large", "REbase_RESlarge", condition)]
u0_base_d3[, condition := as.factor(condition)]

u0_small_d3[, par := "u0"]
u0_small_d3[, Predictor := ""]
u0_small_d3[, Level := ""]
u0_small_d3[, condition := "REsmall_RESlarge"]
u0_small_d3[, condition := as.factor(condition)]

u0_large_d3[, par := "u0"]
u0_large_d3[, Predictor := ""]
u0_large_d3[, Level := ""]
u0_large_d3[, condition := "RElarge_RESsmall"]
u0_large_d3[, condition := as.factor(condition)]

## d3 residual ----------------
sigma_base_d3  <- as.data.table(tidy(summary(s_sigma_base_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_small_d3 <- as.data.table(tidy(summary(s_sigma_small_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_large_d3 <- as.data.table(tidy(summary(s_sigma_large_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

sigma_base_d3[, par := "sigma"]
sigma_base_d3[, Predictor := ""]
sigma_base_d3[, Level := ""]
sigma_base_d3[, condition := "base"]
sigma_base_d3[, condition := as.factor(condition)]

sigma_small_d3[, par := "sigma"]
sigma_small_d3[, Predictor := ""]
sigma_small_d3[, Level := ""]
sigma_small_d3[, condition := NA]
sigma_small_d3[, condition := ifelse(u0_condition == "base", "REbase_RESsmall", condition)]
sigma_small_d3[, condition := ifelse(u0_condition == "large", "RElarge_RESsmall", condition)]
sigma_small_d3[, condition := as.factor(condition)]

sigma_large_d3[, par := "sigma"]
sigma_large_d3[, Predictor := ""]
sigma_large_d3[, Level := ""]
sigma_large_d3[, condition := NA]
sigma_large_d3[, condition := ifelse(u0_condition == "base", "REbase_RESlarge", condition)]
sigma_large_d3[, condition := ifelse(u0_condition == "small", "REsmall_RESlarge", condition)]
sigma_large_d3[, condition := as.factor(condition)]

## d3 brmcoda ---------------
brmcoda_d3 <- rbind(
  b0_d3[condition == "base"],
  bilr1_d3[condition == "base"],
  bilr2_d3[condition == "base"],
  wilr1_d3[condition == "base"],
  wilr2_d3[condition == "base"],
  u0_base_d3[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d3,
  
  b0_d3[condition == "REbase_RESsmall"],
  bilr1_d3[condition == "REbase_RESsmall"],
  bilr2_d3[condition == "REbase_RESsmall"],
  wilr1_d3[condition == "REbase_RESsmall"],
  wilr2_d3[condition == "REbase_RESsmall"],
  u0_base_d3[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d3[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d3[condition == "REbase_RESlarge"],
  bilr1_d3[condition == "REbase_RESlarge"],
  bilr2_d3[condition == "REbase_RESlarge"],
  wilr1_d3[condition == "REbase_RESlarge"],
  wilr2_d3[condition == "REbase_RESlarge"],
  u0_base_d3[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d3[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d3[condition == "REsmall_RESlarge"],
  bilr1_d3[condition == "REsmall_RESlarge"],
  bilr2_d3[condition == "REsmall_RESlarge"],
  wilr1_d3[condition == "REsmall_RESlarge"],
  wilr2_d3[condition == "REsmall_RESlarge"],
  u0_small_d3,
  sigma_large_d3[u0_condition == "small"][, -c("u0_condition")],
  
  b0_d3[condition == "RElarge_RESsmall"],
  bilr1_d3[condition == "RElarge_RESsmall"],
  bilr2_d3[condition == "RElarge_RESsmall"],
  wilr1_d3[condition == "RElarge_RESsmall"],
  wilr2_d3[condition == "RElarge_RESsmall"],
  u0_large_d3,
  sigma_small_d3[u0_condition == "large"][, -c("u0_condition")]
  )

# add by variable for plots
brmcoda_d3[, by := paste0(Level, " ", Predictor, " ", par)]
unique(brmcoda_d3$by)
brmcoda_d3[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

brmcoda_d3[, sd_ID_Intercept := NA_real_]
brmcoda_d3[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
brmcoda_d3[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
brmcoda_d3[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
brmcoda_d3[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
brmcoda_d3[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

brmcoda_d3[, sigma := NA_real_]
brmcoda_d3[, sigma := ifelse(condition == "base", 1, sigma)]
brmcoda_d3[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
brmcoda_d3[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
brmcoda_d3[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
brmcoda_d3[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]
brmcoda_d3[, D := 3]

## d3 sub ----------------
bsub_d3 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_sleep_pa_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "Sleep" & From == "PA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_sb_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_pa_sleep_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "PA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_pa_sb_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "PA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_sleep_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_pa_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "SB" & From == "PA",
                             .(To, From)])
))

wsub_d3 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_sleep_pa_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "Sleep" & From == "PA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_sb_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_pa_sleep_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "PA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_pa_sb_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "PA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_sleep_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_pa_d3), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d3[To == "SB" & From == "PA",
                             .(To, From)])
))

bsub_d3[, Level := "between"]
wsub_d3[, Level := "within"]

sub_d3 <- rbind(bsub_d3, wsub_d3)

sub_d3[, sd_ID_Intercept := NA_real_]
sub_d3[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
sub_d3[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
sub_d3[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
sub_d3[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
sub_d3[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

sub_d3[, sigma := NA_real_]
sub_d3[, sigma := ifelse(condition == "base", 1, sigma)]
sub_d3[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
sub_d3[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
sub_d3[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
sub_d3[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]

sub_d3[, D := 3]

sub_d3[, by := paste0(Level, " ", From, " - ", To)]
sub_d3[, by := factor(
  by,
  levels = c(
    "within SB - PA",
    "within SB - Sleep",
    "within PA - SB",
    "within PA - Sleep",
    "within Sleep - SB",
    "within Sleep - PA",
    
    "between SB - PA",
    "between SB - Sleep",
    "between PA - SB",
    "between PA - Sleep",
    "between Sleep - SB",
    "between Sleep - PA"
    
  )
)]

## d4 fixed----------------
b0_d4    <- as.data.table(tidy(summary(s_b0_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr1_d4 <- as.data.table(tidy(summary(s_bilr1_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr2_d4 <- as.data.table(tidy(summary(s_bilr2_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr3_d4 <- as.data.table(tidy(summary(s_bilr3_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr1_d4 <- as.data.table(tidy(summary(s_wilr1_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr2_d4 <- as.data.table(tidy(summary(s_wilr2_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr3_d4 <- as.data.table(tidy(summary(s_wilr3_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

b0_d4[, par := "b0"]
b0_d4[, Predictor := ""]
b0_d4[, Level := ""]

bilr1_d4[, par := "beta"]
bilr1_d4[, Predictor := "ilr1"]
bilr1_d4[, Level := "between"]

bilr2_d4[, par := "beta"]
bilr2_d4[, Predictor := "ilr2"]
bilr2_d4[, Level := "between"]

bilr3_d4[, par := "beta"]
bilr3_d4[, Predictor := "ilr3"]
bilr3_d4[, Level := "between"]

wilr1_d4[, par := "beta"]
wilr1_d4[, Predictor := "ilr1"]
wilr1_d4[, Level := "within"]

wilr2_d4[, par := "beta"]
wilr2_d4[, Predictor := "ilr2"]
wilr2_d4[, Level := "within"]

wilr3_d4[, par := "beta"]
wilr3_d4[, Predictor := "ilr3"]
wilr3_d4[, Level := "within"]

## d4 random ----------------
u0_base_d4  <- as.data.table(tidy(summary(s_u0_base_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_small_d4 <- as.data.table(tidy(summary(s_u0_small_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_large_d4 <- as.data.table(tidy(summary(s_u0_large_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

u0_base_d4[, par := "u0"]
u0_base_d4[, Predictor := ""]
u0_base_d4[, Level := ""]
u0_base_d4[, condition := NA]
u0_base_d4[, condition := ifelse(sigma_condition == "base", "base", condition)]
u0_base_d4[, condition := ifelse(sigma_condition == "small", "REbase_RESsmall", condition)]
u0_base_d4[, condition := ifelse(sigma_condition == "large", "REbase_RESlarge", condition)]
u0_base_d4[, condition := as.factor(condition)]

u0_small_d4[, par := "u0"]
u0_small_d4[, Predictor := ""]
u0_small_d4[, Level := ""]
u0_small_d4[, condition := "REsmall_RESlarge"]
u0_small_d4[, condition := as.factor(condition)]

u0_large_d4[, par := "u0"]
u0_large_d4[, Predictor := ""]
u0_large_d4[, Level := ""]
u0_large_d4[, condition := "RElarge_RESsmall"]
u0_large_d4[, condition := as.factor(condition)]

## d4 residual  ----------------
sigma_base_d4  <- as.data.table(tidy(summary(s_sigma_base_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_small_d4 <- as.data.table(tidy(summary(s_sigma_small_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_large_d4 <- as.data.table(tidy(summary(s_sigma_large_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

sigma_base_d4[, par := "sigma"]
sigma_base_d4[, Predictor := ""]
sigma_base_d4[, Level := ""]
sigma_base_d4[, condition := "base"]
sigma_base_d4[, condition := as.factor(condition)]

sigma_small_d4[, par := "sigma"]
sigma_small_d4[, Predictor := ""]
sigma_small_d4[, Level := ""]
sigma_small_d4[, condition := NA]
sigma_small_d4[, condition := ifelse(u0_condition == "base", "REbase_RESsmall", condition)]
sigma_small_d4[, condition := ifelse(u0_condition == "large", "RElarge_RESsmall", condition)]
sigma_small_d4[, condition := as.factor(condition)]

sigma_large_d4[, par := "sigma"]
sigma_large_d4[, Predictor := ""]
sigma_large_d4[, Level := ""]
sigma_large_d4[, condition := NA]
sigma_large_d4[, condition := ifelse(u0_condition == "base", "REbase_RESlarge", condition)]
sigma_large_d4[, condition := ifelse(u0_condition == "small", "REsmall_RESlarge", condition)]
sigma_large_d4[, condition := as.factor(condition)]

## d4 brmcoda ---------------
brmcoda_d4 <- rbind(
  b0_d4[condition == "base"],
  bilr1_d4[condition == "base"],
  bilr2_d4[condition == "base"],
  bilr3_d4[condition == "base"],
  wilr1_d4[condition == "base"],
  wilr2_d4[condition == "base"],
  wilr3_d4[condition == "base"],
  u0_base_d4[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d4,
  
  b0_d4[condition == "REbase_RESsmall"],
  bilr1_d4[condition == "REbase_RESsmall"],
  bilr2_d4[condition == "REbase_RESsmall"],
  bilr3_d4[condition == "REbase_RESsmall"],
  wilr1_d4[condition == "REbase_RESsmall"],
  wilr2_d4[condition == "REbase_RESsmall"],
  wilr3_d4[condition == "REbase_RESsmall"],
  u0_base_d4[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d4[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d4[condition == "REbase_RESlarge"],
  bilr1_d4[condition == "REbase_RESlarge"],
  bilr2_d4[condition == "REbase_RESlarge"],
  bilr3_d4[condition == "REbase_RESlarge"],
  wilr1_d4[condition == "REbase_RESlarge"],
  wilr2_d4[condition == "REbase_RESlarge"],
  wilr3_d4[condition == "REbase_RESlarge"],
  u0_base_d4[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d4[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d4[condition == "REsmall_RESlarge"],
  bilr1_d4[condition == "REsmall_RESlarge"],
  bilr2_d4[condition == "REsmall_RESlarge"],
  bilr3_d4[condition == "REsmall_RESlarge"],
  wilr1_d4[condition == "REsmall_RESlarge"],
  wilr2_d4[condition == "REsmall_RESlarge"],
  wilr3_d4[condition == "REsmall_RESlarge"],
  u0_small_d4,
  sigma_large_d4[u0_condition == "small"][, -c("u0_condition")],
  
  b0_d4[condition == "RElarge_RESsmall"],
  bilr1_d4[condition == "RElarge_RESsmall"],
  bilr2_d4[condition == "RElarge_RESsmall"],
  bilr3_d4[condition == "RElarge_RESsmall"],
  wilr1_d4[condition == "RElarge_RESsmall"],
  wilr2_d4[condition == "RElarge_RESsmall"],
  wilr3_d4[condition == "RElarge_RESsmall"],
  u0_large_d4,
  sigma_small_d4[u0_condition == "large"][, -c("u0_condition")]
)

# add by variable for plots
brmcoda_d4[, by := paste0(Level, " ", Predictor, " ", par)]
unique(brmcoda_d4$by)
brmcoda_d4[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]

brmcoda_d4[, sd_ID_Intercept := NA_real_]
brmcoda_d4[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
brmcoda_d4[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
brmcoda_d4[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
brmcoda_d4[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
brmcoda_d4[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

brmcoda_d4[, sigma := NA_real_]
brmcoda_d4[, sigma := ifelse(condition == "base", 1, sigma)]
brmcoda_d4[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
brmcoda_d4[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
brmcoda_d4[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
brmcoda_d4[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]

brmcoda_d4[, D := 4]

## d4 sub ---------
bsub_d4 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_sleep_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind( tidy(summary(s_bsub_sb_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
         substutitution_gt_d4[To == "SB" & From == "MVPA",
                              .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "SB" & From == "LPA",
                             .(To, From)])
))

wsub_d4 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_sleep_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sb_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_sleep_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind( tidy(summary(s_wsub_sb_mvpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
         substutitution_gt_d4[To == "SB" & From == "MVPA",
                              .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_lpa_d4), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d4[To == "SB" & From == "LPA",
                             .(To, From)])
))

bsub_d4[, Level := "between"]
wsub_d4[, Level := "within"]

sub_d4 <- rbind(bsub_d4, wsub_d4)

sub_d4[, sd_ID_Intercept := NA_real_]
sub_d4[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
sub_d4[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
sub_d4[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
sub_d4[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
sub_d4[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

sub_d4[, sigma := NA_real_]
sub_d4[, sigma := ifelse(condition == "base", 1, sigma)]
sub_d4[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
sub_d4[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
sub_d4[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
sub_d4[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]

sub_d4[, D := 4]

sub_d4[, by := paste0(Level, " ", From, " - ", To)]
sub_d4[, by := factor(
  by,
  levels = c(
    "within SB - LPA",
    "within SB - MVPA",
    "within SB - Sleep",
    "within LPA - SB",
    "within LPA - MVPA",
    "within LPA - Sleep",
    "within MVPA - SB",
    "within MVPA - LPA",
    "within MVPA - Sleep",
    "within Sleep - SB",
    "within Sleep - LPA",
    "within Sleep - MVPA",
    
    "between SB - LPA",
    "between SB - MVPA",
    "between SB - Sleep",
    "between LPA - SB",
    "between LPA - MVPA",
    "between LPA - Sleep",
    "between MVPA - SB",
    "between MVPA - LPA",
    "between MVPA - Sleep",
    "between Sleep - SB",
    "between Sleep - LPA",
    "between Sleep - MVPA"
  )
)]

## d5 fixed----------------
b0_d5    <- as.data.table(tidy(summary(s_b0_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr1_d5 <- as.data.table(tidy(summary(s_bilr1_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr2_d5 <- as.data.table(tidy(summary(s_bilr2_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr3_d5 <- as.data.table(tidy(summary(s_bilr3_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
bilr4_d5 <- as.data.table(tidy(summary(s_bilr4_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr1_d5 <- as.data.table(tidy(summary(s_wilr1_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr2_d5 <- as.data.table(tidy(summary(s_wilr2_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr3_d5 <- as.data.table(tidy(summary(s_wilr3_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
wilr4_d5 <- as.data.table(tidy(summary(s_wilr4_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

b0_d5[, par := "b0"]
b0_d5[, Predictor := ""]
b0_d5[, Level := ""]

bilr1_d5[, par := "beta"]
bilr1_d5[, Predictor := "ilr1"]
bilr1_d5[, Level := "between"]

bilr2_d5[, par := "beta"]
bilr2_d5[, Predictor := "ilr2"]
bilr2_d5[, Level := "between"]

bilr3_d5[, par := "beta"]
bilr3_d5[, Predictor := "ilr3"]
bilr3_d5[, Level := "between"]

bilr4_d5[, par := "beta"]
bilr4_d5[, Predictor := "ilr4"]
bilr4_d5[, Level := "between"]

wilr1_d5[, par := "beta"]
wilr1_d5[, Predictor := "ilr1"]
wilr1_d5[, Level := "within"]

wilr2_d5[, par := "beta"]
wilr2_d5[, Predictor := "ilr2"]
wilr2_d5[, Level := "within"]

wilr3_d5[, par := "beta"]
wilr3_d5[, Predictor := "ilr3"]
wilr3_d5[, Level := "within"]

wilr4_d5[, par := "beta"]
wilr4_d5[, Predictor := "ilr4"]
wilr4_d5[, Level := "within"]

## d5 random ----------------
u0_base_d5  <- as.data.table(tidy(summary(s_u0_base_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_small_d5 <- as.data.table(tidy(summary(s_u0_small_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
u0_large_d5 <- as.data.table(tidy(summary(s_u0_large_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

u0_base_d5[, par := "u0"]
u0_base_d5[, Predictor := ""]
u0_base_d5[, Level := ""]
u0_base_d5[, condition := NA]
u0_base_d5[, condition := ifelse(sigma_condition == "base", "base", condition)]
u0_base_d5[, condition := ifelse(sigma_condition == "small", "REbase_RESsmall", condition)]
u0_base_d5[, condition := ifelse(sigma_condition == "large", "REbase_RESlarge", condition)]
u0_base_d5[, condition := as.factor(condition)]

u0_small_d5[, par := "u0"]
u0_small_d5[, Predictor := ""]
u0_small_d5[, Level := ""]
u0_small_d5[, condition := "REsmall_RESlarge"]
u0_small_d5[, condition := as.factor(condition)]

u0_large_d5[, par := "u0"]
u0_large_d5[, Predictor := ""]
u0_large_d5[, Level := ""]
u0_large_d5[, condition := "RElarge_RESsmall"]
u0_large_d5[, condition := as.factor(condition)]

## d5 residual  ----------------
sigma_base_d5  <- as.data.table(tidy(summary(s_sigma_base_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_small_d5 <- as.data.table(tidy(summary(s_sigma_small_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))
sigma_large_d5 <- as.data.table(tidy(summary(s_sigma_large_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")))

sigma_base_d5[, par := "sigma"]
sigma_base_d5[, Predictor := ""]
sigma_base_d5[, Level := ""]
sigma_base_d5[, condition := "base"]
sigma_base_d5[, condition := as.factor(condition)]

sigma_small_d5[, par := "sigma"]
sigma_small_d5[, Predictor := ""]
sigma_small_d5[, Level := ""]
sigma_small_d5[, condition := NA]
sigma_small_d5[, condition := ifelse(u0_condition == "base", "REbase_RESsmall", condition)]
sigma_small_d5[, condition := ifelse(u0_condition == "large", "RElarge_RESsmall", condition)]
sigma_small_d5[, condition := as.factor(condition)]

sigma_large_d5[, par := "sigma"]
sigma_large_d5[, Predictor := ""]
sigma_large_d5[, Level := ""]
sigma_large_d5[, condition := NA]
sigma_large_d5[, condition := ifelse(u0_condition == "base", "REbase_RESlarge", condition)]
sigma_large_d5[, condition := ifelse(u0_condition == "small", "REsmall_RESlarge", condition)]
sigma_large_d5[, condition := as.factor(condition)]

## d5 brmcoda ---------------
brmcoda_d5 <- rbind(
  b0_d5[condition == "base"],
  bilr1_d5[condition == "base"],
  bilr2_d5[condition == "base"],
  bilr3_d5[condition == "base"],
  bilr4_d5[condition == "base"],
  wilr1_d5[condition == "base"],
  wilr2_d5[condition == "base"],
  wilr3_d5[condition == "base"],
  wilr4_d5[condition == "base"],
  u0_base_d5[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d5,
  
  b0_d5[condition == "REbase_RESsmall"],
  bilr1_d5[condition == "REbase_RESsmall"],
  bilr2_d5[condition == "REbase_RESsmall"],
  bilr3_d5[condition == "REbase_RESsmall"],
  bilr4_d5[condition == "REbase_RESsmall"],
  wilr1_d5[condition == "REbase_RESsmall"],
  wilr2_d5[condition == "REbase_RESsmall"],
  wilr3_d5[condition == "REbase_RESsmall"],
  wilr4_d5[condition == "REbase_RESsmall"],
  u0_base_d5[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d5[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d5[condition == "REbase_RESlarge"],
  bilr1_d5[condition == "REbase_RESlarge"],
  bilr2_d5[condition == "REbase_RESlarge"],
  bilr3_d5[condition == "REbase_RESlarge"],
  bilr4_d5[condition == "REbase_RESlarge"],
  wilr1_d5[condition == "REbase_RESlarge"],
  wilr2_d5[condition == "REbase_RESlarge"],
  wilr3_d5[condition == "REbase_RESlarge"],
  wilr4_d5[condition == "REbase_RESlarge"],
  u0_base_d5[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d5[u0_condition == "base"][, -c("u0_condition")],
  
  b0_d5[condition == "REsmall_RESlarge"],
  bilr1_d5[condition == "REsmall_RESlarge"],
  bilr2_d5[condition == "REsmall_RESlarge"],
  bilr3_d5[condition == "REsmall_RESlarge"],
  bilr4_d5[condition == "REsmall_RESlarge"],
  wilr1_d5[condition == "REsmall_RESlarge"],
  wilr2_d5[condition == "REsmall_RESlarge"],
  wilr3_d5[condition == "REsmall_RESlarge"],
  wilr4_d5[condition == "REsmall_RESlarge"],
  u0_small_d5,
  sigma_large_d5[u0_condition == "small"][, -c("u0_condition")],
  
  b0_d5[condition == "RElarge_RESsmall"],
  bilr1_d5[condition == "RElarge_RESsmall"],
  bilr2_d5[condition == "RElarge_RESsmall"],
  bilr3_d5[condition == "RElarge_RESsmall"],
  bilr4_d5[condition == "RElarge_RESsmall"],
  wilr1_d5[condition == "RElarge_RESsmall"],
  wilr2_d5[condition == "RElarge_RESsmall"],
  wilr3_d5[condition == "RElarge_RESsmall"],
  wilr4_d5[condition == "RElarge_RESsmall"],
  u0_large_d5,
  sigma_small_d5[u0_condition == "large"][, -c("u0_condition")]
)

# add by variable for plots
brmcoda_d5[, by := paste0(Level, " ", Predictor, " ", par)]
unique(brmcoda_d5$by)
brmcoda_d5[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "between ilr3 beta", "within ilr3 beta",
    "between ilr4 beta", "within ilr4 beta",
    "  sigma"
  )
)]

brmcoda_d5[, sd_ID_Intercept := NA_real_]
brmcoda_d5[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
brmcoda_d5[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
brmcoda_d5[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
brmcoda_d5[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
brmcoda_d5[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

brmcoda_d5[, sigma := NA_real_]
brmcoda_d5[, sigma := ifelse(condition == "base", 1, sigma)]
brmcoda_d5[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
brmcoda_d5[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
brmcoda_d5[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
brmcoda_d5[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]

brmcoda_d5[, D := 5]

## d5 sub ---------
bsub_d5 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_tst_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "WAKE",
                             .(To, From)]),
  cbind( tidy(summary(s_bsub_mvpa_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
         substutitution_gt_d5[To == "MVPA" & From == "LPA",
                              .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "LPA",
                             .(To, From)])
))

wsub_d5 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_tst_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "TST" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "WAKE" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "WAKE",
                             .(To, From)]),
  cbind( tidy(summary(s_wsub_mvpa_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
         substutitution_gt_d5[To == "MVPA" & From == "LPA",
                              .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sb_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_tst_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_wake_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_mvpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_lpa_d5), stats = c("bias", "becover", "cover", "empse", "mse", "power")),
        substutitution_gt_d5[To == "SB" & From == "LPA",
                             .(To, From)])
))

bsub_d5[, Level := "between"]
wsub_d5[, Level := "within"]

sub_d5 <- rbind(bsub_d5, wsub_d5)

sub_d5[, sd_ID_Intercept := NA_real_]
sub_d5[, sd_ID_Intercept := ifelse(condition == "base", 1, sd_ID_Intercept)]
sub_d5[, sd_ID_Intercept := ifelse(condition == "REbase_RESsmall", 1, sd_ID_Intercept)]
sub_d5[, sd_ID_Intercept := ifelse(condition == "REbase_RESlarge", 1, sd_ID_Intercept)]
sub_d5[, sd_ID_Intercept := ifelse(condition == "REsmall_RESlarge", sqrt(0.5), sd_ID_Intercept)]
sub_d5[, sd_ID_Intercept := ifelse(condition == "RElarge_RESsmall", sqrt(1.5), sd_ID_Intercept)]

sub_d5[, sigma := NA_real_]
sub_d5[, sigma := ifelse(condition == "base", 1, sigma)]
sub_d5[, sigma := ifelse(condition == "REbase_RESsmall", sqrt(0.5), sigma)]
sub_d5[, sigma := ifelse(condition == "REbase_RESlarge", sqrt(1.5), sigma)]
sub_d5[, sigma := ifelse(condition == "REsmall_RESlarge", sqrt(1.5), sigma)]
sub_d5[, sigma := ifelse(condition == "RElarge_RESsmall", sqrt(0.5), sigma)]

sub_d5[, D := 5]

sub_d5[, by := paste0(Level, " ", From, " - ", To)]
sub_d5[, by := factor(
  by,
  levels = c(
    "within SB - LPA",
    "within SB - MVPA",
    "within SB - WAKE",
    "within SB - TST",
    "within LPA - SB",
    "within LPA - MVPA",
    "within LPA - WAKE",
    "within LPA - TST",
    "within MVPA - SB",
    "within MVPA - LPA",
    "within MVPA - WAKE",
    "within MVPA - TST",
    "within WAKE - SB",
    "within WAKE - LPA",
    "within WAKE - MVPA",
    "within WAKE - TST",
    "within TST - SB",
    "within TST - LPA",
    "within TST - WAKE",
    "within TST - MVPA",
    
    "between SB - LPA",
    "between SB - MVPA",
    "between SB - WAKE",
    "between SB - TST",
    "between LPA - SB",
    "between LPA - MVPA",
    "between LPA - WAKE",
    "between LPA - TST",
    "between MVPA - SB",
    "between MVPA - LPA",
    "between MVPA - WAKE",
    "between MVPA - TST",
    "between WAKE - SB",
    "between WAKE - LPA",
    "between WAKE - MVPA",
    "between WAKE - TST",
    "between TST - SB",
    "between TST - LPA",
    "between TST - WAKE",
    "between TST - MVPA"
  )
)]

## save brmcoda data for shiny tables  -----------
brmcoda_tab <- rbind(brmcoda_d3,
                     brmcoda_d4,
                     brmcoda_d5)

# brmcoda_tab[] <- as.data.table(lapply(brmcoda_tab, function(j) if(is.numeric(j)) format(round(j, 2), nsmall = 2) else j))

brmcoda_tab[, Est   := format(round(est, 2), nsmall = 2)]
brmcoda_tab[, Lower := format(round(lower, 2), nsmall = 2)]
brmcoda_tab[, Upper := format(round(upper, 2), nsmall = 2)]
brmcoda_tab[, MCSE := format(round(mcse, 2), nsmall = 2)]
brmcoda_tab[, `$\\sigma_u$` := format(round(sd_ID_Intercept, 2), nsmall = 2)]
brmcoda_tab[, `$\\sigma_{\varepsilon}$` := format(round(sigma, 2), nsmall = 2)]

brmcoda_tab[, Estimates := paste0(Est, " [", Lower, ",", Upper, "]")]
# brmcoda_tab[, Estimates := paste0(Est, ", ", MCSE)]
brmcoda_tab[, `Est [95% CI], MCSE` := paste0(Est, " [", Lower, ",", Upper, "]", ", ", MCSE)]

brmcoda_tab[, Estimand := NA]
brmcoda_tab[, Estimand := ifelse(by == "  b0"             , "b_Intercept", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "between ilr1 beta", "b_bilr1", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "between ilr2 beta", "b_bilr2", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "between ilr3 beta", "b_bilr3", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "between ilr4 beta", "b_bilr4", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "within ilr1 beta" , "b_wilr1", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "within ilr2 beta" , "b_wilr2", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "within ilr3 beta" , "b_wilr3", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "within ilr4 beta" , "b_wilr4", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "  u0"             , "sd_ID_Intercept", Estimand)]
brmcoda_tab[, Estimand := ifelse(by == "  sigma"          , "sigma", Estimand)]
brmcoda_tab[, Estimand := factor(Estimand, levels = c(
  "sigma",
  "sd_ID_Intercept", 
  "b_wilr4", "b_wilr3", "b_wilr2", "b_wilr1",
  "b_bilr4", "b_bilr3", "b_bilr2", "b_bilr1",
  "b_Intercept"
))]
brmcoda_tab[, Parameter := Estimand]

brmcoda_tab[, EstimandF := NA]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_Intercept"    , "$\\gamma_{0}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_bilr1"        , "$\\beta{z^{(b)}_{1}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_bilr2"        , "$\\beta{z^{(b)}_{2}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_bilr3"        , "$\\beta{z^{(b)}_{3}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_bilr4"        , "$\\beta{z^{(b)}_{4}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_wilr1"        , "$\\beta{z^{(w)}_{1}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_wilr2"        , "$\\beta{z^{(w)}_{2}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_wilr3"        , "$\\beta{z^{(w)}_{3}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "b_wilr4"        , "$\\beta{z^{(w)}_{4}}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "sd_ID_Intercept", "$\\sigma_{u}$", EstimandF)]
brmcoda_tab[, EstimandF := ifelse(Estimand == "sigma"          , "$\\sigma_{\\epsilon}$", EstimandF)]
# brmcoda_tab[, EstimandF := factor(EstimandF, levels = c(
#   "$\\sigma_{\\varepsilon}$", 
#   "$\\sigma_{u}$",
#   "\\beta_{(wilr4)}$",  "\\beta_{(wilr3)}$", "\\beta_{(wilr2)}$","\\beta_{(wilr1)}$",
#   "\\beta_{(bilr4)}$", "\\beta_{(bilr3)}$", "\\beta_{(bilr2)}$", "\\beta_{(bilr1)}$",
#   "$\\beta_{0}$"
# ))]

brmcoda_tab[, OnTarget := NA]
brmcoda_tab[stat == "bias", OnTarget := ifelse(data.table::between(0, lower, upper, incbounds = TRUE), TRUE, FALSE)]
brmcoda_tab[stat == "cover", OnTarget := ifelse(data.table::between(0.95, lower, upper, incbounds = TRUE), TRUE, FALSE)]
brmcoda_tab[stat == "becover", OnTarget := ifelse(data.table::between(0.95, lower, upper, incbounds = TRUE), TRUE, FALSE)]
# brmcoda_tab[stat == "mse", OnTarget := ifelse(data.table::between(0, lower, upper), TRUE, FALSE)]
# brmcoda_tab[stat == "empse", OnTarget := ifelse(data.table::between(0, lower, upper), TRUE, FALSE)]
brmcoda_tab[, OnTarget := ifelse(OnTarget == TRUE, "Y", "N")]

brmcoda_tab[, NK := paste0("N: ", N, ", K: ", K)]
brmcoda_tab[, NK := factor(NK, levels = c("N: 30, K: 3",
                                          "N: 30, K: 5",
                                          "N: 30, K: 7",
                                          "N: 30, K: 14",
                                          
                                          "N: 50, K: 3",
                                          "N: 50, K: 5",
                                          "N: 50, K: 7",
                                          "N: 50, K: 14",
                                          
                                          "N: 360, K: 3",
                                          "N: 360, K: 5",
                                          "N: 360, K: 7",
                                          "N: 360, K: 14",
                                          
                                          "N: 1200, K: 3",
                                          "N: 1200, K: 5",
                                          "N: 1200, K: 7",
                                          "N: 1200, K: 14"))]

brmcoda_tab[, I := K]
brmcoda_tab[, J := N]
brmcoda_tab[, JI := paste0("J: ", J, ", I: ", I)]
brmcoda_tab[, JI := factor(JI, levels = c("J: 30, I: 3",
                                          "J: 30, I: 5",
                                          "J: 30, I: 7",
                                          "J: 30, I: 14",
                                          
                                          "J: 50, I: 3",
                                          "J: 50, I: 5",
                                          "J: 50, I: 7",
                                          "J: 50, I: 14",
                                          
                                          "J: 360, I: 3",
                                          "J: 360, I: 5",
                                          "J: 360, I: 7",
                                          "J: 360, I: 14",
                                          
                                          "J: 1200, I: 3",
                                          "J: 1200, I: 5",
                                          "J: 1200, I: 7",
                                          "J: 1200, I: 14"))]

brmcoda_tab[, cond := paste0("J: ", J, ", ", "I: ", I, ", ", "sigma: ", condition)]

brmcoda_tab <- brmcoda_tab[stat %in% c("bias", "cover", "becover")]
setnames(brmcoda_tab, "stat", "Stat")

# merge diag stats
simsum_brmcoda_tab <- brmcoda_tab[, .(Stat, cond, D, JI, I, J, Parameter, Estimand, EstimandF, Estimates)]
out <- list()
out <- lapply(c("bias", "cover", "becover"), function(x){
  out[[x]] <- reshape(simsum_brmcoda_tab[Stat == x], idvar = c("Parameter", "Estimand", "EstimandF", "cond", "D", "JI", "I", "J"), timevar = "Stat", direction = "wide")
})
simsum_brmcoda_tab <- mergeDTs(out, by = c("Parameter", "Estimand", "EstimandF", "cond", "D", "JI", "I", "J"))
setnames(simsum_brmcoda_tab, "Estimates.bias", "Bias")
setnames(simsum_brmcoda_tab, "Estimates.cover", "Coverage")
setnames(simsum_brmcoda_tab, "Estimates.becover", "Bias-Eliminated Coverage")
setnames(simsum_brmcoda_tab, "cond", "condition")

simsum_brmcoda_tab <- merge(simsum_brmcoda_tab, simsum_diag[Estimand != "ndt"],
                            by.x = c("condition", "Estimand", "D"), by.y = c("condition", "Estimand", "D"), all = TRUE)

simsum_brmcoda_tab[, Estimand := factor(Estimand, levels = c(
  "sigma",
  "sd_ID_Intercept", 
  "b_wilr4", "b_wilr3", "b_wilr2", "b_wilr1",
  "b_bilr4", "b_bilr3", "b_bilr2", "b_bilr1",
  "b_Intercept"
))]

saveRDS(simsum_brmcoda_tab, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_tabb.RDS")

## save all brmcoda dat for plots -----------
brmcoda_dat <- split(brmcoda_tab, by = "D")
names(brmcoda_dat) <- c("brmcoda_d3", "brmcoda_d4","brmcoda_d5")

saveRDS(brmcoda_dat, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_dat.RDS")

## save all sub dat for shiny tables -------------------------
sub_tab <- rbind(sub_d3,
                 sub_d4,
                 sub_d5)

# sub_tab[] <- as.data.table(lapply(sub_tab, function(j) if(is.numeric(j)) format(round(j, 2), nsmall = 2) else j))

sub_tab[, Est   := format(round(est, 2), nsmall = 2)]
sub_tab[, Lower := format(round(lower, 2), nsmall = 2)]
sub_tab[, Upper := format(round(upper, 2), nsmall = 2)]
sub_tab[, MCSE := format(round(mcse, 2), nsmall = 2)]
sub_tab[, `$\\sigma_u$` := format(round(sd_ID_Intercept, 2), nsmall = 2)]
sub_tab[, `$\\sigma_{\varepsilon}$` := format(round(sigma, 2), nsmall = 2)]

sub_tab[, Estimates := paste0(Est, " [", Lower, ",", Upper, "]")]
# sub_tab[, Estimates := paste0(Est, ", ", MCSE)]
sub_tab[, `Est [95% CI], MCSE` := paste0(Est, " [", Lower, ",", Upper, "]", ", ", MCSE)]

sub_tab[, OnTarget := NA]
sub_tab[stat == "bias", OnTarget := ifelse(data.table::between(0, lower, upper, incbounds = TRUE), TRUE, FALSE)]
sub_tab[stat == "cover", OnTarget := ifelse(data.table::between(0.95, lower, upper, incbounds = TRUE), TRUE, FALSE)]
sub_tab[stat == "becover", OnTarget := ifelse(data.table::between(0.95, lower, upper, incbounds = TRUE), TRUE, FALSE)]
sub_tab[, OnTarget := ifelse(OnTarget == TRUE, "Y", "N")]
sub_tab[, Substitution := by]

sub_tab[, I := K]
sub_tab[, J := N]
sub_tab[, NK := paste0("N: ", N, ", K: ", K)]
sub_tab[, NK := factor(NK, levels = c("N: 30, K: 3",
                                      "N: 30, K: 5",
                                      "N: 30, K: 7",
                                      "N: 30, K: 14",
                                      
                                      "N: 50, K: 3",
                                      "N: 50, K: 5",
                                      "N: 50, K: 7",
                                      "N: 50, K: 14",
                                      
                                      "N: 360, K: 3",
                                      "N: 360, K: 5",
                                      "N: 360, K: 7",
                                      "N: 360, K: 14",
                                      
                                      "N: 1200, K: 3",
                                      "N: 1200, K: 5",
                                      "N: 1200, K: 7",
                                      "N: 1200, K: 14"))]

sub_tab[, JI := paste0("J: ", J, ", I: ", I)]
sub_tab[, JI := factor(JI, levels = c("J: 30, I: 3",
                                      "J: 30, I: 5",
                                      "J: 30, I: 7",
                                      "J: 30, I: 14",
                                      
                                      "J: 50, I: 3",
                                      "J: 50, I: 5",
                                      "J: 50, I: 7",
                                      "J: 50, I: 14",
                                      
                                      "J: 360, I: 3",
                                      "J: 360, I: 5",
                                      "J: 360, I: 7",
                                      "J: 360, I: 14",
                                      
                                      "J: 1200, I: 3",
                                      "J: 1200, I: 5",
                                      "J: 1200, I: 7",
                                      "J: 1200, I: 14"))]

sub_tab[, cond := paste0("J: ", J, ", ", "I: ", I, ", ", "sigma: ", condition)]

sub_tab[, Estimand := NA]
sub_tab[, Estimand := ifelse(by == "between Sleep - PA"  , "$\\Delta{\\hat{y}^{(b)}_{(Sleep - PA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within Sleep - PA"   , "$\\Delta{\\hat{y}^{(w)}_{(Sleep - PA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between Sleep - SB"  , "$\\Delta{\\hat{y}^{(b)}_{(Sleep - SB)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within Sleep - SB"   , "$\\Delta{\\hat{y}^{(w)}_{(Sleep - SB)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between PA - Sleep"  , "$\\Delta{\\hat{y}^{(b)}_{(PA - Sleep)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within PA - Sleep"   , "$\\Delta{\\hat{y}^{(w)}_{(PA - Sleep)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between PA - SB"     , "$\\Delta{\\hat{y}^{(b)}_{(PA - SB)}}$"     , Estimand)]
sub_tab[, Estimand := ifelse(by == "within PA - SB"      , "$\\Delta{\\hat{y}^{(w)}_{(PA - SB)}}$"     , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - Sleep"  , "$\\Delta{\\hat{y}^{(b)}_{(SB - Sleep)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - Sleep"   , "$\\Delta{\\hat{y}^{(w)}_{(SB - Sleep)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - PA"     , "$\\Delta{\\hat{y}^{(b)}_{(SB - PA)}}$"     , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - PA"      , "$\\Delta{\\hat{y}^{(w)}_{(SB - PA)}}$"     , Estimand)]
sub_tab[, Estimand := ifelse(by == "between Sleep - MVPA", "$\\Delta{\\hat{y}^{(b)}_{(Sleep - MVPA)}}$", Estimand)]
sub_tab[, Estimand := ifelse(by == "within Sleep - MVPA" , "$\\Delta{\\hat{y}^{(w)}_{(Sleep - MVPA)}}$", Estimand)]
sub_tab[, Estimand := ifelse(by == "between Sleep - LPA" , "$\\Delta{\\hat{y}^{(b)}_{(Sleep - LPA)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "within Sleep - LPA"  , "$\\Delta{\\hat{y}^{(w)}_{(Sleep - LPA)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "between MVPA - Sleep", "$\\Delta{\\hat{y}^{(b)}_{(MVPA - Sleep)}}$", Estimand)]
sub_tab[, Estimand := ifelse(by == "within MVPA - Sleep" , "$\\Delta{\\hat{y}^{(w)}_{(MVPA - Sleep)}}$", Estimand)]
sub_tab[, Estimand := ifelse(by == "between MVPA - LPA"  , "$\\Delta{\\hat{y}^{(b)}_{(MVPA - LPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within MVPA - LPA"   , "$\\Delta{\\hat{y}^{(w)}_{(MVPA - LPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between MVPA - SB"   , "$\\Delta{\\hat{y}^{(b)}_{(MVPA - SB)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within MVPA - SB"    , "$\\Delta{\\hat{y}^{(w)}_{(MVPA - SB)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "between LPA - Sleep" , "$\\Delta{\\hat{y}^{(b)}_{(LPA - Sleep)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "within LPA - Sleep"  , "$\\Delta{\\hat{y}^{(w)}_{(LPA - Sleep)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "between LPA - MVPA"  , "$\\Delta{\\hat{y}^{(b)}_{(LPA - MVPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within LPA - MVPA"   , "$\\Delta{\\hat{y}^{(w)}_{(LPA - MVPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between LPA - SB"    , "$\\Delta{\\hat{y}^{(b)}_{(LPA - SB)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "within LPA - SB"     , "$\\Delta{\\hat{y}^{(w)}_{(LPA - SB)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - MVPA"   , "$\\Delta{\\hat{y}^{(b)}_{(SB - MVPA)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - MVPA"    , "$\\Delta{\\hat{y}^{(w)}_{(SB - MVPA)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - LPA"    , "$\\Delta{\\hat{y}^{(b)}_{(SB - LPA)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - LPA"     , "$\\Delta{\\hat{y}^{(w)}_{(SB - LPA)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "between TST - MVPA"  , "$\\Delta{\\hat{y}^{(b)}_{(TST - MVPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within TST - MVPA"   , "$\\Delta{\\hat{y}^{(w)}_{(TST - MVPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between TST - WAKE"  , "$\\Delta{\\hat{y}^{(b)}_{(TST - WAKE)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within TST - WAKE"   , "$\\Delta{\\hat{y}^{(w)}_{(TST - WAKE)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between TST - LPA"   , "$\\Delta{\\hat{y}^{(b)}_{(TST - LPA)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within TST - LPA"    , "$\\Delta{\\hat{y}^{(w)}_{(TST - LPA)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "between TST - SB"    , "$\\Delta{\\hat{y}^{(b)}_{(TST - SB)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "within TST - SB"     , "$\\Delta{\\hat{y}^{(w)}_{(TST - SB)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "between WAKE - TST"  , "$\\Delta{\\hat{y}^{(b)}_{(WAKE - TST)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within WAKE - TST"   , "$\\Delta{\\hat{y}^{(w)}_{(WAKE - TST)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between WAKE - MVPA" , "$\\Delta{\\hat{y}^{(b)}_{(WAKE - MVPA)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "within WAKE - MVPA"  , "$\\Delta{\\hat{y}^{(w)}_{(WAKE - MVPA)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "between WAKE - LPA"  , "$\\Delta{\\hat{y}^{(b)}_{(WAKE - LPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within WAKE - LPA"   , "$\\Delta{\\hat{y}^{(w)}_{(WAKE - LPA)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between WAKE - SB"   , "$\\Delta{\\hat{y}^{(b)}_{(WAKE - SB)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within WAKE - SB"    , "$\\Delta{\\hat{y}^{(w)}_{(WAKE - SB)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "between MVPA - TST"  , "$\\Delta{\\hat{y}^{(b)}_{(MVPA - TST)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within MVPA - TST"   , "$\\Delta{\\hat{y}^{(w)}_{(MVPA - TST)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between MVPA - WAKE" , "$\\Delta{\\hat{y}^{(b)}_{(MVPA - WAKE)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "within MVPA - WAKE"  , "$\\Delta{\\hat{y}^{(w)}_{(MVPA - WAKE)}}$" , Estimand)]
sub_tab[, Estimand := ifelse(by == "between LPA - TST"   , "$\\Delta{\\hat{y}^{(b)}_{(LPA - TST)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within LPA - TST"    , "$\\Delta{\\hat{y}^{(w)}_{(LPA - TST)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "between LPA - WAKE"  , "$\\Delta{\\hat{y}^{(b)}_{(LPA - WAKE)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "within LPA - WAKE"   , "$\\Delta{\\hat{y}^{(w)}_{(LPA - WAKE)}}$"  , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - TST"    , "$\\Delta{\\hat{y}^{(b)}_{(SB - TST)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - TST"     , "$\\Delta{\\hat{y}^{(w)}_{(SB - TST)}}$"    , Estimand)]
sub_tab[, Estimand := ifelse(by == "between SB - WAKE"   , "$\\Delta{\\hat{y}^{(b)}_{(SB - WAKE)}}$"   , Estimand)]
sub_tab[, Estimand := ifelse(by == "within SB - WAKE"    , "$\\Delta{\\hat{y}^{(w)}_{(SB - WAKE)}}$"   , Estimand)]

sub_tab[, Parameter := Estimand]

sub_tab <- sub_tab[stat %in% c("bias", "cover", "becover")]
setnames(sub_tab, "stat", "Stat")

saveRDS(sub_tab, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_tab.RDS")

## subset sub dat for plots --------
sub_dat <- split(sub_tab, by = "D")
names(sub_dat) <- c("sub_d3", "sub_d4", "sub_d5")

sub_dat[["sub_d3"]] <- sub_dat[["sub_d3"]][by %in% c(
  "within PA - SB",
  "within Sleep - SB",
  "within Sleep - PA",
  
  "between PA - SB",
  "between Sleep - SB",
  "between Sleep - PA"
)]
sub_dat[["sub_d3"]][, Substitution := droplevels(Substitution)]
sub_dat[["sub_d3"]][, Substitution := factor(
  Substitution,
  levels = c(
    "within PA - SB",
    "within Sleep - SB",
    "within Sleep - PA",
    
    "between PA - SB",
    "between Sleep - SB",
    "between Sleep - PA"
  )
)]   
    
sub_dat[["sub_d4"]] <- sub_dat[["sub_d4"]][by %in% c(
  "within LPA - SB",
  "within MVPA - SB",
  "within MVPA - LPA",
  "within Sleep - SB",
  "within Sleep - LPA",
  "within Sleep - MVPA",
  
  "between LPA - SB",
  "between MVPA - SB",
  "between MVPA - LPA",
  "between Sleep - SB",
  "between Sleep - LPA",
  "between Sleep - MVPA"
)]
sub_dat[["sub_d4"]][, Substitution := droplevels(Substitution)]
sub_dat[["sub_d4"]][, Substitution := factor(
  Substitution,
  levels = c(
    "within LPA - SB",
    "within MVPA - SB",
    "within MVPA - LPA",
    "within Sleep - SB",
    "within Sleep - LPA",
    "within Sleep - MVPA",
    
    "between LPA - SB",
    "between MVPA - SB",
    "between MVPA - LPA",
    "between Sleep - SB",
    "between Sleep - LPA",
    "between Sleep - MVPA"
  )
)]
sub_dat[["sub_d5"]] <- sub_dat[["sub_d5"]][by %in% c(
  "within LPA - SB",
  "within MVPA - SB",
  "within MVPA - LPA",
  "within WAKE - SB",
  "within WAKE - LPA",
  "within WAKE - MVPA",
  "within TST - SB",
  "within TST - LPA",
  "within TST - WAKE",
  "within TST - MVPA",
  
  "between LPA - SB",
  "between MVPA - SB",
  "between MVPA - LPA",
  "between WAKE - SB",
  "between WAKE - LPA",
  "between WAKE - MVPA",
  "between TST - SB",
  "between TST - LPA",
  "between TST - WAKE",
  "between TST - MVPA"
)]
sub_dat[["sub_d5"]][, Substitution := droplevels(Substitution)]
sub_dat[["sub_d5"]][, Substitution := factor(
  Substitution,
  levels = c(
    "within LPA - SB",
    "within MVPA - SB",
    "within MVPA - LPA",
    "within WAKE - SB",
    "within WAKE - LPA",
    "within WAKE - MVPA",
    "within TST - SB",
    "within TST - LPA",
    "within TST - WAKE",
    "within TST - MVPA",
    
    "between LPA - SB",
    "between MVPA - SB",
    "between MVPA - LPA",
    "between WAKE - SB",
    "between WAKE - LPA",
    "between WAKE - MVPA",
    "between TST - SB",
    "between TST - LPA",
    "between TST - WAKE",
    "between TST - MVPA"
  )
)]

saveRDS(sub_dat, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_dat.RDS")
