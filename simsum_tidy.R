## d3 fixed ----------------
b0_d3    <- as.data.table(tidy(summary(s_b0_d3), stats = c("bias", "becover")))
bilr1_d3 <- as.data.table(tidy(summary(s_bilr1_d3), stats = c("bias", "becover")))
bilr2_d3 <- as.data.table(tidy(summary(s_bilr2_d3), stats = c("bias", "becover")))
wilr1_d3 <- as.data.table(tidy(summary(s_wilr1_d3), stats = c("bias", "becover")))
wilr2_d3 <- as.data.table(tidy(summary(s_wilr2_d3), stats = c("bias", "becover")))

b0_d3[, Parameter := "b0"]
b0_d3[, Predictor := ""]
b0_d3[, Level := ""]

bilr1_d3[, Parameter := "beta"]
bilr1_d3[, Predictor := "ilr1"]
bilr1_d3[, Level := "between"]

bilr2_d3[, Parameter := "beta"]
bilr2_d3[, Predictor := "ilr2"]
bilr2_d3[, Level := "between"]

wilr1_d3[, Parameter := "beta"]
wilr1_d3[, Predictor := "ilr1"]
wilr1_d3[, Level := "within"]

wilr2_d3[, Parameter := "beta"]
wilr2_d3[, Predictor := "ilr2"]
wilr2_d3[, Level := "within"]

## d3 random ----------------
u0_base_d3  <- as.data.table(tidy(summary(s_u0_base_d3), stats = c("bias", "becover")))
u0_small_d3 <- as.data.table(tidy(summary(s_u0_small_d3), stats = c("bias", "becover")))
u0_large_d3 <- as.data.table(tidy(summary(s_u0_large_d3), stats = c("bias", "becover")))

u0_base_d3[, Parameter := "u0"]
u0_base_d3[, Predictor := ""]
u0_base_d3[, Level := ""]

u0_small_d3[, Parameter := "u0"]
u0_small_d3[, Predictor := ""]
u0_small_d3[, Level := ""]

u0_large_d3[, Parameter := "u0"]
u0_large_d3[, Predictor := ""]
u0_large_d3[, Level := ""]

## d3 residual ----------------
sigma_base_d3  <- as.data.table(tidy(summary(s_sigma_base_d3), stats = c("bias", "becover")))
sigma_small_d3 <- as.data.table(tidy(summary(s_sigma_small_d3), stats = c("bias", "becover")))
sigma_large_d3 <- as.data.table(tidy(summary(s_sigma_large_d3), stats = c("bias", "becover")))

sigma_base_d3[, Parameter := "sigma"]
sigma_base_d3[, Predictor := ""]
sigma_base_d3[, Level := ""]

sigma_small_d3[, Parameter := "sigma"]
sigma_small_d3[, Predictor := ""]
sigma_small_d3[, Level := ""]

sigma_large_d3[, Parameter := "sigma"]
sigma_large_d3[, Predictor := ""]
sigma_large_d3[, Level := ""]

## d3 brmcoda base ----------------
brmcoda_d3 <- rbind(
  b0_d3[condition == "base"][, -c("condition")],
  bilr1_d3[condition == "base"][, -c("condition")],
  bilr2_d3[condition == "base"][, -c("condition")],
  wilr1_d3[condition == "base"][, -c("condition")],
  wilr2_d3[condition == "base"][, -c("condition")],
  u0_base_d3[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d3
)

brmcoda_d3[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3$by)
brmcoda_d3[, by := factor(
  by,
  levels = c(
    "within ilr2 beta",
    "within ilr1 beta",
    "between ilr2 beta",
    "between ilr1 beta",
    "  u0",
    "  b0",
    "  sigma"
  )
)]

## d3 sub ----------------
bsub_d3 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_sleep_pa_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "Sleep" & From == "PA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_sb_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_pa_sleep_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "PA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_pa_sb_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "PA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_sleep_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_pa_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "SB" & From == "PA",
                             .(To, From)])
))

wsub_d3 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_sleep_pa_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "Sleep" & From == "PA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_sb_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_pa_sleep_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "PA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_pa_sb_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "PA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_sleep_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_pa_d3), stats = c("bias", "becover")),
        substutitution_gt_d3[To == "SB" & From == "PA",
                             .(To, From)])
))

bsub_d3[, Level := "between"]
wsub_d3[, Level := "within"]

sub_d3 <- rbind(bsub_d3, wsub_d3)

sub_d3[, by := paste0(Level, " ", From, " - ", To)]
unique(sub_d3$by)
sub_d3[, by := factor(by, levels = c(
  "between Sleep - PA", "within Sleep - PA",
  "between Sleep - SB", "within Sleep - SB",
  
  "between PA - Sleep", "within PA - Sleep",
  "between PA - SB",    "within PA - SB",
  
  "between SB - Sleep", "within SB - Sleep",
  "between SB - PA",    "within SB - PA"
))]
sub_d3 <- sub_d3[by %in% c(
  "between Sleep - PA", "within Sleep - PA",
  "between Sleep - SB", "within Sleep - SB",
  
  "between PA - SB",    "within PA - SB"
)]

## d4 fixed----------------
b0_d4    <- as.data.table(tidy(summary(s_b0_d4), stats = c("bias", "becover")))
bilr1_d4 <- as.data.table(tidy(summary(s_bilr1_d4), stats = c("bias", "becover")))
bilr2_d4 <- as.data.table(tidy(summary(s_bilr2_d4), stats = c("bias", "becover")))
bilr3_d4 <- as.data.table(tidy(summary(s_bilr3_d4), stats = c("bias", "becover")))
wilr1_d4 <- as.data.table(tidy(summary(s_wilr1_d4), stats = c("bias", "becover")))
wilr2_d4 <- as.data.table(tidy(summary(s_wilr2_d4), stats = c("bias", "becover")))
wilr3_d4 <- as.data.table(tidy(summary(s_wilr3_d4), stats = c("bias", "becover")))

b0_d4[, Parameter := "b0"]
b0_d4[, Predictor := ""]
b0_d4[, Level := ""]

bilr1_d4[, Parameter := "beta"]
bilr1_d4[, Predictor := "ilr1"]
bilr1_d4[, Level := "between"]

bilr2_d4[, Parameter := "beta"]
bilr2_d4[, Predictor := "ilr2"]
bilr2_d4[, Level := "between"]

bilr3_d4[, Parameter := "beta"]
bilr3_d4[, Predictor := "ilr3"]
bilr3_d4[, Level := "between"]

wilr1_d4[, Parameter := "beta"]
wilr1_d4[, Predictor := "ilr1"]
wilr1_d4[, Level := "within"]

wilr2_d4[, Parameter := "beta"]
wilr2_d4[, Predictor := "ilr2"]
wilr2_d4[, Level := "within"]

wilr3_d4[, Parameter := "beta"]
wilr3_d4[, Predictor := "ilr3"]
wilr3_d4[, Level := "within"]

## d4 random ----------------
u0_base_d4  <- as.data.table(tidy(summary(s_u0_base_d4), stats = c("bias", "becover")))
u0_small_d4 <- as.data.table(tidy(summary(s_u0_small_d4), stats = c("bias", "becover")))
u0_large_d4 <- as.data.table(tidy(summary(s_u0_large_d4), stats = c("bias", "becover")))

u0_base_d4[, Parameter := "u0"]
u0_base_d4[, Predictor := ""]
u0_base_d4[, Level := ""]

u0_small_d4[, Parameter := "u0"]
u0_small_d4[, Predictor := ""]
u0_small_d4[, Level := ""]

u0_large_d4[, Parameter := "u0"]
u0_large_d4[, Predictor := ""]
u0_large_d4[, Level := ""]

## d4 residual  ----------------
sigma_base_d4  <- as.data.table(tidy(summary(s_sigma_base_d4), stats = c("bias", "becover")))
sigma_small_d4 <- as.data.table(tidy(summary(s_sigma_small_d4), stats = c("bias", "becover")))
sigma_large_d4 <- as.data.table(tidy(summary(s_sigma_large_d4), stats = c("bias", "becover")))

sigma_base_d4[, Parameter := "sigma"]
sigma_base_d4[, Predictor := ""]
sigma_base_d4[, Level := ""]

sigma_small_d4[, Parameter := "sigma"]
sigma_small_d4[, Predictor := ""]
sigma_small_d4[, Level := ""]

sigma_large_d4[, Parameter := "sigma"]
sigma_large_d4[, Predictor := ""]
sigma_large_d4[, Level := ""]

## d4 brmcoda base -------
brmcoda_d4 <- rbind(
  b0_d4[condition == "base"][, -c("condition")],
  bilr1_d4[condition == "base"][, -c("condition")],
  bilr2_d4[condition == "base"][, -c("condition")],
  bilr3_d4[condition == "base"][, -c("condition")],
  wilr1_d4[condition == "base"][, -c("condition")],
  wilr2_d4[condition == "base"][, -c("condition")],
  wilr3_d4[condition == "base"][, -c("condition")],
  u0_base_d4[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d4
)

brmcoda_d4[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4$by)
brmcoda_d4[, by := factor(
  by,
  levels = c(
    "within ilr3 beta",
    "within ilr2 beta",
    "within ilr1 beta",
    "between ilr3 beta",
    "between ilr2 beta",
    "between ilr1 beta",
    "  u0",
    "  b0",
    "  sigma"
  )
)]
## d4 sub ---------
bsub_d4 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_sleep_mvpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sleep_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_mvpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind( tidy(summary(s_bsub_sb_mvpa_d4), stats = c("bias", "becover")),
         substutitution_gt_d4[To == "SB" & From == "MVPA",
                              .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "SB" & From == "LPA",
                             .(To, From)])
))

wsub_d4 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_sleep_mvpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sleep_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "Sleep" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "Sleep",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_mvpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sb_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_sleep_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "SB" & From == "Sleep",
                             .(To, From)]),
  cbind( tidy(summary(s_wsub_sb_mvpa_d4), stats = c("bias", "becover")),
         substutitution_gt_d4[To == "SB" & From == "MVPA",
                              .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_lpa_d4), stats = c("bias", "becover")),
        substutitution_gt_d4[To == "SB" & From == "LPA",
                             .(To, From)])
))

bsub_d4[, Level := "between"]
wsub_d4[, Level := "within"]

sub_d4 <- rbind(bsub_d4, wsub_d4)

sub_d4[, by := paste0(Level, " ", From, " - ", To)]
unique(sub_d4$by)
sub_d4[, by := factor(by, levels = c(
  "between Sleep - MVPA", "within Sleep - MVPA",
  "between Sleep - LPA",  "within Sleep - LPA",
  "between Sleep - SB",   "within Sleep - SB",

  "between MVPA - Sleep", "within MVPA - Sleep",
  "between MVPA - LPA",   "within MVPA - LPA",
  "between MVPA - SB",    "within MVPA - SB",
  
  "between LPA - Sleep",  "within LPA - Sleep",
  "between LPA - MVPA",   "within LPA - MVPA",
  "between LPA - SB",     "within LPA - SB",
  
  "between SB - Sleep",   "within SB - Sleep",
  "between SB - MVPA",    "within SB - MVPA",
  "between SB - LPA",     "within SB - LPA"
))]
sub_d4 <- sub_d4[by %in% c(
  "between Sleep - MVPA", "within Sleep - MVPA",
  "between Sleep - LPA",  "within Sleep - LPA",
  "between Sleep - SB",   "within Sleep - SB",
  
  "between MVPA - LPA",   "within MVPA - LPA",
  "between MVPA - SB",    "within MVPA - SB",
  
  "between LPA - SB",     "within LPA - SB"
)]

## d5 fixed----------------
b0_d5    <- as.data.table(tidy(summary(s_b0_d5), stats = c("bias", "becover")))
bilr1_d5 <- as.data.table(tidy(summary(s_bilr1_d5), stats = c("bias", "becover")))
bilr2_d5 <- as.data.table(tidy(summary(s_bilr2_d5), stats = c("bias", "becover")))
bilr3_d5 <- as.data.table(tidy(summary(s_bilr3_d5), stats = c("bias", "becover")))
bilr4_d5 <- as.data.table(tidy(summary(s_bilr4_d5), stats = c("bias", "becover")))
wilr1_d5 <- as.data.table(tidy(summary(s_wilr1_d5), stats = c("bias", "becover")))
wilr2_d5 <- as.data.table(tidy(summary(s_wilr2_d5), stats = c("bias", "becover")))
wilr3_d5 <- as.data.table(tidy(summary(s_wilr3_d5), stats = c("bias", "becover")))
wilr4_d5 <- as.data.table(tidy(summary(s_wilr4_d5), stats = c("bias", "becover")))

b0_d5[, Parameter := "b0"]
b0_d5[, Predictor := ""]
b0_d5[, Level := ""]

bilr1_d5[, Parameter := "beta"]
bilr1_d5[, Predictor := "ilr1"]
bilr1_d5[, Level := "between"]

bilr2_d5[, Parameter := "beta"]
bilr2_d5[, Predictor := "ilr2"]
bilr2_d5[, Level := "between"]

bilr3_d5[, Parameter := "beta"]
bilr3_d5[, Predictor := "ilr3"]
bilr3_d5[, Level := "between"]

bilr4_d5[, Parameter := "beta"]
bilr4_d5[, Predictor := "ilr4"]
bilr4_d5[, Level := "between"]

wilr1_d5[, Parameter := "beta"]
wilr1_d5[, Predictor := "ilr1"]
wilr1_d5[, Level := "within"]

wilr2_d5[, Parameter := "beta"]
wilr2_d5[, Predictor := "ilr2"]
wilr2_d5[, Level := "within"]

wilr3_d5[, Parameter := "beta"]
wilr3_d5[, Predictor := "ilr3"]
wilr3_d5[, Level := "within"]

wilr4_d5[, Parameter := "beta"]
wilr4_d5[, Predictor := "ilr4"]
wilr4_d5[, Level := "within"]

## d5 random ----------------
u0_base_d5  <- as.data.table(tidy(summary(s_u0_base_d5), stats = c("bias", "becover")))
u0_small_d5 <- as.data.table(tidy(summary(s_u0_small_d5), stats = c("bias", "becover")))
u0_large_d5 <- as.data.table(tidy(summary(s_u0_large_d5), stats = c("bias", "becover")))

u0_base_d5[, Parameter := "u0"]
u0_base_d5[, Predictor := ""]
u0_base_d5[, Level := ""]

u0_small_d5[, Parameter := "u0"]
u0_small_d5[, Predictor := ""]
u0_small_d5[, Level := ""]

u0_large_d5[, Parameter := "u0"]
u0_large_d5[, Predictor := ""]
u0_large_d5[, Level := ""]

## d5 residual  ----------------
sigma_base_d5  <- as.data.table(tidy(summary(s_sigma_base_d5), stats = c("bias", "becover")))
sigma_small_d5 <- as.data.table(tidy(summary(s_sigma_small_d5), stats = c("bias", "becover")))
sigma_large_d5 <- as.data.table(tidy(summary(s_sigma_large_d5), stats = c("bias", "becover")))

sigma_base_d5[, Parameter := "sigma"]
sigma_base_d5[, Predictor := ""]
sigma_base_d5[, Level := ""]

sigma_small_d5[, Parameter := "sigma"]
sigma_small_d5[, Predictor := ""]
sigma_small_d5[, Level := ""]

sigma_large_d5[, Parameter := "sigma"]
sigma_large_d5[, Predictor := ""]
sigma_large_d5[, Level := ""]

## d5 brmcoda base -------
brmcoda_d5 <- rbind(
  b0_d5[condition == "base"][, -c("condition")],
  bilr1_d5[condition == "base"][, -c("condition")],
  bilr2_d5[condition == "base"][, -c("condition")],
  bilr3_d5[condition == "base"][, -c("condition")],
  bilr4_d5[condition == "base"][, -c("condition")],
  wilr1_d5[condition == "base"][, -c("condition")],
  wilr2_d5[condition == "base"][, -c("condition")],
  wilr3_d5[condition == "base"][, -c("condition")],
  wilr4_d5[condition == "base"][, -c("condition")],
  u0_base_d5[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d5
)

brmcoda_d5[, by := paste0(Level, " ", Predictor, " ", Parameter)]
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
## d5 sub ---------
bsub_d5 <- as.data.table(rbind(
  cbind(tidy(summary(s_bsub_tst_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_tst_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_wake_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "WAKE",
                             .(To, From)]),
  cbind( tidy(summary(s_bsub_mvpa_lpa_d5), stats = c("bias", "becover")),
         substutitution_gt_d5[To == "MVPA" & From == "LPA",
                              .(To, From)]),
  cbind(tidy(summary(s_bsub_mvpa_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_lpa_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_bsub_sb_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "LPA",
                             .(To, From)])
))

wsub_d5 <- as.data.table(rbind(
  cbind(tidy(summary(s_wsub_tst_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_tst_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "TST" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "LPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_wake_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "WAKE" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "WAKE",
                             .(To, From)]),
  cbind( tidy(summary(s_wsub_mvpa_lpa_d5), stats = c("bias", "becover")),
         substutitution_gt_d5[To == "MVPA" & From == "LPA",
                              .(To, From)]),
  cbind(tidy(summary(s_wsub_mvpa_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "MVPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_lpa_sb_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "LPA" & From == "SB",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_tst_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "TST",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_wake_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "WAKE",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_mvpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "MVPA",
                             .(To, From)]),
  cbind(tidy(summary(s_wsub_sb_lpa_d5), stats = c("bias", "becover")),
        substutitution_gt_d5[To == "SB" & From == "LPA",
                             .(To, From)])
))

bsub_d5[, Level := "between"]
wsub_d5[, Level := "within"]

sub_d5 <- rbind(bsub_d5, wsub_d5)

sub_d5[, by := paste0(Level, " ", From, " - ", To)]
unique(sub_d5$by)
sub_d5[, by := factor(by, levels = c(
  "between TST - MVPA",  "within TST - MVPA",
  "between TST - WAKE",  "within TST - WAKE",
  "between TST - LPA",   "within TST - LPA",
  "between TST - SB",    "within TST - SB",
  
  "between WAKE - TST",  "within WAKE - TST",
  "between WAKE - MVPA", "within WAKE - MVPA",
  "between WAKE - LPA",  "within WAKE - LPA",
  "between WAKE - SB",   "within WAKE - SB",
  
  "between MVPA - TST",  "within MVPA - TST",
  "between MVPA - WAKE", "within MVPA - WAKE",
  "between MVPA - LPA",  "within MVPA - LPA",
  "between MVPA - SB",   "within MVPA - SB",
  
  "between LPA - TST",   "within LPA - TST",
  "between LPA - WAKE",  "within LPA - WAKE",
  "between LPA - MVPA",  "within LPA - MVPA",
  "between LPA - SB",    "within LPA - SB",
  
  "between SB - TST",    "within SB - TST",
  "between SB - WAKE",   "within SB - WAKE",
  "between SB - MVPA",   "within SB - MVPA",
  "between SB - LPA",    "within SB - LPA"
))]
sub_d5 <- sub_d5[by %in% c(
  "between TST - MVPA",  "within TST - MVPA",
  "between TST - WAKE",  "within TST - WAKE",
  "between TST - LPA",   "within TST - LPA",
  "between TST - SB",    "within TST - SB",
  
  "between WAKE - MVPA", "within WAKE - MVPA",
  "between WAKE - LPA",  "within WAKE - LPA",
  "between WAKE - SB",   "within WAKE - SB",
  
  "between MVPA - LPA",  "within MVPA - LPA",
  "between MVPA - SB",   "within MVPA - SB",

  "between LPA - SB",     "within LPA - SB"
)]
