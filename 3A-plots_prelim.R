# substitution bias -------------------
bsub_bias_d3 <- as.data.table(rbind(
  tidy(summary(s_bsub_sleep_pa_d3), stats = "bias"),
  tidy(summary(s_bsub_sleep_sb_d3), stats = "bias"),
  tidy(summary(s_bsub_pa_sleep_d3), stats = "bias"),
  tidy(summary(s_bsub_pa_sb_d3), stats = "bias"),
  tidy(summary(s_bsub_sb_sleep_d3), stats = "bias"),
  tidy(summary(s_bsub_sb_pa_d3), stats = "bias")
))

bsub_bias_d4 <- as.data.table(rbind(
  tidy(summary(s_bsub_sleep_mvpa_d4), stats = "bias"),
  tidy(summary(s_bsub_sleep_lpa_d4), stats = "bias"),
  tidy(summary(s_bsub_sleep_sb_d4), stats = "bias"),
  tidy(summary(s_bsub_mvpa_sleep_d4), stats = "bias"),
  tidy(summary(s_bsub_mvpa_lpa_d4), stats = "bias"),
  tidy(summary(s_bsub_mvpa_sb_d4), stats = "bias"),
  tidy(summary(s_bsub_lpa_sleep_d4), stats = "bias"),
  tidy(summary(s_bsub_lpa_mvpa_d4), stats = "bias"),
  tidy(summary(s_bsub_lpa_sb_d4), stats = "bias"),
  tidy(summary(s_bsub_sb_sleep_d4), stats = "bias"),
  tidy(summary(s_bsub_sb_mvpa_d4), stats = "bias"),
  tidy(summary(s_bsub_sb_lpa_d4), stats = "bias")
))

bsub_bias_d5 <- as.data.table(rbind(
  tidy(summary(s_bsub_tst_wake_d5), stats = "bias"),
  tidy(summary(s_bsub_tst_mvpa_d5), stats = "bias"),
  tidy(summary(s_bsub_tst_lpa_d5), stats = "bias"),
  tidy(summary(s_bsub_tst_sb_d5), stats = "bias"),
  tidy(summary(s_bsub_wake_tst_d5), stats = "bias"),
  tidy(summary(s_bsub_wake_mvpa_d5), stats = "bias"),
  tidy(summary(s_bsub_wake_lpa_d5), stats = "bias"),
  tidy(summary(s_bsub_wake_sb_d5), stats = "bias"),
  tidy(summary(s_bsub_mvpa_tst_d5), stats = "bias"),
  tidy(summary(s_bsub_mvpa_wake_d5), stats = "bias"),
  tidy(summary(s_bsub_mvpa_lpa_d5), stats = "bias"),
  tidy(summary(s_bsub_mvpa_sb_d5), stats = "bias"),
  tidy(summary(s_bsub_lpa_tst_d5), stats = "bias"),
  tidy(summary(s_bsub_lpa_wake_d5), stats = "bias"),
  tidy(summary(s_bsub_lpa_mvpa_d5), stats = "bias"),
  tidy(summary(s_bsub_lpa_sb_d5), stats = "bias"),
  tidy(summary(s_bsub_sb_tst_d5), stats = "bias"),
  tidy(summary(s_bsub_sb_wake_d5), stats = "bias"),
  tidy(summary(s_bsub_sb_mvpa_d5), stats = "bias"),
  tidy(summary(s_bsub_sb_lpa_d5), stats = "bias")
))


bsub_bias_d3[, NoOfParts := 3]
bsub_bias_d3[, Level := "between"]

bsub_bias_d4[, NoOfParts := 4]
bsub_bias_d4[, Level := "between"]

bsub_bias_d5[, NoOfParts := 5]
bsub_bias_d5[, Level := "between"]

wsub_bias_d3 <- as.data.table(rbind(
  tidy(summary(s_wsub_sleep_pa_d3), stats = "bias"),
  tidy(summary(s_wsub_sleep_sb_d3), stats = "bias"),
  tidy(summary(s_wsub_pa_sleep_d3), stats = "bias"),
  tidy(summary(s_wsub_pa_sb_d3), stats = "bias"),
  tidy(summary(s_wsub_sb_sleep_d3), stats = "bias"),
  tidy(summary(s_wsub_sb_pa_d3), stats = "bias")
))

wsub_bias_d4 <- as.data.table(rbind(
  tidy(summary(s_wsub_sleep_mvpa_d4), stats = "bias"),
  tidy(summary(s_wsub_sleep_lpa_d4), stats = "bias"),
  tidy(summary(s_wsub_sleep_sb_d4), stats = "bias"),
  tidy(summary(s_wsub_mvpa_sleep_d4), stats = "bias"),
  tidy(summary(s_wsub_mvpa_lpa_d4), stats = "bias"),
  tidy(summary(s_wsub_mvpa_sb_d4), stats = "bias"),
  tidy(summary(s_wsub_lpa_sleep_d4), stats = "bias"),
  tidy(summary(s_wsub_lpa_mvpa_d4), stats = "bias"),
  tidy(summary(s_wsub_lpa_sb_d4), stats = "bias"),
  tidy(summary(s_wsub_sb_sleep_d4), stats = "bias"),
  tidy(summary(s_wsub_sb_mvpa_d4), stats = "bias"),
  tidy(summary(s_wsub_sb_lpa_d4), stats = "bias")
))

wsub_bias_d5 <- as.data.table(rbind(
  tidy(summary(s_wsub_tst_wake_d5), stats = "bias"),
  tidy(summary(s_wsub_tst_mvpa_d5), stats = "bias"),
  tidy(summary(s_wsub_tst_lpa_d5), stats = "bias"),
  tidy(summary(s_wsub_tst_sb_d5), stats = "bias"),
  tidy(summary(s_wsub_wake_tst_d5), stats = "bias"),
  tidy(summary(s_wsub_wake_mvpa_d5), stats = "bias"),
  tidy(summary(s_wsub_wake_lpa_d5), stats = "bias"),
  tidy(summary(s_wsub_wake_sb_d5), stats = "bias"),
  tidy(summary(s_wsub_mvpa_tst_d5), stats = "bias"),
  tidy(summary(s_wsub_mvpa_wake_d5), stats = "bias"),
  tidy(summary(s_wsub_mvpa_lpa_d5), stats = "bias"),
  tidy(summary(s_wsub_mvpa_sb_d5), stats = "bias"),
  tidy(summary(s_wsub_lpa_tst_d5), stats = "bias"),
  tidy(summary(s_wsub_lpa_wake_d5), stats = "bias"),
  tidy(summary(s_wsub_lpa_mvpa_d5), stats = "bias"),
  tidy(summary(s_wsub_lpa_sb_d5), stats = "bias"),
  tidy(summary(s_wsub_sb_tst_d5), stats = "bias"),
  tidy(summary(s_wsub_sb_wake_d5), stats = "bias"),
  tidy(summary(s_wsub_sb_mvpa_d5), stats = "bias"),
  tidy(summary(s_wsub_sb_lpa_d5), stats = "bias")
))

wsub_bias_d3[, NoOfParts := 3]
wsub_bias_d3[, Level := "within"]

wsub_bias_d4[, NoOfParts := 4]
wsub_bias_d4[, Level := "within"]

wsub_bias_d5[, NoOfParts := 5]
wsub_bias_d5[, Level := "within"]

sub_bias <- rbind(bsub_bias_d3,
                  bsub_bias_d4,
                  bsub_bias_d5,
                  wsub_bias_d3,
                  wsub_bias_d4,
                  wsub_bias_d5)

setnames(sub_bias, "est", "Bias")
sub_bias[, NK := as.factor(paste0(N, "-", K))]
sub_bias[, cond := paste0(N, "-", K, "-", condition)]
sub_bias[, cond := factor(cond,
                          levels = c(
                            "30-3-base",    "30-3-REbase_RESlarge",    "30-3-REbase_RESsmall",    "30-3-REsmall_RESlarge",    "30-3-RElarge_RESsmall",
                            "30-5-base",    "30-5-REbase_RESlarge",    "30-5-REbase_RESsmall",    "30-5-REsmall_RESlarge",    "30-5-RElarge_RESsmall",
                            "30-7-base",    "30-7-REbase_RESlarge",    "30-7-REbase_RESsmall",    "30-7-REsmall_RESlarge",    "30-7-RElarge_RESsmall",
                            "30-14-base",   "30-14-REbase_RESlarge",   "30-14-REbase_RESsmall",   "30-14-REsmall_RESlarge",   "30-14-RElarge_RESsmall",
                            "50-3-base",    "50-3-REbase_RESlarge",    "50-3-REbase_RESsmall",    "50-3-REsmall_RESlarge",    "50-3-RElarge_RESsmall",
                            "50-5-base",    "50-5-REbase_RESlarge",    "50-5-REbase_RESsmall",    "50-5-REsmall_RESlarge",    "50-5-RElarge_RESsmall",
                            "50-7-base",    "50-7-REbase_RESlarge",    "50-7-REbase_RESsmall",    "50-7-REsmall_RESlarge",    "50-7-RElarge_RESsmall",
                            "50-14-base",   "50-14-REbase_RESlarge",   "50-14-REbase_RESsmall",   "50-14-REsmall_RESlarge",   "50-14-RElarge_RESsmall",
                            "360-3-base",   "360-3-REbase_RESlarge",   "360-3-REbase_RESsmall",   "360-3-REsmall_RESlarge",   "360-3-RElarge_RESsmall",
                            "360-5-base",   "360-5-REbase_RESlarge",   "360-5-REbase_RESsmall",   "360-5-REsmall_RESlarge",   "360-5-RElarge_RESsmall",
                            "360-7-base",   "360-7-REbase_RESlarge",   "360-7-REbase_RESsmall",   "360-7-REsmall_RESlarge",   "360-7-RElarge_RESsmall",
                            "360-14-base",  "360-14-REbase_RESlarge",  "360-14-REbase_RESsmall",  "360-14-REsmall_RESlarge",  "360-14-RElarge_RESsmall",
                            "1200-3-base",  "1200-3-REbase_RESlarge",  "1200-3-REbase_RESsmall",  "1200-3-REsmall_RESlarge",  "1200-3-RElarge_RESsmall",
                            "1200-5-base",  "1200-5-REbase_RESlarge",  "1200-5-REbase_RESsmall",  "1200-5-REsmall_RESlarge",  "1200-5-RElarge_RESsmall",
                            "1200-7-base",  "1200-7-REbase_RESlarge",  "1200-7-REbase_RESsmall",  "1200-7-REsmall_RESlarge",  "1200-7-RElarge_RESsmall",
                            "1200-14-base", "1200-14-REbase_RESlarge", "1200-14-REbase_RESsmall", "1200-14-REsmall_RESlarge", "1200-14-RElarge_RESsmall"
                          ))]

ggplot(sub_bias, aes(Bias, N)) +
  geom_point()
ggplot(sub_bias, aes(Bias, K)) +
  geom_point()
ggplot(sub_bias, aes(Bias, condition)) +
  geom_point()
ggplot(sub_bias, aes(Bias, Level)) +
  geom_point()
ggplot(sub_bias, aes(Bias, NoOfParts)) +
  geom_point()

ggplot(sub_bias, aes(Bias, NK, by = condition)) +
  geom_point() +
  facet_grid(~condition)
ggplot(sub_bias, aes(Bias, cond, by = as.factor(N))) +
  geom_point() +
  facet_grid(~N)
ggplot(sub_bias, aes(Bias, NK, by = as.factor(cond))) +
  geom_point() +
  facet_grid(~cond)

ggplot(sub_bias, aes(Bias, NoOfParts, by = Level)) +
  geom_point() +
  facet_grid(~Level)

bsub_bias_plot <-
  ggplot(sub_bias[Level == "between"], aes(Bias, cond, by = NoOfParts)) +
  geom_point() +
  geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 1 / 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("Bias Between") +
  xlim(c(-0.05, 0.05))

wsub_bias_plot <-
  ggplot(sub_bias[Level == "within"], aes(Bias, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("Bias Within") +
  xlim(c(-0.05, 0.05))

ggarrange(bsub_bias_plot,
          wsub_bias_plot,
          nrow = 2)

# substitution cover -------------------
bsub_cover_d3 <- as.data.table(rbind(
  tidy(summary(s_bsub_sleep_pa_d3), stats = "becover"),
  tidy(summary(s_bsub_sleep_sb_d3), stats = "becover"),
  tidy(summary(s_bsub_pa_sleep_d3), stats = "becover"),
  tidy(summary(s_bsub_pa_sb_d3), stats = "becover"),
  tidy(summary(s_bsub_sb_sleep_d3), stats = "becover"),
  tidy(summary(s_bsub_sb_pa_d3), stats = "becover")
))

bsub_cover_d4 <- as.data.table(rbind(
  tidy(summary(s_bsub_sleep_mvpa_d4), stats = "becover"),
  tidy(summary(s_bsub_sleep_lpa_d4), stats = "becover"),
  tidy(summary(s_bsub_sleep_sb_d4), stats = "becover"),
  tidy(summary(s_bsub_mvpa_sleep_d4), stats = "becover"),
  tidy(summary(s_bsub_mvpa_lpa_d4), stats = "becover"),
  tidy(summary(s_bsub_mvpa_sb_d4), stats = "becover"),
  tidy(summary(s_bsub_lpa_sleep_d4), stats = "becover"),
  tidy(summary(s_bsub_lpa_mvpa_d4), stats = "becover"),
  tidy(summary(s_bsub_lpa_sb_d4), stats = "becover"),
  tidy(summary(s_bsub_sb_sleep_d4), stats = "becover"),
  tidy(summary(s_bsub_sb_mvpa_d4), stats = "becover"),
  tidy(summary(s_bsub_sb_lpa_d4), stats = "becover")
))

bsub_cover_d5 <- as.data.table(rbind(
  tidy(summary(s_bsub_tst_wake_d5), stats = "becover"),
  tidy(summary(s_bsub_tst_mvpa_d5), stats = "becover"),
  tidy(summary(s_bsub_tst_lpa_d5), stats = "becover"),
  tidy(summary(s_bsub_tst_sb_d5), stats = "becover"),
  tidy(summary(s_bsub_wake_tst_d5), stats = "becover"),
  tidy(summary(s_bsub_wake_mvpa_d5), stats = "becover"),
  tidy(summary(s_bsub_wake_lpa_d5), stats = "becover"),
  tidy(summary(s_bsub_wake_sb_d5), stats = "becover"),
  tidy(summary(s_bsub_mvpa_tst_d5), stats = "becover"),
  tidy(summary(s_bsub_mvpa_wake_d5), stats = "becover"),
  tidy(summary(s_bsub_mvpa_lpa_d5), stats = "becover"),
  tidy(summary(s_bsub_mvpa_sb_d5), stats = "becover"),
  tidy(summary(s_bsub_lpa_tst_d5), stats = "becover"),
  tidy(summary(s_bsub_lpa_wake_d5), stats = "becover"),
  tidy(summary(s_bsub_lpa_mvpa_d5), stats = "becover"),
  tidy(summary(s_bsub_lpa_sb_d5), stats = "becover"),
  tidy(summary(s_bsub_sb_tst_d5), stats = "becover"),
  tidy(summary(s_bsub_sb_wake_d5), stats = "becover"),
  tidy(summary(s_bsub_sb_mvpa_d5), stats = "becover"),
  tidy(summary(s_bsub_sb_lpa_d5), stats = "becover")
))


bsub_cover_d3[, NoOfParts := 3]
bsub_cover_d3[, Level := "between"]

bsub_cover_d4[, NoOfParts := 4]
bsub_cover_d4[, Level := "between"]

bsub_cover_d5[, NoOfParts := 5]
bsub_cover_d5[, Level := "between"]

wsub_cover_d3 <- as.data.table(rbind(
  tidy(summary(s_wsub_sleep_pa_d3), stats = "becover"),
  tidy(summary(s_wsub_sleep_sb_d3), stats = "becover"),
  tidy(summary(s_wsub_pa_sleep_d3), stats = "becover"),
  tidy(summary(s_wsub_pa_sb_d3), stats = "becover"),
  tidy(summary(s_wsub_sb_sleep_d3), stats = "becover"),
  tidy(summary(s_wsub_sb_pa_d3), stats = "becover")
))

wsub_cover_d4 <- as.data.table(rbind(
  tidy(summary(s_wsub_sleep_mvpa_d4), stats = "becover"),
  tidy(summary(s_wsub_sleep_lpa_d4), stats = "becover"),
  tidy(summary(s_wsub_sleep_sb_d4), stats = "becover"),
  tidy(summary(s_wsub_mvpa_sleep_d4), stats = "becover"),
  tidy(summary(s_wsub_mvpa_lpa_d4), stats = "becover"),
  tidy(summary(s_wsub_mvpa_sb_d4), stats = "becover"),
  tidy(summary(s_wsub_lpa_sleep_d4), stats = "becover"),
  tidy(summary(s_wsub_lpa_mvpa_d4), stats = "becover"),
  tidy(summary(s_wsub_lpa_sb_d4), stats = "becover"),
  tidy(summary(s_wsub_sb_sleep_d4), stats = "becover"),
  tidy(summary(s_wsub_sb_mvpa_d4), stats = "becover"),
  tidy(summary(s_wsub_sb_lpa_d4), stats = "becover")
))

wsub_cover_d5 <- as.data.table(rbind(
  tidy(summary(s_wsub_tst_wake_d5), stats = "becover"),
  tidy(summary(s_wsub_tst_mvpa_d5), stats = "becover"),
  tidy(summary(s_wsub_tst_lpa_d5), stats = "becover"),
  tidy(summary(s_wsub_tst_sb_d5), stats = "becover"),
  tidy(summary(s_wsub_wake_tst_d5), stats = "becover"),
  tidy(summary(s_wsub_wake_mvpa_d5), stats = "becover"),
  tidy(summary(s_wsub_wake_lpa_d5), stats = "becover"),
  tidy(summary(s_wsub_wake_sb_d5), stats = "becover"),
  tidy(summary(s_wsub_mvpa_tst_d5), stats = "becover"),
  tidy(summary(s_wsub_mvpa_wake_d5), stats = "becover"),
  tidy(summary(s_wsub_mvpa_lpa_d5), stats = "becover"),
  tidy(summary(s_wsub_mvpa_sb_d5), stats = "becover"),
  tidy(summary(s_wsub_lpa_tst_d5), stats = "becover"),
  tidy(summary(s_wsub_lpa_wake_d5), stats = "becover"),
  tidy(summary(s_wsub_lpa_mvpa_d5), stats = "becover"),
  tidy(summary(s_wsub_lpa_sb_d5), stats = "becover"),
  tidy(summary(s_wsub_sb_tst_d5), stats = "becover"),
  tidy(summary(s_wsub_sb_wake_d5), stats = "becover"),
  tidy(summary(s_wsub_sb_mvpa_d5), stats = "becover"),
  tidy(summary(s_wsub_sb_lpa_d5), stats = "becover")
))

wsub_cover_d3[, NoOfParts := 3]
wsub_cover_d3[, Level := "within"]

wsub_cover_d4[, NoOfParts := 4]
wsub_cover_d4[, Level := "within"]

wsub_cover_d5[, NoOfParts := 5]
wsub_cover_d5[, Level := "within"]

sub_cover <- rbind(bsub_cover_d3,
                   bsub_cover_d4,
                   bsub_cover_d5,
                   wsub_cover_d3,
                   wsub_cover_d4,
                   wsub_cover_d5)

setnames(sub_cover, "est", "Coverage")
sub_cover <- sub_cover[order(Coverage)]
sub_cover[, NK := as.factor(paste0(N, "-", K))]
sub_cover[, cond := paste0(N, "-", K, "-", condition)]
sub_cover[, cond := factor(cond,
                           levels = c(
                             "30-3-base",    "30-3-REbase_RESlarge",    "30-3-REbase_RESsmall",    "30-3-REsmall_RESlarge",    "30-3-RElarge_RESsmall",
                             "30-5-base",    "30-5-REbase_RESlarge",    "30-5-REbase_RESsmall",    "30-5-REsmall_RESlarge",    "30-5-RElarge_RESsmall",
                             "30-7-base",    "30-7-REbase_RESlarge",    "30-7-REbase_RESsmall",    "30-7-REsmall_RESlarge",    "30-7-RElarge_RESsmall",
                             "30-14-base",   "30-14-REbase_RESlarge",   "30-14-REbase_RESsmall",   "30-14-REsmall_RESlarge",   "30-14-RElarge_RESsmall",
                             "50-3-base",    "50-3-REbase_RESlarge",    "50-3-REbase_RESsmall",    "50-3-REsmall_RESlarge",    "50-3-RElarge_RESsmall",
                             "50-5-base",    "50-5-REbase_RESlarge",    "50-5-REbase_RESsmall",    "50-5-REsmall_RESlarge",    "50-5-RElarge_RESsmall",
                             "50-7-base",    "50-7-REbase_RESlarge",    "50-7-REbase_RESsmall",    "50-7-REsmall_RESlarge",    "50-7-RElarge_RESsmall",
                             "50-14-base",   "50-14-REbase_RESlarge",   "50-14-REbase_RESsmall",   "50-14-REsmall_RESlarge",   "50-14-RElarge_RESsmall",
                             "360-3-base",   "360-3-REbase_RESlarge",   "360-3-REbase_RESsmall",   "360-3-REsmall_RESlarge",   "360-3-RElarge_RESsmall",
                             "360-5-base",   "360-5-REbase_RESlarge",   "360-5-REbase_RESsmall",   "360-5-REsmall_RESlarge",   "360-5-RElarge_RESsmall",
                             "360-7-base",   "360-7-REbase_RESlarge",   "360-7-REbase_RESsmall",   "360-7-REsmall_RESlarge",   "360-7-RElarge_RESsmall",
                             "360-14-base",  "360-14-REbase_RESlarge",  "360-14-REbase_RESsmall",  "360-14-REsmall_RESlarge",  "360-14-RElarge_RESsmall",
                             "1200-3-base",  "1200-3-REbase_RESlarge",  "1200-3-REbase_RESsmall",  "1200-3-REsmall_RESlarge",  "1200-3-RElarge_RESsmall",
                             "1200-5-base",  "1200-5-REbase_RESlarge",  "1200-5-REbase_RESsmall",  "1200-5-REsmall_RESlarge",  "1200-5-RElarge_RESsmall",
                             "1200-7-base",  "1200-7-REbase_RESlarge",  "1200-7-REbase_RESsmall",  "1200-7-REsmall_RESlarge",  "1200-7-RElarge_RESsmall",
                             "1200-14-base", "1200-14-REbase_RESlarge", "1200-14-REbase_RESsmall", "1200-14-REsmall_RESlarge", "1200-14-RElarge_RESsmall"
                           ))]
bsub_cover_plot <-
  ggplot(sub_cover[Level == "between"], aes(Coverage, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 1 / 3) +
  ggplot2::geom_vline(xintercept = 0.95, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("Coverage Between")

wsub_cover_plot <-
  ggplot(sub_cover[Level == "within"], aes(Coverage, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_vline(xintercept = 0.95, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("Coverage Within") +
  xlim(c(0, 1))

ggarrange(bsub_cover_plot,
          wsub_cover_plot,
          nrow = 2)

## brmcoda bias ----------------------
bbrmcoda_bias_d3 <- as.data.table(rbind(
  tidy(summary(s_b0_d3), stats = "bias"),
  tidy(summary(s_bilr1_d3), stats = "bias"),
  tidy(summary(s_bilr2_d3), stats = "bias")
))

bbrmcoda_bias_d4 <- as.data.table(rbind(
  tidy(summary(s_b0_d4), stats = "bias"),
  tidy(summary(s_bilr1_d4), stats = "bias"),
  tidy(summary(s_bilr2_d4), stats = "bias"),
  tidy(summary(s_bilr3_d4), stats = "bias")
))

bbrmcoda_bias_d5 <- as.data.table(rbind(
  tidy(summary(s_b0_d5), stats = "bias"),
  tidy(summary(s_bilr1_d5), stats = "bias"),
  tidy(summary(s_bilr2_d5), stats = "bias"),
  tidy(summary(s_bilr3_d5), stats = "bias"),
  tidy(summary(s_bilr4_d5), stats = "bias")
))

bbrmcoda_bias_d3[, NoOfParts := 3]
bbrmcoda_bias_d3[, Level := "between"]

bbrmcoda_bias_d4[, NoOfParts := 4]
bbrmcoda_bias_d4[, Level := "between"]

bbrmcoda_bias_d5[, NoOfParts := 5]
bbrmcoda_bias_d5[, Level := "between"]

wbrmcoda_bias_d3 <- as.data.table(rbind(
  tidy((s_wilr1_d3), stats = "bias"),
  tidy((s_wilr2_d3), stats = "bias")
))

wbrmcoda_bias_d4 <- as.data.table(rbind(
  tidy(summary(s_wilr1_d4), stats = "bias"),
  tidy(summary(s_wilr2_d4), stats = "bias"),
  tidy(summary(s_wilr3_d4), stats = "bias")
))

wbrmcoda_bias_d5 <- as.data.table(rbind(
  tidy(summary(s_wilr1_d5), stats = "bias"),
  tidy(summary(s_wilr2_d5), stats = "bias"),
  tidy(summary(s_wilr3_d5), stats = "bias"),
  tidy(summary(s_wilr4_d5), stats = "bias")
))

wbrmcoda_bias_d3[, NoOfParts := 3]
wbrmcoda_bias_d3[, Level := "within"]

wbrmcoda_bias_d4[, NoOfParts := 4]
wbrmcoda_bias_d4[, Level := "within"]

wbrmcoda_bias_d5[, NoOfParts := 5]
wbrmcoda_bias_d5[, Level := "within"]

brmcoda_bias <- rbind(
  bbrmcoda_bias_d3,
  bbrmcoda_bias_d4,
  bbrmcoda_bias_d5,
  wbrmcoda_bias_d3,
  wbrmcoda_bias_d4,
  wbrmcoda_bias_d5
)

setnames(brmcoda_bias, "est", "Bias")
brmcoda_bias[, condition := factor(condition,
                                   levels = c(
                                     "base",
                                     "REbase_RESsmall",
                                     "REbase_RESlarge",
                                     "RElarge_RESsmall"
                                   ))]
brmcoda_bias[, N := factor(N, levels = c(
  30, 50, 360, 1200
))]
brmcoda_bias[, K := factor(K, levels = c(
  3, 5, 7, 14
))]
brmcoda_bias[, NoOfParts := as.factor(NoOfParts)]
brmcoda_bias[, Level := as.factor(Level)]
brmcoda_bias[, NK := as.factor(paste0(N, "-", K))]
brmcoda_bias[, cond := paste0(N, "-", K, "-", condition)]
brmcoda_bias[, cond := factor(cond,
                              levels = c(
                                "30-3-base",    "30-3-REbase_RESlarge",    "30-3-REbase_RESsmall",    "30-3-RElarge_RESsmall",
                                "30-5-base",    "30-5-REbase_RESlarge",    "30-5-REbase_RESsmall",    "30-5-RElarge_RESsmall",
                                "30-7-base",    "30-7-REbase_RESlarge",    "30-7-REbase_RESsmall",    "30-7-RElarge_RESsmall",
                                "30-14-base",   "30-14-REbase_RESlarge",   "30-14-REbase_RESsmall",   "30-14-RElarge_RESsmall",
                                "50-3-base",    "50-3-REbase_RESlarge",    "50-3-REbase_RESsmall",    "50-3-RElarge_RESsmall",
                                "50-5-base",    "50-5-REbase_RESlarge",    "50-5-REbase_RESsmall",    "50-5-RElarge_RESsmall",
                                "50-7-base",   "50-7-REbase_RESlarge",    "50-7-REbase_RESsmall",    "50-7-RElarge_RESsmall",
                                "50-14-base",   "50-14-REbase_RESlarge",   "50-14-REbase_RESsmall",   "50-14-RElarge_RESsmall",
                                "360-3-base",   "360-3-REbase_RESlarge",   "360-3-REbase_RESsmall",   "360-3-RElarge_RESsmall",
                                "360-5-base",   "360-5-REbase_RESlarge",   "360-5-REbase_RESsmall",   "360-5-RElarge_RESsmall",
                                "360-7-base",  "360-7-REbase_RESlarge",   "360-7-REbase_RESsmall",   "360-7-RElarge_RESsmall",
                                "360-14-base",  "360-14-REbase_RESlarge",  "360-14-REbase_RESsmall",  "360-14-RElarge_RESsmall",
                                "1200-3-base",  "1200-3-REbase_RESlarge",  "1200-3-REbase_RESsmall",  "1200-3-RElarge_RESsmall",
                                "1200-5-base",  "1200-5-REbase_RESlarge",  "1200-5-REbase_RESsmall",  "1200-5-RElarge_RESsmall",
                                "1200-7-base",  "1200-7-REbase_RESlarge",  "1200-7-REbase_RESsmall",  "1200-7-RElarge_RESsmall",
                                "1200-14-base", "1200-14-REbase_RESlarge", "1200-14-REbase_RESsmall", "1200-14-RElarge_RESsmall"
                              ))]
bbrmcoda_bias_plot <-
  ggplot(c[Level == "between"], aes(Bias, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("brmcoda Bias Between") +
  xlim(c(-0.20, 0.20))

wbrmcoda_bias_plot <-
  ggplot(brmcoda_bias[Level == "within"], aes(Bias, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("brmcoda Bias Within") +
  xlim(c(-0.20, 0.20))

ggarrange(bbrmcoda_bias_plot,
          wbrmcoda_bias_plot,
          nrow = 2)

## brmcoda cover ----------------------
brmcoda_cover_d3 <- as.data.table(rbind(
  tidy(summary(s_b0_d3), stats = "becover"),
  tidy(summary(s_bilr1_d3), stats = "becover"),
  tidy(summary(s_bilr2_d3), stats = "becover"),
  tidy(summary(s_wilr1_d3), stats = "becover"),
  tidy(summary(s_wilr2_d3), stats = "becover")
))

brmcoda_cover_d4 <- as.data.table(rbind(
  tidy(summary(s_b0_d4), stats = "becover"),
  tidy(summary(s_bilr1_d4), stats = "becover"),
  tidy(summary(s_bilr2_d4), stats = "becover"),
  tidy(summary(s_bilr3_d4), stats = "becover"),
  tidy(summary(s_wilr1_d4), stats = "becover"),
  tidy(summary(s_wilr2_d4), stats = "becover"),
  tidy(summary(s_wilr3_d4), stats = "becover")
))

brmcoda_cover_d5 <- as.data.table(rbind(
  tidy(summary(s_b0_d5), stats = "becover"),
  tidy(summary(s_bilr1_d5), stats = "becover"),
  tidy(summary(s_bilr2_d5), stats = "becover"),
  tidy(summary(s_bilr3_d5), stats = "becover"),
  tidy(summary(s_bilr4_d5), stats = "becover"),
  tidy(summary(s_wilr1_d5), stats = "becover"),
  tidy(summary(s_wilr2_d5), stats = "becover"),
  tidy(summary(s_wilr3_d5), stats = "becover"),
  tidy(summary(s_wilr4_d5), stats = "becover")
))


brmcoda_cover_d3[, NoOfParts := 3]
brmcoda_cover_d4[, NoOfParts := 4]
brmcoda_cover_d5[, NoOfParts := 5]

brmcoda_cover <- rbind(
  brmcoda_cover_d3,
  brmcoda_cover_d4,
  brmcoda_cover_d5
)

setnames(brmcoda_cover, "est", "Coverage")
brmcoda_cover[, condition := factor(condition,
                                    levels = c(
                                      "base",
                                      "REbase_RESsmall",
                                      "REbase_RESlarge",
                                      "RElarge_RESsmall"
                                    ))]
brmcoda_cover[, N := factor(N, levels = c(
  30, 50, 360, 1200
))]
brmcoda_cover[, K := factor(K, levels = c(
  3, 5, 7, 14
))]
brmcoda_cover[, NoOfParts := as.factor(NoOfParts)]
brmcoda_cover[, NK := as.factor(paste0(N, "-", K))]
brmcoda_cover[, cond := paste0(N, "-", K, "-", condition)]
brmcoda_cover[, cond := factor(cond,
                               levels = c(
                                 "30-3-base",    "30-3-REbase_RESlarge",    "30-3-REbase_RESsmall",    "30-3-RElarge_RESsmall",
                                 "30-5-base",    "30-5-REbase_RESlarge",    "30-5-REbase_RESsmall",    "30-5-RElarge_RESsmall",
                                 "30-7-base",    "30-7-REbase_RESlarge",    "30-7-REbase_RESsmall",    "30-7-RElarge_RESsmall",
                                 "30-14-base",   "30-14-REbase_RESlarge",   "30-14-REbase_RESsmall",   "30-14-RElarge_RESsmall",
                                 "50-3-base",    "50-3-REbase_RESlarge",    "50-3-REbase_RESsmall",    "50-3-RElarge_RESsmall",
                                 "50-5-base",    "50-5-REbase_RESlarge",    "50-5-REbase_RESsmall",    "50-5-RElarge_RESsmall",
                                 "50-7-base",   "50-7-REbase_RESlarge",    "50-7-REbase_RESsmall",    "50-7-RElarge_RESsmall",
                                 "50-14-base",   "50-14-REbase_RESlarge",   "50-14-REbase_RESsmall",   "50-14-RElarge_RESsmall",
                                 "360-3-base",   "360-3-REbase_RESlarge",   "360-3-REbase_RESsmall",   "360-3-RElarge_RESsmall",
                                 "360-5-base",   "360-5-REbase_RESlarge",   "360-5-REbase_RESsmall",   "360-5-RElarge_RESsmall",
                                 "360-7-base",  "360-7-REbase_RESlarge",   "360-7-REbase_RESsmall",   "360-7-RElarge_RESsmall",
                                 "360-14-base",  "360-14-REbase_RESlarge",  "360-14-REbase_RESsmall",  "360-14-RElarge_RESsmall",
                                 "1200-3-base",  "1200-3-REbase_RESlarge",  "1200-3-REbase_RESsmall",  "1200-3-RElarge_RESsmall",
                                 "1200-5-base",  "1200-5-REbase_RESlarge",  "1200-5-REbase_RESsmall",  "1200-5-RElarge_RESsmall",
                                 "1200-7-base",  "1200-7-REbase_RESlarge",  "1200-7-REbase_RESsmall",  "1200-7-RElarge_RESsmall",
                                 "1200-14-base", "1200-14-REbase_RESlarge", "1200-14-REbase_RESsmall", "1200-14-RElarge_RESsmall"
                               ))]
ggplot(brmcoda_cover, aes(Coverage, cond, by = NoOfParts)) +
  geom_point() +
  ggplot2::geom_vline(xintercept = 0.95, color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid( ~ NoOfParts) +
  xlab("brmcoda Coverage") +
  xlim(c(0.9, 1))


# individual plot
ggplot(brmcoda_d3[N == 30 & K == 3 & stat == "bias" & by == "  b0" & condition == "base"], 
       aes(x = by, y = est, 
           ymin = lower, ymax = upper,
           colour = by)) +
         # geom_hline(color = "#666666", linetype = "dotted", linewidth = 0.5) +
         geom_point() +
         geom_linerange(linewidth = 1)


