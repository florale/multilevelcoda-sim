source("2c_simsum_brmcoda_out.R")
source("2d_simsum_sub_out.R")

.heat_plot(
  data = tidy(s_b0_d3, stats = "bias"),
  methodvar = s_b0_d3$methodvar,
  by = s_b0_d3$by,
  stats = tidy(s_b0_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr1_d3, stats = "bias"),
  methodvar = s_bilr1_d3$methodvar,
  by = s_bilr1_d3$by,
  stats = tidy(s_bilr1_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr2_d3, stats = "bias"),
  methodvar = s_bilr2_d3$methodvar,
  by = s_bilr2_d3$by,
  stats = tidy(s_bilr2_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr1_d3, stats = "bias"),
  methodvar = s_wilr1_d3$methodvar,
  by = s_wilr1_d3$by,
  stats = tidy(s_wilr1_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr2_d3, stats = "bias"),
  methodvar = s_wilr2_d3$methodvar,
  by = s_wilr2_d3$by,
  stats = tidy(s_wilr2_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_b0_d4, stats = "bias"),
  methodvar = s_b0_d4$methodvar,
  by = s_b0_d4$by,
  stats = tidy(s_b0_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr1_d4, stats = "bias"),
  methodvar = s_bilr1_d4$methodvar,
  by = s_bilr1_d4$by,
  stats = tidy(s_bilr1_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr2_d4, stats = "bias"),
  methodvar = s_bilr2_d4$methodvar,
  by = s_bilr2_d4$by,
  stats = tidy(s_bilr2_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr3_d4, stats = "bias"),
  methodvar = s_bilr3_d4$methodvar,
  by = s_bilr3_d4$by,
  stats = tidy(s_bilr3_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr1_d4, stats = "bias"),
  methodvar = s_wilr1_d4$methodvar,
  by = s_wilr1_d4$by,
  stats = tidy(s_wilr1_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr2_d4, stats = "bias"),
  methodvar = s_wilr2_d4$methodvar,
  by = s_wilr2_d4$by,
  stats = tidy(s_wilr2_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr3_d4, stats = "bias"),
  methodvar = s_wilr3_d4$methodvar,
  by = s_wilr3_d4$by,
  stats = tidy(s_wilr3_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_b0_d5, stats = "bias"),
  methodvar = s_b0_d5$methodvar,
  by = s_b0_d5$by,
  stats = tidy(s_b0_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr1_d5, stats = "bias"),
  methodvar = s_bilr1_d5$methodvar,
  by = s_bilr1_d5$by,
  stats = tidy(s_bilr1_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr2_d5, stats = "bias"),
  methodvar = s_bilr2_d5$methodvar,
  by = s_bilr2_d5$by,
  stats = tidy(s_bilr2_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr3_d5, stats = "bias"),
  methodvar = s_bilr3_d5$methodvar,
  by = s_bilr3_d5$by,
  stats = tidy(s_bilr3_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bilr4_d5, stats = "bias"),
  methodvar = s_bilr4_d5$methodvar,
  by = s_bilr4_d5$by,
  stats = tidy(s_bilr4_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr1_d5, stats = "bias"),
  methodvar = s_wilr1_d5$methodvar,
  by = s_wilr1_d5$by,
  stats = tidy(s_wilr1_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr2_d5, stats = "bias"),
  methodvar = s_wilr2_d5$methodvar,
  by = s_wilr2_d5$by,
  stats = tidy(s_wilr2_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr3_d5, stats = "bias"),
  methodvar = s_wilr3_d5$methodvar,
  by = s_wilr3_d5$by,
  stats = tidy(s_wilr3_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wilr4_d5, stats = "bias"),
  methodvar = s_wilr4_d5$methodvar,
  by = s_wilr4_d5$by,
  stats = tidy(s_wilr4_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient(low = "#6D8B74",
                      high = "#FAF7F3",
                      na.value = NA) +
  hrbrthemes::theme_ipsum()

## substitution ---------------
.heat_plot(
  data = tidy(s_bsub_sleep_pa_d3, stats = "bias"),
  methodvar = s_bsub_sleep_pa_d3$methodvar,
  by = s_bsub_sleep_pa_d3$by,
  stats = tidy(s_bsub_sleep_pa_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sleep_sb_d3, stats = "bias"),
  methodvar = s_bsub_sleep_sb_d3$methodvar,
  by = s_bsub_sleep_sb_d3$by,
  stats = tidy(s_bsub_sleep_sb_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_pa_sleep_d3, stats = "bias"),
  methodvar = s_bsub_pa_sleep_d3$methodvar,
  by = s_bsub_pa_sleep_d3$by,
  stats = tidy(s_bsub_pa_sleep_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_pa_sb_d3, stats = "bias"),
  methodvar = s_bsub_pa_sb_d3$methodvar,
  by = s_bsub_pa_sb_d3$by,
  stats = tidy(s_bsub_pa_sb_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_sleep_d3, stats = "bias"),
  methodvar = s_bsub_sb_sleep_d3$methodvar,
  by = s_bsub_sb_sleep_d3$by,
  stats = tidy(s_bsub_sb_sleep_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_pa_d3, stats = "bias"),
  methodvar = s_bsub_sb_pa_d3$methodvar,
  by = s_bsub_sb_pa_d3$by,
  stats = tidy(s_bsub_sb_pa_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sleep_mvpa_d4, stats = "bias"),
  methodvar = s_bsub_sleep_mvpa_d4$methodvar,
  by = s_bsub_sleep_mvpa_d4$by,
  stats = tidy(s_bsub_sleep_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sleep_lpa_d4, stats = "bias"),
  methodvar = s_bsub_sleep_lpa_d4$methodvar,
  by = s_bsub_sleep_lpa_d4$by,
  stats = tidy(s_bsub_sleep_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sleep_sb_d4, stats = "bias"),
  methodvar = s_bsub_sleep_sb_d4$methodvar,
  by = s_bsub_sleep_sb_d4$by,
  stats = tidy(s_bsub_sleep_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_sleep_d4, stats = "bias"),
  methodvar = s_bsub_mvpa_sleep_d4$methodvar,
  by = s_bsub_mvpa_sleep_d4$by,
  stats = tidy(s_bsub_mvpa_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_lpa_d4, stats = "bias"),
  methodvar = s_bsub_mvpa_lpa_d4$methodvar,
  by = s_bsub_mvpa_lpa_d4$by,
  stats = tidy(s_bsub_mvpa_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_sb_d4, stats = "bias"),
  methodvar = s_bsub_mvpa_sb_d4$methodvar,
  by = s_bsub_mvpa_sb_d4$by,
  stats = tidy(s_bsub_mvpa_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_sleep_d4, stats = "bias"),
  methodvar = s_bsub_lpa_sleep_d4$methodvar,
  by = s_bsub_lpa_sleep_d4$by,
  stats = tidy(s_bsub_lpa_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_mvpa_d4, stats = "bias"),
  methodvar = s_bsub_lpa_mvpa_d4$methodvar,
  by = s_bsub_lpa_mvpa_d4$by,
  stats = tidy(s_bsub_lpa_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_sb_d4, stats = "bias"),
  methodvar = s_bsub_lpa_sb_d4$methodvar,
  by = s_bsub_lpa_sb_d4$by,
  stats = tidy(s_bsub_lpa_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_sleep_d4, stats = "bias"),
  methodvar = s_bsub_sb_sleep_d4$methodvar,
  by = s_bsub_sb_sleep_d4$by,
  stats = tidy(s_bsub_sb_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_mvpa_d4, stats = "bias"),
  methodvar = s_bsub_sb_mvpa_d4$methodvar,
  by = s_bsub_sb_mvpa_d4$by,
  stats = tidy(s_bsub_sb_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_lpa_d4, stats = "bias"),
  methodvar = s_bsub_sb_lpa_d4$methodvar,
  by = s_bsub_sb_lpa_d4$by,
  stats = tidy(s_bsub_sb_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_tst_wake_d5, stats = "bias"),
  methodvar = s_bsub_tst_wake_d5$methodvar,
  by = s_bsub_tst_wake_d5$by,
  stats = tidy(s_bsub_tst_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_tst_mvpa_d5, stats = "bias"),
  methodvar = s_bsub_tst_mvpa_d5$methodvar,
  by = s_bsub_tst_mvpa_d5$by,
  stats = tidy(s_bsub_tst_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_tst_lpa_d5, stats = "bias"),
  methodvar = s_bsub_tst_lpa_d5$methodvar,
  by = s_bsub_tst_lpa_d5$by,
  stats = tidy(s_bsub_tst_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_tst_sb_d5, stats = "bias"),
  methodvar = s_bsub_tst_sb_d5$methodvar,
  by = s_bsub_tst_sb_d5$by,
  stats = tidy(s_bsub_tst_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_wake_tst_d5, stats = "bias"),
  methodvar = s_bsub_wake_tst_d5$methodvar,
  by = s_bsub_wake_tst_d5$by,
  stats = tidy(s_bsub_wake_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_wake_mvpa_d5, stats = "bias"),
  methodvar = s_bsub_wake_mvpa_d5$methodvar,
  by = s_bsub_wake_mvpa_d5$by,
  stats = tidy(s_bsub_wake_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_wake_lpa_d5, stats = "bias"),
  methodvar = s_bsub_wake_lpa_d5$methodvar,
  by = s_bsub_wake_lpa_d5$by,
  stats = tidy(s_bsub_wake_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_wake_sb_d5, stats = "bias"),
  methodvar = s_bsub_wake_sb_d5$methodvar,
  by = s_bsub_wake_sb_d5$by,
  stats = tidy(s_bsub_wake_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_tst_d5, stats = "bias"),
  methodvar = s_bsub_mvpa_tst_d5$methodvar,
  by = s_bsub_mvpa_tst_d5$by,
  stats = tidy(s_bsub_mvpa_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_wake_d5, stats = "bias"),
  methodvar = s_bsub_mvpa_wake_d5$methodvar,
  by = s_bsub_mvpa_wake_d5$by,
  stats = tidy(s_bsub_mvpa_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_lpa_d5, stats = "bias"),
  methodvar = s_bsub_mvpa_lpa_d5$methodvar,
  by = s_bsub_mvpa_lpa_d5$by,
  stats = tidy(s_bsub_mvpa_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_mvpa_sb_d5, stats = "bias"),
  methodvar = s_bsub_mvpa_sb_d5$methodvar,
  by = s_bsub_mvpa_sb_d5$by,
  stats = tidy(s_bsub_mvpa_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_tst_d5, stats = "bias"),
  methodvar = s_bsub_lpa_tst_d5$methodvar,
  by = s_bsub_lpa_tst_d5$by,
  stats = tidy(s_bsub_lpa_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_wake_d5, stats = "bias"),
  methodvar = s_bsub_lpa_wake_d5$methodvar,
  by = s_bsub_lpa_wake_d5$by,
  stats = tidy(s_bsub_lpa_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_mvpa_d5, stats = "bias"),
  methodvar = s_bsub_lpa_mvpa_d5$methodvar,
  by = s_bsub_lpa_mvpa_d5$by,
  stats = tidy(s_bsub_lpa_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_lpa_sb_d5, stats = "bias"),
  methodvar = s_bsub_lpa_sb_d5$methodvar,
  by = s_bsub_lpa_sb_d5$by,
  stats = tidy(s_bsub_lpa_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_tst_d5, stats = "bias"),
  methodvar = s_bsub_sb_tst_d5$methodvar,
  by = s_bsub_sb_tst_d5$by,
  stats = tidy(s_bsub_sb_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_wake_d5, stats = "bias"),
  methodvar = s_bsub_sb_wake_d5$methodvar,
  by = s_bsub_sb_wake_d5$by,
  stats = tidy(s_bsub_sb_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_mvpa_d5, stats = "bias"),
  methodvar = s_bsub_sb_mvpa_d5$methodvar,
  by = s_bsub_sb_mvpa_d5$by,
  stats = tidy(s_bsub_sb_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_bsub_sb_lpa_d5, stats = "bias"),
  methodvar = s_bsub_sb_lpa_d5$methodvar,
  by = s_bsub_sb_lpa_d5$by,
  stats = tidy(s_bsub_sb_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sleep_pa_d3, stats = "bias"),
  methodvar = s_wsub_sleep_pa_d3$methodvar,
  by = s_wsub_sleep_pa_d3$by,
  stats = tidy(s_wsub_sleep_pa_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sleep_sb_d3, stats = "bias"),
  methodvar = s_wsub_sleep_sb_d3$methodvar,
  by = s_wsub_sleep_sb_d3$by,
  stats = tidy(s_wsub_sleep_sb_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_pa_sleep_d3, stats = "bias"),
  methodvar = s_wsub_pa_sleep_d3$methodvar,
  by = s_wsub_pa_sleep_d3$by,
  stats = tidy(s_wsub_pa_sleep_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_pa_sb_d3, stats = "bias"),
  methodvar = s_wsub_pa_sb_d3$methodvar,
  by = s_wsub_pa_sb_d3$by,
  stats = tidy(s_wsub_pa_sb_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_sleep_d3, stats = "bias"),
  methodvar = s_wsub_sb_sleep_d3$methodvar,
  by = s_wsub_sb_sleep_d3$by,
  stats = tidy(s_wsub_sb_sleep_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_pa_d3, stats = "bias"),
  methodvar = s_wsub_sb_pa_d3$methodvar,
  by = s_wsub_sb_pa_d3$by,
  stats = tidy(s_wsub_sb_pa_d3, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sleep_mvpa_d4, stats = "bias"),
  methodvar = s_wsub_sleep_mvpa_d4$methodvar,
  by = s_wsub_sleep_mvpa_d4$by,
  stats = tidy(s_wsub_sleep_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sleep_lpa_d4, stats = "bias"),
  methodvar = s_wsub_sleep_lpa_d4$methodvar,
  by = s_wsub_sleep_lpa_d4$by,
  stats = tidy(s_wsub_sleep_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sleep_sb_d4, stats = "bias"),
  methodvar = s_wsub_sleep_sb_d4$methodvar,
  by = s_wsub_sleep_sb_d4$by,
  stats = tidy(s_wsub_sleep_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_sleep_d4, stats = "bias"),
  methodvar = s_wsub_mvpa_sleep_d4$methodvar,
  by = s_wsub_mvpa_sleep_d4$by,
  stats = tidy(s_wsub_mvpa_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_lpa_d4, stats = "bias"),
  methodvar = s_wsub_mvpa_lpa_d4$methodvar,
  by = s_wsub_mvpa_lpa_d4$by,
  stats = tidy(s_wsub_mvpa_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_sb_d4, stats = "bias"),
  methodvar = s_wsub_mvpa_sb_d4$methodvar,
  by = s_wsub_mvpa_sb_d4$by,
  stats = tidy(s_wsub_mvpa_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_sleep_d4, stats = "bias"),
  methodvar = s_wsub_lpa_sleep_d4$methodvar,
  by = s_wsub_lpa_sleep_d4$by,
  stats = tidy(s_wsub_lpa_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_mvpa_d4, stats = "bias"),
  methodvar = s_wsub_lpa_mvpa_d4$methodvar,
  by = s_wsub_lpa_mvpa_d4$by,
  stats = tidy(s_wsub_lpa_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_sb_d4, stats = "bias"),
  methodvar = s_wsub_lpa_sb_d4$methodvar,
  by = s_wsub_lpa_sb_d4$by,
  stats = tidy(s_wsub_lpa_sb_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_sleep_d4, stats = "bias"),
  methodvar = s_wsub_sb_sleep_d4$methodvar,
  by = s_wsub_sb_sleep_d4$by,
  stats = tidy(s_wsub_sb_sleep_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_mvpa_d4, stats = "bias"),
  methodvar = s_wsub_sb_mvpa_d4$methodvar,
  by = s_wsub_sb_mvpa_d4$by,
  stats = tidy(s_wsub_sb_mvpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_lpa_d4, stats = "bias"),
  methodvar = s_wsub_sb_lpa_d4$methodvar,
  by = s_wsub_sb_lpa_d4$by,
  stats = tidy(s_wsub_sb_lpa_d4, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_tst_wake_d5, stats = "bias"),
  methodvar = s_wsub_tst_wake_d5$methodvar,
  by = s_wsub_tst_wake_d5$by,
  stats = tidy(s_wsub_tst_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_tst_mvpa_d5, stats = "bias"),
  methodvar = s_wsub_tst_mvpa_d5$methodvar,
  by = s_wsub_tst_mvpa_d5$by,
  stats = tidy(s_wsub_tst_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_tst_lpa_d5, stats = "bias"),
  methodvar = s_wsub_tst_lpa_d5$methodvar,
  by = s_wsub_tst_lpa_d5$by,
  stats = tidy(s_wsub_tst_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_tst_sb_d5, stats = "bias"),
  methodvar = s_wsub_tst_sb_d5$methodvar,
  by = s_wsub_tst_sb_d5$by,
  stats = tidy(s_wsub_tst_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_wake_tst_d5, stats = "bias"),
  methodvar = s_wsub_wake_tst_d5$methodvar,
  by = s_wsub_wake_tst_d5$by,
  stats = tidy(s_wsub_wake_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_wake_mvpa_d5, stats = "bias"),
  methodvar = s_wsub_wake_mvpa_d5$methodvar,
  by = s_wsub_wake_mvpa_d5$by,
  stats = tidy(s_wsub_wake_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_wake_lpa_d5, stats = "bias"),
  methodvar = s_wsub_wake_lpa_d5$methodvar,
  by = s_wsub_wake_lpa_d5$by,
  stats = tidy(s_wsub_wake_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_wake_sb_d5, stats = "bias"),
  methodvar = s_wsub_wake_sb_d5$methodvar,
  by = s_wsub_wake_sb_d5$by,
  stats = tidy(s_wsub_wake_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_tst_d5, stats = "bias"),
  methodvar = s_wsub_mvpa_tst_d5$methodvar,
  by = s_wsub_mvpa_tst_d5$by,
  stats = tidy(s_wsub_mvpa_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_wake_d5, stats = "bias"),
  methodvar = s_wsub_mvpa_wake_d5$methodvar,
  by = s_wsub_mvpa_wake_d5$by,
  stats = tidy(s_wsub_mvpa_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_lpa_d5, stats = "bias"),
  methodvar = s_wsub_mvpa_lpa_d5$methodvar,
  by = s_wsub_mvpa_lpa_d5$by,
  stats = tidy(s_wsub_mvpa_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_mvpa_sb_d5, stats = "bias"),
  methodvar = s_wsub_mvpa_sb_d5$methodvar,
  by = s_wsub_mvpa_sb_d5$by,
  stats = tidy(s_wsub_mvpa_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_tst_d5, stats = "bias"),
  methodvar = s_wsub_lpa_tst_d5$methodvar,
  by = s_wsub_lpa_tst_d5$by,
  stats = tidy(s_wsub_lpa_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_wake_d5, stats = "bias"),
  methodvar = s_wsub_lpa_wake_d5$methodvar,
  by = s_wsub_lpa_wake_d5$by,
  stats = tidy(s_wsub_lpa_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_mvpa_d5, stats = "bias"),
  methodvar = s_wsub_lpa_mvpa_d5$methodvar,
  by = s_wsub_lpa_mvpa_d5$by,
  stats = tidy(s_wsub_lpa_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_lpa_sb_d5, stats = "bias"),
  methodvar = s_wsub_lpa_sb_d5$methodvar,
  by = s_wsub_lpa_sb_d5$by,
  stats = tidy(s_wsub_lpa_sb_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_tst_d5, stats = "bias"),
  methodvar = s_wsub_sb_tst_d5$methodvar,
  by = s_wsub_sb_tst_d5$by,
  stats = tidy(s_wsub_sb_tst_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_wake_d5, stats = "bias"),
  methodvar = s_wsub_sb_wake_d5$methodvar,
  by = s_wsub_sb_wake_d5$by,
  stats = tidy(s_wsub_sb_wake_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_mvpa_d5, stats = "bias"),
  methodvar = s_wsub_sb_mvpa_d5$methodvar,
  by = s_wsub_sb_mvpa_d5$by,
  stats = tidy(s_wsub_sb_mvpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()

.heat_plot(
  data = tidy(s_wsub_sb_lpa_d5, stats = "bias"),
  methodvar = s_wsub_sb_lpa_d5$methodvar,
  by = s_wsub_sb_lpa_d5$by,
  stats = tidy(s_wsub_sb_lpa_d5, stats = "bias")$stat[[1]]
) +
  scale_fill_gradient2(
    low = "#6D8B74",
    high = "#FAF7F3",
    na.value = NA
  ) +
  hrbrthemes::theme_ipsum()
