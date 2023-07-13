# set up ------------
colour <- wes_palette("IsleofDogs2", 5)
colour <- wes_palette("Cavalcanti1", 5)
colour <- c("#EFE3E0", "#BEACA2", "#708885", "#5A6367")
colour <- c("#A69188", "#DCD5CE", "#FAF7F3", "#A1B2C2", "#CFDAE2")

# autoplot(summary(s_b0_d3), type = "lolly", stats = "bias")
# autoplot(summary(s_b0_d3), type = "lolly", stats = "becover")
# 
# autoplot(summary(s_b0_d3), type = "forest", stats = "becover")
# autoplot(summary(s_b0_d3), type = "forest", stats = "bias")

## bias brmcoda 3part ------------------------
.forest_plot(
  object = s_b0_d3,
  stats = "bias"
)

.forest_plot(
  object = s_bilr1_d3,
  stats = "bias"
)

.forest_plot(
  object = s_bilr2_d3,
  stats = "bias"
)

.forest_plot(
  object = s_wilr1_d3,
  stats = "bias"
)

.forest_plot(
  object = s_wilr2_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_u0_base_d3,
  stats = "bias"
)

.forest_plot(
  object = s_u0_small_d3,
  stats = "bias"
)

.forest_plot(
  object = s_u0_large_d3,
  stats = "bias"
)

.forest_plot(
  object = s_sigma_base_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_small_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_large_d3,
  stats = "bias"
) 

## bias brmcoda 4part ------------------------
.forest_plot(
  object = s_b0_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr1_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr2_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr3_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr1_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr2_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr3_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_u0_base_d4,
  stats = "bias"
)

.forest_plot(
  object = s_u0_small_d4,
  stats = "bias"
)

.forest_plot(
  object = s_u0_large_d4,
  stats = "bias"
)

.forest_plot(
  object = s_sigma_base_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_small_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_large_d4,
  stats = "bias"
) 

## bias brmcoda 5part ------------------------
.forest_plot(
  object = s_b0_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr1_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr2_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr3_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bilr4_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr1_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr2_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr3_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wilr4_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_u0_base_d5,
  stats = "bias"
)

.forest_plot(
  object = s_u0_small_d5,
  stats = "bias"
)

.forest_plot(
  object = s_u0_large_d5,
  stats = "bias"
)

.forest_plot(
  object = s_sigma_base_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_small_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_sigma_large_d5,
  stats = "bias"
) 

# bias between substitution 3part --------------------
.forest_plot(
  object = s_bsub_sleep_pa_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sleep_sb_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_pa_sleep_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_pa_sb_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_sleep_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_pa_d3,
  stats = "bias"
) 

# bias between substitution 4part ----------------
.forest_plot(
  object = s_bsub_sleep_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sleep_lpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sleep_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_lpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_lpa_d4,
  stats = "bias"
) 

# bias between substitution 5part ---------------------
.forest_plot(
  object = s_bsub_tst_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_tst_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_tst_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_tst_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_wake_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_wake_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_wake_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_wake_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_mvpa_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_lpa_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_bsub_sb_lpa_d5,
  stats = "bias"
) 

# bias within substitution 3part ----------------------
.forest_plot(
  object = s_wsub_sleep_pa_d3,
  stats = "bias"
) + ylim(c(-0.05, 0.05))

.forest_plot(
  object = s_wsub_sleep_sb_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_pa_sleep_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_pa_sb_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_sleep_d3,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_pa_d3,
  stats = "bias"
) 

# bias within substitution 4part ----------------------
.forest_plot(
  object = s_wsub_sleep_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sleep_lpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sleep_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_lpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_sb_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_sleep_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_mvpa_d4,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_lpa_d4,
  stats = "bias"
) 

# bias within substitution 5part ----------------------

.forest_plot(
  object = s_wsub_tst_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_tst_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_tst_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_tst_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_wake_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_wake_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_wake_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_wake_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_lpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_mvpa_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_lpa_sb_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_tst_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_wake_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_mvpa_d5,
  stats = "bias"
) 

.forest_plot(
  object = s_wsub_sb_lpa_d5,
  stats = "bias"
) 

## becover brmcoda 3part ------------------------
.forest_plot(
  object = s_b0_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr1_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr2_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr1_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr2_d3,
  stats = "becover"
) 

## becover brmcoda 4part ------------------------
.forest_plot(
  object = s_b0_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr1_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr2_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr3_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr1_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr2_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr3_d4,
  stats = "becover"
) 

## becover brmcoda 5part ------------------------
.forest_plot(
  object = s_b0_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr1_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr2_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr3_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bilr4_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr1_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr2_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr3_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wilr4_d5,
  stats = "becover"
) 

# becover between substitution 3part --------------------
.forest_plot(
  object = s_bsub_sleep_pa_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sleep_sb_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_pa_sleep_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_pa_sb_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_sleep_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_pa_d3,
  stats = "becover"
) 

# becover between substitution 4part ---------------
.forest_plot(
  object = s_bsub_sleep_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sleep_lpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sleep_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_lpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_lpa_d4,
  stats = "becover"
) 

# becover between substitution 5part ------------------
.forest_plot(
  object = s_bsub_tst_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_tst_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_tst_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_tst_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_wake_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_wake_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_wake_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_wake_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_mvpa_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_lpa_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_bsub_sb_lpa_d5,
  stats = "becover"
) 

# becover within substitution 3part ------------------
.forest_plot(
  object = s_wsub_sleep_pa_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sleep_sb_d3,
  stats = "cover"
) 

.forest_plot(
  object = s_wsub_pa_sleep_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_pa_sb_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_sleep_d3,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_pa_d3,
  stats = "becover"
) 

# becover within substitution 4part ------------------
.forest_plot(
  object = s_wsub_sleep_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sleep_lpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sleep_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_lpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_sb_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_sleep_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_mvpa_d4,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_lpa_d4,
  stats = "becover"
) 

# becover within substitution 5part ------------------
.forest_plot(
  object = s_wsub_tst_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_tst_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_tst_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_tst_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_wake_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_wake_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_wake_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_wake_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_lpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_mvpa_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_lpa_sb_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_tst_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_wake_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_mvpa_d5,
  stats = "becover"
) 

.forest_plot(
  object = s_wsub_sb_lpa_d5,
  stats = "becover"
) 
