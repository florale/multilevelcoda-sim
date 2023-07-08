source("simsum_tidy.R")

colour <- c("#A69188", "#DCD5CE", "#FAF7F3", "#A1B2C2", "#CFDAE2")
col5 <- wes_palette("Cavalcanti1", 5)
col4 <- c("#EFE3E0", "#BEACA2", "#708885", "#5A6367")
col3 <- wes_palette("IsleofDogs1", 3)
col20 <- c("#1C1718", "#5A6367",
            "#2A3E59", "#456691", 
            "#647F9A", "#8CAACB", 
            # "#9c8aa4", "#ABA2C3",
            # "#9A5C7D", "#B98AA3", 
            # "#cc8a8c", "#A54E50", 
            "#DCD5CE",
            # "#B49797",
            # "#C99696",
            "#DAA5AE",
            # "#d18d9a",
            "#b6485d", 
            
            # "#D1ACA5",
            
            # "#C7AAA5", "#ba6c6e", 
            # "#4F7375", "#769798",
            
            "#944C4C", 
            "#bf5b4b", "#bb847a", 
            "#A69188", "#EAD3BF", 
            "#FAD899", 
            
            # "#353D60", "#6171a9",
            
            "#8DA290", "#133A1B", 
            
            "#6d765b", "#3b4031", 
            
            # "#c48462",
            "#3d251e",
            
            # "#D1ACA5", "#ab8b8b",
            "#D1ACA5"
)

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

## d3 brmcoda base plot ----------------
plot_bias_brmcoda_d3 <- 
  .par_plot(brmcoda_d3[stat == "bias"])  +
  ylim(c(-0.12, 0.1))
plot__brmcoda_d3 <- 
  .par_plot(brmcoda_d3[stat == "becover"]) +
  ylim(c(0.9, 1))

## d3 sub base plot ----------------
plot_bias_sub_d3 <-
  .par_plot(sub_d3[stat == "bias" & condition == "base"]) +
  ylim(c(-0.05, 0.05))
plot_bias_sub_d3

plot_becover_sub_d3 <-
  .par_plot(sub_d3[stat == "becover" & condition == "base"]) +
  ylim(c(0.75, 1))
plot_becover_sub_d3

ggarrange(plot_bias_sub_d3,
          plot_becover_sub_d3,
          ncol = 2,
          common.legend = TRUE)

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
## d4 brmcoda base plot -------
.par_plot(brmcoda_d4[stat == "bias"])  +
  ylim(c(-0.12, 0.1))
.par_plot(brmcoda_d4[stat == "becover"]) +
  ylim(c(0.9, 1))

## d4 sub base plot ---------
plot_bias_sub_d4 <-
  .par_plot(sub_d4[stat == "bias" & condition == "base"]) +
  ylim(c(-0.05, 0.05))
plot_bias_sub_d4

plot_becover_sub_d4 <-
  .par_plot(sub_d4[stat == "becover" & condition == "base"]) +
  ylim(c(0.65, 1))
plot_becover_sub_d4

ggarrange(plot_bias_sub_d4,
          plot_becover_sub_d4,
          ncol = 2,
          common.legend = TRUE)

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

## d5 brmcoda base plot -------
.par_plot(brmcoda_d5[stat == "bias"])  +
  ylim(c(-0.2, 0.2))
.par_plot(brmcoda_d5[stat == "becover"]) +
  ylim(c(0.9, 1))

## d5 sub base plot ---------
plot_bias_sub_d5 <-
  .par_plot(sub_d5[stat == "bias" & condition == "base"]) +
  ylim(c(-0.05, 0.05))
plot_bias_sub_d5

plot_becover_sub_d5 <-
  .par_plot(sub_d5[stat == "becover" & condition == "base"]) +
  ylim(c(0.8, 1))
plot_becover_sub_d5

ggarrange(plot_bias_sub_d5,
          plot_becover_sub_d5,
          ncol = 2,
          common.legend = TRUE)
