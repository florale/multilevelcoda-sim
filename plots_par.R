source("simsum_tidy.R")

# library, colours ----------------------
colour <- c("#A69188", "#DCD5CE", "#FAF7F3", "#A1B2C2", "#CFDAE2")
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
            
            "#C7AAA5", "#ba6c6e",
            "#4F7375", "#769798",
            
            "#944C4C", 
            "#bf5b4b", "#bb847a", 
            "#A69188", "#EAD3BF", 
            "#FAD899", 
            
            "#353D60", "#6171a9",
            
            "#8DA290", "#133A1B", 
            
            "#6d765b", "#3b4031", 
            
            # "#c48462",
            "#3d251e",
            
            # "#D1ACA5", "#ab8b8b",
            "#D1ACA5"
)

col_brmcoda_d3 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#1C1718")
col_brmcoda_d4 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#456691", "#2A3E59", 
    "#1C1718")
col_brmcoda_d5 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#456691", "#2A3E59", 
    "#9c8aa4", "#5E4F65", 
    "#1C1718")

col_sub_d3 <- 
  c("#bf5b4b", "#A69188", 
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B")
col_sub_d4 <-
  c("#2A3E59", "#456691",
    "#944C4C", "#C99696",
    "#bf5b4b", "#A69188", 
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B",
    "#6d765b", "#3d251e")
col_sub_d5 <- 
  c("#1C1718", "#2A3E59", 
    "#456691", "#647F9A", 
    "#8CAACB", "#DCD5CE", 
    "#DAA5AE", "#b6485d", 
    "#944C4C", "#C99696",
    "#bf5b4b", "#bb847a", 
    "#A69188", "#EAD3BF", 
    "#FAD899", "#8DA290", 
    "#133A1B", "#6d765b", 
    "#3b4031", "#3d251e")
  
## d3 brmcoda base plot ----------------------------------------------------------------------------
colour <- col_brmcoda_d3
brmcoda_d3_base <- rbind(
  b0_d3[condition == "base"][, -c("condition")],
  bilr1_d3[condition == "base"][, -c("condition")],
  bilr2_d3[condition == "base"][, -c("condition")],
  wilr1_d3[condition == "base"][, -c("condition")],
  wilr2_d3[condition == "base"][, -c("condition")],
  u0_base_d3[sigma_condition == "base"][, -c("sigma_condition")],
  sigma_base_d3
)

brmcoda_d3_base[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3_base$by)
brmcoda_d3_base[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

plot_bias_brmcoda_d3_base <- .par_plot(brmcoda_d3_base[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.1, 0.1),
                     breaks = c(-0.1, 0, 0.1))
plot_bias_brmcoda_d3_base

plot_becover_brmcoda_d3_base <- .par_plot(brmcoda_d3_base[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d3_base

## d3 sub base plot --------------------------------------------------------------------------------
colour <- col_sub_d3
plot_bias_sub_d3_base <-
  .par_plot(sub_d3[stat == "bias" & condition == "base"])  +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d3_base

plot_becover_sub_d3_base <-
  .par_plot(sub_d3[stat == "becover" & condition == "base"]) +
  scale_y_continuous(limits = c(0.75, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d3_base

## d4 brmcoda base plot ----------------------------------------------------------------------------
colour <- col_brmcoda_d4
brmcoda_d4_base <- rbind(
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

brmcoda_d4_base[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4_base$by)
brmcoda_d4_base[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]
plot_bias_brmcoda_d4_base <- .par_plot(brmcoda_d4_base[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.12, 0.12),
                     breaks = c(-0.1, 0, 0.1))
plot_bias_brmcoda_d4_base

plot_becover_brmcoda_d4_base <- .par_plot(brmcoda_d4_base[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d4_base

## d4 sub base plot --------------------------------------------------------------------------------
colour <- col_sub_d4
plot_bias_sub_d4_base <-
  .par_plot(sub_d4[stat == "bias" & condition == "base"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d4_base

plot_becover_sub_d4_base <-
  .par_plot(sub_d4[stat == "becover" & condition == "base"]) +
  scale_y_continuous(limits = c(0.65, 1),
                     breaks = c(0.65, 0.95, 1))
plot_becover_sub_d4_base

## d5 brmcoda base plot ----------------------------------------------------------------------------
colour <- col_brmcoda_d5
brmcoda_d5_base <- rbind(
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

brmcoda_d5_base[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d5_base$by)
brmcoda_d5_base[, by := factor(
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

plot_bias_brmcoda_d5_base <- .par_plot(brmcoda_d5_base[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.1, 0, 0.1))
plot_bias_brmcoda_d5_base

plot_becover_brmcoda_d5_base <- .par_plot(brmcoda_d5_base[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d5_base

## d5 sub base plot --------------------------------------------------------------------------------
colour <- col_sub_d5
plot_bias_sub_d5_base <-
  .par_plot(sub_d5[stat == "bias" & condition == "base"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d5_base

plot_becover_sub_d5_base <-
  .par_plot(sub_d5[stat == "becover" & condition == "base"]) +
  scale_y_continuous(limits = c(0.8, 1),
                     breaks = c(0.8, 0.95, 1))
plot_becover_sub_d5_base

## d3 brmcoda ubes plot - u0 base e small ----------------------------------------------------------
colour <- col_brmcoda_d3
brmcoda_d3_ubes <- rbind(
  b0_d3[condition == "REbase_RESsmall"][, -c("condition")],
  bilr1_d3[condition == "REbase_RESsmall"][, -c("condition")],
  bilr2_d3[condition == "REbase_RESsmall"][, -c("condition")],
  wilr1_d3[condition == "REbase_RESsmall"][, -c("condition")],
  wilr2_d3[condition == "REbase_RESsmall"][, -c("condition")],
  u0_base_d3[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d3
)

brmcoda_d3_ubes[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3_ubes$by)
brmcoda_d3_ubes[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

plot_bias_brmcoda_d3_ubes <- .par_plot(brmcoda_d3_ubes[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.1, 0.1),
                     breaks = c(-0.1, 0, 0.1))
plot_bias_brmcoda_d3_ubes

plot_becover_brmcoda_d3_ubes <- .par_plot(brmcoda_d3_ubes[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d3_ubes

## d3 sub ubes plot - u0 base e small --------------------------------------------------------------
colour <- col_sub_d3
plot_bias_sub_d3_ubes <-
  .par_plot(sub_d3[stat == "bias" & condition == "REbase_RESsmall"])  +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d3_ubes

plot_becover_sub_d3_ubes <-
  .par_plot(sub_d3[stat == "becover" & condition == "REbase_RESsmall"]) +
  scale_y_continuous(limits = c(0.75, 1),
                     breaks = c(0.75, 0.95, 1))
plot_becover_sub_d3_ubes

## d4 brmcoda ubes plot - u0 base e small ----------------------------------------------------------
colour <- col_brmcoda_d4
brmcoda_d4_ubes <- rbind(
  b0_d4[condition == "REbase_RESsmall"][, -c("condition")],
  bilr1_d4[condition == "REbase_RESsmall"][, -c("condition")],
  bilr2_d4[condition == "REbase_RESsmall"][, -c("condition")],
  bilr3_d4[condition == "REbase_RESsmall"][, -c("condition")],
  wilr1_d4[condition == "REbase_RESsmall"][, -c("condition")],
  wilr2_d4[condition == "REbase_RESsmall"][, -c("condition")],
  wilr3_d4[condition == "REbase_RESsmall"][, -c("condition")],
  u0_base_d4[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d4
)

brmcoda_d4_ubes[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4_ubes$by)
brmcoda_d4_ubes[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]
plot_bias_brmcoda_d4_ubes <- .par_plot(brmcoda_d4_ubes[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.12, 0.1),
                     breaks = c(-0.1, 0, 0.1))
plot_bias_brmcoda_d4_ubes

plot_becover_brmcoda_d4_ubes <- .par_plot(brmcoda_d4_ubes[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d4_ubes

## d4 sub ubes plot - u0 base e small --------------------------------------------------------------
colour <- col_sub_d4
plot_bias_sub_d4_ubes <-
  .par_plot(sub_d4[stat == "bias" & condition == "REbase_RESsmall"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d4_ubes

plot_becover_sub_d4_ubes <-
  .par_plot(sub_d4[stat == "becover" & condition == "REbase_RESsmall"]) +
  scale_y_continuous(limits = c(0.40, 1),
                     breaks = c(0.40, 0.95, 1))
plot_becover_sub_d4_ubes

## d5 brmcoda ubes plot - u0 base e small ----------------------------------------------------------
colour <- col_brmcoda_d5
brmcoda_d5_ubes <- rbind(
  b0_d5[condition == "REbase_RESsmall"][, -c("condition")],
  bilr1_d5[condition == "REbase_RESsmall"][, -c("condition")],
  bilr2_d5[condition == "REbase_RESsmall"][, -c("condition")],
  bilr3_d5[condition == "REbase_RESsmall"][, -c("condition")],
  bilr4_d5[condition == "REbase_RESsmall"][, -c("condition")],
  wilr1_d5[condition == "REbase_RESsmall"][, -c("condition")],
  wilr2_d5[condition == "REbase_RESsmall"][, -c("condition")],
  wilr3_d5[condition == "REbase_RESsmall"][, -c("condition")],
  wilr4_d5[condition == "REbase_RESsmall"][, -c("condition")],
  u0_base_d5[sigma_condition == "small"][, -c("sigma_condition")],
  sigma_small_d5
)

brmcoda_d5_ubes[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d5_ubes$by)
brmcoda_d5_ubes[, by := factor(
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

plot_bias_brmcoda_d5_ubes <- .par_plot(brmcoda_d5_ubes[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d5_ubes

plot_becover_brmcoda_d5_ubes <- .par_plot(brmcoda_d5_ubes[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d5_ubes

## d5 sub ubes plot - u0 base e small --------------------------------------------------------------
colour <- col_sub_d5
plot_bias_sub_d5_ubes <-
  .par_plot(sub_d5[stat == "bias" & condition == "REbase_RESsmall"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d5_ubes

plot_becover_sub_d5_ubes <-
  .par_plot(sub_d5[stat == "becover" & condition == "REbase_RESsmall"]) +
  scale_y_continuous(limits = c(0.65, 1),
                     breaks = c(0.65, 0.95, 1))
plot_becover_sub_d5_ubes

## d3 brmcoda ubel plot - u0 base e large ----------------------------------------------------------
colour <- col_brmcoda_d3
brmcoda_d3_ubel <- rbind(
  b0_d3[condition == "REbase_RESlarge"][, -c("condition")],
  bilr1_d3[condition == "REbase_RESlarge"][, -c("condition")],
  bilr2_d3[condition == "REbase_RESlarge"][, -c("condition")],
  wilr1_d3[condition == "REbase_RESlarge"][, -c("condition")],
  wilr2_d3[condition == "REbase_RESlarge"][, -c("condition")],
  u0_base_d3[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d3
)

brmcoda_d3_ubel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3_ubel$by)
brmcoda_d3_ubel[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

plot_bias_brmcoda_d3_ubel <- .par_plot(brmcoda_d3_ubel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d3_ubel

plot_becover_brmcoda_d3_ubel <- .par_plot(brmcoda_d3_ubel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d3_ubel

## d3 sub ubel plot - u0 base e large --------------------------------------------------------------
colour <- col_sub_d3
plot_bias_sub_d3_ubel <-
  .par_plot(sub_d3[stat == "bias" & condition == "REbase_RESlarge"])  +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d3_ubel

plot_becover_sub_d3_ubel <-
  .par_plot(sub_d3[stat == "becover" & condition == "REbase_RESlarge"]) +
  scale_y_continuous(limits = c(0.75, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d3_ubel

## d4 brmcoda ubel plot - u0 base e large ----------------------------------------------------------
colour <- col_brmcoda_d4
brmcoda_d4_ubel <- rbind(
  b0_d4[condition == "REbase_RESlarge"][, -c("condition")],
  bilr1_d4[condition == "REbase_RESlarge"][, -c("condition")],
  bilr2_d4[condition == "REbase_RESlarge"][, -c("condition")],
  bilr3_d4[condition == "REbase_RESlarge"][, -c("condition")],
  wilr1_d4[condition == "REbase_RESlarge"][, -c("condition")],
  wilr2_d4[condition == "REbase_RESlarge"][, -c("condition")],
  wilr3_d4[condition == "REbase_RESlarge"][, -c("condition")],
  u0_base_d4[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d4
)

brmcoda_d4_ubel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4_ubel$by)
brmcoda_d4_ubel[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]
plot_bias_brmcoda_d4_ubel <- .par_plot(brmcoda_d4_ubel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d4_ubel

plot_becover_brmcoda_d4_ubel <- .par_plot(brmcoda_d4_ubel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d4_ubel

## d4 sub ubel plot - u0 base e large --------------------------------------------------------------
colour <- col_sub_d4
plot_bias_sub_d4_ubel <-
  .par_plot(sub_d4[stat == "bias" & condition == "REbase_RESlarge"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d4_ubel

plot_becover_sub_d4_ubel <-
  .par_plot(sub_d4[stat == "becover" & condition == "REbase_RESlarge"]) +
  scale_y_continuous(limits = c(0.75, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d4_ubel

## d5 brmcoda ubel plot - u0 base e large ----------------------------------------------------------
colour <- col_brmcoda_d5
brmcoda_d5_ubel <- rbind(
  b0_d5[condition == "REbase_RESlarge"][, -c("condition")],
  bilr1_d5[condition == "REbase_RESlarge"][, -c("condition")],
  bilr2_d5[condition == "REbase_RESlarge"][, -c("condition")],
  bilr3_d5[condition == "REbase_RESlarge"][, -c("condition")],
  bilr4_d5[condition == "REbase_RESlarge"][, -c("condition")],
  wilr1_d5[condition == "REbase_RESlarge"][, -c("condition")],
  wilr2_d5[condition == "REbase_RESlarge"][, -c("condition")],
  wilr3_d5[condition == "REbase_RESlarge"][, -c("condition")],
  wilr4_d5[condition == "REbase_RESlarge"][, -c("condition")],
  u0_base_d5[sigma_condition == "large"][, -c("sigma_condition")],
  sigma_large_d5
)

brmcoda_d5_ubel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d5_ubel$by)
brmcoda_d5_ubel[, by := factor(
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

plot_bias_brmcoda_d5_ubel <- .par_plot(brmcoda_d5_ubel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d5_ubel

plot_becover_brmcoda_d5_ubel <- .par_plot(brmcoda_d5_ubel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d5_ubel

## d5 sub ubel plot - u0 base e large --------------------------------------------------------------
colour <- col_sub_d5
plot_bias_sub_d5_ubel <-
  .par_plot(sub_d5[stat == "bias" & condition == "REbase_RESlarge"]) +
  scale_y_continuous(limits = c(-0.06, 0.06),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d5_ubel

plot_becover_sub_d5_ubel <-
  .par_plot(sub_d5[stat == "becover" & condition == "REbase_RESlarge"]) +
  scale_y_continuous(limits = c(0.65, 1),
                     breaks = c(0.65, 0.95, 1))
plot_becover_sub_d5_ubel

## d3 brmcoda usel plot - u0 small e large ----------------------------------------------------------
colour <- col_brmcoda_d3
brmcoda_d3_usel <- rbind(
  b0_d3[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr1_d3[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr2_d3[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr1_d3[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr2_d3[condition == "REsmall_RESlarge"][, -c("condition")],
  u0_small_d3,
  sigma_large_d3
)

brmcoda_d3_usel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3_usel$by)
brmcoda_d3_usel[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

plot_bias_brmcoda_d3_usel <- .par_plot(brmcoda_d3_usel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d3_usel

plot_becover_brmcoda_d3_usel <- .par_plot(brmcoda_d3_usel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d3_usel

## d3 sub usel plot - u0 small e large --------------------------------------------------------------
colour <- col_sub_d3
plot_bias_sub_d3_usel <-
  .par_plot(sub_d3[stat == "bias" & condition == "REsmall_RESlarge"])  +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d3_usel

plot_becover_sub_d3_usel <-
  .par_plot(sub_d3[stat == "becover" & condition == "REsmall_RESlarge"]) +
  scale_y_continuous(limits = c(0.75, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d3_usel

## d4 brmcoda usel plot - u0 small e large ----------------------------------------------------------
colour <- col_brmcoda_d4
brmcoda_d4_usel <- rbind(
  b0_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr1_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr2_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr3_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr1_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr2_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr3_d4[condition == "REsmall_RESlarge"][, -c("condition")],
  u0_small_d4,
  sigma_large_d4
)

brmcoda_d4_usel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4_usel$by)
brmcoda_d4_usel[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]
plot_bias_brmcoda_d4_usel <- .par_plot(brmcoda_d4_usel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d4_usel

plot_becover_brmcoda_d4_usel <- .par_plot(brmcoda_d4_usel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d4_usel

## d4 sub usel plot - u0 small e large --------------------------------------------------------------
colour <- col_sub_d4
plot_bias_sub_d4_usel <-
  .par_plot(sub_d4[stat == "bias" & condition == "REsmall_RESlarge"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d4_usel

plot_becover_sub_d4_usel <-
  .par_plot(sub_d4[stat == "becover" & condition == "REsmall_RESlarge"]) +
  scale_y_continuous(limits = c(0.70, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d4_usel

## d5 brmcoda usel plot - u0 small e large ----------------------------------------------------------
colour <- col_brmcoda_d5
brmcoda_d5_usel <- rbind(
  b0_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr1_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr2_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr3_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  bilr4_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr1_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr2_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr3_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  wilr4_d5[condition == "REsmall_RESlarge"][, -c("condition")],
  u0_small_d5,
  sigma_large_d5
)

brmcoda_d5_usel[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d5_usel$by)
brmcoda_d5_usel[, by := factor(
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

plot_bias_brmcoda_d5_usel <- .par_plot(brmcoda_d5_usel[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d5_usel

plot_becover_brmcoda_d5_usel <- .par_plot(brmcoda_d5_usel[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d5_usel

## d5 sub usel plot - u0 small e large --------------------------------------------------------------
colour <- col_sub_d5
plot_bias_sub_d5_usel <-
  .par_plot(sub_d5[stat == "bias" & condition == "REsmall_RESlarge"]) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d5_usel

plot_becover_sub_d5_usel <-
  .par_plot(sub_d5[stat == "becover" & condition == "REsmall_RESlarge"]) +
  scale_y_continuous(limits = c(0.70, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d5_usel

## d3 brmcoda ules plot - u0 large e small ----------------------------------------------------------
colour <- col_brmcoda_d3
brmcoda_d3_ules <- rbind(
  b0_d3[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr1_d3[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr2_d3[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr1_d3[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr2_d3[condition == "RElarge_RESsmall"][, -c("condition")],
  u0_large_d3,
  sigma_small_d3
)

brmcoda_d3_ules[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d3_ules$by)
brmcoda_d3_ules[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "  sigma"
  )
)]

plot_bias_brmcoda_d3_ules <- .par_plot(brmcoda_d3_ules[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d3_ules

plot_becover_brmcoda_d3_ules <- .par_plot(brmcoda_d3_ules[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d3_ules

## d3 sub ules plot - u0 large e small --------------------------------------------------------------
colour <- col_sub_d3
plot_bias_sub_d3_ules <-
  .par_plot(sub_d3[stat == "bias" & condition == "RElarge_RESsmall"])  +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d3_ules

plot_becover_sub_d3_ules <-
  .par_plot(sub_d3[stat == "becover" & condition == "RElarge_RESsmall"]) +
  scale_y_continuous(limits = c(0.65, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d3_ules

## d4 brmcoda ules plot - u0 large e small ----------------------------------------------------------
colour <- col_brmcoda_d4
brmcoda_d4_ules <- rbind(
  b0_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr1_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr2_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr3_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr1_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr2_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr3_d4[condition == "RElarge_RESsmall"][, -c("condition")],
  u0_large_d4,
  sigma_small_d4
)

brmcoda_d4_ules[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d4_ules$by)
brmcoda_d4_ules[, by := factor(
  by,
  levels = c(
    "  b0", "  u0",
    "between ilr1 beta", "within ilr1 beta",
    "between ilr2 beta", "within ilr2 beta",
    "within ilr3 beta", "between ilr3 beta",
    "  sigma"
  )
)]
plot_bias_brmcoda_d4_ules <- .par_plot(brmcoda_d4_ules[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d4_ules

plot_becover_brmcoda_d4_ules <- .par_plot(brmcoda_d4_ules[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d4_ules

## d4 sub ules plot - u0 large e small --------------------------------------------------------------
colour <- col_sub_d4
plot_bias_sub_d4_ules <-
  .par_plot(sub_d4[stat == "bias" & condition == "RElarge_RESsmall"]) +
  scale_y_continuous(limits = c(-0.1, 0.1),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d4_ules

plot_becover_sub_d4_ules <-
  .par_plot(sub_d4[stat == "becover" & condition == "RElarge_RESsmall"]) +
  scale_y_continuous(limits = c(0.40, 1),
                     breaks = c(0.50, 0.75, 0.95))
plot_becover_sub_d4_ules

## d5 brmcoda ules plot - u0 large e small ----------------------------------------------------------
colour <- col_brmcoda_d5
brmcoda_d5_ules <- rbind(
  b0_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr1_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr2_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr3_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  bilr4_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr1_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr2_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr3_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  wilr4_d5[condition == "RElarge_RESsmall"][, -c("condition")],
  u0_large_d5,
  sigma_small_d5
)

brmcoda_d5_ules[, by := paste0(Level, " ", Predictor, " ", Parameter)]
unique(brmcoda_d5_ules$by)
brmcoda_d5_ules[, by := factor(
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

plot_bias_brmcoda_d5_ules <- .par_plot(brmcoda_d5_ules[stat == "bias"])  +
  scale_y_continuous(limits = c(-0.2, 0.2),
                     breaks = c(-0.2, 0, 0.2))
plot_bias_brmcoda_d5_ules

plot_becover_brmcoda_d5_ules <- .par_plot(brmcoda_d5_ules[stat == "becover"]) +
  scale_y_continuous(limits = c(0.9, 1),
                     breaks = c(0.9, 0.95, 1))
plot_becover_brmcoda_d5_ules

## d5 sub ules plot - u0 large e small --------------------------------------------------------------
colour <- col_sub_d5
plot_bias_sub_d5_ules <-
  .par_plot(sub_d5[stat == "bias" & condition == "RElarge_RESsmall"]) +
  scale_y_continuous(limits = c(-0.1, 0.1),
                     breaks = c(-0.05, 0, 0.05))
plot_bias_sub_d5_ules

plot_becover_sub_d5_ules <-
  .par_plot(sub_d5[stat == "becover" & condition == "RElarge_RESsmall"]) +
  scale_y_continuous(limits = c(0.65, 1),
                     breaks = c(0.75, 0.95))
plot_becover_sub_d5_ules
