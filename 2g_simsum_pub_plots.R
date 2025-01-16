library(JWileymisc)
library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)

library(doFuture)
library(foreach)
library(doRNG)
library(parallel)

library(rsimsum) # https://cran.r-project.org/web/packages/rsimsum/vignettes/A-introduction.html
library(heatmaply)
library(ggthemes)
library(bayesplot)

library(hrbrthemes)
library(wesanderson)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(patchwork)
library(latex2exp)

source("0a_functions.R")
extrafont::font_import()
# par(family = "LM Roman 10")
font <- "LM Roman 10"
font <- "Roboto"
font <- "Times New Roman"
font <- "Arial Narrow"

brmcoda_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/brmcoda_dat.RDS")
sub_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/sub_dat.RDS")

outputdir <- "/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/"

# D4 -------------------
brmcoda_dat_d4 <- brmcoda_dat[["brmcoda_d4"]]
brmcoda_dat_d4[, Estimand := droplevels(Estimand)]

sub_dat_d4 <- sub_dat[["sub_d4"]]
levels(sub_dat_d4$Substitution)

col_brmcoda_d4 <-
  c(
    "#354140",
    "#5E4F65",
    "#ABA2C3",
    # "#5A6367",
    "#708885",
    "#99ABA9",
    "#EFE3E0",
    # "#DAA5AE",
    "#B98AA3",
    # "#6171a9",
    "#9A5C7D",
    "#3d251e"
  )

col_sub_d4 <-
  c(
    "#2A3E59",
    "#456691",
    "#8CAACB",
    # "#A1B2C2",
    "#EAD3BF",
    "#FAD899",
    "#8DA290",
    "#5B6F5D",
    "#133A1B",
    "#944C4C",
    # "#A69188",
    # "#C99696",
    # "#DCD5CE",
    "#bb847a",
    "#A69188",
    "#3d251e"
  )

layout <- c(
  area(t = 0, l = 0, b = 20, r = 3),
  area(t = 0, l = 4, b = 20, r = 8)
)
plot(layout)

## PLOTS ------------
### common column ---------------
brmcoda_par_d4 <- 
  ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0, label = brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Estimand", y = 10, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0, 1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5) 
brmcoda_par_d4 

## use latex symbolss
brmcoda_par_d4 <-
  ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$EstimandF, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 10, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5)
brmcoda_par_d4

### test plot --------------
p01 <- .par_plot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = `Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 10, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    # plot.margin       = unit(c(1,1,1,0), "lines")
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### brmcoda bias plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d4[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    # geom_text(aes(x = -0.25, label = `Est [95% CI], MCSE`, family = font),
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 10, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0, 1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.45, yend = Inf, xend = 9.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
# p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)
# SAVED AS 13X12

### brmcoda cover plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d4[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d4[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d4[Stat == "cover" & condition == "base" & JI == ji]
  
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 10, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.45, yend = Inf, xend = 9.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
# p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### brmcoda becover plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d4[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d4[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d4[Stat == "becover" & condition == "base" & JI == ji]
  
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", 
                  y = 10, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.45, yend = Inf, xend = 9.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
# p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub common column ---------------
sub_par_d4 <- 
  ggplot(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  geom_text(aes(x = 0, label = sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Substitution", y = 13, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5) 
sub_par_d4 

## use latex symbolss
sub_par_d4 <-
  ggplot(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 13, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5)
sub_par_d4

### test plot ------------------
p01 <- .par_plot(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = `Estimates`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(sub_dat_d4[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Bias", 
                y = 13, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### sub bias plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d4[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d4[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d4[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 13, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 12.5, yend = Inf, xend = 12.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4
# save as 16x12

### sub cover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d4[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d4[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d4[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 13, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 12.5, yend = Inf, xend = 12.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

# ggarrange(p1, p2, p3, p4,
#           nrow = 4)

### sub becover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d4[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d4[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d4[Stat == "becover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", 
                  y = 13, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 12.5, yend = Inf, xend = 12.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d4 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d4 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d4 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d4 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

# D3 -------------------
brmcoda_dat_d3 <- brmcoda_dat[["brmcoda_d3"]]
brmcoda_dat_d3[, Estimand := droplevels(Estimand)]
levels(brmcoda_dat_d3$Estimand)

sub_dat_d3 <- sub_dat[["sub_d3"]]
levels(sub_dat_d3$Substitution)

col_brmcoda_d3 <-
  c(
    "#354140",
    # "#5E4F65",
    # "#ABA2C3",
    # "#5A6367",
    "#708885",
    "#99ABA9",
    "#EFE3E0",
    # "#DAA5AE",
    "#B98AA3",
    # "#6171a9",
    "#9A5C7D",
    "#3d251e"
  )

col_sub_d3 <-
  c(
    "#2A3E59",
    # "#456691",
    "#8CAACB",
    # "#A1B2C2",
    "#EAD3BF",
    "#FAD899",
    "#8DA290",
    # "#5B6F5D",
    # "#133A1B",
    # "#944C4C",
    # "#A69188",
    # "#C99696",
    # "#DCD5CE",
    # "#bb847a",
    "#A69188",
    "#3d251e"
  )

layout <- c(
  area(t = 0, l = 0, b = 18, r = 3),
  area(t = 0, l = 4, b = 18, r = 7)
)
plot(layout)

## PLOTS ------------
### common column ---------------
brmcoda_par_d3 <- 
  ggplot(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0, label = brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Estimand", y = 8, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 7.45, xend = Inf, yend = 7.45), color = "black", linewidth = 0.5) 
brmcoda_par_d3 

## use latex symbolss
brmcoda_par_d3 <-
  ggplot(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$EstimandF, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 8, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 7.45, xend = Inf, yend = 7.45), color = "black", linewidth = 0.5)
brmcoda_par_d3

### test plot ---------------------
p01 <- .par_plot(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], d = 3, font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = `Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 8, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### brmcoda bias plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d3[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d3[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d3[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 3, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 8, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0, 1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 7.45, yend = Inf, xend = 7.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 7.45, xend = Inf, yend = 7.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})
# check
brmcoda_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d3 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d3 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d3 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### brmcoda cover plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d3[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d3[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d3[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 3, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 8, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0, 1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 7.45, yend = Inf, xend = 7.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 7.45, xend = Inf, yend = 7.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d3 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d3 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d3 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

png("brmcoda_cover_d3.png", width = 14, height = 13, units = 'in', res = 900)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()

### sub common column ---------------
sub_par_d3 <- 
  ggplot(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  geom_text(aes(x = 0, label = sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Substitution", y = 7, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) 
sub_par_d3 

## use latex symbolss
sub_par_d3 <-
  ggplot(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Estimand", y = 7, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5)
sub_par_d3

### test plot --------------------------
p01 <- .par_plot(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], d = 3, font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$`Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(sub_dat_d3[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", 
                y = 7, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02
p01 + p02 + plot_layout(design = layout)

### sub bias plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d3[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d3[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d3[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 3, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias",
                  y = 7, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}

# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})
# check
sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d3 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d3 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d3 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub cover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d3[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d3[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d3[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 3, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage",
                  y = 7, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}

# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d3 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d3 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d3 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

png("sub_cover_d3.png", width = 14, height = 14, units = 'in', res = 900)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()

### sub bcover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d3[Stat == "bcover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d3[Stat == "bcover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d3[Stat == "bcover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 3, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bcover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage",
                  y = 7, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}

# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d3 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d3 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d3 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d3 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

png("sub_cover_d3.png", width = 14, height = 14, units = 'in', res = 900)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()

# D5 -------------------
brmcoda_dat_d5 <- brmcoda_dat[["brmcoda_d5"]]
brmcoda_dat_d5[, Estimand := droplevels(Estimand)]
levels(brmcoda_dat_d5$Estimand)

sub_dat_d5 <- sub_dat[["sub_d5"]]
levels(sub_dat_d5$Substitution)

col_brmcoda_d5 <-
  c(
    "#354140",
    "#5E4F65",
    "#8C7A9B",
    "#ABA2C3",
    # "#5A6367",
    "#708885",
    "#99ABA9",
    "#EFE3E0",
    # "#C7AFBB",
    "#D2BEC8",
    "#B98AA3",
    "#9A5C7D",
    "#3d251e"
  )

col_sub_d5 <-
  c(
    "#2A3E59",
    # "#456691",
    "#8CAACB",
    # "#A1B2C2",
    "#EAD3BF",
    "#FAD899",
    "#8DA290",
    # "#5B6F5D",
    # "#133A1B",
    # "#944C4C",
    # "#A69188",
    # "#C99696",
    # "#DCD5CE",
    # "#bb847a",
    "#A69188",
    "#3d251e"
  )

layout <- c(
  area(t = 0, l = 0, b = 8, r = 3),
  area(t = 0, l = 4, b = 8, r = 7)
)
plot(layout)

## PLOTS ------------
### common column ---------------
brmcoda_par_d5 <- 
  ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0, label = brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Parameter", y = 12, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) 
brmcoda_par_d5 

## use latex symbolss
brmcoda_par_d5 <-
  ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$EstimandF, output = "character"),
                family = font), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 12, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5)
brmcoda_par_d5

### test plot ---------------------
p01 <- .par_plot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], d = 5, font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$`Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 12, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### brmcoda bias plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 11.45, yend = Inf, xend = 11.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### brmcoda cover plots ---------------------
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 11.45, yend = Inf, xend = 11.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### brmcoda becover plots ---------------------
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "becover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 11.45, yend = Inf, xend = 11.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub common column ---------------
sub_par_d5 <- 
  ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  geom_text(aes(x = 0, label = sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Parameter", y = 7, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) 
sub_par_d5 

## use latex symbolss
sub_par_d5 <-
  ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, output = "character"),
                family = font), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 7, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5)
sub_par_d5

### test plot ---------------------
p01 <- .par_plot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], d = 3, font = font)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$`Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", 
                y = 7, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02
p01 + p02 + plot_layout(design = layout)

### sub bias plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", y = 7, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub cover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
    
    p02 <- ggplot(tmp, aes(y = Substitution)) +
      geom_text(aes(x = -0.25, label = `Estimates`, family = font),
                vjust = 0.5,
                # colour = "black",
                # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
                # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
      ) + 
      # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
      geom_text(aes(label = "Coverage", y = 7, x = -0.25, 
                    family = font), 
                color = "black",
                fontface = "bold",
                vjust = "inward", hjust = "inward") + 
      xlim(-0.3, -0.2) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(expand = c(0,1.05)) +
      hrbrthemes::theme_ipsum() + theme_void() +
      theme(
        axis.ticks        = element_blank(),
        panel.background  = element_blank(),
        panel.border      = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        axis.title.y      = element_blank(),
        axis.title.x      = element_blank(),
        axis.text.x       = element_blank(),
        axis.text.y       = element_blank()
      ) 
    out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub becover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "becover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, d = 5, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", y = 7, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 6.5, yend = Inf, xend = 6.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 6.5, xend = Inf, yend = 6.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)



# D5 manuscript -------------------
brmcoda_dat_d5 <- brmcoda_dat[["brmcoda_d5"]]
brmcoda_dat_d5[, Estimand := droplevels(Estimand)]

sub_dat_d5 <- sub_dat[["sub_d5"]]
levels(sub_dat_d5$Substitution)

col_brmcoda_d5 <-
  c(
    "#354140",
    "#5E4F65",
    "#ABA2C3",
    "#5A6367",
    "#708885",
    "#99ABA9",
    "#EFE3E0",
    "#DAA5AE",
    "#B98AA3",
    # "#6171a9",
    "#9A5C7D",
    "#3d251e"
  )

col_sub_d5 <-
  c(
    "#1C1718", "#2A3E59",
    "#456691", "#7083a4",
    "#8CAACB", "#DCD5CE",
    "#DAA5AE", "#b6485d",
    "#944C4C", "#C99696",
    "#bf5b4b", "#bb847a",
    "#ea967c", "#A69188", 
    "#EAD3BF", "#FAD899", 
    "#AFC7BB", "#83A192",
    "#133A1B", "#3d251e"
  )

layout <- c(
  area(t = 0, l = 0, b = 20, r = 3),
  area(t = 0, l = 4, b = 20, r = 8)
)
plot(layout)

## PLOTS ------------
### common column ---------------
brmcoda_par_d5 <- 
  ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0, label = brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Parameter", y = 12, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0, 1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) 
brmcoda_par_d5 

## use latex symbolss
brmcoda_par_d5 <-
  ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$EstimandF, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 12, x = 0, 
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5)
brmcoda_par_d5

### test plot --------------
p01 <- .par_plot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], font = font, d = 5)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = `Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 12, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    # plot.margin       = unit(c(1,1,1,0), "lines")
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### brmcoda bias plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font, d = 5)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    # geom_text(aes(x = -0.25, label = `Est [95% CI], MCSE`, family = font),
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0, 1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 11.45, yend = Inf, xend = 11.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
# p1 / p2 / p3 / p4

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_brmcoda_bias_d5", ".pdf"),
  width = 12,
  height = 14,
)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()

### brmcoda cover plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "cover" & condition == "base" & JI == ji]
  
  p01 <- .par_plot(tmp, font = font, d = 5)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 11.45, yend = Inf, xend = 11.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 11.45, xend = Inf, yend = 11.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
# p1 / p2 / p3 / p4

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_brmcoda_cover_d5", ".pdf"),
  width = 12,
  height = 14,
)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()

### brmcoda becover plots ---------------------
out = list()
for (i in seq_along(levels(brmcoda_dat_d5[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(brmcoda_dat_d5[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- brmcoda_dat_d5[Stat == "becover" & condition == "base" & JI == ji]
  
  p01 <- .par_plot(tmp, font = font, d = 5)
  
  p02 <- ggplot(tmp, aes(y = Estimand)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", 
                  y = 12, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.45, yend = Inf, xend = 9.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.45, xend = Inf, yend = 9.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- brmcoda_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- brmcoda_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- brmcoda_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- brmcoda_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
# p1 / p2 / p3 / p4

ggarrange(p1, p2, p3, p4,
          nrow = 4)

### sub common column ---------------
sub_par_d5 <- 
  ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  geom_text(aes(x = 0, label = sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution, 
                family = font),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Substitution", y = 21, x = 0, 
                family = font), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 20.45, xend = Inf, yend = 20.45), color = "black", linewidth = 0.5)
sub_par_d5 

## use latex symbolss
sub_par_d5 <-
  ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0,
                label = TeX(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand, output = "character"),
                family = "LM Roman 10"), parse = TRUE,
            vjust = 0.5,
            colour = "black",
            fontface = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.0, .2)  +
  geom_text(aes(label = "Parameter", y = 21, x = 0,
                family = font),
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(label = TeX, expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank()
    # axis.text.x       = element_blank()
    # axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 20.45, xend = Inf, yend = 20.45), color = "black", linewidth = 0.5)
sub_par_d5

### test plot ------------------
p01 <- .par_plot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], font = font, d = 5)
# annotate("text", x = 9.75, y =0, label = "JI")
p01
p02 <- ggplot(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3], aes(y = Substitution)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Substitution == "Substitution", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = `Estimates`, 
                # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
                family = font), 
            vjust = 0,
            colour = ifelse(sub_dat_d5[Stat == "bias" & condition == "base" & J == 30 & I == 3]$OnTarget == "Y", "black", "#7A7A7A")) +
  geom_text(aes(label = "Bias", 
                y = 21, x = -0.25, 
                family = font), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(expand = c(0,1.05)) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

p01 + p02 + plot_layout(design = layout)

### sub bias plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "bias" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "bias" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "bias" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font, d = 5)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "bias" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Bias", 
                  y = 21, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 20.45, yend = Inf, xend = 20.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 20.45, xend = Inf, yend = 20.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})
# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_sub_bias_d5", ".pdf"),
  width = 13,
  height = 22,
)
p1 / p2 / p3 / p4
dev.off()

# save as 16x12

### sub cover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "cover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "cover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "cover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font, d = 5)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "cover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "Coverage", 
                  y = 21, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 20.45, yend = Inf, xend = 20.45), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 20.45, xend = Inf, yend = 20.45), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})
# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,0,0,0), "lines"))

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_sub_cover_d5", ".pdf"),
  width = 13,
  height = 22,
)
p1 / p2 / p3 / p4
dev.off()

### sub becover plots ---------------------
out = list()
for (i in seq_along(levels(sub_dat_d5[Stat == "becover" & condition == "base"]$JI))) {
  
  layout <- layout
  ji <- levels(sub_dat_d5[Stat == "becover" & condition == "base"]$JI)[i]
  
  tmp <- sub_dat_d5[Stat == "becover" & condition == "base" & JI == ji]
  p01 <- .par_plot(tmp, font = font)
  
  p02 <- ggplot(tmp, aes(y = Substitution)) +
    geom_text(aes(x = -0.25, label = `Estimates`, family = font),
              vjust = 0.5,
              # colour = "black",
              # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
              # colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")
    ) + 
    # colour = d[Stat == "becover" & condition == "base" & J == 30 & I == 3]$Estimates,
    geom_text(aes(label = "BE Coverage", 
                  y = 13, x = -0.25, 
                  family = font), 
              color = "black",
              fontface = "bold",
              vjust = "inward", hjust = "inward") + 
    xlim(-0.3, -0.2) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(expand = c(0,1.05)) +
    hrbrthemes::theme_ipsum() + theme_void() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_blank(),
      axis.title.x      = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank()
    ) 
  out[[i]] <- list(list(p01, p02))
}
# patch 1st layer
p <- lapply(do.call(c, out), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 12.5, yend = Inf, xend = 12.5), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 12.5, xend = Inf, yend = 12.5), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check
sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- sub_par_d5 | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- sub_par_d5 | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- sub_par_d5 | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- sub_par_d5 | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4
