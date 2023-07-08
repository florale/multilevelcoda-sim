## 
colour <- c("#A69188", "#DCD5CE", "#FAF7F3", "#A1B2C2", "#CFDAE2")
colour5 <- wes_palette("Cavalcanti1", 5)
colour4 <- c("#EFE3E0", "#BEACA2", "#708885", "#5A6367")
colour3 <- wes_palette("IsleofDogs1", 3)
colour <- c("#1C1718", "#5A6367",
            "#3d5a80", "#98c1d9",
            "#AA9486", "#EAD3BF",
            "#926890", alpha("#B394B1", 0.8), 
            "#596C6A", "#8DA290"
            )
### d3 ----------------
b0_d3 <- as.data.table(rbind(
  tidy(summary(s_b0_d3), stats = "bias"),
  tidy(summary(s_b0_d3), stats = "becover")
))
b0_d3[, Parameter := "b0"]
b0_d3[, Predictor := " "]
b0_d3[, Level := " "]

bilr1_d3 <- as.data.table(rbind(
  tidy(summary(s_bilr1_d3), stats = "bias"),
  tidy(summary(s_bilr1_d3), stats = "becover")
))
bilr1_d3[, Parameter := "beta"]
bilr1_d3[, Predictor := "ilr1"]
bilr1_d3[, Level := "between"]

bilr2_d3 <- as.data.table(rbind(
  tidy(summary(s_bilr2_d3), stats = "bias"),
  tidy(summary(s_bilr2_d3), stats = "becover")
))
bilr2_d3[, Parameter := "beta"]
bilr2_d3[, Predictor := "ilr2"]
bilr2_d3[, Level := "between"]

wilr1_d3 <- as.data.table(rbind(
  tidy(summary(s_wilr1_d3), stats = "bias"),
  tidy(summary(s_wilr1_d3), stats = "becover")
))
wilr1_d3[, Parameter := "beta"]
wilr1_d3[, Predictor := "ilr1"]
wilr1_d3[, Level := "within"]

wilr2_d3 <- as.data.table(rbind(
  tidy(summary(s_wilr2_d3), stats = "bias"),
  tidy(summary(s_wilr2_d3), stats = "becover")
))
wilr2_d3[, Parameter := "beta"]
wilr2_d3[, Predictor := "ilr2"]
wilr2_d3[, Level := "within"]

u0_base_d3 <- as.data.table(rbind(
  tidy(summary(s_u0_base_d3), stats = "bias"),
  tidy(summary(s_u0_base_d3), stats = "becover")
))
u0_base_d3[, Parameter := "u0"]
u0_base_d3[, Predictor := " "]
u0_base_d3[, Level := " "]
setnames(u0_base_d3, "sigma_condition", "condition")

u0_small_d3 <- as.data.table(rbind(
  tidy(summary(s_u0_small_d3), stats = "bias"),
  tidy(summary(s_u0_small_d3), stats = "becover")
))
u0_small_d3[, Parameter := "u0"]
u0_small_d3[, Predictor := " "]
u0_small_d3[, Level := "small"]

u0_large_d3 <- as.data.table(rbind(
  tidy(summary(s_u0_large_d3), stats = "bias"),
  tidy(summary(s_u0_large_d3), stats = "becover")
))
u0_large_d3[, Parameter := "u0"]
u0_large_d3[, Predictor := " "]
u0_large_d3[, Level := "large"]

sigma_base_d3 <- as.data.table(rbind(
  tidy(summary(s_sigma_base_d3), stats = "bias"),
  tidy(summary(s_sigma_base_d3), stats = "becover")
))
sigma_base_d3[, Parameter := "sigma"]
sigma_base_d3[, Predictor := " "]
sigma_base_d3[, Level := "medium"]

sigma_small_d3 <- as.data.table(rbind(
  tidy(summary(s_sigma_small_d3), stats = "bias"),
  tidy(summary(s_sigma_small_d3), stats = "becover")
))
sigma_small_d3[, Parameter := "sigma"]
sigma_small_d3[, Predictor := " "]
sigma_small_d3[, Level := "small"]

sigma_large_d3 <- as.data.table(rbind(
  tidy(summary(s_sigma_large_d3), stats = "bias"),
  tidy(summary(s_sigma_large_d3), stats = "becover")
))
sigma_large_d3[, Parameter := "sigma"]
sigma_large_d3[, Predictor := " "]
sigma_large_d3[, Level := "large"]

brmcoda_d3 <- rbind(b0_d3, u0_base_d3,
                    bilr1_d3, bilr2_d3,
                    wilr1_d3, wilr2_d3)

# plot
ggplot(brmcoda_d3[stat == "bias" & Parameter %in% c("b0")], 
       aes(x = condition, y = est, 
           ymin = lower, ymax = upper,
           colour = condition)) +
  geom_point() +
  geom_errorbar(width = 1 / 3) +
  geom_hline(yintercept = 0, color = "#A1B2C2", linetype = "dotted", linewidth = 1 / 3) +
  scale_colour_jco() +
  coord_flip() +
  facet_wrap(ggplot2::vars(N, K)) +
  theme_ipsum() +
  theme(
    axis.ticks        = element_blank(),
    legend.position   = "bottom",
    panel.background  = element_rect(
      fill = "transparent",
      colour = "black",
      linewidth = 0.5
    )
  )
ggplot(brmcoda_d3[stat == "bias" & Parameter %in% c("u0")], 
       aes(x = condition, y = est, 
           ymin = lower, ymax = upper,
           colour = condition)) +
  geom_point() +
  geom_errorbar(width = 1 / 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", linewidth = 1 / 2) +
  scale_colour_manual(values = colour3) + 
  coord_flip() +
  facet_wrap(ggplot2::vars(N, K)) +
  theme_ipsum() +
  theme(
    axis.ticks        = element_blank(),
    legend.position   = "bottom",
    panel.background  = element_rect(
      fill = "transparent",
      colour = "black",
      linewidth = 0.5
    )
  )
# ggplot(brmcoda_d3[stat == "bias" & Parameter %in% c("sigma")], 
#        aes(x = condition, y = est, 
#            ymin = lower, ymax = upper,
#            colour = condition)) +
#   geom_point() +
#   geom_errorbar(width = 1 / 3) +
#   geom_hline(yintercept = 0, color = "red", linetype = "dotted", linewidth = 1 / 2) +
#   scale_colour_manual(values = colour3) + 
#   coord_flip() +
#   facet_wrap(ggplot2::vars(N, K)) +
#   theme_ipsum() +
#   theme(
#     axis.ticks        = element_blank(),
#     legend.position   = "bottom",
#     panel.background  = element_rect(
#       fill = "transparent",
#       colour = "black",
#       linewidth = 0.5
#     )
#   )

brmcoda_d3[, by := paste0(condition, "-", Level)]

ggplot(brmcoda_d3[stat == "bias" & Parameter %in% c("beta") & Predictor == "ilr1"], 
       aes(x = by, y = est, 
           ymin = lower, ymax = upper,
           colour = by)) +
  geom_point() +
  geom_errorbar(width = 1 / 2) +
  geom_hline(yintercept = 0, color = "#A1B2C2", linetype = "dotted", linewidth = 1 / 2) +
  scale_colour_manual(values = colour) + 
  coord_flip() +
  facet_wrap(ggplot2::vars(N, K)) +
  theme_ipsum() +
  theme(
    axis.ticks        = element_blank(),
    legend.position   = "bottom",
    panel.background  = element_rect(
      fill = "transparent",
      colour = "black",
      linewidth = 0.5
    )
  )

brmcoda_d3[, by := paste0(Parameter, " ", Level, " ", Predictor)]
brmcoda_d3[, by := factor(by, levels = c(
  "b0    ", "u0    ",
  "beta between ilr1", "beta between ilr2",
  "beta within ilr1", "beta within ilr2" 
))]

ggplot(brmcoda_d3[stat == "bias" & Parameter %in% c("b0", "beta", "u0") & condition == "base"], 
       aes(x = by, y = est, 
           ymin = lower, ymax = upper,
           colour = by)) +
  geom_point() +
  geom_errorbar(width = 1 / 2) +
  geom_hline(yintercept = 0, color = "#A1B2C2", linetype = "dotted", linewidth = 1 / 2) +
  scale_colour_manual(values = colour) + 
  coord_flip() +
  facet_wrap(ggplot2::vars(N, K)) +
  theme_ipsum() +
  theme(
    axis.ticks        = element_blank(),
    legend.position   = "bottom",
    panel.background  = element_rect(
      fill = "transparent",
      colour = "black",
      linewidth = 0.5
    )
  )

ggplot(brmcoda_d3[stat == "becover" & Parameter %in% c("b0", "beta", "u0") & condition == "base"], 
       aes(x = by, y = est, 
           ymin = lower, ymax = upper,
           colour = by)) +
  geom_point() +
  geom_errorbar(width = 1 / 2) +
  geom_hline(yintercept = 0.95, color = "#A1B2C2", linetype = "dotted", linewidth = 1 / 2) +
  scale_colour_manual(values = colour) + 
  coord_flip() +
  facet_wrap(ggplot2::vars(N, K)) +
  theme_ipsum() +
  theme(
    axis.ticks        = element_blank(),
    legend.position   = "bottom",
    panel.background  = element_rect(
      fill = "transparent",
      colour = "black",
      linewidth = 0.5
    )
  )
