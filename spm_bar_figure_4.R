# ============================================================
# Bar graphs for Figure 4A and 4B
# ============================================================

library(ggplot2)

# ------------------------------------------------------------
# Shared plotting settings
# ------------------------------------------------------------

fill_colours <- c(
  "Semantic"   = "#D97A7A",
  "Perceptual" = "#7F9AC8",
  "Rest"       = "#20B2AA"
)

outline_colours <- c(
  "Semantic"   = "#B96A6A",
  "Perceptual" = "#4F6B92",
  "Rest"       = "#008B8B"
)

plot_theme <- theme_minimal(base_size = 26) +
  theme(
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "plain", size = 30, hjust = 0.5),
    axis.title = element_text(face = "plain", size = 22),
    axis.text = element_text(color = "black", size = 20),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20)
  )

create_contrast_plot <- function(data_roi,
                                 coordinate_roi,
                                 y_breaks_roi,
                                 y_limits_roi,
                                 title_roi = "Contrast Estimates and 90% C.I.") {
  
  ggplot(
    data_roi,
    aes(
      x = Category,
      y = Mean,
      fill = Type,
      colour = Type,
      group = Type
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.8
    ) +
    geom_bar(
      stat = "identity",
      position = position_dodge(width = 0.6),
      width = 0.6,
      linewidth = 1,
      alpha = 0.95
    ) +
    geom_errorbar(
      aes(ymin = CI_Lower, ymax = CI_Upper),
      position = position_dodge(width = 0.6),
      width = 0.25,
      linewidth = 1
    ) +
    scale_fill_manual(values = fill_colours) +
    scale_colour_manual(values = outline_colours) +
    scale_y_continuous(breaks = y_breaks_roi) +
    coord_cartesian(ylim = y_limits_roi) +
    labs(
      title = title_roi,
      x = "Intensity",
      y = paste("Contrast Estimate at", coordinate_roi),
      fill = "Condition",
      colour = "Condition"
    ) +
    plot_theme
}

# ------------------------------------------------------------
# lSMG: -54 -30 42
# ------------------------------------------------------------

data_lSMG <- data.frame(
  Category = factor(
    rep(c("High", "Low"), each = 2),
    levels = c("High", "Low")
  ),
  Type = factor(
    rep(c("Semantic", "Perceptual"), times = 2),
    levels = c("Semantic", "Perceptual")
  ),
  Mean = c(2.80, 0.84, -0.49, 2.97),
  CI_Lower = c(1.75, -0.23, -1.54, 1.88),
  CI_Upper = c(3.84, 1.91, 0.57, 4.05)
)

p_lSMG <- create_contrast_plot(
  data_roi = data_lSMG,
  coordinate_roi = "-54 -30 42",
  y_breaks_roi = seq(-2, 4, by = 1),
  y_limits_roi = c(-2, 4)
)

ggsave(
  filename = "contrast_estimates_plot_lSMG.png",
  plot = p_lSMG,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# rPrG: 34 -20 38
# ------------------------------------------------------------

data_rPrG <- data.frame(
  Category = factor(
    rep(c("High", "Low"), each = 2),
    levels = c("High", "Low")
  ),
  Type = factor(
    rep(c("Semantic", "Perceptual"), times = 2),
    levels = c("Semantic", "Perceptual")
  ),
  Mean = c(0.93, 0.04, -0.48, 0.36),
  CI_Lower = c(0.58, -0.31, -0.83, 0.01),
  CI_Upper = c(1.27, 0.40, -0.13, 0.72)
)

p_rPrG <- create_contrast_plot(
  data_roi = data_rPrG,
  coordinate_roi = "34 -20 38",
  y_breaks_roi = seq(-2, 4, by = 1),
  y_limits_roi = c(-2, 4)
)

ggsave(
  filename = "contrast_estimates_plot_rPrG.png",
  plot = p_rPrG,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# rSPL: 32 -24 36
# ------------------------------------------------------------

data_rSPL <- data.frame(
  Category = factor(
    rep(c("High", "Low"), each = 2),
    levels = c("High", "Low")
  ),
  Type = factor(
    rep(c("Semantic", "Rest"), times = 2),
    levels = c("Semantic", "Rest")
  ),
  Mean = c(0.33, -1.04, -0.63, -0.73),
  CI_Lower = c(0.07, -1.34, -0.89, -1.01),
  CI_Upper = c(0.59, -0.73, -0.37, -0.45)
)

p_rSPL <- create_contrast_plot(
  data_roi = data_rSPL,
  coordinate_roi = "32 -24 36",
  y_breaks_roi = seq(-8, 1, by = 1),
  y_limits_roi = c(-8, 1)
)

ggsave(
  filename = "contrast_estimates_plot_rSPL.png",
  plot = p_rSPL,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# rSFG: 26 30 46
# ------------------------------------------------------------

data_rSFG <- data.frame(
  Category = factor(
    rep(c("High", "Low"), each = 2),
    levels = c("High", "Low")
  ),
  Type = factor(
    rep(c("Semantic", "Rest"), times = 2),
    levels = c("Semantic", "Rest")
  ),
  Mean = c(-2.27, -5.92, -5.75, -3.07),
  CI_Lower = c(-3.55, -7.41, -7.03, -4.42),
  CI_Upper = c(-0.99, -4.43, -4.48, -1.71)
)

p_rSFG <- create_contrast_plot(
  data_roi = data_rSFG,
  coordinate_roi = "26 30 46",
  y_breaks_roi = seq(-8, 1, by = 1),
  y_limits_roi = c(-8, 1)
)

ggsave(
  filename = "contrast_estimates_plot_rSFG.png",
  plot = p_rSFG,
  width = 8,
  height = 6,
  dpi = 600
)