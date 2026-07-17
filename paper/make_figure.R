# Figure for the BORG JOSS paper, in the house style of couplr/paper/make-figure.R.
#   (a) lead: simulation with known ground truth -- the error each CV scheme
#       hides relative to the true generalization error (shaded gap).
#   (b) support: Meuse real data -- the CV scheme alone moves RMSE by ~22%.
# Shared semantics across both panels:
#   vermillion = random / no blocking (optimistic), blue = spatial blocking,
#   green = ground-truth reference (panel a only).
# Reproduce: Rscript paper/make_figure.R  (after the two bench_*.R scripts)

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

hh  <- read.csv("paper/bench_results.csv", stringsAsFactors = FALSE)
sim <- read.csv("paper/bench_sim_results.csv", stringsAsFactors = FALSE)

col_rand  <- "#d55e00"  # random / no blocking
col_spat  <- "#0072b2"  # spatial blocking
col_truth <- "#009e73"  # ground truth
grey_seg  <- "#b0b0b0"
ann_size  <- 3.5        # one size for every in-panel text annotation

theme_fig <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#ececec", linewidth = 0.35),
      axis.line.x      = element_line(colour = "#1a1a1a", linewidth = 0.5),
      axis.ticks.x     = element_line(colour = "#444", linewidth = 0.4),
      axis.ticks.y     = element_blank(),
      axis.text        = element_text(colour = "#333", size = 9.5),
      axis.text.y      = element_text(colour = "#1a1a1a", size = 10),
      axis.title.x     = element_text(colour = "#1a1a1a", size = 11),
      axis.title.y     = element_blank(),
      plot.title       = element_text(face = "bold", size = 11.5, colour = "#1a1a1a",
                                      margin = margin(b = 6)),
      plot.tag         = element_text(face = "bold", size = 14, colour = "#1a1a1a"),
      plot.tag.position = c(0.005, 0.98),
      legend.position  = "none"
    )
}

# ---------- Panel (a): bias against a known ceiling ------------------------
# Different chart from (b) on purpose: here the true error is known, so the
# panel shows each scheme dropping BELOW a truth ceiling. Vertical shortfall
# columns with one big number each -- the message is optimism/bias, not spread.
t_med <- sim$rmse_median[sim$estimate == "independent-test (truth)"]
t_q25 <- sim$rmse_q25[sim$estimate == "independent-test (truth)"]
t_q75 <- sim$rmse_q75[sim$estimate == "independent-test (truth)"]

sa <- sim[sim$estimate != "independent-test (truth)", ]
sa <- sa[match(c("random 5-fold CV", "BORG spatial-block CV"), sa$estimate), ]
sa$x    <- c(1, 2)
sa$name <- c("random\n5-fold CV", "BORG\nspatial-block CV")
sa$col  <- c(col_rand, col_spat)
sa$lab  <- paste0(round(abs(sa$error_vs_truth_pct)), "% below truth")

# per-seed raw estimates (20 seeds) for the two CV schemes
ps <- read.csv("paper/bench_sim_perseed.csv", stringsAsFactors = FALSE)
ps <- ps[ps$estimate %in% c("random 5-fold CV", "BORG spatial-block CV"), ]
ps$x   <- ifelse(ps$estimate == "random 5-fold CV", 1, 2)
ps$col <- ifelse(ps$estimate == "random 5-fold CV", col_rand, col_spat)

p_a <- ggplot(sa) +
  # truth ceiling: horizontal line + uncertainty band across both columns
  annotate("rect", xmin = 0.4, xmax = 2.6, ymin = t_q25, ymax = t_q75,
           fill = col_truth, alpha = 0.08) +
  geom_hline(yintercept = t_med, colour = col_truth, linewidth = 0.9) +
  annotate("text", x = 0.46, y = t_med, label = "true error 0.68",
           hjust = 0, vjust = -0.7, size = ann_size, fontface = "italic",
           colour = col_truth) +
  # shortfall column: from the median CV estimate up to the truth ceiling
  geom_rect(aes(xmin = x - 0.30, xmax = x + 0.30,
                ymin = rmse_median, ymax = t_med, fill = col), alpha = 0.12) +
  # the 20 per-seed estimates as a jittered cloud
  geom_point(data = ps, aes(x = x, y = rmse, colour = col),
             position = position_jitter(width = 0.16, height = 0, seed = 42),
             size = 1.6, alpha = 0.38) +
  # median as a solid crossbar over the cloud
  geom_segment(aes(x = x - 0.26, xend = x + 0.26,
                   y = rmse_median, yend = rmse_median, colour = col),
               linewidth = 1.6, lineend = "round") +
  # shortfall label sits above the ceiling, clear of the cloud
  geom_text(aes(x = x, y = 0.86, label = lab, colour = col), size = ann_size) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = sa$x, labels = sa$name,
                     limits = c(0.4, 2.6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.35, 0.90),
                     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8)) +
  labs(title = "Simulation with known ground truth",
       y = "CV-estimated RMSE", tag = "(a)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#ececec", linewidth = 0.35),
    axis.line.y        = element_line(colour = "#1a1a1a", linewidth = 0.5),
    axis.ticks.y       = element_line(colour = "#444", linewidth = 0.4),
    axis.ticks.x       = element_blank(),
    axis.text.y        = element_text(colour = "#333", size = 9.5),
    axis.text.x        = element_text(colour = "#1a1a1a", size = 10.5,
                                      face = "bold", lineheight = 0.9),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(colour = "#1a1a1a", size = 11),
    plot.title         = element_text(face = "bold", size = 11.5,
                                      colour = "#1a1a1a", margin = margin(b = 6)),
    plot.tag           = element_text(face = "bold", size = 14, colour = "#1a1a1a"),
    plot.tag.position  = c(0.005, 0.98),
    legend.position    = "none",
    plot.margin        = margin(6, 12, 4, 6)
  )

# ---------- Panel (b): Meuse -- scheme choice swings the estimate -----------
# Raw RMSE across the six schemes; the takeaway is the ~22% swing from the CV
# scheme alone. Real data has no ground truth, so no scheme is marked correct:
# soft orange/blue zones show direction only (optimistic vs conservative),
# reusing (a)'s semantics where orange = optimistic and blue = conservative.
pretty <- c(
  "random k-fold"       = "random k-fold",
  "CAST knndm"          = "CAST (kNNDM)",
  "spatialsample block" = "spatialsample (block)",
  "BORG spatial_block"  = "BORG (spatial block)",
  "blockCV cv_spatial"  = "blockCV (spatial)",
  "sperrorest kmeans"   = "sperrorest (k-means)"
)
hh$label <- pretty[hh$scheme]

# lowest RMSE (most optimistic) at the top
hh <- hh[order(hh$rmse_median, decreasing = TRUE), ]
hh$label <- factor(hh$label, levels = hh$label)

swing_pct <- round(100 * (max(hh$rmse_median) - min(hh$rmse_median)) /
                     min(hh$rmse_median))
x_pad <- diff(range(hh$rmse_q25, hh$rmse_q75)) * 0.10
x_lo  <- min(hh$rmse_q25) - x_pad
x_hi  <- max(hh$rmse_q75) + x_pad
x_mid <- (x_lo + x_hi) / 2

p_b <- ggplot(hh, aes(rmse_median, label)) +
  # direction-only zones: orange = optimistic (lower), blue = conservative
  annotate("rect", xmin = x_lo, xmax = x_mid, ymin = 0.4, ymax = nrow(hh) + 0.5,
           fill = col_rand, alpha = 0.07) +
  annotate("rect", xmin = x_mid, xmax = x_hi, ymin = 0.4, ymax = nrow(hh) + 0.5,
           fill = col_spat, alpha = 0.07) +
  geom_segment(aes(x = rmse_q25, xend = rmse_q75, yend = label),
               colour = grey_seg, linewidth = 1.0, lineend = "round") +
  geom_point(size = 3.3, colour = "#333333") +
  scale_x_continuous(limits = c(x_lo, x_hi), expand = c(0, 0)) +
  labs(title = sprintf("Meuse (real data): the CV scheme alone swings RMSE ~%d%%",
                       swing_pct),
       x = "CV RMSE (log zinc)", tag = "(b)") +
  annotate("text", x = x_lo + x_pad * 0.3, y = 0.62, label = "optimistic (lower)",
           hjust = 0, vjust = 0.5, size = ann_size, fontface = "italic",
           colour = col_rand) +
  annotate("text", x = x_hi - x_pad * 0.3, y = 0.62, label = "more conservative (higher)",
           hjust = 1, vjust = 0.5, size = ann_size, fontface = "italic",
           colour = col_spat) +
  theme_fig() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(12, 14, 4, 6))

p <- p_a / p_b + plot_layout(heights = c(0.72, 1.15))
ggsave("paper/figures/benchmark.png", p, width = 7.2, height = 5.2,
       dpi = 300, bg = "white", device = ragg::agg_png)
cat("wrote paper/figures/benchmark.png\n")
