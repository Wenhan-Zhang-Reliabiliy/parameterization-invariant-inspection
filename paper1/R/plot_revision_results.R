summary <- read.csv("paper1/output/criterion_summary.csv")
scenario_order <- c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                    "Shape prior offset", "Location prior offset",
                    "Two-stress baseline", "Acceleration prior offset")
policy_levels <- c("Trace-1", "Myopic", "Trace-full", "Batch-full")
policy_labels <- c("Trace-1", "Reliability-1", "Trace-full", "Reliability-full")
colors <- c(`Trace-1` = "#666666", `Reliability-1` = "#D55E00",
            `Trace-full` = "#009E73", `Reliability-full` = "#0072B2")

plot_rmse <- function() {
  par(mfrow = c(4, 2), mar = c(4, 4, 2.4, 0.8), oma = c(3, 2, 0, 0))
  for (scenario in scenario_order) {
    subset <- summary[summary$scenario == scenario, ]
    subset <- subset[match(policy_levels, subset$policy), ]
    lower <- subset$rmse - 1.96 * subset$rmse_mcse
    upper <- subset$rmse + 1.96 * subset$rmse_mcse
    ylim <- range(c(lower, upper)) * c(0.96, 1.04)
    plot(NA, xlim = c(0.7, 4.3), ylim = ylim, xaxt = "n", xlab = "", ylab = "",
         main = scenario, cex.main = 0.95)
    grid(nx = NA, ny = NULL, col = "#E5E5E5", lty = 1)
    for (i in seq_along(policy_levels)) {
      x <- subset[i, ]
      arrows(i, x$rmse - 1.96 * x$rmse_mcse, i, x$rmse + 1.96 * x$rmse_mcse,
             angle = 90, code = 3, length = 0.025, col = colors[policy_labels[i]])
      points(i, x$rmse, pch = 19, col = colors[policy_labels[i]], cex = 1.35)
    }
    axis(1, at = 1:4, labels = FALSE)
    text(1:4, par("usr")[3] - diff(par("usr")[3:4]) * 0.08,
         policy_labels, srt = 25, adj = 1, xpd = TRUE, cex = 0.72)
    title(ylab = "RMSE", line = 2.1)
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", policy_labels, col = colors, pch = 19, horiz = TRUE, bty = "n", cex = 0.85)
}

cairo_pdf("paper1/output/revision_rmse.pdf", width = 7.2, height = 9.2,
    onefile = TRUE)
plot_rmse()
dev.off()
png("paper1/output/revision_rmse.png", width = 7.2, height = 9.2,
    units = "in", res = 180)
plot_rmse()
dev.off()
