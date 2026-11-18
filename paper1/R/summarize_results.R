results <- read.csv("paper1/output/revision_results.csv")
summary <- read.csv("paper1/output/revision_performance_summary.csv")

colors <- c(Fixed = "#666666", Static = "#7A7A7A", Myopic = "#D55E00",
            `Batch-2` = "#E69F00", `Batch-full` = "#0072B2",
            `Outcome-2` = "#009E73", RITO = "#CC79A7")

save_plot <- function(filename, plot_function, width = 10, height = 7) {
  pdf(paste0("paper1/output/", filename, ".pdf"), width = width, height = height,
      onefile = TRUE, useDingbats = FALSE)
  plot_function()
  dev.off()
  png(paste0("paper1/output/", filename, ".png"), width = width, height = height,
      units = "in", res = 180)
  plot_function()
  dev.off()
}

example <- results[results$scenario == "Baseline" & results$replicate == 1, ]
plot_schedules <- function() {
  ordered <- rev(c("Fixed", "Static", "Myopic", "Batch-2", "Batch-full", "Outcome-2"))
  par(mar = c(4, 9, 1, 1))
  plot(NA, xlim = c(0, 1000), ylim = c(0.5, 6.5), xlab = "Inspection time",
       ylab = "", yaxt = "n", bty = "n")
  for (i in seq_along(ordered)) {
    policy <- ordered[i]
    times <- as.numeric(strsplit(example$schedule[example$policy == policy], ";")[[1]])
    lines(c(0, 1000), c(i, i), col = "#E5E5E5")
    points(times, rep(i, length(times)), pch = 19, cex = 1.1, col = colors[policy])
  }
  axis(2, at = 1:6, labels = ordered, las = 1, tick = FALSE)
}
save_plot("figure_schedules", plot_schedules, width = 9, height = 4.5)

plot_tradeoff <- function() {
  selected <- c("Sparse failures", "Location prior offset")
  adaptive <- c("Myopic", "Batch-full", "Outcome-2")
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  for (scenario in selected) {
    subset <- summary[summary$scenario == scenario & summary$policy %in% adaptive, ]
    plot(subset$seconds, subset$rmse, log = "x", pch = 19, cex = 1.4,
         col = colors[subset$policy], main = scenario, xlab = "Planning time per replicate (s)",
         ylab = "RMSE", xlim = range(subset$seconds) * c(0.75, 1.35),
         ylim = range(subset$rmse) + c(-0.004, 0.004))
    legend("topright", adaptive, col = colors[adaptive], pch = 19,
           bty = "n", cex = 0.85)
  }
}
save_plot("figure_computation", plot_tradeoff, width = 10, height = 4.5)

writeLines(capture.output(sessionInfo()), "paper1/output/final_session_info.txt")
