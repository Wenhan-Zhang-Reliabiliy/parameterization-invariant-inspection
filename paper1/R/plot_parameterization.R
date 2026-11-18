results <- read.csv("paper1/output/parameterization_results.csv")
example <- results[results$scenario == "Two-stress baseline" & results$replicate == 1, ]
colors <- c(`1` = "#666666", `2` = "#E69F00", `10` = "#0072B2")

plot_schedules <- function() {
  par(mfrow = c(1, 2), mar = c(4, 5, 2.5, 1), oma = c(0, 0, 0, 0))
  for (policy in c("Trace-full", "Reliability-full")) {
    plot(NA, xlim = c(0, 1000), ylim = c(0.6, 3.4), xlab = "Inspection time",
         ylab = "Stress coding scale", yaxt = "n", main = policy, bty = "n")
    axis(2, at = 1:3, labels = c(1, 2, 10), las = 1)
    for (scale in c(1, 2, 10)) {
      x <- example[example$policy == policy & example$stress_scale == scale, ]
      stopifnot(nrow(x) == 1)
      times <- as.numeric(strsplit(x$schedule, ";", fixed = TRUE)[[1]])
      y <- match(scale, c(1, 2, 10))
      lines(range(times), rep(y, 2), col = colors[as.character(scale)], lwd = 1.5)
      points(times, rep(y, length(times)), pch = 19, cex = 1.1,
             col = colors[as.character(scale)])
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c("Scale 1", "Scale 2", "Scale 10"), col = colors,
         pch = 19, lty = 1, horiz = TRUE, bty = "n")
}

cairo_pdf("paper1/output/parameterization_schedules.pdf", width = 10, height = 4.8)
plot_schedules()
dev.off()
png("paper1/output/parameterization_schedules.png", width = 10, height = 4.8,
    units = "in", res = 180)
plot_schedules()
dev.off()
