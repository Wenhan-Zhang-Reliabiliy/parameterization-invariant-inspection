source("paper1/R/model_version.R")

case <- read.csv("paper1/output/motorette_case_results.csv")
case$policy[case$policy == "Full"] <- "Reliability-full"
case <- case[order(match(case$policy, c("Trace-full", "Reliability-full")),
                   case$stress_scale, decreasing = c(FALSE, TRUE)), ]

lines <- c("\\begin{tabular}{lrllrr}", "\\toprule",
  "Policy & Scale $c$ & Inspection times (h) & Interval counts & Estimate & Reliability \\\\",
  "\\midrule")
for (i in seq_len(nrow(case))) {
  lines <- c(lines, sprintf("%s & %d & %s & %s & %.3f & %.5f \\\\",
    case$policy[i], case$stress_scale[i], gsub(";", ", ", case$schedule[i]),
    gsub(";", ", ", case$counts[i]), case$estimate[i], case$reliability[i]))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/motorette_case_table.tex")

colors <- c(`1000` = "#0072B2", `1` = "#D55E00")
plot_case <- function() {
  par(mfrow = c(1, 2), mar = c(4, 6, 2.5, 1))
  for (policy in c("Trace-full", "Reliability-full")) {
    plot(NA, xlim = c(0, 650), ylim = c(0.6, 2.4), xlab = "Inspection time (h)",
         ylab = "Scale c in c / Kelvin", yaxt = "n", main = policy, bty = "n")
    axis(2, at = 1:2, labels = c(1000, 1), las = 1)
    for (scale in c(1000, 1)) {
      x <- case[case$policy == policy & case$stress_scale == scale, ]
      times <- as.numeric(strsplit(x$schedule, ";", fixed = TRUE)[[1]])
      y <- if (scale == 1000) 1 else 2
      lines(range(times), rep(y, 2), col = colors[as.character(scale)], lwd = 1.5)
      points(times, rep(y, length(times)), pch = 19, cex = 1.15,
             col = colors[as.character(scale)])
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c("c = 1000", "c = 1"), col = colors, pch = 19,
         lty = 1, horiz = TRUE, bty = "n")
}

cairo_pdf("paper1/output/motorette_case.pdf", width = 10, height = 4.5)
plot_case()
dev.off()
png("paper1/output/motorette_case.png", width = 10, height = 4.5,
    units = "in", res = 180)
plot_case()
dev.off()
