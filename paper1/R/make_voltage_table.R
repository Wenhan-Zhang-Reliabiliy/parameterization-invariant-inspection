source("paper1/R/model_version.R")

results <- read.csv("work/exploratory/voltage_case/voltage_empirical_results.csv")
summary <- read.csv("work/exploratory/voltage_case/voltage_empirical_summary.csv")
policy_order <- c("Trace-full", "Weighted-full", "Full")
policy_labels <- c("Trace-full", "Weighted-full", "Reliability-full")
lines <- c("\\begin{tabular}{llrrrr}", "\\toprule",
  "Policy & Inspection times (h) & $h$ (normalized) & $h$ (log ratio) & Max. schedule $\\Delta t$ & First detection (h) \\\\",
  "\\midrule")
for (j in seq_along(policy_order)) {
  policy <- policy_order[j]
  first <- results[results$policy == policy & results$coding == "normalized", ]
  second <- results[results$policy == policy & results$coding == "log_ratio", ]
  difference <- summary$max_schedule_difference[summary$policy == policy]
  schedule <- gsub(";", ", ", first$schedule)
  detection <- as.numeric(strsplit(first$schedule, ";", fixed = TRUE)[[1]])[2]
  lines <- c(lines, sprintf("%s & %s & %.3f & %.3f & %.3f & %.1f \\\\",
    policy_labels[j], schedule, first$estimate, second$estimate, difference, detection))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/voltage_case_table.tex")
