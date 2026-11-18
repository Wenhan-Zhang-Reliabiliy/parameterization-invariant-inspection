source("paper1/R/model_version.R")

summary <- read.csv("paper1/output/criterion_summary.csv")
scenario_order <- c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                    "Shape prior offset", "Location prior offset",
                    "Two-stress baseline", "Acceleration prior offset")
policy_order <- c("Trace-1", "Myopic", "Trace-full", "Batch-full")
lines <- c("\\begin{tabular}{lrrrr}", "\\toprule",
  "Scenario & Trace-1 & Reliability-1 & Trace-full & Reliability-full \\\\", "\\midrule")
for (scenario in scenario_order) {
  values <- vapply(policy_order, function(policy) {
    x <- summary[summary$scenario == scenario & summary$policy == policy, ]
    stopifnot(nrow(x) == 1)
    sprintf("%.3f", x$seconds)
  }, character(1))
  lines <- c(lines, paste0(scenario, " & ", paste(values, collapse = " & "), " \\\\"))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/computation_table.tex")
