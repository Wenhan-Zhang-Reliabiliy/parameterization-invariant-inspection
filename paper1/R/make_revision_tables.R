source("paper1/R/model_version.R")
source("paper1/R/format_numbers.R")

summary <- read.csv("paper1/output/revision_performance_summary.csv")
reductions <- read.csv("paper1/output/revision_paired_reductions.csv")
scenario_order <- c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                    "Shape prior offset", "Location prior offset")
policy_order <- c("Fixed", "Static", "Myopic", "Batch-2", "Batch-full", "Outcome-2")

lines <- c("\\begin{tabular}{lrrrrrr}", "\\toprule",
  "Scenario & Fixed & Static & Reliability-1 & Reliability-2 & Reliability-full & Outcome-2 \\\\",
  "\\midrule")
for (scenario in scenario_order) {
  values <- vapply(policy_order, function(policy) {
    x <- summary[summary$scenario == scenario & summary$policy == policy, ]
    sprintf("%.3f (%.3f)", x$rmse, x$rmse_mcse)
  }, character(1))
  lines <- c(lines, paste0(scenario, " & ", paste(values, collapse = " & "), " \\\\"))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/revision_performance_table.tex")

lines <- c("\\begin{tabular}{lrrrr}", "\\toprule",
  "Scenario & \\multicolumn{2}{c}{Reliability-full vs Reliability-1} & \\multicolumn{2}{c}{Outcome-2 vs Reliability-full} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Reduction (\\%) & 95\\% CI & Reduction (\\%) & 95\\% CI \\\\", "\\midrule")
for (scenario in scenario_order) {
  first <- reductions[reductions$scenario == scenario & reductions$policy == "Batch-full" &
                        reductions$baseline == "Myopic", ]
  second <- reductions[reductions$scenario == scenario & reductions$policy == "Outcome-2" &
                         reductions$baseline == "Batch-full", ]
  lines <- c(lines, sprintf("%s & %s & [%s, %s] & %s & [%s, %s] \\\\",
                            scenario, format_fixed(first$reduction, 2),
                            format_fixed(first$lower, 2), format_fixed(first$upper, 2),
                            format_fixed(second$reduction, 2), format_fixed(second$lower, 2),
                            format_fixed(second$upper, 2)))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/revision_reduction_table.tex")
