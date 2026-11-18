source("paper1/R/model_version.R")
source("paper1/R/format_numbers.R")

definitions <- data.frame(
  label = c("main", "main", "grid31", "grid41", "grid41", "draws32", "draws64"),
  setting = c("Grid 21, draws 16", "Grid 21, draws 16", "Grid 31, draws 16",
              "Grid 41, draws 16", "Grid 41, draws 16", "Grid 21, draws 32",
              "Grid 21, draws 64"),
  additional_starts = c(0, 4, 0, 0, 4, 0, 0), stringsAsFactors = FALSE)
scenario_names <- c("Two-stress baseline", "Acceleration prior offset")
output <- list()
for (index in 1:2) {
  for (i in seq_len(nrow(definitions))) {
    definition <- definitions[i, ]
    path <- paste0("paper1/output/common_regret_", definition$label, "_scenario_",
                   index, "_starts_", definition$additional_starts, ".csv")
    if (!file.exists(path)) next
    x <- read.csv(path)
    assert_model_version(x, path)
    x$regret <- pmax(abs(x$base_regret), abs(x$scaled_regret))
    subsets <- list(x)
    if (definition$label == "main" && definition$additional_starts == 0 && nrow(x) == 1000) {
      subsets <- list(x, x[x$replicate <= 200, ])
    }
    for (subset in subsets) {
      output[[length(output) + 1]] <- data.frame(
        scenario = scenario_names[index], setting = definition$setting,
        additional_starts = definition$additional_starts, replicates = nrow(subset),
        median_time_difference = median(subset$max_time_difference),
        p90_time_difference = quantile(subset$max_time_difference, 0.9),
        median_regret = median(subset$regret), p90_regret = quantile(subset$regret, 0.9),
        maximum_regret = max(subset$regret),
        percent_over_1h = 100 * mean(subset$max_time_difference > 1),
        percent_over_5h = 100 * mean(subset$max_time_difference > 5),
        percent_over_10h = 100 * mean(subset$max_time_difference > 10),
        maximum_identity_error = max(subset$objective_identity_error))
    }
  }
}
output <- do.call(rbind, output)
write.csv(with_model_version(output), "paper1/output/common_regret_summary.csv",
          row.names = FALSE)

main <- output[output$setting == "Grid 21, draws 16" &
                 output$additional_starts == 0 & output$replicates == 1000, ]
stopifnot(nrow(main) == 2)
lines <- c("\\begin{tabular}{lllrrrrr}", "\\toprule",
  "Scenario & Comparison & Policy & Median regret & 90th regret & $>1$ h (\\%) & $>5$ h (\\%) & $>10$ h (\\%) \\\\",
  "\\midrule")
for (i in seq_len(nrow(main))) {
  x <- main[i, ]
  lines <- c(lines, sprintf("%s & 1 vs 10 & Reliability-full & %s & %s & %.1f & %.1f & %.1f \\\\",
    x$scenario, tex_scientific(x$median_regret), tex_scientific(x$p90_regret),
    x$percent_over_1h, x$percent_over_5h, x$percent_over_10h))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/parameterization_objective_table.tex")

lines <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
  "Scenario & Fixed setting & Starts & Replicates & Median $\\Delta t$ & 90th $\\Delta t$ & Median regret & 90th regret \\\\",
  "\\midrule")
sensitivity <- output[output$replicates == 200, ]
format_small_time <- function(x) {
  ifelse(x == 0, "$0$", ifelse(abs(x) < 0.001, "$<0.001$", format_fixed(x, 3)))
}
for (i in seq_len(nrow(sensitivity))) {
  x <- sensitivity[i, ]
  lines <- c(lines, sprintf("%s & %s & %d & %d & %s & %s & %s & %s \\\\",
    x$scenario, x$setting, x$additional_starts + 1, x$replicates,
    format_small_time(x$median_time_difference), format_small_time(x$p90_time_difference),
    tex_scientific(x$median_regret), tex_scientific(x$p90_regret)))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/common_regret_sensitivity_table.tex")
print(output, row.names = FALSE)
