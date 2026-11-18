source("paper1/R/mcse.R")
source("paper1/R/format_numbers.R")

scenario_order <- c("Two-stress baseline", "Acceleration prior offset")
policy_order <- c("Plug-in trace", "Bayesian trace", "Plug-in reliability", "Bayesian reliability")
results <- do.call(rbind, lapply(1:2, function(index) {
  read.csv(paste0("paper1/output/factorial_scenario_", index, ".csv"))
}))
results$scenario <- factor(results$scenario, levels = scenario_order)
results$policy <- factor(results$policy, levels = policy_order)
stopifnot(all(table(results$scenario, results$policy) == 1000),
          all(is.finite(results$error)), all(results$convergence == 0),
          all(results$planning_convergence == 0))
write.csv(results, "paper1/output/factorial_results.csv", row.names = FALSE)

summary <- do.call(rbind, lapply(split(results, list(results$scenario, results$policy),
                                      drop = TRUE), function(x) {
  mse <- mean(x$error^2)
  data.frame(scenario = as.character(x$scenario[1]), policy = as.character(x$policy[1]),
             replicates = nrow(x), bias = mean(x$error), rmse = sqrt(mse),
             rmse_mcse = rmse_mcse(x$error),
             seconds = mean(x$elapsed), convergence_failures = sum(x$convergence != 0),
             planning_failures = sum(x$planning_convergence != 0))
}))
write.csv(summary, "paper1/output/factorial_summary.csv", row.names = FALSE)

set.seed(20261116)
paired_reduction <- function(policy, baseline) {
  do.call(rbind, lapply(scenario_order, function(scenario) {
    x <- results[results$scenario == scenario & results$policy == policy, ]
    b <- results[results$scenario == scenario & results$policy == baseline, ]
    x <- x[order(x$replicate), ]
    b <- b[order(b$replicate), ]
    stopifnot(identical(x$replicate, b$replicate))
    n <- nrow(x)
    indices <- matrix(sample.int(n, n * 5000, replace = TRUE), nrow = n, ncol = 5000)
    ratios <- sqrt(colMeans(matrix(x$error[indices]^2, nrow = n)) /
                     colMeans(matrix(b$error[indices]^2, nrow = n)))
    interval <- 100 * (1 - quantile(ratios, c(0.975, 0.025)))
    data.frame(scenario = scenario, policy = policy, baseline = baseline,
               reduction = 100 * (1 - sqrt(mean(x$error^2) / mean(b$error^2))),
               lower = interval[1], upper = interval[2])
  }))
}
reductions <- rbind(
  paired_reduction("Bayesian reliability", "Plug-in trace"),
  paired_reduction("Bayesian trace", "Plug-in trace"),
  paired_reduction("Plug-in reliability", "Plug-in trace"),
  paired_reduction("Bayesian reliability", "Bayesian trace"),
  paired_reduction("Bayesian reliability", "Plug-in reliability")
)
write.csv(reductions, "paper1/output/factorial_reductions.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{lrrrr}", "\\toprule",
  "Scenario & Plug-in trace & Bayesian trace & Plug-in reliability & Bayesian reliability \\\\",
  "\\midrule")
for (scenario in scenario_order) {
  values <- vapply(policy_order, function(policy) {
    x <- summary[summary$scenario == scenario & summary$policy == policy, ]
    stopifnot(nrow(x) == 1)
    sprintf("%.3f (%.3f)", x$rmse, x$rmse_mcse)
  }, character(1))
  lines <- c(lines, paste0(scenario, " & ", paste(values, collapse = " & "), " \\\\"))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/factorial_table.tex")

comparisons <- data.frame(
  policy = c("Bayesian reliability", "Bayesian trace", "Plug-in reliability",
             "Bayesian reliability", "Bayesian reliability"),
  baseline = c("Plug-in trace", "Plug-in trace", "Plug-in trace",
               "Bayesian trace", "Plug-in reliability"),
  label = c("Bayesian reliability vs Plug-in trace",
            "Bayesian trace vs Plug-in trace",
            "Plug-in reliability vs Plug-in trace",
            "Bayesian reliability vs Bayesian trace",
            "Bayesian reliability vs Plug-in reliability")
)
lines <- c("\\begin{tabular}{lrrrr}", "\\toprule",
  " & \\multicolumn{2}{c}{Two-stress baseline} & \\multicolumn{2}{c}{Acceleration offset} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Comparison & Reduction (\\%) & 95\\% CI & Reduction (\\%) & 95\\% CI \\\\",
  "\\midrule")
for (i in seq_len(nrow(comparisons))) {
  values <- lapply(scenario_order, function(scenario) {
    x <- reductions[reductions$scenario == scenario &
                      reductions$policy == comparisons$policy[i] &
                      reductions$baseline == comparisons$baseline[i], ]
    stopifnot(nrow(x) == 1)
    x
  })
  lines <- c(lines, sprintf("%s & %s & [%s, %s] & %s & [%s, %s] \\\\",
    comparisons$label[i], format_fixed(values[[1]]$reduction, 3),
    format_fixed(values[[1]]$lower, 3), format_fixed(values[[1]]$upper, 3),
    format_fixed(values[[2]]$reduction, 3), format_fixed(values[[2]]$lower, 3),
    format_fixed(values[[2]]$upper, 3)))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/factorial_reduction_table.tex")
print(summary, row.names = FALSE)
print(reductions, row.names = FALSE)
