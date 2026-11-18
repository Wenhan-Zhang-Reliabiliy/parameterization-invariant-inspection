source("paper1/R/mcse.R")
source("paper1/R/format_numbers.R")

scenario_names <- c("Two-stress baseline", "Acceleration prior offset")
policy_levels <- c("Trace-full", "Full")
policy_labels <- c("Trace-full", "Reliability-full")

read_scale <- function(index, scale) {
  if (scale == 1) {
    trace <- read.csv(paste0("paper1/output/trace3d_scenario_", index, ".csv"))
    reliability_file <- paste0("paper1/output/parameterization_scale_1_scenario_", index, ".csv")
    if (file.exists(reliability_file)) {
      reliability <- read.csv(reliability_file)
    } else {
      reliability <- read.csv(paste0("paper1/output/grid3d_scenario_", index, ".csv"))
      reliability <- reliability[reliability$policy == "Full", ]
      reliability$planning_objective <- NA_real_
      reliability$optimizer_convergence <- 0
    }
    trace$model_version <- NULL
    reliability$model_version <- NULL
    trace$stress_scale <- scale
    reliability$stress_scale <- scale
    x <- rbind(trace, reliability)
  } else {
    x <- read.csv(paste0("paper1/output/parameterization_scale_", scale,
                         "_scenario_", index, ".csv"))
  }
  x$model_version <- NULL
  x$stress_scale <- scale
  x
}
results <- do.call(rbind, lapply(1:2, function(index) {
  do.call(rbind, lapply(c(1, 2, 10), function(scale) read_scale(index, scale)))
}))
results$policy <- factor(results$policy, levels = policy_levels, labels = policy_labels)
stopifnot(all(table(results$scenario, results$policy, results$stress_scale) == 1000),
          all(is.finite(results$error)), all(results$convergence == 0))
write.csv(results, "paper1/output/parameterization_results.csv", row.names = FALSE)

accuracy <- do.call(rbind, lapply(split(results,
  list(results$scenario, results$policy, results$stress_scale), drop = TRUE), function(x) {
  mse <- mean(x$error^2)
  data.frame(scenario = x$scenario[1], policy = as.character(x$policy[1]),
             scale = x$stress_scale[1], replicates = nrow(x), rmse = sqrt(mse),
             rmse_mcse = rmse_mcse(x$error),
             seconds = mean(x$elapsed))
}))
write.csv(accuracy, "paper1/output/parameterization_accuracy.csv", row.names = FALSE)

schedule_matrix <- function(schedule) {
  matrix(as.numeric(unlist(strsplit(schedule, ";", fixed = TRUE))), nrow = 5)
}
differences <- do.call(rbind, lapply(scenario_names, function(scenario) {
  do.call(rbind, lapply(c(2, 10), function(scale) {
    do.call(rbind, lapply(policy_labels, function(policy) {
      a <- results[results$scenario == scenario & results$policy == policy &
                     results$stress_scale == 1, ]
      b <- results[results$scenario == scenario & results$policy == policy &
                     results$stress_scale == scale, ]
      a <- a[order(a$replicate), ]
      b <- b[order(b$replicate), ]
      stopifnot(identical(a$replicate, b$replicate), max(abs(a$truth - b$truth)) < 1e-10)
      change <- vapply(seq_len(nrow(a)), function(i) {
        max(abs(schedule_matrix(a$schedule[i]) - schedule_matrix(b$schedule[i])))
      }, numeric(1))
      data.frame(scenario = scenario, policy = policy, comparison = paste0("1 vs ", scale),
                 replicate = a$replicate, max_time_difference = change)
    }))
  }))
}))
write.csv(differences, "paper1/output/parameterization_schedule_differences.csv", row.names = FALSE)

schedule_summary <- do.call(rbind, lapply(split(differences,
  list(differences$scenario, differences$policy, differences$comparison), drop = TRUE), function(x) {
  data.frame(scenario = x$scenario[1], policy = x$policy[1], comparison = x$comparison[1],
             median = median(x$max_time_difference), mean = mean(x$max_time_difference),
             p90 = quantile(x$max_time_difference, 0.9), maximum = max(x$max_time_difference),
             percent_over_1h = 100 * mean(x$max_time_difference > 1),
             percent_over_5h = 100 * mean(x$max_time_difference > 5),
             percent_over_10h = 100 * mean(x$max_time_difference > 10))
}))
write.csv(schedule_summary, "paper1/output/parameterization_schedule_summary.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{lllrrr}", "\\toprule",
  "Scenario & Coding comparison & Policy & Median & 90th percentile & Maximum \\\\",
  "\\midrule")
for (i in seq_len(nrow(schedule_summary))) {
  x <- schedule_summary[i, ]
  lines <- c(lines, sprintf("%s & %s & %s & %.2f & %.2f & %.2f \\\\",
                            x$scenario, x$comparison, x$policy, x$median, x$p90, x$maximum))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/parameterization_schedule_table.tex")

lines <- c("\\begin{tabular}{llrrr}", "\\toprule",
  "Scenario & Policy & Scale 1 & Scale 2 & Scale 10 \\\\", "\\midrule")
for (scenario in scenario_names) {
  for (policy in policy_labels) {
    values <- vapply(c(1, 2, 10), function(scale) {
      x <- accuracy[accuracy$scenario == scenario & accuracy$policy == policy &
                      accuracy$scale == scale, ]
      stopifnot(nrow(x) == 1)
      sprintf("%.3f (%.3f)", x$rmse, x$rmse_mcse)
    }, character(1))
    lines <- c(lines, sprintf("%s & %s & %s \\\\", scenario, policy,
                              paste(values, collapse = " & ")))
  }
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/parameterization_accuracy_table.tex")

set.seed(20261116)
accuracy_reductions <- do.call(rbind, lapply(scenario_names, function(scenario) {
  do.call(rbind, lapply(policy_labels, function(policy) {
    do.call(rbind, lapply(c(2, 10), function(scale) {
      a <- results[results$scenario == scenario & results$policy == policy &
                     results$stress_scale == scale, ]
      b <- results[results$scenario == scenario & results$policy == policy &
                     results$stress_scale == 1, ]
      a <- a[order(a$replicate), ]
      b <- b[order(b$replicate), ]
      n <- nrow(a)
      indices <- matrix(sample.int(n, n * 5000, replace = TRUE), nrow = n, ncol = 5000)
      ratios <- sqrt(colMeans(matrix(a$error[indices]^2, nrow = n)) /
                       colMeans(matrix(b$error[indices]^2, nrow = n)))
      interval <- 100 * (1 - quantile(ratios, c(0.975, 0.025)))
      data.frame(scenario = scenario, policy = policy, comparison = paste0(scale, " vs 1"),
                 reduction = 100 * (1 - sqrt(mean(a$error^2) / mean(b$error^2))),
                 lower = interval[1], upper = interval[2])
    }))
  }))
}))
write.csv(accuracy_reductions, "paper1/output/parameterization_accuracy_reductions.csv",
          row.names = FALSE)
lines <- c("\\begin{tabular}{lllrr}", "\\toprule",
  "Scenario & Policy & Comparison & Reduction (\\%) & 95\\% paired CI \\\\", "\\midrule")
for (i in seq_len(nrow(accuracy_reductions))) {
  x <- accuracy_reductions[i, ]
  lines <- c(lines, sprintf("%s & %s & %s & %s & [%s, %s] \\\\",
    x$scenario, x$policy, x$comparison, format_fixed(x$reduction, 2),
    format_fixed(x$lower, 2), format_fixed(x$upper, 2)))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/parameterization_accuracy_reduction_table.tex")
print(accuracy, row.names = FALSE)
print(schedule_summary, row.names = FALSE)
