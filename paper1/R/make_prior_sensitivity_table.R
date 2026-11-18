source("paper1/R/mcse.R")

strengths <- c("Weak", "Current", "Diffuse")
scenario_indices <- c(1, 3)
scenario_names <- c("Baseline", "Sparse failures")
policy_levels <- c("Fixed", "Static", "Myopic", "Two-step", "Full")
policy_labels <- c("Fixed", "Static", "Myopic", "Batch-2", "Batch-full")

results <- do.call(rbind, lapply(scenario_indices, function(index) {
  do.call(rbind, lapply(strengths, function(strength) {
    x <- read.csv(paste0("paper1/output/prior_", strength, "_scenario_", index, ".csv"))
    stopifnot(nrow(x) == 5000, identical(sort(unique(x$replicate)), 1:1000),
              all(is.finite(x$error)), all(x$convergence == 0))
    x$model_version <- NULL
    x$prior_strength <- factor(strength, levels = strengths)
    x
  }))
}))
results$policy <- factor(results$policy, levels = policy_levels, labels = policy_labels)

summary <- do.call(rbind, lapply(split(results, list(results$scenario, results$prior_strength,
                                                      results$policy), drop = TRUE), function(x) {
  data.frame(scenario = x$scenario[1], prior_strength = as.character(x$prior_strength[1]),
             policy = as.character(x$policy[1]), replicates = nrow(x),
             bias = mean(x$error), rmse = sqrt(mean(x$error^2)),
             rmse_mcse = rmse_mcse(x$error),
             seconds = mean(x$elapsed), convergence_failures = sum(x$convergence != 0))
}))
write.csv(summary, "paper1/output/prior_sensitivity_summary.csv", row.names = FALSE)

set.seed(20261116)
reductions <- do.call(rbind, lapply(scenario_names, function(scenario) {
  do.call(rbind, lapply(strengths, function(strength) {
    x <- results[results$scenario == scenario & results$prior_strength == strength &
                   results$policy == "Batch-full", ]
    b <- results[results$scenario == scenario & results$prior_strength == strength &
                   results$policy == "Myopic", ]
    x <- x[order(x$replicate), ]
    b <- b[order(b$replicate), ]
    n <- nrow(x)
    indices <- matrix(sample.int(n, n * 5000, replace = TRUE), nrow = n, ncol = 5000)
    ratios <- sqrt(colMeans(matrix(x$error[indices]^2, nrow = n)) /
                     colMeans(matrix(b$error[indices]^2, nrow = n)))
    interval <- 100 * (1 - quantile(ratios, c(0.975, 0.025)))
    data.frame(scenario = scenario, prior_strength = strength,
               reduction = 100 * (1 - sqrt(mean(x$error^2) / mean(b$error^2))),
               lower = interval[1], upper = interval[2])
  }))
}))
canonical <- read.csv("paper1/output/criterion_reductions.csv")
canonical <- canonical[canonical$policy == "Batch-full" & canonical$baseline == "Myopic", ]
for (scenario in scenario_names) {
  target <- which(reductions$scenario == scenario & reductions$prior_strength == "Current")
  source_row <- canonical[canonical$scenario == scenario, ]
  stopifnot(length(target) == 1, nrow(source_row) == 1)
  reductions[target, c("reduction", "lower", "upper")] <-
    source_row[, c("reduction", "lower", "upper")]
}
write.csv(reductions, "paper1/output/prior_sensitivity_reductions.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{llrrrrr}", "\\toprule",
  "Scenario & Prior SD & Fixed & Static & Reliability-1 & Reliability-full & Reduction (\\%) \\\\",
  "\\midrule")
for (scenario in scenario_names) {
  for (strength in strengths) {
    values <- vapply(c("Fixed", "Static", "Myopic", "Batch-full"), function(policy) {
      x <- summary[summary$scenario == scenario & summary$prior_strength == strength &
                     summary$policy == policy, ]
      stopifnot(nrow(x) == 1)
      sprintf("%.3f", x$rmse)
    }, character(1))
    reduction <- reductions[reductions$scenario == scenario &
                              reductions$prior_strength == strength, ]
    lines <- c(lines, sprintf("%s & %s & %s & %.1f \\\\",
      if (strength == "Weak") scenario else "", strength, paste(values, collapse = " & "),
      reduction$reduction))
  }
  if (scenario != scenario_names[length(scenario_names)]) lines <- c(lines, "\\midrule")
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/prior_sensitivity_table.tex")
print(summary, row.names = FALSE)
print(reductions, row.names = FALSE)
