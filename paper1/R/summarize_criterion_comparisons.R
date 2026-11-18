source("paper1/R/mcse.R")
source("paper1/R/format_numbers.R")

rbind_shared <- function(x, y) {
  columns <- setdiff(intersect(names(x), names(y)), "model_version")
  rbind(x[, columns, drop = FALSE], y[, columns, drop = FALSE])
}

scenario_order <- c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                    "Shape prior offset", "Location prior offset",
                    "Two-stress baseline", "Acceleration prior offset")
policy_order <- c("Trace-1", "Myopic", "Trace-full", "Batch-full")

single <- do.call(rbind, lapply(1:6, function(index) {
  reliability <- read.csv(paste0("paper1/output/grid_scenario_", index, ".csv"))
  reliability <- reliability[reliability$policy %in% c("Myopic", "Full"), ]
  reliability$policy[reliability$policy == "Full"] <- "Batch-full"
  trace <- read.csv(paste0("paper1/output/trace_scenario_", index, ".csv"))
  rbind_shared(reliability, trace)
}))
joint <- do.call(rbind, lapply(1:2, function(index) {
  reliability <- read.csv(paste0("paper1/output/grid3d_scenario_", index, ".csv"))
  reliability <- reliability[reliability$policy %in% c("Myopic", "Full", "RITO"), ]
  reliability$policy[reliability$policy == "Full"] <- "Batch-full"
  reliability$policy[reliability$policy == "RITO"] <- "Trace-1"
  trace <- read.csv(paste0("paper1/output/trace3d_scenario_", index, ".csv"))
  if (!"planning_objective" %in% names(reliability)) reliability$planning_objective <- NA_real_
  if (!"optimizer_convergence" %in% names(reliability)) reliability$optimizer_convergence <- 0
  rbind_shared(reliability, trace)
}))
common_columns <- c("scenario", "replicate", "policy", "estimate", "truth", "error",
                    "se", "elapsed", "convergence", "schedule")
results <- rbind(single[, common_columns], joint[, common_columns])
results$scenario <- factor(results$scenario, levels = scenario_order)
results$policy <- factor(results$policy, levels = policy_order)
stopifnot(all(table(results$scenario, results$policy) == 1000),
          all(is.finite(results$error)), all(results$convergence == 0))
write.csv(results, "paper1/output/criterion_results.csv", row.names = FALSE)

summary <- do.call(rbind, lapply(split(results, list(results$scenario, results$policy),
                                      drop = TRUE), function(x) {
  mse <- mean(x$error^2)
  data.frame(scenario = as.character(x$scenario[1]), policy = as.character(x$policy[1]),
             replicates = nrow(x), bias = mean(x$error), rmse = sqrt(mse),
             rmse_mcse = rmse_mcse(x$error),
             seconds = mean(x$elapsed), convergence_failures = sum(x$convergence != 0))
}))
write.csv(summary, "paper1/output/criterion_summary.csv", row.names = FALSE)

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
reductions <- rbind(paired_reduction("Batch-full", "Trace-full"),
                    paired_reduction("Trace-full", "Trace-1"),
                    paired_reduction("Batch-full", "Myopic"))
write.csv(reductions, "paper1/output/criterion_reductions.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{lrrrrrr}", "\\toprule",
  "Scenario & Trace-1 & Reliability-1 & Trace-full & Reliability-full & Reduction (\\%) & 95\\% CI \\\\",
  "\\midrule")
for (scenario in scenario_order) {
  values <- vapply(policy_order, function(policy) {
    x <- summary[summary$scenario == scenario & summary$policy == policy, ]
    stopifnot(nrow(x) == 1)
    sprintf("%.3f", x$rmse)
  }, character(1))
  reduction <- reductions[reductions$scenario == scenario &
                            reductions$policy == "Batch-full" &
                            reductions$baseline == "Trace-full", ]
  lines <- c(lines, sprintf("%s & %s & %s & [%s, %s] \\\\",
                            scenario, paste(values, collapse = " & "),
                            format_fixed(reduction$reduction, 2),
                            format_fixed(reduction$lower, 2), format_fixed(reduction$upper, 2)))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/criterion_table.tex")
print(summary, row.names = FALSE)
print(reductions, row.names = FALSE)
