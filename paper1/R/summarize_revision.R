source("paper1/R/mcse.R")

batch <- do.call(rbind, lapply(1:6, function(index) {
  x <- read.csv(paste0("paper1/output/grid_scenario_", index, ".csv"))
  x$model_version <- NULL
  x
}))
outcome <- do.call(rbind, lapply(1:6, function(index) {
  x <- read.csv(paste0("paper1/output/grid_outcome_", index, ".csv"))
  x$model_version <- NULL
  x
}))
revision <- do.call(rbind, lapply(1:2, function(index) {
  x <- read.csv(paste0("paper1/output/grid3d_scenario_", index, ".csv"))
  x$model_version <- NULL
  x
}))
batch$failures <- NULL
batch$covered <- NULL
batch$n <- NULL
batch$planning_objective <- NA_real_
batch$optimizer_convergence <- 0
outcome$planning_objective <- NA_real_
outcome$optimizer_convergence <- 0
results <- rbind(batch, outcome, revision)
results$policy <- factor(results$policy,
  levels = c("Fixed", "Static", "Myopic", "Two-step", "Full", "Lookahead", "RITO"),
  labels = c("Fixed", "Static", "Myopic", "Batch-2", "Batch-full", "Outcome-2", "RITO"))
scenario_order <- c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                    "Shape prior offset", "Location prior offset",
                    "Two-stress baseline", "Acceleration prior offset")
results$scenario <- factor(results$scenario, levels = scenario_order)
write.csv(results, "paper1/output/revision_results.csv", row.names = FALSE)

summary <- do.call(rbind, lapply(split(results, list(results$scenario, results$policy), drop = TRUE), function(x) {
  mse <- mean(x$error^2)
  data.frame(scenario = as.character(x$scenario[1]), policy = as.character(x$policy[1]),
             replicates = nrow(x), bias = mean(x$error), rmse = sqrt(mse),
             rmse_mcse = rmse_mcse(x$error),
             error_sd = sd(x$error), mean_se = mean(x$se), seconds = mean(x$elapsed),
             convergence_failures = sum(x$convergence != 0))
}))
summary <- summary[order(match(summary$scenario, scenario_order), summary$policy), ]
write.csv(summary, "paper1/output/revision_performance_summary.csv", row.names = FALSE)

set.seed(20261116)
paired_reduction <- function(policy, baseline, scenarios = scenario_order, boot = 5000) {
  output <- lapply(scenarios, function(scenario) {
    x <- results[results$scenario == scenario & results$policy == policy, ]
    b <- results[results$scenario == scenario & results$policy == baseline, ]
    x <- x[order(x$replicate), ]
    b <- b[order(b$replicate), ]
    stopifnot(nrow(x) > 0, identical(x$replicate, b$replicate))
    n <- nrow(x)
    estimate <- 100 * (1 - sqrt(mean(x$error^2) / mean(b$error^2)))
    indices <- matrix(sample.int(n, n * boot, replace = TRUE), nrow = n, ncol = boot)
    boot_ratio <- sqrt(colMeans(matrix(x$error[indices]^2, nrow = n)) /
                         colMeans(matrix(b$error[indices]^2, nrow = n)))
    interval <- 100 * (1 - quantile(boot_ratio, c(0.975, 0.025)))
    data.frame(scenario = scenario, policy = policy, baseline = baseline,
               reduction = estimate, lower = interval[1], upper = interval[2])
  })
  do.call(rbind, output)
}
reductions <- rbind(paired_reduction("Batch-full", "Myopic", scenarios = scenario_order[1:6]),
                    paired_reduction("Batch-full", "Fixed", scenarios = scenario_order[1:6]),
                    paired_reduction("Batch-full", "RITO", scenarios = scenario_order[7:8]),
                    paired_reduction("Outcome-2", "Myopic", scenarios = scenario_order[1:6]),
                    paired_reduction("Outcome-2", "Batch-full", scenarios = scenario_order[1:6]))
canonical <- read.csv("paper1/output/criterion_reductions.csv")
canonical <- canonical[canonical$policy == "Batch-full" & canonical$baseline == "Myopic" &
                         canonical$scenario %in% scenario_order[1:6], ]
for (scenario in canonical$scenario) {
  target <- which(reductions$scenario == scenario & reductions$policy == "Batch-full" &
                    reductions$baseline == "Myopic")
  source_row <- canonical[canonical$scenario == scenario, ]
  stopifnot(length(target) == 1, nrow(source_row) == 1)
  reductions[target, c("reduction", "lower", "upper")] <-
    source_row[, c("reduction", "lower", "upper")]
}
write.csv(reductions, "paper1/output/revision_paired_reductions.csv", row.names = FALSE)
print(summary, row.names = FALSE)
print(reductions, row.names = FALSE)
