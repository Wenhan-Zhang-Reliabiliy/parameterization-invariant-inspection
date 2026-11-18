source("paper1/R/optimizer_diagnostics.R")

scenario <- readRDS("paper1/output/revision_scenarios.rds")[[1]]
checks <- list()
for (grid_size in c(21, 31)) {
  for (planning_samples in c(16, 32)) {
    x <- run_group_coding_replicate(1, scenario, planning_samples = planning_samples,
                                    grid_size = grid_size)
    stopifnot(is.finite(x$regret), x$regret >= 0,
              x$objective_identity_error < 1e-4,
              abs(x$base_regret - x$scaled_regret) < 1e-4)
    checks[[length(checks) + 1]] <- data.frame(
      grid_size = grid_size, planning_samples = planning_samples,
      regret = x$regret, identity_error = x$objective_identity_error,
      directional_regret_difference = abs(x$base_regret - x$scaled_regret))
  }
}
checks <- do.call(rbind, checks)
write.csv(with_model_version(checks), "paper1/output/optimizer_diagnostic_validation.csv",
          row.names = FALSE)
print(checks, row.names = FALSE)
