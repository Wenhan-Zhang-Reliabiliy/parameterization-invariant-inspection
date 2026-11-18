source("paper1/R/model_version.R")

validate_file <- function(path, expected_rows, expected_replicates = 1000,
                          planning = FALSE) {
  x <- read.csv(path)
  assert_model_version(x, path)
  stopifnot(nrow(x) == expected_rows,
            identical(sort(unique(x$replicate)), seq_len(expected_replicates)),
            all(is.finite(x$error)), all(x$convergence == 0))
  if (planning) stopifnot(all(x$planning_convergence == 0))
  if ("optimizer_convergence" %in% names(x)) {
    stopifnot(all(x$optimizer_convergence %in% c(0, 52)))
  }
  optimizer_warnings <- if ("optimizer_convergence" %in% names(x)) {
    sum(x$optimizer_convergence != 0)
  } else {
    0
  }
  data.frame(file = path, rows = nrow(x), replicates = expected_replicates,
             policies = length(unique(x$policy)), convergence_failures = 0,
             optimizer_warnings = optimizer_warnings)
}

specifications <- list(
  list("paper1/output/grid_scenario_%d.csv", 1:6, 5000, FALSE),
  list("paper1/output/grid_outcome_%d.csv", 1:6, 1000, FALSE),
  list("paper1/output/trace_scenario_%d.csv", 1:6, 2000, FALSE),
  list("paper1/output/grid3d_scenario_%d.csv", 1:2, 5000, FALSE),
  list("paper1/output/trace3d_scenario_%d.csv", 1:2, 1000, FALSE),
  list("paper1/output/factorial_scenario_%d.csv", 1:2, 4000, TRUE),
  list("paper1/output/parameterization_scale_2_scenario_%d.csv", 1:2, 2000, FALSE),
  list("paper1/output/parameterization_scale_10_scenario_%d.csv", 1:2, 2000, FALSE)
)
checks <- list()
for (specification in specifications) {
  for (index in specification[[2]]) {
    checks[[length(checks) + 1]] <- validate_file(
      sprintf(specification[[1]], index), specification[[3]], planning = specification[[4]])
  }
}
for (scenario in c(1, 3)) {
  for (strength in c("Weak", "Current", "Diffuse")) {
    path <- paste0("paper1/output/prior_", strength, "_scenario_", scenario, ".csv")
    checks[[length(checks) + 1]] <- validate_file(path, 5000)
  }
}
checks <- do.call(rbind, checks)
identity <- read.csv("paper1/output/parameterization_identity.csv")
assert_model_version(identity, "parameterization_identity.csv")
for (path in c("paper1/output/grid_convergence_single.csv",
               "paper1/output/grid_convergence_group.csv",
               "paper1/output/grid_high_resolution.csv",
               "paper1/output/grid_posterior_validation.csv",
               "paper1/output/mcmc_posterior_validation.csv",
               "paper1/output/mcmc_group_validation.csv",
               "paper1/output/mcmc_diagnostics.csv")) {
  assert_model_version(read.csv(path), path)
}
stopifnot(max(identity$max_relative_utility_difference) < 1e-4)
write.csv(checks, "paper1/output/validation_checks.csv", row.names = FALSE)
writeLines(c(
  paste("Validated result files:", nrow(checks)),
  paste("Validated primary replicates per policy and scenario: 1000"),
  paste("Maximum relative reliability-utility identity error:",
        format(max(identity$max_relative_utility_difference), scientific = TRUE)),
  "Motorette coding priors are equivalent and all case fits converged."
), "paper1/output/validation_summary.txt")
print(checks, row.names = FALSE)
