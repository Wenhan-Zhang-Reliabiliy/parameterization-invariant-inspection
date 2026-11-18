source("paper1/R/grid3d_sequential_inspection.R")

set.seed(20261118)
A <- diag(c(1, 0.1, 1))
C <- crossprod(matrix(rnorm(9), 3, 3)) + diag(3)
I <- crossprod(matrix(rnorm(9), 3, 3)) + diag(3)
C_transformed <- A %*% C %*% t(A)
I_transformed <- t(solve(A)) %*% I %*% solve(A)
base_value <- sum(diag(C %*% I))
transformed_value <- sum(diag(C_transformed %*% I_transformed))
stopifnot(abs(base_value - transformed_value) < 1e-8)

scenario <- readRDS("paper1/output/revision_scenarios.rds")[[1]]
run_pilot <- function(scale) {
  scaled <- scenario
  scaled$stress <- scaled$stress * scale
  scaled$beta <- scaled$beta / scale
  scaled$prior_beta <- scaled$prior_beta / scale
  scaled$prior_sd[2] <- scaled$prior_sd[2] / scale
  scaled$x_use <- -1.5 * scale
  run_grid3d_replicate(1, scaled, policies = "Weighted-trace", planning_samples = 16,
                       grid_size = 21)
}
base <- run_pilot(1)
scaled <- run_pilot(10)
stopifnot(nrow(base) == 1, nrow(scaled) == 1,
          is.finite(base$error), is.finite(scaled$error),
          base$convergence == 0, scaled$convergence == 0)
schedule_difference <- max(abs(as.numeric(strsplit(base$schedule, ";")[[1]]) -
                                 as.numeric(strsplit(scaled$schedule, ";")[[1]])))
output <- data.frame(base_weighted_trace = base_value,
                     transformed_weighted_trace = transformed_value,
                     absolute_identity_error = abs(base_value - transformed_value),
                     pilot_schedule_difference = schedule_difference,
                     base_seconds = base$elapsed, scaled_seconds = scaled$elapsed)
write.csv(output, "paper1/output/weighted_trace_validation.csv", row.names = FALSE)
print(output, row.names = FALSE)
