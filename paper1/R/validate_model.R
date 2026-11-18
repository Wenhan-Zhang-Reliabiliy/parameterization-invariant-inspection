source("paper1/R/grid_sequential_inspection.R")
source("paper1/R/grid3d_sequential_inspection.R")

set.seed(20261116)
max_target_error <- 0
max_gradient_error <- 0
max_cdf_gradient_error <- 0
for (i in 1:100) {
  theta <- c(runif(1, 5, 9), runif(1, -2, -0.2), runif(1, -1.2, 0.4))
  time <- runif(1, 100, 3000)
  x <- runif(1, -2, 1)
  t0 <- runif(1, 500, 2000)
  x_use <- runif(1, -2, -0.2)
  sigma <- exp(theta[3])
  z <- (log(time) - theta[1] - theta[2] * x) / sigma
  reliability <- exp(-exp(z))
  stopifnot(abs(weibull_cdf(time, theta, x) + reliability - 1) < 1e-12)
  expected_target <- (log(t0) - theta[1] - theta[2] * x_use) / sigma
  max_target_error <- max(max_target_error, abs(target_value(theta, t0, x_use) - expected_target))
  for (j in 1:3) {
    step <- 1e-6
    upper <- lower <- theta
    upper[j] <- upper[j] + step
    lower[j] <- lower[j] - step
    numerical_target <- (target_value(upper, t0, x_use) - target_value(lower, t0, x_use)) /
      (2 * step)
    numerical_cdf <- (weibull_cdf(time, upper, x) - weibull_cdf(time, lower, x)) /
      (2 * step)
    max_gradient_error <- max(max_gradient_error,
      abs(numerical_target - target_gradient(theta, t0, x_use)[j]))
    max_cdf_gradient_error <- max(max_cdf_gradient_error,
      abs(numerical_cdf - cdf_derivative(time, theta, x)[j]))
  }
}
stopifnot(max_target_error < 1e-12, max_gradient_error < 1e-5,
          max_cdf_gradient_error < 1e-5)

single <- readRDS("paper1/output/scenarios.rds")
baseline <- single[[1]]
theta <- c(baseline$alpha, baseline$beta, log(baseline$sigma))
baseline_reliability <- exp(-exp(target_value(theta)))
stopifnot(abs(baseline_reliability - 0.850275) < 1e-5)
set.seed(991)
simulated <- rweibull(200000, shape = 1 / baseline$sigma,
                      scale = exp(baseline$alpha + baseline$beta * (-1)))
empirical_reliability <- mean(simulated > 1000)
stopifnot(abs(empirical_reliability - baseline_reliability) < 0.005)

floor_diagnostics <- list()
for (index in seq_along(single)) {
  scenario <- single[[index]]
  for (replicate in 1:20) {
    set.seed(scenario$seed + replicate)
    failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                              scale = exp(scenario$alpha))
    prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
    counts <- observed_counts(failure_times, 200)
    posterior <- fit_grid_posterior(200, counts, prior_mean, scenario$prior_sd,
                                    grid_size = 41)
    observed <- posterior$hessian - prior_precision(scenario$prior_sd)
    floor_diagnostics[[length(floor_diagnostics) + 1]] <- data.frame(
      scenario = scenario$name, replicate = replicate,
      minimum_eigenvalue = min(eigen(observed, symmetric = TRUE)$values),
      posterior_precision_minimum = min(eigen(posterior$hessian, symmetric = TRUE)$values))
  }
}
floor_diagnostics <- do.call(rbind, floor_diagnostics)

joint <- readRDS("paper1/output/revision_scenarios.rds")
joint_diagnostics <- list()
for (index in seq_along(joint)) {
  scenario <- joint[[index]]
  for (replicate in 1:20) {
    set.seed(scenario$seed + replicate)
    failure_times <- lapply(scenario$stress, function(x) {
      rweibull(scenario$n, shape = 1 / scenario$sigma,
               scale = exp(scenario$alpha + scenario$beta * x))
    })
    prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
    counts <- group_observed_counts(failure_times, 200)
    fit <- fit_group_posterior(200, counts, scenario$stress, prior_mean, scenario$prior_sd)
    posterior <- grid3d_posterior(200, counts, scenario$stress, prior_mean,
                                  scenario$prior_sd, fit, grid_size = 21)
    observed <- posterior$hessian - prior_precision(scenario$prior_sd)
    joint_diagnostics[[length(joint_diagnostics) + 1]] <- data.frame(
      scenario = scenario$name, replicate = replicate,
      minimum_eigenvalue = min(eigen(observed, symmetric = TRUE)$values),
      posterior_precision_minimum = min(eigen(posterior$hessian, symmetric = TRUE)$values))
  }
}
joint_diagnostics <- do.call(rbind, joint_diagnostics)
all_diagnostics <- rbind(floor_diagnostics, joint_diagnostics)
stopifnot(min(all_diagnostics$posterior_precision_minimum) > 0)

metrics <- data.frame(
  max_target_error = max_target_error,
  max_target_gradient_error = max_gradient_error,
  max_cdf_gradient_error = max_cdf_gradient_error,
  baseline_reliability = baseline_reliability,
  empirical_reliability = empirical_reliability,
  minimum_observed_information_eigenvalue = min(all_diagnostics$minimum_eigenvalue),
  nonpositive_observed_information_cases = sum(all_diagnostics$minimum_eigenvalue <= 0),
  minimum_posterior_precision_eigenvalue = min(all_diagnostics$posterior_precision_minimum)
)
write.csv(metrics, "paper1/output/model_validation.csv", row.names = FALSE)
write.csv(all_diagnostics, "paper1/output/information_diagnostics.csv", row.names = FALSE)
print(metrics, row.names = FALSE)
