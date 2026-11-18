source("paper1/R/grid3d_sequential_inspection.R")

stopifnot(requireNamespace("MASS", quietly = TRUE))
data(motors, package = "MASS")
motorette_data <- motors
names(motorette_data) <- c("temperature_c", "time", "failed")
motorette_data$kelvin <- motorette_data$temperature_c + 273.15
motorette_data$failed <- as.integer(motorette_data$failed)

exact_log_likelihood <- function(theta, times, x, failed) {
  sigma <- exp(theta[3])
  z <- (log(times) - theta[1] - theta[2] * x) / sigma
  event <- z - exp(z) - theta[3] - log(times)
  sum(ifelse(failed == 1, event, -exp(z)))
}

case_log_posterior <- function(theta, times, counts, historical, prior_mean, prior_sd) {
  if (theta[3] < -3 || theta[3] > 2 || abs(theta[1]) > 30 || abs(theta[2]) > 30000) {
    return(-Inf)
  }
  probabilities <- cell_probabilities(times, theta, attr(historical, "test_stress"))
  sum(counts * log(probabilities)) +
    exact_log_likelihood(theta, historical$time, historical$x, historical$failed) -
    0.5 * sum(((theta - prior_mean) / prior_sd)^2)
}

fit_case_posterior <- function(times, counts, historical, prior_mean, prior_sd,
                               start = prior_mean) {
  objective <- function(theta) {
    value <- case_log_posterior(theta, times, counts, historical, prior_mean, prior_sd)
    if (is.finite(value)) -value else 1e100
  }
  fit <- optim(start, objective, method = "BFGS", hessian = TRUE,
               control = list(reltol = 1e-9, maxit = 500))
  covariance <- tryCatch(solve(fit$hessian), error = function(e) diag(prior_sd^2, 3))
  list(mean = as.numeric(fit$par), covariance = psd_matrix(covariance),
       hessian = fit$hessian, convergence = fit$convergence)
}

fit_case_grid <- function(times, counts, historical, prior_mean, prior_sd,
                          start = prior_mean) {
  laplace <- fit_case_posterior(times, counts, historical, prior_mean, prior_sd, start)
  grid3d_posterior(times, counts, historical, prior_mean, prior_sd, laplace,
                   log_posterior_fun = case_log_posterior)
}

fit_case_grid_multistart <- function(times, counts, historical, prior_mean, prior_sd, starts) {
  fits <- lapply(starts, function(start) {
    fit_case_posterior(times, counts, historical, prior_mean, prior_sd, start)
  })
  values <- vapply(fits, function(fit) {
    case_log_posterior(fit$mean, times, counts, historical, prior_mean, prior_sd)
  }, numeric(1))
  laplace <- fits[[which.max(values)]]
  grid3d_posterior(times, counts, historical, prior_mean, prior_sd, laplace,
                   log_posterior_fun = case_log_posterior)
}

run_motorette_policy <- function(policy, failure_times, historical, test_stress, x_use,
                                 prior_mean, prior_sd, pilot_time = 100,
                                 end_time = 600, inspections = 5) {
  counts <- observed_counts(failure_times, pilot_time)
  posterior <- fit_case_grid(pilot_time, counts, historical, prior_mean, prior_sd)
  draws <- sample_grid3d(posterior, 40, 86001)
  remaining <- inspections - 1
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  at_risk <- sum(failure_times > pilot_time)
  objective <- function(z) {
    future_times <- increments_to_times(z, pilot_time, end_time, 20)
    if (policy == "Trace-full") {
      values <- vapply(seq_len(nrow(draws)), function(m) {
        sum(diag(conditional_information(future_times, pilot_time, at_risk,
                                         draws[m, ], test_stress)))
      }, numeric(1))
      return(-mean(values))
    }
    values <- vapply(seq_len(nrow(draws)), function(m) {
      information <- observed_information + prior_precision_matrix +
        conditional_information(future_times, pilot_time, at_risk, draws[m, ], test_stress)
      gradient <- target_gradient(draws[m, ], x_use = x_use)
      crossprod(gradient, solve(information) %*% gradient)
    }, numeric(1))
    mean(log(pmax(values, 1e-12)))
  }
  set.seed(7717)
  starts <- c(list(rep(0, remaining + 1)),
              lapply(1:29, function(i) rnorm(remaining + 1, sd = 2)))
  start_time <- proc.time()[3]
  fits <- lapply(starts, function(start) {
    optim(start, objective, method = "L-BFGS-B", lower = rep(-7, remaining + 1),
          upper = rep(7, remaining + 1),
          control = list(maxit = 300, factr = 1e7, ndeps = rep(1e-5, remaining + 1)))
  })
  elapsed <- proc.time()[3] - start_time
  best <- fits[[which.min(vapply(fits, function(fit) fit$value, numeric(1)))]]
  times <- c(pilot_time, increments_to_times(best$par, pilot_time, end_time, 20))
  counts <- observed_counts(failure_times, times)
  posterior <- fit_case_grid(times, counts, historical, prior_mean, prior_sd, posterior$mean)
  target_draws <- apply(sample_grid3d(posterior, 10000, 99001), 1,
                        target_value, t0 = 100000, x_use = x_use)
  estimate <- mean(target_draws)
  target_se <- sd(target_draws)
  data.frame(policy = policy, schedule = paste(round(times, 2), collapse = ";"),
             counts = paste(counts, collapse = ";"), estimate = estimate,
             reliability = exp(-exp(estimate)), se = target_se,
             lower = estimate - qnorm(0.95) * target_se,
             upper = estimate + qnorm(0.95) * target_se, objective = best$value,
             elapsed = elapsed, convergence = posterior$convergence)
}

historical <- motorette_data[motorette_data$temperature_c != 220, ]
adaptive_group <- ifelse(motorette_data$failed[motorette_data$temperature_c == 220] == 1,
                         motorette_data$time[motorette_data$temperature_c == 220], Inf)
policies <- c("Trace-full", "Full")
historical$x <- 1000 / historical$kelvin
attr(historical, "test_stress") <- 1000 / (220 + 273.15)
reference_fit <- fit_case_grid_multistart(100, c(0, 0), historical,
  c(10, 0.05, log(0.8)), c(100, 30000, 0.5),
  list(c(10, 0.05, log(0.8)), c(20, -10, log(0.8)), c(0, 10, log(0.8)),
       c(10, 1, log(0.8)), c(5, -5, log(0.8))))
reference_mean <- reference_fit$mean

case_results <- do.call(rbind, lapply(c(1000, 1), function(stress_scale) {
  historical$x <- stress_scale / historical$kelvin
  test_stress <- stress_scale / (220 + 273.15)
  x_use <- stress_scale / (100 + 273.15)
  attr(historical, "test_stress") <- test_stress
  beta_ratio <- 1000 / stress_scale
  prior_mean <- reference_mean * c(1, beta_ratio, 1)
  prior_sd <- c(0.7, 0.35 * abs(prior_mean[2]), 0.4)
  output <- do.call(rbind, lapply(policies, run_motorette_policy,
    failure_times = adaptive_group, historical = historical, test_stress = test_stress,
    x_use = x_use, prior_mean = prior_mean, prior_sd = prior_sd))
  output$stress_scale <- stress_scale
  output$prior_alpha <- prior_mean[1]
  output$prior_beta <- prior_mean[2]
  output$prior_logsigma <- prior_mean[3]
  output
}))
write.csv(motorette_data, "paper1/output/motorette_data.csv", row.names = FALSE)
write.csv(with_model_version(case_results), "paper1/output/motorette_case_results.csv", row.names = FALSE)
print(case_results, row.names = FALSE)
