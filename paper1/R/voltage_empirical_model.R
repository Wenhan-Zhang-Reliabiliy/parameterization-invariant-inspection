source("paper1/R/grid3d_sequential_inspection.R")

voltage_data <- data.frame(
  voltage = rep(c(80, 100, 120), each = 8),
  time = c(1770, 2448, 3230, 3445, 3538, 5809, 6590, 6744,
           1090, 1907, 2147, 2645, 2903, 3357, 4135, 4381,
           630, 848, 1121, 1307, 1321, 1357, 1984, 2331)
)

voltage_exact_log_likelihood <- function(theta, times, x) {
  sigma <- exp(theta[3])
  z <- (log(times) - theta[1] - theta[2] * x) / sigma
  sum(z - exp(z) - theta[3] - log(times))
}

voltage_historical_posterior <- function(theta, times, counts, historical,
                                         prior_mean, prior_sd) {
  if (theta[3] < -3 || theta[3] > 2) return(-Inf)
  voltage_exact_log_likelihood(theta, historical$time, historical$x) -
    0.5 * sum(((theta - prior_mean) / prior_sd)^2)
}

voltage_new_posterior <- function(theta, times, counts, historical, prior_mean, prior_sd) {
  if (theta[3] < -3 || theta[3] > 2) return(-Inf)
  probabilities <- cell_probabilities(times, theta, attr(historical, "test_stress"))
  sum(counts * log(probabilities)) -
    0.5 * sum(((theta - prior_mean) / prior_sd)^2)
}

fit_voltage_posterior <- function(times, counts, historical, prior_mean, prior_sd,
                                  start = prior_mean, log_posterior_fun = voltage_new_posterior) {
  objective <- function(theta) {
    value <- log_posterior_fun(theta, times, counts, historical, prior_mean, prior_sd)
    if (is.finite(value)) -value else 1e100
  }
  fit <- optim(start, objective, method = "BFGS", hessian = TRUE,
               control = list(reltol = 1e-9, maxit = 1000))
  covariance <- tryCatch(solve(fit$hessian), error = function(e) diag(prior_sd^2, 3))
  list(mean = as.numeric(fit$par), covariance = psd_matrix(covariance),
       hessian = fit$hessian, convergence = fit$convergence)
}

fit_voltage_grid <- function(times, counts, historical, prior_mean, prior_sd,
                             start = prior_mean, log_posterior_fun = voltage_new_posterior) {
  laplace <- fit_voltage_posterior(times, counts, historical, prior_mean, prior_sd,
                                   start, log_posterior_fun)
  grid3d_posterior(times, counts, historical, prior_mean, prior_sd, laplace,
                   log_posterior_fun = log_posterior_fun)
}

voltage_historical <- voltage_data[voltage_data$voltage != 120, ]
voltage_adaptive <- voltage_data$time[voltage_data$voltage == 120]
voltage_historical$x <- (log(voltage_historical$voltage) - log(120)) / log(120 / 80)
attr(voltage_historical, "test_stress") <- 0
voltage_reference <- fit_voltage_grid(500, c(0, 0), voltage_historical,
  c(log(2500), -3, log(0.8)), c(10, 10, 10), log_posterior_fun = voltage_historical_posterior)
voltage_reference_mean <- as.numeric(voltage_reference$mean)

voltage_inputs <- function(stress_scale) {
  historical <- voltage_historical
  historical$x <- historical$x * stress_scale
  test_stress <- 0
  x_use <- (log(60) - log(120)) / log(120 / 80) * stress_scale
  prior_mean <- voltage_reference_mean * c(1, 1 / stress_scale, 1)
  prior_sd <- c(0.7, 0.7 / stress_scale, 0.4)
  attr(historical, "test_stress") <- test_stress
  list(historical = historical, test_stress = test_stress, x_use = x_use,
       prior_mean = prior_mean, prior_sd = prior_sd)
}

run_voltage_policy <- function(policy, stress_scale = 1, extra_starts = 30,
                               failure_times = voltage_adaptive, holdout_time = NULL) {
  inputs <- voltage_inputs(stress_scale)
  pilot_time <- 500
  end_time <- 2500
  inspections <- 5
  times <- pilot_time
  counts <- observed_counts(failure_times, times)
  posterior <- fit_voltage_grid(times, counts, inputs$historical,
                                inputs$prior_mean, inputs$prior_sd)
  warm_start <- NULL
  elapsed <- 0
  objectives <- numeric(0)
  for (stage in 2:inspections) {
    remaining <- inspections - stage + 1
    draws <- sample_grid3d(posterior, 40, 76001 + 100 * stage)
    start_time <- proc.time()[3]
    if (policy %in% c("Trace-full", "Weighted-full")) {
      future <- plan_trace_schedule(times[length(times)], end_time, remaining,
        sum(failure_times > times[length(times)]), 0, remaining, draws, warm_start,
        maxit = 200, weight_matrix = if (policy == "Weighted-full") posterior$covariance else NULL,
        additional_starts = extra_starts, start_seed = 76001 + 100 * stage)
    } else {
      future <- plan_group_schedule(posterior, times[length(times)], end_time, remaining,
        sum(failure_times > times[length(times)]), 0, inputs$prior_mean, inputs$prior_sd,
        remaining, draws, warm_start, maxit = 200, x_use = inputs$x_use,
        additional_starts = extra_starts, start_seed = 76001 + 100 * stage)
      objectives <- c(objectives, attr(future, "objective"))
    }
    elapsed <- elapsed + proc.time()[3] - start_time
    times <- c(times, future[1])
    counts <- observed_counts(failure_times, times)
    posterior <- fit_voltage_grid(times, counts, inputs$historical,
      inputs$prior_mean, inputs$prior_sd, posterior$mean)
    warm_start <- if (length(future) > 1) future[-1] else NULL
  }
  counts <- observed_counts(failure_times, times)
  posterior <- fit_voltage_grid(times, counts, inputs$historical,
    inputs$prior_mean, inputs$prior_sd, posterior$mean)
  posterior_draws <- sample_grid3d(posterior, 10000, 99001)
  target_draws <- apply(posterior_draws, 1, target_value,
                        t0 = 1000, x_use = inputs$x_use)
  estimate <- mean(target_draws)
  se <- sd(target_draws)
  log_score <- if (is.null(holdout_time)) NA_real_ else {
    sigma <- exp(posterior_draws[, 3])
    z <- (log(holdout_time) - posterior_draws[, 1] -
            posterior_draws[, 2] * inputs$test_stress) / sigma
    density <- exp(z - exp(z)) / (sigma * holdout_time)
    log(mean(density))
  }
  data.frame(policy = policy, stress_scale = stress_scale,
             schedule = paste(sprintf("%.3f", times), collapse = ";"),
             counts = paste(counts, collapse = ";"), estimate = estimate,
             reliability = exp(-exp(estimate)), se = se,
             lower = estimate - qnorm(0.95) * se,
             upper = estimate + qnorm(0.95) * se,
             holdout_time = if (is.null(holdout_time)) NA_real_ else holdout_time,
             log_score = log_score,
             objective = if (length(objectives)) sum(objectives) else NA_real_,
             elapsed = elapsed, convergence = posterior$convergence)
}
