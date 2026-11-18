source("paper1/R/grid3d_sequential_inspection.R")

group_log_likelihood <- function(theta, times, counts, stress) {
  if (theta[3] < -3 || theta[3] > 2 || abs(theta[1]) > 20 || abs(theta[2]) > 10) {
    return(-Inf)
  }
  likelihood <- 0
  for (g in seq_along(stress)) {
    probabilities <- cell_probabilities(times, theta, stress[g])
    likelihood <- likelihood + sum(counts[g, ] * log(probabilities))
  }
  likelihood
}

fit_group_mle <- function(times, counts, stress, start) {
  objective <- function(theta) {
    value <- group_log_likelihood(theta, times, counts, stress)
    if (is.finite(value)) -value else 1e100
  }
  fit <- optim(start, objective, method = "BFGS", hessian = TRUE,
               control = list(reltol = 1e-9, maxit = 500))
  list(mean = as.numeric(fit$par), observed_information = psd_matrix(fit$hessian, floor = 0),
       convergence = fit$convergence)
}

plan_plugin_reliability_schedule <- function(current_time, end_time, remaining, at_risk,
                                             stress, theta, observed_information,
                                             warm_start = NULL, maxit = 100,
                                             minimum_gap = 0.02 * end_time) {
  latest_time <- end_time
  objective <- function(z) {
    times <- increments_to_times(z, current_time, latest_time, minimum_gap)
    information <- observed_information
    for (g in seq_along(stress)) {
      information <- information + conditional_information(
        times, current_time, at_risk[g], theta, stress[g])
    }
    covariance <- tryCatch(solve(information), error = function(e) NULL)
    if (is.null(covariance)) return(1e100)
    gradient <- target_gradient(theta, x_use = -1.5)
    as.numeric(crossprod(gradient, covariance %*% gradient))
  }
  starts <- list(rep(0, remaining + 1))
  if (!is.null(warm_start) && length(warm_start) == remaining &&
      all(warm_start > current_time) && all(warm_start < latest_time)) {
    starts[[2]] <- times_to_increments(warm_start, current_time, latest_time, minimum_gap)
  }
  best <- NULL
  for (start in starts) {
    fit <- optim(start, objective, method = "L-BFGS-B",
                 lower = rep(-7, remaining + 1), upper = rep(7, remaining + 1),
                 control = list(maxit = maxit, factr = 1e7))
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  increments_to_times(best$par, current_time, latest_time, minimum_gap)
}

simulate_factorial_policy <- function(policy, failure_times, stress, prior_mean, prior_sd,
                                      pilot_times = c(200, 400), end_time = 1000,
                                      inspections = 5, planning_samples = 16,
                                      seed = 1, grid_size = 21) {
  times <- pilot_times
  counts <- group_observed_counts(failure_times, times)
  bayesian <- policy %in% c("Bayesian trace", "Bayesian reliability")
  initial_fit <- fit_group_posterior(times, counts, stress, prior_mean, prior_sd)
  if (bayesian) {
    state <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                              initial_fit, grid_size = grid_size)
  } else {
    state <- fit_group_mle(times, counts, stress, initial_fit$mean)
  }
  planning_convergence <- if (bayesian) initial_fit$convergence else state$convergence
  at_risk <- vapply(failure_times, function(x) sum(x > times[length(times)]), numeric(1))
  warm_start <- NULL
  elapsed <- 0
  for (stage in (length(pilot_times) + 1):inspections) {
    remaining <- inspections - stage + 1
    start_time <- proc.time()[3]
    if (bayesian) {
      draws <- sample_grid3d(state, planning_samples, seed + 100 * stage)
      if (policy == "Bayesian trace") {
        future <- plan_trace_schedule(times[length(times)], end_time, remaining,
                                     at_risk, stress, remaining, draws, warm_start)
      } else {
        future <- plan_group_schedule(state, times[length(times)], end_time, remaining,
                                      at_risk, stress, prior_mean, prior_sd,
                                      remaining, draws, warm_start)
      }
    } else {
      if (policy == "Plug-in trace") {
        future <- plan_trace_schedule(times[length(times)], end_time, remaining,
                                     at_risk, stress, remaining, matrix(state$mean, nrow = 1),
                                     warm_start)
      } else {
        future <- plan_plugin_reliability_schedule(
          times[length(times)], end_time, remaining, at_risk, stress,
          state$mean, state$observed_information, warm_start)
      }
    }
    elapsed <- elapsed + proc.time()[3] - start_time
    times <- c(times, future[1])
    counts <- group_observed_counts(failure_times, times)
    if (bayesian) {
      fit <- fit_group_posterior(times, counts, stress, prior_mean, prior_sd, state$mean)
      state <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                                fit, grid_size = grid_size)
      planning_convergence <- max(planning_convergence, fit$convergence)
    } else {
      state <- fit_group_mle(times, counts, stress, state$mean)
      planning_convergence <- max(planning_convergence, state$convergence)
    }
    at_risk <- vapply(failure_times, function(x) sum(x > times[length(times)]), numeric(1))
    warm_start <- if (length(future) > 1) future[-1] else NULL
  }
  final_fit <- fit_group_posterior(times, counts, stress, prior_mean, prior_sd)
  posterior <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                                final_fit, grid_size = grid_size)
  target_draws <- apply(sample_grid3d(posterior, 5000, seed + 991), 1,
                        target_value, x_use = -1.5)
  estimate <- mean(target_draws)
  target_se <- sd(target_draws)
  list(times = times, estimate = estimate, se = target_se,
       elapsed = elapsed, convergence = final_fit$convergence,
       planning_convergence = planning_convergence)
}

run_factorial_replicate <- function(replicate, scenario, planning_samples = 16, grid_size = 21) {
  set.seed(scenario$seed + replicate)
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  truth <- target_value(theta, x_use = -1.5)
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  policies <- c("Plug-in trace", "Bayesian trace", "Plug-in reliability", "Bayesian reliability")
  output <- lapply(policies, function(policy) {
    result <- simulate_factorial_policy(policy, failure_times, scenario$stress,
      prior_mean, scenario$prior_sd, planning_samples = planning_samples,
      seed = scenario$seed + 10000 * replicate, grid_size = grid_size)
    data.frame(scenario = scenario$name, replicate = replicate, policy = policy,
               estimate = result$estimate, truth = truth, error = result$estimate - truth,
               se = result$se, elapsed = result$elapsed, convergence = result$convergence,
               planning_convergence = result$planning_convergence,
               schedule = paste(sprintf("%.3f", result$times), collapse = ";"))
  })
  do.call(rbind, output)
}
