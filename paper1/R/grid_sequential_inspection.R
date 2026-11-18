source("paper1/R/posterior_grid.R")
source("paper1/R/trace_inspection.R")

fit_grid_posterior <- function(times, counts, prior_mean, prior_sd, start = prior_mean,
                               grid_size = 61, range_scale = 4) {
  laplace <- fit_posterior(times, counts, prior_mean, prior_sd, start)
  grid <- grid_posterior(times, counts, prior_mean, prior_sd, laplace, grid_size, range_scale)
  grid$hessian <- solve(grid$covariance)
  grid$convergence <- laplace$convergence
  grid
}

simulate_grid_policy <- function(policy, failure_times, prior_mean, prior_sd,
                                 pilot_time = 200, end_time = 1000, inspections = 5,
                                 horizon = 5, planning_samples = 32, seed = 1,
                                 grid_size = 61) {
  n <- length(failure_times)
  fixed_times <- seq(pilot_time, end_time, length.out = inspections)
  times <- pilot_time
  counts <- observed_counts(failure_times, times)
  posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd,
                                  grid_size = grid_size)
  elapsed <- 0
  warm_start <- NULL
  if (policy == "Fixed") {
    times <- fixed_times
  } else {
    if (policy == "Static") {
      draws <- sample_grid(posterior, planning_samples, seed)
      start_time <- proc.time()[3]
      future <- plan_schedule(posterior, pilot_time, end_time, inspections - 1,
                              sum(failure_times > pilot_time), prior_mean, prior_sd,
                              inspections - 1, draws, maxit = 120)
      elapsed <- elapsed + proc.time()[3] - start_time
      times <- c(times, future)
    } else {
      for (stage in 2:inspections) {
        remaining <- inspections - stage + 1
        current_horizon <- if (policy %in% c("Myopic", "Trace-1")) 1 else
          if (policy == "Two-step") 2 else horizon
        draws <- sample_grid(posterior, planning_samples, seed + 100 * stage)
        start_time <- proc.time()[3]
        if (policy %in% c("Trace-1", "Trace-full")) {
          future <- plan_trace_schedule(times[length(times)], end_time, remaining,
                                       sum(failure_times > times[length(times)]), 0,
                                       current_horizon, draws, warm_start)
        } else {
          future <- plan_schedule(posterior, times[length(times)], end_time, remaining,
                                  sum(failure_times > times[length(times)]), prior_mean,
                                  prior_sd, current_horizon, draws, warm_start)
        }
        elapsed <- elapsed + proc.time()[3] - start_time
        next_time <- future[1]
        times <- c(times, next_time)
        counts <- observed_counts(failure_times, times)
        posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd,
                                        posterior$mean, grid_size = grid_size)
        warm_start <- if (length(future) > 1) future[-1] else NULL
      }
    }
  }
  counts <- observed_counts(failure_times, times)
  posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd,
                                  posterior$mean, grid_size = grid_size)
  draws <- sample_grid(posterior, 5000, seed + 991)
  target_draws <- apply(draws, 1, target_value)
  estimate <- mean(target_draws)
  target_se <- sd(target_draws)
  list(policy = policy, times = times, counts = counts, estimate = estimate,
       se = target_se, lower = estimate - qnorm(0.95) * target_se,
       upper = estimate + qnorm(0.95) * target_se,
       elapsed = elapsed, convergence = posterior$convergence)
}

run_grid_replicate <- function(replicate, scenario,
                               policies = c("Fixed", "Static", "Myopic", "Two-step", "Full"),
                               planning_samples = 32, inspections = 5, grid_size = 61) {
  set.seed(scenario$seed + replicate)
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                            scale = exp(scenario$alpha))
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  prior_sd <- if (is.null(scenario$prior_sd)) c(0.55, 0.55, 0.35) else scenario$prior_sd
  truth <- target_value(theta)
  output <- vector("list", length(policies))
  for (j in seq_along(policies)) {
    result <- simulate_grid_policy(policies[j], failure_times, prior_mean, prior_sd,
                                   inspections = inspections, planning_samples = planning_samples,
                                   seed = scenario$seed + 10000 * replicate,
                                   grid_size = grid_size)
    output[[j]] <- data.frame(scenario = scenario$name, replicate = replicate,
                              policy = policies[j], n = scenario$n,
                              estimate = result$estimate, truth = truth,
                              error = result$estimate - truth,
                              covered = result$lower <= truth & truth <= result$upper,
                              se = result$se, elapsed = result$elapsed,
                              convergence = result$convergence,
                              schedule = paste(sprintf("%.3f", result$times), collapse = ";"),
                              failures = scenario$n - result$counts[length(result$counts)])
  }
  with_model_version(do.call(rbind, output))
}
