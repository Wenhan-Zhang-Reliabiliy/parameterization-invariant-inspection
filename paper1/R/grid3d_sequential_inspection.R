source("paper1/R/grid3d_posterior.R")
source("paper1/R/trace_inspection.R")

simulate_grid3d_group_policy <- function(policy, failure_times, stress, prior_mean, prior_sd,
                                         pilot_time = 200, end_time = 1000, inspections = 5,
                                         planning_samples = 16, seed = 1, grid_size = 21,
                                         x_use = -1.5, additional_starts = 0) {
  times <- pilot_time
  fixed_times <- seq(pilot_time, end_time, length.out = inspections)
  counts <- group_observed_counts(failure_times, times)
  posterior <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                                fit_group_posterior(times, counts, stress, prior_mean, prior_sd),
                                grid_size = grid_size)
  at_risk <- vapply(failure_times, function(x) sum(x > pilot_time), numeric(1))
  warm_start <- NULL
  elapsed <- 0
  planning_objectives <- numeric(0)
  optimizer_convergence <- 0
  if (policy == "Fixed") {
    times <- fixed_times
  } else if (policy == "Static") {
    draws <- sample_grid3d(posterior, planning_samples, seed)
    future <- plan_group_schedule(posterior, pilot_time, end_time, inspections - 1,
                                  at_risk, stress, prior_mean, prior_sd,
                                  inspections - 1, draws, maxit = 120, x_use = x_use,
                                  additional_starts = additional_starts, start_seed = seed)
    planning_objectives <- attr(future, "objective")
    optimizer_convergence <- max(optimizer_convergence, attr(future, "optimizer_convergence"))
    times <- c(times, future)
  } else {
    for (stage in 2:inspections) {
      remaining <- inspections - stage + 1
      horizon <- if (policy == "Myopic") 1 else if (policy == "Two-step") 2 else 5
      draws <- sample_grid3d(posterior, planning_samples, seed + 100 * stage)
      start_time <- proc.time()[3]
      if (policy == "RITO") {
        future <- plan_group_rito(posterior, times[length(times)], end_time, remaining,
                                  at_risk, stress, prior_sd, draws)
      } else if (policy %in% c("Trace-full", "Weighted-trace")) {
        future <- plan_trace_schedule(times[length(times)], end_time, remaining,
          at_risk, stress, horizon, draws, warm_start,
          weight_matrix = if (policy == "Weighted-trace") posterior$covariance else NULL)
      } else {
        future <- plan_group_schedule(posterior, times[length(times)], end_time, remaining,
                                      at_risk, stress, prior_mean, prior_sd,
                                      horizon, draws, warm_start, x_use = x_use,
                                      additional_starts = additional_starts,
                                      start_seed = seed + 100 * stage)
      }
      if (!policy %in% c("RITO", "Trace-full", "Weighted-trace")) {
        planning_objectives <- c(planning_objectives, attr(future, "objective"))
        optimizer_convergence <- max(optimizer_convergence, attr(future, "optimizer_convergence"))
      }
      elapsed <- elapsed + proc.time()[3] - start_time
      times <- c(times, future[1])
      counts <- group_observed_counts(failure_times, times)
      posterior <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                                    fit_group_posterior(times, counts, stress, prior_mean,
                                                        prior_sd, posterior$mean),
                                    grid_size = grid_size)
      at_risk <- vapply(failure_times, function(x) sum(x > times[length(times)]), numeric(1))
      warm_start <- if (length(future) > 1) future[-1] else NULL
    }
  }
  counts <- group_observed_counts(failure_times, times)
  posterior <- grid3d_posterior(times, counts, stress, prior_mean, prior_sd,
                                fit_group_posterior(times, counts, stress, prior_mean,
                                                    prior_sd, posterior$mean),
                                grid_size = grid_size)
  target_draws <- apply(sample_grid3d(posterior, 5000, seed + 991), 1,
                        target_value, x_use = x_use)
  estimate <- mean(target_draws)
  target_se <- sd(target_draws)
  list(policy = policy, times = times, counts = counts, estimate = estimate,
       se = target_se, lower = estimate - qnorm(0.95) * target_se,
       upper = estimate + qnorm(0.95) * target_se,
       planning_objective = if (length(planning_objectives)) sum(planning_objectives) else NA_real_,
       optimizer_convergence = optimizer_convergence,
       elapsed = elapsed, convergence = posterior$convergence)
}

run_grid3d_replicate <- function(replicate, scenario,
                                 policies = c("Fixed", "Static", "Myopic", "Full", "RITO"),
                                 planning_samples = 16, inspections = 5, grid_size = 21,
                                 additional_starts = 0) {
  set.seed(scenario$seed + replicate)
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  x_use <- if (is.null(scenario$x_use)) -1.5 else scenario$x_use
  truth <- target_value(theta, x_use = x_use)
  output <- vector("list", length(policies))
  for (j in seq_along(policies)) {
    result <- simulate_grid3d_group_policy(policies[j], failure_times, scenario$stress,
                                           prior_mean, scenario$prior_sd,
                                           inspections = inspections,
                                           planning_samples = planning_samples,
                                           seed = scenario$seed + 10000 * replicate,
                                           grid_size = grid_size, x_use = x_use,
                                           additional_starts = additional_starts)
    output[[j]] <- data.frame(scenario = scenario$name, replicate = replicate,
                              policy = policies[j], estimate = result$estimate,
                              truth = truth, error = result$estimate - truth,
                              se = result$se, elapsed = result$elapsed,
                              planning_objective = result$planning_objective,
                              optimizer_convergence = result$optimizer_convergence,
                              convergence = result$convergence,
                              schedule = paste(sprintf("%.3f", result$times), collapse = ";"))
  }
  with_model_version(do.call(rbind, output))
}
