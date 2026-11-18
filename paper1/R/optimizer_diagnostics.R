source("paper1/R/grid3d_sequential_inspection.R")

single_plan_objective <- function(times, posterior, current_time, at_risk,
                                  prior_sd, draws, x_use = -1) {
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  values <- vapply(seq_len(nrow(draws)), function(m) {
    theta <- draws[m, ]
    information <- observed_information + prior_precision_matrix +
      conditional_information(times, current_time, at_risk, theta)
    covariance <- tryCatch(solve(information), error = function(e) NULL)
    if (is.null(covariance)) return(1e100)
    gradient <- target_gradient(theta, x_use = x_use)
    crossprod(gradient, covariance %*% gradient)
  }, numeric(1))
  mean(log(pmax(values, 1e-12)))
}

group_plan_objective <- function(times, posterior, current_time, at_risk, stress,
                                 prior_sd, draws, x_use = -1.5) {
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  values <- vapply(seq_len(nrow(draws)), function(m) {
    theta <- draws[m, ]
    information <- observed_information + prior_precision_matrix
    for (g in seq_along(stress)) {
      information <- information + conditional_information(
        times, current_time, at_risk[g], theta, stress[g])
    }
    covariance <- tryCatch(solve(information), error = function(e) NULL)
    if (is.null(covariance)) return(1e100)
    gradient <- target_gradient(theta, x_use = x_use)
    crossprod(gradient, covariance %*% gradient)
  }, numeric(1))
  mean(log(pmax(values, 1e-12)))
}

optimize_single_schedule <- function(posterior, current_time, end_time, remaining,
                                     at_risk, prior_sd, horizon, draws,
                                     additional_starts = 0, start_seed = NULL,
                                     maxit = 100, minimum_gap = 0.02 * end_time) {
  k <- min(remaining, horizon)
  latest_time <- end_time - (remaining - k) * minimum_gap
  objective <- function(z) {
    times <- increments_to_times(z, current_time, latest_time, minimum_gap)
    single_plan_objective(times, posterior, current_time, at_risk, prior_sd, draws)
  }
  starts <- list(rep(0, k + 1))
  if (additional_starts > 0) {
    if (!is.null(start_seed)) set.seed(start_seed)
    starts <- c(starts, lapply(seq_len(additional_starts), function(i) rnorm(k + 1, sd = 2)))
  }
  fits <- lapply(starts, function(start) {
    optim(start, objective, method = "L-BFGS-B", lower = rep(-7, k + 1),
          upper = rep(7, k + 1), control = list(maxit = maxit, factr = 1e7))
  })
  best <- fits[[which.min(vapply(fits, function(x) x$value, numeric(1)))]]
  times <- increments_to_times(best$par, current_time, latest_time, minimum_gap)
  attr(times, "objective") <- best$value
  attr(times, "optimizer_convergence") <- best$convergence
  times
}

transform_group_posterior <- function(posterior, scale) {
  transformation <- diag(c(1, 1 / scale, 1))
  posterior$mean <- as.numeric(transformation %*% posterior$mean)
  posterior$covariance <- transformation %*% posterior$covariance %*% t(transformation)
  posterior$hessian <- solve(posterior$covariance)
  posterior$values[[2]] <- posterior$values[[2]] / scale
  posterior
}

transform_draws <- function(draws, scale) {
  draws[, 2] <- draws[, 2] / scale
  draws
}

optimize_group_codings <- function(posterior, failure_times, scenario, planning_samples,
                                   seed, additional_starts = 0) {
  at_risk <- vapply(failure_times, function(x) sum(x > 200), numeric(1))
  draws <- sample_grid3d(posterior, planning_samples, seed)
  scaled_posterior <- transform_group_posterior(posterior, 10)
  scaled_scenario <- scenario
  scaled_scenario$stress <- scenario$stress * 10
  scaled_scenario$prior_beta <- scenario$prior_beta / 10
  scaled_scenario$prior_sd[2] <- scenario$prior_sd[2] / 10
  scaled_draws <- transform_draws(draws, 10)
  base_plan <- plan_group_schedule(posterior, 200, 1000, 4, at_risk, scenario$stress,
    c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma)),
    scenario$prior_sd, 4, draws, additional_starts = additional_starts,
    start_seed = seed, x_use = -1.5)
  scaled_plan <- plan_group_schedule(scaled_posterior, 200, 1000, 4, at_risk,
    scaled_scenario$stress,
    c(scaled_scenario$prior_alpha, scaled_scenario$prior_beta, log(scaled_scenario$prior_sigma)),
    scaled_scenario$prior_sd, 4, scaled_draws, additional_starts = additional_starts,
    start_seed = seed, x_use = -15)
  base_at_base <- group_plan_objective(base_plan, posterior, 200, at_risk,
    scenario$stress, scenario$prior_sd, draws, x_use = -1.5)
  scaled_at_base <- group_plan_objective(scaled_plan, posterior, 200, at_risk,
    scenario$stress, scenario$prior_sd, draws, x_use = -1.5)
  base_at_scaled <- group_plan_objective(base_plan, scaled_posterior, 200, at_risk,
    scaled_scenario$stress, scaled_scenario$prior_sd, scaled_draws, x_use = -15)
  scaled_at_scaled <- group_plan_objective(scaled_plan, scaled_posterior, 200, at_risk,
    scaled_scenario$stress, scaled_scenario$prior_sd, scaled_draws, x_use = -15)
  regret <- max(abs(scaled_at_base - base_at_base),
                abs(base_at_scaled - scaled_at_scaled))
  data.frame(max_time_difference = max(abs(base_plan - scaled_plan)),
             regret = regret, base_regret = scaled_at_base - base_at_base,
             scaled_regret = base_at_scaled - scaled_at_scaled,
             objective_identity_error = abs(base_at_base - base_at_scaled) +
               abs(scaled_at_base - scaled_at_scaled),
             base_plan = paste(base_plan, collapse = ";"),
             scaled_plan = paste(scaled_plan, collapse = ";"))
}

run_group_coding_replicate <- function(replicate, scenario, planning_samples = 16,
                                       grid_size = 21, additional_starts = 0) {
  set.seed(scenario$seed + replicate)
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  fit <- fit_group_posterior(200, group_observed_counts(failure_times, 200),
                             scenario$stress, prior_mean, scenario$prior_sd)
  posterior <- grid3d_posterior(200, group_observed_counts(failure_times, 200),
    scenario$stress, prior_mean, scenario$prior_sd, fit, grid_size = grid_size)
  output <- optimize_group_codings(posterior, failure_times, scenario, planning_samples,
    scenario$seed + 10000 * replicate, additional_starts = additional_starts)
  output$replicate <- replicate
  output$scenario <- scenario$name
  output$grid_size <- grid_size
  output$planning_samples <- planning_samples
  output$additional_starts <- additional_starts
  output
}
