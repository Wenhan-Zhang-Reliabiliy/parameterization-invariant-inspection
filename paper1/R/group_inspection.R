source("paper1/R/sequential_inspection.R")

group_log_posterior <- function(theta, times, counts, stress, prior_mean, prior_sd) {
  if (theta[3] < -3 || theta[3] > 2 || abs(theta[1]) > 20 || abs(theta[2]) > 10) {
    return(-Inf)
  }
  likelihood <- 0
  for (g in seq_along(stress)) {
    p <- cell_probabilities(times, theta, stress[g])
    likelihood <- likelihood + sum(counts[g, ] * log(p))
  }
  likelihood - 0.5 * sum(((theta - prior_mean) / prior_sd)^2)
}

fit_group_posterior <- function(times, counts, stress, prior_mean, prior_sd,
                                start = prior_mean) {
  objective <- function(theta) {
    value <- group_log_posterior(theta, times, counts, stress, prior_mean, prior_sd)
    if (is.finite(value)) -value else 1e100
  }
  fit <- optim(start, objective, method = "BFGS", hessian = TRUE,
               control = list(reltol = 1e-9, maxit = 500))
  covariance <- tryCatch(solve(fit$hessian), error = function(e) diag(prior_sd^2, 3))
  list(mean = as.numeric(fit$par), covariance = psd_matrix(covariance),
       hessian = fit$hessian, convergence = fit$convergence)
}

group_observed_counts <- function(failure_times, inspections) {
  t(vapply(failure_times, observed_counts, numeric(length(inspections) + 1),
           inspections = inspections))
}

plan_group_schedule <- function(posterior, current_time, end_time, remaining,
                                at_risk, stress, prior_mean, prior_sd, horizon,
                                draws, warm_start = NULL, maxit = 100,
                                minimum_gap = 0.02 * end_time, x_use = -1.5,
                                additional_starts = 0, start_seed = NULL) {
  k <- min(remaining, horizon)
  latest_time <- end_time - (remaining - k) * minimum_gap
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  objective <- function(z) {
    times <- increments_to_times(z, current_time, latest_time, minimum_gap)
    values <- numeric(nrow(draws))
    for (m in seq_len(nrow(draws))) {
      theta <- draws[m, ]
      information <- observed_information + prior_precision_matrix
      for (g in seq_along(stress)) {
        information <- information + conditional_information(
          times, current_time, at_risk[g], theta, stress[g])
      }
      covariance <- tryCatch(solve(information), error = function(e) NULL)
      gradient <- target_gradient(theta, x_use = x_use)
      values[m] <- if (is.null(covariance)) 1e100 else
        crossprod(gradient, covariance %*% gradient)
    }
    mean(log(pmax(values, 1e-12)))
  }
  starts <- list(rep(0, k + 1))
  if (!is.null(warm_start) && length(warm_start) == k &&
      all(warm_start > current_time) && all(warm_start < latest_time)) {
    starts[[2]] <- times_to_increments(warm_start, current_time, latest_time, minimum_gap)
  }
  if (additional_starts > 0) {
    if (!is.null(start_seed)) set.seed(start_seed)
    starts <- c(starts, lapply(seq_len(additional_starts), function(i) rnorm(k + 1, sd = 2)))
  }
  best <- NULL
  for (start in starts) {
    fit <- optim(start, objective, method = "L-BFGS-B",
                 lower = rep(-7, k + 1), upper = rep(7, k + 1),
                 control = list(maxit = maxit, factr = 1e7))
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  times <- increments_to_times(best$par, current_time, latest_time, minimum_gap)
  attr(times, "objective") <- best$value
  attr(times, "optimizer_convergence") <- best$convergence
  times
}

plan_group_rito <- function(posterior, current_time, end_time, remaining,
                            at_risk, stress, prior_sd, draws,
                            minimum_gap = 0.02 * end_time) {
  latest_time <- end_time - (remaining - 1) * minimum_gap
  transform_time <- function(z) {
    current_time + minimum_gap +
      (latest_time - current_time - minimum_gap) / (1 + exp(-z))
  }
  objective <- function(z) {
    next_time <- transform_time(z)
    information <- matrix(0, 3, 3)
    for (m in seq_len(nrow(draws))) {
      for (g in seq_along(stress)) {
        information <- information + conditional_information(
          next_time, current_time, at_risk[g], draws[m, ], stress[g])
      }
    }
    -sum(diag(information))
  }
  best <- optimize(objective, interval = c(-7, 7), tol = 1e-5)
  transform_time(best$minimum)
}

simulate_group_policy <- function(policy, failure_times, stress, prior_mean, prior_sd,
                                  pilot_time = 200, end_time = 1000, inspections = 5,
                                  planning_samples = 16, seed = 1) {
  times <- pilot_time
  fixed_times <- seq(pilot_time, end_time, length.out = inspections)
  counts <- group_observed_counts(failure_times, times)
  posterior <- fit_group_posterior(times, counts, stress, prior_mean, prior_sd)
  at_risk <- vapply(failure_times, function(x) sum(x > pilot_time), numeric(1))
  warm_start <- NULL
  elapsed <- 0
  if (policy == "Fixed") {
    times <- fixed_times
  } else if (policy == "Static") {
    draws <- planning_draws(posterior, planning_samples, seed)
    future <- plan_group_schedule(posterior, pilot_time, end_time, inspections - 1,
                                  at_risk, stress, prior_mean, prior_sd,
                                  inspections - 1, draws, maxit = 120)
    times <- c(times, future)
  } else {
    for (stage in 2:inspections) {
      remaining <- inspections - stage + 1
      horizon <- if (policy == "Myopic") 1 else if (policy == "Two-step") 2 else 5
      draws <- planning_draws(posterior, planning_samples, seed + 100 * stage)
      start_time <- proc.time()[3]
      if (policy == "RITO") {
        future <- plan_group_rito(posterior, times[length(times)], end_time, remaining,
                                  at_risk, stress, prior_sd, draws)
      } else {
        future <- plan_group_schedule(posterior, times[length(times)], end_time, remaining,
                                      at_risk, stress, prior_mean, prior_sd,
                                      horizon, draws, warm_start)
      }
      elapsed <- elapsed + proc.time()[3] - start_time
      times <- c(times, future[1])
      counts <- group_observed_counts(failure_times, times)
      posterior <- fit_group_posterior(times, counts, stress, prior_mean,
                                       prior_sd, posterior$mean)
      at_risk <- vapply(failure_times, function(x) sum(x > times[length(times)]), numeric(1))
      warm_start <- if (length(future) > 1) future[-1] else NULL
    }
  }
  counts <- group_observed_counts(failure_times, times)
  posterior <- fit_group_posterior(times, counts, stress, prior_mean,
                                   prior_sd, posterior$mean)
  estimate <- target_value(posterior$mean, x_use = -1.5)
  gradient <- target_gradient(posterior$mean, x_use = -1.5)
  target_se <- sqrt(as.numeric(crossprod(gradient, posterior$covariance %*% gradient)))
  list(policy = policy, times = times, counts = counts, estimate = estimate,
       se = target_se, elapsed = elapsed, convergence = posterior$convergence)
}

run_group_replicate <- function(replicate, scenario,
                                policies = c("Fixed", "Static", "Myopic", "Two-step", "Full"),
                                planning_samples = 16, inspections = 5) {
  set.seed(scenario$seed + replicate)
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  truth <- target_value(theta, x_use = -1.5)
  output <- vector("list", length(policies))
  for (j in seq_along(policies)) {
    result <- simulate_group_policy(policies[j], failure_times, scenario$stress,
                                    prior_mean, scenario$prior_sd,
                                    inspections = inspections,
                                    planning_samples = planning_samples,
                                    seed = scenario$seed + 10000 * replicate)
    output[[j]] <- data.frame(scenario = scenario$name, replicate = replicate,
                              policy = policies[j], estimate = result$estimate,
                              truth = truth, error = result$estimate - truth,
                              se = result$se, elapsed = result$elapsed,
                              convergence = result$convergence,
                              schedule = paste(sprintf("%.3f", result$times), collapse = ";"))
  }
  do.call(rbind, output)
}
