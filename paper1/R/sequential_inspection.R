source("paper1/R/model_version.R")

prior_precision <- function(prior_sd) diag(1 / prior_sd^2, 3)

weibull_cdf <- function(t, theta, x = 0) {
  sigma <- exp(theta[3])
  z <- (log(t) - theta[1] - theta[2] * x) / sigma
  -expm1(-exp(z))
}

cdf_derivative <- function(t, theta, x = 0) {
  sigma <- exp(theta[3])
  z <- (log(t) - theta[1] - theta[2] * x) / sigma
  density <- exp(z - exp(z))
  c(-density / sigma, -density * x / sigma, -density * z)
}

target_value <- function(theta, t0 = 1000, x_use = -1) {
  sigma <- exp(theta[3])
  (log(t0) - theta[1] - theta[2] * x_use) / sigma
}

target_gradient <- function(theta, t0 = 1000, x_use = -1) {
  sigma <- exp(theta[3])
  a <- log(t0) - theta[1] - theta[2] * x_use
  c(-1 / sigma, -x_use / sigma, -a / sigma)
}

cell_probabilities <- function(times, theta, x = 0) {
  f <- weibull_cdf(times, theta, x)
  p <- diff(c(0, f, 1))
  pmax(p, 1e-12)
}

log_posterior <- function(theta, times, counts, prior_mean, prior_sd, x = 0) {
  if (theta[3] < -3 || theta[3] > 2 || abs(theta[1]) > 20 || abs(theta[2]) > 10) {
    return(-Inf)
  }
  p <- cell_probabilities(times, theta, x)
  sum(counts * log(p)) - 0.5 * sum(((theta - prior_mean) / prior_sd)^2)
}

psd_matrix <- function(x, floor = 1e-8) {
  x <- (x + t(x)) / 2
  e <- eigen(x, symmetric = TRUE)
  e$values <- pmax(e$values, floor)
  e$vectors %*% (e$values * t(e$vectors))
}

fit_posterior <- function(times, counts, prior_mean, prior_sd, start = prior_mean) {
  objective <- function(theta) {
    value <- log_posterior(theta, times, counts, prior_mean, prior_sd)
    if (is.finite(value)) -value else 1e100
  }
  fit <- optim(start, objective, method = "BFGS", hessian = TRUE,
               control = list(reltol = 1e-9, maxit = 500))
  covariance <- tryCatch(solve(fit$hessian), error = function(e) diag(prior_sd^2, 3))
  list(mean = as.numeric(fit$par), covariance = psd_matrix(covariance),
       hessian = fit$hessian, convergence = fit$convergence)
}

observed_counts <- function(failure_times, inspections) {
  k <- length(inspections)
  breaks <- c(0, inspections, Inf)
  as.numeric(table(cut(failure_times, breaks = breaks, right = TRUE,
                       include.lowest = TRUE)))
}

conditional_information <- function(future_times, current_time, at_risk,
                                    theta, x = 0) {
  current_survival <- 1 - weibull_cdf(current_time, theta, x)
  if (current_survival < 1e-10 || at_risk <= 0) return(matrix(0, 3, 3))
  future_cdf <- weibull_cdf(future_times, theta, x)
  current_cdf <- weibull_cdf(current_time, theta, x)
  probabilities <- diff(c(current_cdf, future_cdf, 1)) / current_survival
  derivatives <- vapply(future_times, cdf_derivative, numeric(3), theta = theta, x = x)
  current_derivative <- cdf_derivative(current_time, theta, x)
  derivatives <- cbind(current_derivative, derivatives, rep(0, 3))
  cell_derivatives <- derivatives[, -1, drop = FALSE] -
    derivatives[, -ncol(derivatives), drop = FALSE]
  cell_derivatives[, ncol(cell_derivatives)] <- -derivatives[, ncol(derivatives) - 1]
  information <- matrix(0, 3, 3)
  for (j in seq_along(probabilities)) {
    if (probabilities[j] > 1e-10) {
      gradient <- cell_derivatives[, j] / current_survival
      information <- information + tcrossprod(gradient) / probabilities[j]
    }
  }
  at_risk * information
}

increments_to_times <- function(z, current_time, latest_time, minimum_gap) {
  k <- length(z) - 1
  weights <- exp(z - max(z))
  weights <- weights / sum(weights)
  free_time <- latest_time - current_time - k * minimum_gap
  current_time + cumsum(minimum_gap + free_time * weights[-length(weights)])
}

times_to_increments <- function(times, current_time, latest_time, minimum_gap) {
  k <- length(times)
  free_time <- latest_time - current_time - k * minimum_gap
  intervals <- diff(c(current_time, times, latest_time)) - minimum_gap
  log(pmax(intervals / free_time, 1e-8))
}

planning_draws <- function(posterior, number, seed) {
  set.seed(seed)
  root <- chol(posterior$covariance)
  standard <- matrix(rnorm(number * 3), nrow = number, ncol = 3)
  sweep(standard %*% root, 2, posterior$mean, "+")
}

plan_schedule <- function(posterior, current_time, end_time, remaining, at_risk,
                          prior_mean, prior_sd, horizon, draws, warm_start = NULL,
                          maxit = 100, minimum_gap = 0.02 * end_time) {
  k <- min(remaining, horizon)
  latest_time <- end_time - (remaining - k) * minimum_gap
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  objective <- function(z) {
    times <- increments_to_times(z, current_time, latest_time, minimum_gap)
    values <- numeric(nrow(draws))
    for (m in seq_len(nrow(draws))) {
      theta <- draws[m, ]
      information <- observed_information +
        conditional_information(times, current_time, at_risk, theta) +
        prior_precision_matrix
      covariance <- tryCatch(solve(information), error = function(e) NULL)
      values[m] <- if (is.null(covariance)) 1e100 else
        crossprod(target_gradient(theta), covariance %*% target_gradient(theta))
    }
    mean(log(pmax(values, 1e-12)))
  }
  starts <- list(rep(0, k + 1))
  if (!is.null(warm_start) && length(warm_start) == k &&
      all(warm_start > current_time) && all(warm_start < latest_time)) {
    starts[[2]] <- times_to_increments(warm_start, current_time, latest_time, minimum_gap)
  }
  best <- NULL
  for (start in starts) {
    fit <- optim(start, objective, method = "L-BFGS-B",
                 lower = rep(-7, k + 1), upper = rep(7, k + 1),
                 control = list(maxit = maxit, factr = 1e7))
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  increments_to_times(best$par, current_time, latest_time, minimum_gap)
}

simulate_policy <- function(policy, failure_times, prior_mean, prior_sd,
                            pilot_time = 200, end_time = 1000, inspections = 5,
                            horizon = 5, planning_samples = 16, seed = 1) {
  n <- length(failure_times)
  fixed_times <- seq(pilot_time, end_time, length.out = inspections)
  times <- pilot_time
  counts <- observed_counts(failure_times, times)
  posterior <- fit_posterior(times, counts, prior_mean, prior_sd)
  elapsed <- 0
  warm_start <- NULL
  if (policy == "Fixed") {
    times <- fixed_times
  } else {
    if (policy == "Static") {
      draws <- planning_draws(posterior, planning_samples, seed)
      start_time <- proc.time()[3]
      future <- plan_schedule(posterior, pilot_time, end_time, inspections - 1,
                              sum(failure_times > pilot_time), prior_mean, prior_sd,
                              inspections - 1, draws, maxit = 120)
      elapsed <- elapsed + proc.time()[3] - start_time
      times <- c(times, future)
    } else {
      for (stage in 2:inspections) {
        remaining <- inspections - stage + 1
        current_horizon <- if (policy == "Myopic") 1 else if (policy == "Two-step") 2 else horizon
        draws <- planning_draws(posterior, planning_samples, seed + 100 * stage)
        start_time <- proc.time()[3]
        future <- plan_schedule(posterior, times[length(times)], end_time, remaining,
                                sum(failure_times > times[length(times)]), prior_mean,
                                prior_sd, current_horizon, draws, warm_start)
        elapsed <- elapsed + proc.time()[3] - start_time
        next_time <- future[1]
        times <- c(times, next_time)
        counts <- observed_counts(failure_times, times)
        posterior <- fit_posterior(times, counts, prior_mean, prior_sd, posterior$mean)
        warm_start <- if (length(future) > 1) future[-1] else NULL
      }
    }
  }
  counts <- observed_counts(failure_times, times)
  posterior <- fit_posterior(times, counts, prior_mean, prior_sd, posterior$mean)
  estimate <- target_value(posterior$mean)
  gradient <- target_gradient(posterior$mean)
  target_se <- sqrt(as.numeric(crossprod(gradient, posterior$covariance %*% gradient)))
  list(policy = policy, times = times, counts = counts, estimate = estimate,
       se = target_se, lower = estimate - qnorm(0.95) * target_se,
       upper = estimate + qnorm(0.95) * target_se,
       elapsed = elapsed, convergence = posterior$convergence)
}

run_replicate <- function(replicate, scenario, policies = c("Fixed", "Static", "Myopic", "Two-step", "Full"),
                          planning_samples = 16, inspections = 5) {
  set.seed(scenario$seed + replicate)
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                            scale = exp(scenario$alpha))
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  prior_sd <- if (is.null(scenario$prior_sd)) c(0.55, 0.55, 0.35) else scenario$prior_sd
  truth <- target_value(theta)
  output <- vector("list", length(policies))
  for (j in seq_along(policies)) {
    result <- simulate_policy(policies[j], failure_times, prior_mean, prior_sd,
                              inspections = inspections, planning_samples = planning_samples,
                              seed = scenario$seed + 10000 * replicate)
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
  do.call(rbind, output)
}
