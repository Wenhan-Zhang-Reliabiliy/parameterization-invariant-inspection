source("paper1/R/grid_sequential_inspection.R")

outcome_nodes <- function(n, maximum = 5) {
  unique(round(seq(0, n, length.out = min(n + 1, maximum))))
}

outcome_probabilities <- function(n, probabilities, nodes) {
  probabilities[!is.finite(probabilities)] <- 0
  if (length(nodes) == 1) return(matrix(1, nrow = length(probabilities), ncol = 1))
  boundaries <- c(-0.5, head(nodes, -1) + diff(nodes) / 2, n + 0.5)
  output <- matrix(0, nrow = length(probabilities), ncol = length(nodes))
  for (m in seq_along(probabilities)) {
    cumulative <- pbinom(boundaries, size = n, prob = probabilities[m])
    output[m, ] <- pmax(diff(cumulative), 1e-12)
  }
  output[!is.finite(output)] <- 1e-12
  output
}

posterior_target_variance <- function(theta, information) {
  covariance <- tryCatch(solve(information), error = function(e) NULL)
  if (is.null(covariance)) return(1e100)
  gradient <- target_gradient(theta)
  as.numeric(crossprod(gradient, covariance %*% gradient))
}

lookahead_schedule <- function(posterior, current_time, end_time, remaining, at_risk,
                               prior_sd, draws, candidates = 7, quadrature = 5,
                               minimum_gap = 0.02 * end_time) {
  prior_precision_matrix <- prior_precision(prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  first_latest <- end_time - (remaining - 1) * minimum_gap
  first_grid <- seq(current_time + minimum_gap, first_latest,
                    length.out = candidates)
  first_nodes <- outcome_nodes(at_risk, quadrature)
  first_values <- numeric(length(first_grid))
  for (i in seq_along(first_grid)) {
    first_time <- first_grid[i]
    current_cdf <- vapply(seq_len(nrow(draws)), function(m) {
      weibull_cdf(current_time, draws[m, ])
    }, numeric(1))
    first_cdf <- vapply(seq_len(nrow(draws)), function(m) {
      weibull_cdf(first_time, draws[m, ])
    }, numeric(1))
    first_probabilities <- (first_cdf - current_cdf) / (1 - current_cdf)
    first_probabilities <- pmin(1, pmax(0, first_probabilities))
    first_outcomes <- outcome_probabilities(at_risk, first_probabilities, first_nodes)
    conditional_values <- numeric(length(first_nodes))
    for (j in seq_along(first_nodes)) {
      first_weights <- first_outcomes[, j]
      first_predictive <- mean(first_weights)
      if (first_predictive < 1e-10) {
        conditional_values[j] <- NA_real_
        next
      }
      first_weights <- first_weights / sum(first_weights)
      if (remaining == 1) {
        values <- vapply(seq_len(nrow(draws)), function(m) {
          information <- observed_information + prior_precision_matrix +
            conditional_information(first_time, current_time, at_risk, draws[m, ])
          posterior_target_variance(draws[m, ], information)
        }, numeric(1))
        conditional_values[j] <- sum(first_weights * log(pmax(values, 1e-12)))
        next
      }
      second_latest <- end_time - (remaining - 2) * minimum_gap
      second_grid <- seq(first_time + minimum_gap, second_latest,
                         length.out = candidates)
      second_values <- numeric(length(second_grid))
      second_at_risk <- at_risk - first_nodes[j]
      second_nodes <- outcome_nodes(second_at_risk, quadrature)
      for (k in seq_along(second_grid)) {
        second_time <- second_grid[k]
        second_cdf <- vapply(seq_len(nrow(draws)), function(m) {
          weibull_cdf(second_time, draws[m, ])
        }, numeric(1))
        second_probabilities <- (second_cdf - first_cdf) / (1 - first_cdf)
        second_probabilities <- pmin(1, pmax(0, second_probabilities))
        second_outcomes <- outcome_probabilities(second_at_risk, second_probabilities,
                                                second_nodes)
        outcome_values <- numeric(length(second_nodes))
        outcome_weights <- numeric(length(second_nodes))
        for (ell in seq_along(second_nodes)) {
          second_weights <- first_weights * second_outcomes[, ell]
          outcome_weights[ell] <- sum(second_weights)
          if (outcome_weights[ell] < 1e-12) {
            outcome_values[ell] <- 0
            next
          }
          second_weights <- second_weights / outcome_weights[ell]
          terminal_at_risk <- second_at_risk - second_nodes[ell]
          terminal_times <- if (remaining > 2)
            seq(second_time, end_time, length.out = remaining - 1)[-1] else numeric(0)
          values <- vapply(seq_len(nrow(draws)), function(m) {
            information <- observed_information + prior_precision_matrix +
              conditional_information(first_time, current_time, at_risk, draws[m, ]) +
              conditional_information(second_time, first_time, second_at_risk, draws[m, ])
            if (length(terminal_times)) {
              information <- information + conditional_information(
                terminal_times, second_time, terminal_at_risk, draws[m, ])
            }
            posterior_target_variance(draws[m, ], information)
          }, numeric(1))
          outcome_values[ell] <- sum(second_weights * log(pmax(values, 1e-12)))
        }
        second_values[k] <- sum(outcome_weights * outcome_values)
      }
      conditional_values[j] <- min(second_values)
    }
    predictive <- colMeans(first_outcomes)
    first_values[i] <- sum(predictive * conditional_values, na.rm = TRUE)
  }
  first_grid[which.min(first_values)]
}

simulate_lookahead_policy <- function(failure_times, prior_mean, prior_sd,
                                      pilot_time = 200, end_time = 1000, inspections = 5,
                                      planning_samples = 12, seed = 1) {
  times <- pilot_time
  counts <- observed_counts(failure_times, times)
  posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd)
  elapsed <- 0
  for (stage in 2:inspections) {
    remaining <- inspections - stage + 1
    draws <- sample_grid(posterior, planning_samples, seed + 100 * stage)
    start_time <- proc.time()[3]
    next_time <- lookahead_schedule(posterior, times[length(times)], end_time,
                                    remaining, sum(failure_times > times[length(times)]),
                                    prior_sd, draws)
    elapsed <- elapsed + proc.time()[3] - start_time
    times <- c(times, next_time)
    counts <- observed_counts(failure_times, times)
    posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd, posterior$mean)
  }
  counts <- observed_counts(failure_times, times)
  posterior <- fit_grid_posterior(times, counts, prior_mean, prior_sd, posterior$mean)
  target_draws <- apply(sample_grid(posterior, 5000, seed + 991), 1, target_value)
  estimate <- mean(target_draws)
  target_se <- sd(target_draws)
  list(times = times, counts = counts, estimate = estimate, se = target_se,
       elapsed = elapsed, convergence = posterior$convergence)
}

run_lookahead_replicate <- function(replicate, scenario, planning_samples = 12) {
  set.seed(scenario$seed + replicate)
  failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                            scale = exp(scenario$alpha))
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  result <- simulate_lookahead_policy(failure_times, prior_mean, scenario$prior_sd,
                                      planning_samples = planning_samples,
                                      seed = scenario$seed + 10000 * replicate)
  theta <- c(scenario$alpha, scenario$beta, log(scenario$sigma))
  truth <- target_value(theta)
  data.frame(scenario = scenario$name, replicate = replicate, policy = "Lookahead",
             estimate = result$estimate, truth = truth,
             error = result$estimate - truth, se = result$se,
             elapsed = result$elapsed, convergence = result$convergence,
             schedule = paste(sprintf("%.3f", result$times), collapse = ";"))
}
