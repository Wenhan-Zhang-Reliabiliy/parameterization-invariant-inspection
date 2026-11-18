source("paper1/R/grid3d_sequential_inspection.R")

scale_scenario <- function(scenario, stress_scale) {
  scenario$stress <- scenario$stress * stress_scale
  scenario$beta <- scenario$beta / stress_scale
  scenario$prior_beta <- scenario$prior_beta / stress_scale
  scenario$prior_sd[2] <- scenario$prior_sd[2] / stress_scale
  scenario$x_use <- -1.5 * stress_scale
  scenario
}

evaluate_parameterization <- function(scenario) {
  set.seed(scenario$seed + 1)
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  counts <- group_observed_counts(failure_times, 200)
  fit <- fit_group_posterior(200, counts, scenario$stress, prior_mean, scenario$prior_sd)
  posterior <- grid3d_posterior(200, counts, scenario$stress, prior_mean, scenario$prior_sd,
                                fit, grid_size = 21)
  draws <- sample_grid3d(posterior, 64, 8181)
  prior_precision_matrix <- prior_precision(scenario$prior_sd)
  observed_information <- posterior$hessian - prior_precision_matrix
  at_risk <- vapply(failure_times, function(x) sum(x > 200), numeric(1))
  times <- c(430, 620, 810, 1000)
  reliability_utility <- trace_utility <- weighted_trace_utility <- numeric(nrow(draws))
  for (m in seq_len(nrow(draws))) {
    information <- observed_information + prior_precision_matrix
    future_information <- matrix(0, 3, 3)
    for (g in seq_along(scenario$stress)) {
      future <- conditional_information(times, 200, at_risk[g], draws[m, ], scenario$stress[g])
      information <- information + future
      future_information <- future_information + future
    }
    gradient <- target_gradient(draws[m, ], x_use = scenario$x_use)
    reliability_utility[m] <- crossprod(gradient, solve(information) %*% gradient)
    trace_utility[m] <- sum(diag(future_information))
    weighted_trace_utility[m] <- sum(diag(posterior$covariance %*% future_information))
  }
  list(mode = fit$mean, draws = draws, reliability_utility = reliability_utility,
       trace_utility = trace_utility, weighted_trace_utility = weighted_trace_utility,
       mean_log_utility = mean(log(reliability_utility)),
       log_mean_utility = log(mean(reliability_utility)))
}

scenarios <- readRDS("paper1/output/revision_scenarios.rds")
output <- lapply(scenarios, function(scenario) {
  first <- evaluate_parameterization(scale_scenario(scenario, 1))
  second <- evaluate_parameterization(scale_scenario(scenario, 10))
  transformation <- diag(c(1, 0.1, 1))
  data.frame(scenario = scenario$name,
             mode_difference = max(abs(second$mode - as.numeric(transformation %*% first$mode))),
             draw_difference = max(abs(second$draws - t(transformation %*% t(first$draws)))),
             max_absolute_utility_difference = max(abs(first$reliability_utility -
               second$reliability_utility)),
             max_relative_utility_difference = max(abs(first$reliability_utility -
               second$reliability_utility) / pmax(abs(first$reliability_utility), 1e-12)),
             max_absolute_weighted_difference = max(abs(first$weighted_trace_utility -
               second$weighted_trace_utility)),
             max_relative_weighted_difference = max(abs(first$weighted_trace_utility -
               second$weighted_trace_utility) / pmax(abs(first$weighted_trace_utility), 1e-12)),
             trace_scale_1 = mean(first$trace_utility),
             trace_scale_10 = mean(second$trace_utility),
             mean_log_utility_scale_1 = first$mean_log_utility,
             mean_log_utility_scale_10 = second$mean_log_utility,
             log_mean_utility_scale_1 = first$log_mean_utility,
             log_mean_utility_scale_10 = second$log_mean_utility)
})
output <- do.call(rbind, output)
write.csv(with_model_version(output), "paper1/output/parameterization_identity.csv", row.names = FALSE)
print(output, row.names = FALSE)
