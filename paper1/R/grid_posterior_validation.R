source("paper1/R/posterior_grid.R")

log_sum_exp <- function(x) {
  maximum <- max(x)
  maximum + log(sum(exp(x - maximum)))
}

grid_posterior <- function(times, counts, prior_mean, prior_sd, fit, grid_size = 161,
                           range_scale = 4) {
  centers <- fit$mean[c(1, 3)]
  scales <- sqrt(diag(fit$covariance)[c(1, 3)])
  ranges <- lapply(1:2, function(j) {
    seq(centers[j] - range_scale * scales[j], centers[j] + range_scale * scales[j],
        length.out = grid_size)
  })
  log_weights <- matrix(NA_real_, nrow = grid_size, ncol = grid_size)
  for (i in seq_len(grid_size)) for (j in seq_len(grid_size)) {
    theta <- c(ranges[[1]][i], prior_mean[2], ranges[[2]][j])
    log_weights[i, j] <- log_posterior(theta, times, counts, prior_mean, prior_sd)
  }
  log_weights <- log_weights - log_sum_exp(log_weights)
  weights <- exp(log_weights)
  alpha_values <- ranges[[1]]
  sigma_values <- ranges[[2]]
  mean_alpha <- sum(weights * alpha_values)
  mean_logsigma <- sum(weights * rep(sigma_values, each = grid_size))
  second_alpha <- sum(weights * alpha_values^2)
  second_logsigma <- sum(weights * rep(sigma_values^2, each = grid_size))
  covariance <- matrix(0, 3, 3)
  covariance[1, 1] <- second_alpha - mean_alpha^2
  covariance[3, 3] <- second_logsigma - mean_logsigma^2
  covariance[2, 2] <- prior_sd[2]^2
  covariance[1, 3] <- covariance[3, 1] <- sum(weights * alpha_values * rep(sigma_values, each = grid_size)) -
    mean_alpha * mean_logsigma
  list(mean = c(mean_alpha, prior_mean[2], mean_logsigma), covariance = covariance,
       hessian = fit$hessian, weights = weights,
       alpha_values = alpha_values, sigma_values = sigma_values)
}

sample_grid <- function(grid, number, seed) {
  set.seed(seed)
  indices <- sample.int(length(grid$weights), number, replace = TRUE,
                        prob = as.numeric(grid$weights))
  rows <- row(grid$weights)[indices]
  cols <- col(grid$weights)[indices]
  cbind(grid$alpha_values[rows], grid$mean[2], grid$sigma_values[cols])
}

validate_case <- function(scenario, replicate, grid_size = 161, range_scale = 4) {
  set.seed(scenario$seed + replicate)
  failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                            scale = exp(scenario$alpha))
  times <- 200
  counts <- observed_counts(failure_times, times)
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  prior_sd <- c(0.8, 0.8, 0.5)
  fit <- fit_posterior(times, counts, prior_mean, prior_sd)
  grid <- grid_posterior(times, counts, prior_mean, prior_sd, fit, grid_size, range_scale)
  set.seed(9981)
  laplace_draws <- planning_draws(fit, 20000, 9981)
  grid_draws <- sample_grid(grid, 20000, 9981)
  laplace_targets <- apply(laplace_draws, 1, target_value)
  grid_targets <- apply(grid_draws, 1, target_value)
  laplace_plan_draws <- planning_draws(fit, 256, 7717)
  grid_plan_draws <- sample_grid(grid, 256, 7717)
  laplace_plan <- plan_schedule(fit, 200, 1000, 4,
                                sum(failure_times > 200), prior_mean, prior_sd,
                                4, laplace_plan_draws)
  grid_plan <- plan_schedule(grid, 200, 1000, 4,
                             sum(failure_times > 200), prior_mean, prior_sd,
                             4, grid_plan_draws)
  data.frame(scenario = scenario$name, replicate = replicate,
             alpha_mean_laplace = fit$mean[1], alpha_mean_grid = grid$mean[1],
             logsigma_mean_laplace = fit$mean[3], logsigma_mean_grid = grid$mean[3],
             target_mean_laplace = mean(laplace_targets), target_mean_grid = mean(grid_targets),
             target_var_laplace = var(laplace_targets), target_var_grid = var(grid_targets),
             max_time_difference = max(abs(laplace_plan - grid_plan)),
             laplace_plan = paste(round(laplace_plan, 2), collapse = ";"),
             grid_plan = paste(round(grid_plan, 2), collapse = ";"))
}

scenarios <- readRDS("paper1/output/scenarios.rds")
indices <- strsplit(Sys.getenv("PAPER1_GRID_INDICES", "1,3,6"), ",")[[1]]
indices <- as.integer(indices)
results <- do.call(rbind, lapply(indices, function(index) validate_case(scenarios[[index]], 1)))
write.csv(with_model_version(results), "paper1/output/grid_posterior_validation.csv", row.names = FALSE)
print(results, row.names = FALSE)
