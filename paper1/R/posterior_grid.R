source("paper1/R/sequential_inspection.R")

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
