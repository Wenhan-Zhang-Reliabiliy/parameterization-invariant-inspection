source("paper1/R/posterior_grid.R")
source("paper1/R/group_inspection.R")

grid3d_posterior <- function(times, counts, stress, prior_mean, prior_sd, fit,
                             grid_size = 21, range_scale = 4,
                             log_posterior_fun = group_log_posterior) {
  centers <- fit$mean
  scales <- sqrt(diag(fit$covariance))
  ranges <- lapply(1:3, function(j) {
    seq(centers[j] - range_scale * scales[j], centers[j] + range_scale * scales[j],
        length.out = grid_size)
  })
  weights <- array(NA_real_, dim = rep(grid_size, 3))
  for (i in seq_len(grid_size)) for (j in seq_len(grid_size)) for (k in seq_len(grid_size)) {
    theta <- c(ranges[[1]][i], ranges[[2]][j], ranges[[3]][k])
    weights[i, j, k] <- log_posterior_fun(theta, times, counts, stress,
                                            prior_mean, prior_sd)
  }
  log_weights <- weights - log_sum_exp(weights)
  weights <- exp(log_weights)
  points <- expand.grid(ranges[[1]], ranges[[2]], ranges[[3]])
  values <- lapply(points, identity)
  means <- vapply(values, function(x) sum(weights * x), numeric(1))
  centered <- lapply(1:3, function(j) values[[j]] - means[j])
  covariance <- matrix(0, 3, 3)
  for (i in 1:3) for (j in 1:3) {
    covariance[i, j] <- sum(weights * centered[[i]] * centered[[j]]) /
      (1 - sum(weights^2))
  }
  list(mean = means, covariance = covariance, hessian = solve(covariance),
       weights = weights, ranges = ranges, values = values, convergence = fit$convergence)
}

sample_grid3d <- function(grid, number, seed) {
  set.seed(seed)
  indices <- sample.int(length(grid$weights), number, replace = TRUE,
                        prob = as.numeric(grid$weights))
  cbind(grid$values[[1]][indices], grid$values[[2]][indices], grid$values[[3]][indices])
}
