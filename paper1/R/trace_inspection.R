plan_trace_schedule <- function(current_time, end_time, remaining, at_risk, stress,
                                horizon, draws, warm_start = NULL, maxit = 100,
                                minimum_gap = 0.02 * end_time, weight_matrix = NULL,
                                additional_starts = 0, start_seed = NULL) {
  k <- min(remaining, horizon)
  latest_time <- end_time - (remaining - k) * minimum_gap
  objective <- function(z) {
    times <- increments_to_times(z, current_time, latest_time, minimum_gap)
    values <- vapply(seq_len(nrow(draws)), function(m) {
      information <- matrix(0, 3, 3)
      for (g in seq_along(stress)) {
        information <- information + conditional_information(
          times, current_time, at_risk[g], draws[m, ], stress[g])
      }
      if (is.null(weight_matrix)) sum(diag(information)) else
        sum(diag(weight_matrix %*% information))
    }, numeric(1))
    -mean(values)
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
  increments_to_times(best$par, current_time, latest_time, minimum_gap)
}
