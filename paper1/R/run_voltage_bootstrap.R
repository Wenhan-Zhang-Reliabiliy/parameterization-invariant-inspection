source("paper1/R/voltage_empirical_model.R")

worker_count <- as.integer(Sys.getenv("PAPER1_WORKERS", "4"))
replicates <- as.integer(Sys.getenv("PAPER1_BOOTSTRAP_REPLICATES", "200"))
extra_starts <- as.integer(Sys.getenv("PAPER1_VOLTAGE_EXTRA_STARTS", "5"))
stopifnot(worker_count >= 1, worker_count <= 8, replicates >= 1, extra_starts >= 0)
dir.create("work/exploratory/voltage_case", recursive = TRUE, showWarnings = FALSE)

full_data <- voltage_data
full_data$x <- (log(full_data$voltage) - log(120)) / log(120 / 80)
attr(full_data, "test_stress") <- 0
anchor_fit <- fit_voltage_grid(500, c(0, 0), full_data, voltage_reference_mean,
  c(100, 100, 100), log_posterior_fun = voltage_historical_posterior)
anchor_theta <- as.numeric(anchor_fit$mean)
anchor_target <- target_value(anchor_theta, t0 = 1000, x_use = 0)
ratio_scale <- log(120 / 80)
tasks <- expand.grid(replicate = seq_len(replicates),
                     policy = c("Trace-full", "Weighted-full", "Full"),
                     coding = c("normalized", "log_ratio"), stringsAsFactors = FALSE)

run_bootstrap_task <- function(task) {
  source("paper1/R/voltage_empirical_model.R")
  scale <- if (task$coding == "normalized") 1 else ratio_scale
  set.seed(88000 + task$replicate)
  failure_times <- rweibull(8, shape = 1 / exp(anchor_theta[3]),
                            scale = exp(anchor_theta[1]))
  result <- run_voltage_policy(task$policy, scale, extra_starts = extra_starts,
                               failure_times = failure_times)
  result$replicate <- task$replicate
  result$coding <- task$coding
  result$failure_time <- min(failure_times)
  result$truth <- anchor_target
  times <- as.numeric(strsplit(result$schedule, ";", fixed = TRUE)[[1]])
  result$first_detection <- if (result$failure_time <= max(times)) {
    times[which(times >= result$failure_time)[1]]
  } else NA_real_
  result
}

cluster <- parallel::makeCluster(worker_count)
parallel::clusterExport(cluster, c("ratio_scale", "anchor_theta", "anchor_target",
                                   "extra_starts"))
on.exit(parallel::stopCluster(cluster), add = TRUE)
results <- parallel::parLapply(cluster, split(tasks, seq_len(nrow(tasks))), run_bootstrap_task)
results <- do.call(rbind, results)
write.csv(results, "work/exploratory/voltage_case/voltage_bootstrap_results.csv",
          row.names = FALSE)

schedule_vector <- function(x) as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])
summary <- do.call(rbind, lapply(c("Trace-full", "Weighted-full", "Full"), function(policy) {
  x <- results[results$policy == policy & results$coding == "normalized", ]
  y <- results[results$policy == policy & results$coding == "log_ratio", ]
  x <- x[order(x$replicate), ]
  y <- y[order(y$replicate), ]
  stopifnot(identical(x$replicate, y$replicate),
            identical(x$failure_time, y$failure_time))
  schedule_difference <- vapply(seq_len(replicates), function(i) {
    max(abs(schedule_vector(x$schedule[i]) - schedule_vector(y$schedule[i])))
  }, numeric(1))
  detection_difference <- abs(x$first_detection - y$first_detection)
  data.frame(policy = policy, replicates = replicates,
    rmse_normalized = sqrt(mean((x$estimate - anchor_target)^2)),
    rmse_log_ratio = sqrt(mean((y$estimate - anchor_target)^2)),
    coverage_normalized = mean(x$lower <= anchor_target & anchor_target <= x$upper),
    coverage_log_ratio = mean(y$lower <= anchor_target & anchor_target <= y$upper),
    median_schedule_difference = median(schedule_difference),
    p90_schedule_difference = quantile(schedule_difference, 0.9),
    maximum_schedule_difference = max(schedule_difference),
    median_detection_difference = median(detection_difference, na.rm = TRUE),
    percent_detection_over_1h = 100 * mean(detection_difference > 1, na.rm = TRUE),
    undetected_pairs = sum(is.na(detection_difference)),
    convergence_failures = sum(x$convergence != 0) + sum(y$convergence != 0),
    mean_seconds = mean(c(x$elapsed, y$elapsed)))
}))
write.csv(summary, "work/exploratory/voltage_case/voltage_bootstrap_summary.csv",
          row.names = FALSE)
print(summary, row.names = FALSE)
