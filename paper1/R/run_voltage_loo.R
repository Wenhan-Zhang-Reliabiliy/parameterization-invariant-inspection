worker_count <- as.integer(Sys.getenv("PAPER1_WORKERS", "4"))
stopifnot(worker_count >= 1, worker_count <= 8)
dir.create("work/exploratory/voltage_case", recursive = TRUE, showWarnings = FALSE)
tasks <- expand.grid(policy = c("Trace-full", "Weighted-full", "Full"), fold = 1:8,
                     stringsAsFactors = FALSE)

run_loo_task <- function(task) {
  source("paper1/R/voltage_empirical_model.R")
  result <- run_voltage_policy(task$policy, stress_scale = 1, extra_starts = 30,
    failure_times = voltage_adaptive[-task$fold], holdout_time = voltage_adaptive[task$fold])
  result$fold <- task$fold
  result
}

cluster <- parallel::makeCluster(worker_count)
on.exit(parallel::stopCluster(cluster), add = TRUE)
results <- parallel::parLapply(cluster, split(tasks, seq_len(nrow(tasks))), run_loo_task)
results <- do.call(rbind, results)
write.csv(results, "work/exploratory/voltage_case/voltage_loo_results.csv", row.names = FALSE)
summary <- do.call(rbind, lapply(c("Trace-full", "Weighted-full", "Full"), function(policy) {
  x <- results[results$policy == policy, ]
  data.frame(policy = policy, folds = nrow(x), mean_log_score = mean(x$log_score),
             median_log_score = median(x$log_score), sd_log_score = sd(x$log_score),
             mean_reliability = mean(x$reliability),
             mean_seconds = mean(x$elapsed), all_converged = all(x$convergence == 0))
}))
write.csv(summary, "work/exploratory/voltage_case/voltage_loo_summary.csv", row.names = FALSE)
print(summary, row.names = FALSE)
