worker_count <- as.integer(Sys.getenv("PAPER1_WORKERS", "4"))
stopifnot(worker_count >= 1, worker_count <= 8)
dir.create("work/exploratory/voltage_case", recursive = TRUE, showWarnings = FALSE)
ratio_scale <- log(120 / 80)
tasks <- expand.grid(policy = c("Trace-full", "Weighted-full", "Full"),
                     coding = c("normalized", "log_ratio"), stringsAsFactors = FALSE)

run_voltage_task <- function(task) {
  scale <- if (task$coding == "normalized") 1 else ratio_scale
  source("paper1/R/voltage_empirical_model.R")
  result <- run_voltage_policy(task$policy, scale, extra_starts = 30)
  result$coding <- task$coding
  output <- paste0("work/exploratory/voltage_case/raw_", task$policy, "_", task$coding, ".csv")
  write.csv(result, output, row.names = FALSE)
  result
}

cluster <- parallel::makeCluster(worker_count)
parallel::clusterExport(cluster, "ratio_scale")
on.exit(parallel::stopCluster(cluster), add = TRUE)
results <- parallel::parLapply(cluster, split(tasks, seq_len(nrow(tasks))), run_voltage_task)
results <- do.call(rbind, results)
write.csv(results, "work/exploratory/voltage_case/voltage_empirical_results.csv",
          row.names = FALSE)

schedule_vector <- function(x) as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])
summary <- do.call(rbind, lapply(c("Trace-full", "Weighted-full", "Full"), function(policy) {
  x <- results[results$policy == policy & results$coding == "normalized", ]
  y <- results[results$policy == policy & results$coding == "log_ratio", ]
  stopifnot(nrow(x) == 1, nrow(y) == 1)
  data.frame(policy = policy,
             max_schedule_difference = max(abs(schedule_vector(x$schedule) -
                                               schedule_vector(y$schedule))),
             absolute_estimate_difference = abs(x$estimate - y$estimate),
             counts_identical = identical(x$counts, y$counts),
             normalized_seconds = x$elapsed, log_ratio_seconds = y$elapsed)
}))
write.csv(summary, "work/exploratory/voltage_case/voltage_empirical_summary.csv",
          row.names = FALSE)
print(summary, row.names = FALSE)
