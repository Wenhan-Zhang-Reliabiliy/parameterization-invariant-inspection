worker_count <- as.integer(Sys.getenv("PAPER1_WORKERS", "4"))
stopifnot(worker_count >= 1, worker_count <= 8)
dir.create("work/exploratory/logs", recursive = TRUE, showWarnings = FALSE)
tasks <- expand.grid(scenario = 1:2, stress_scale = c(1, 2, 10))
tasks <- tasks[order(tasks$scenario, tasks$stress_scale), ]

run_weighted_task <- function(task) {
  index <- task$scenario
  scale <- task$stress_scale
  output <- paste0("paper1/output/weighted_trace_scale_", scale,
                   "_scenario_", index, ".csv")
  log_file <- paste0("work/exploratory/logs/weighted_trace_", scale, "_", index, ".log")
  capture.output({
    Sys.setenv(PAPER1_SCENARIO = index, PAPER1_STRESS_SCALE = scale,
               PAPER1_WEIGHTED_ONLY = 1, PAPER1_REPLICATES = 1000,
               PAPER1_PARAMETERIZATION_OUTPUT = output, PAPER1_RESUME = 0)
    source("paper1/R/run_parameterization_check.R")
  }, file = log_file)
  x <- read.csv(output)
  data.frame(scenario = index, stress_scale = scale, rows = nrow(x),
             replicates = length(unique(x$replicate)),
             finite = all(is.finite(x$error)), output = output)
}

cluster <- parallel::makeCluster(worker_count)
on.exit(parallel::stopCluster(cluster), add = TRUE)
results <- parallel::parLapply(cluster, split(tasks, seq_len(nrow(tasks))), run_weighted_task)
results <- do.call(rbind, results)
stopifnot(nrow(results) == 6, all(results$rows == 1000),
          all(results$replicates == 1000), all(results$finite))
write.csv(results, "paper1/output/weighted_trace_run_manifest.csv", row.names = FALSE)
print(results, row.names = FALSE)
