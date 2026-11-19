source("paper1/R/model_version.R")

results <- read.csv("paper1/output/parameterization_results.csv")
results <- results[results$stress_scale == 1, ]
weighted <- do.call(rbind, lapply(1:2, function(index) {
  x <- read.csv(paste0("paper1/output/weighted_trace_scale_1_scenario_", index, ".csv"))
  x$stress_scale <- 1
  x
}))
weighted$policy <- "Weighted-full"
shared <- intersect(names(results), names(weighted))
results <- rbind(results[, shared], weighted[, shared])
output <- do.call(rbind, lapply(split(results, list(results$scenario, results$policy), drop = TRUE), function(x) {
  final_time <- as.numeric(sub(".*;", "", x$schedule))
  data.frame(scenario = x$scenario[1], policy = x$policy[1], replicates = nrow(x),
             median = median(final_time), q1 = quantile(final_time, 0.25),
             q3 = quantile(final_time, 0.75), iqr = IQR(final_time),
             proportion_at_tmax = mean(abs(final_time - 1000) < 1e-6))
}))
stopifnot(nrow(output) == 6, all(output$median == 1000), all(output$iqr == 0),
          all(output$proportion_at_tmax == 1))
write.csv(with_model_version(output), "paper1/output/actual_duration_summary.csv", row.names = FALSE)
print(output, row.names = FALSE)
