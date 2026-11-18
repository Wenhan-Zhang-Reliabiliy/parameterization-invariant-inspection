source("paper1/R/mcse.R")
source("paper1/R/model_version.R")
source("paper1/R/format_numbers.R")

base <- read.csv("paper1/output/parameterization_results.csv")
weighted <- do.call(rbind, lapply(1:2, function(index) {
  do.call(rbind, lapply(c(1, 2, 10), function(scale) {
    path <- paste0("paper1/output/weighted_trace_scale_", scale, "_scenario_", index, ".csv")
    x <- read.csv(path)
    assert_model_version(x, path)
    stopifnot(nrow(x) == 1000, identical(sort(unique(x$replicate)), 1:1000),
              all(is.finite(x$error)), all(x$convergence == 0))
    x$stress_scale <- scale
    x
  }))
}))
weighted$policy <- "Weighted-full"
base$model_version <- NULL
weighted$model_version <- NULL
results <- rbind(base[, intersect(names(base), names(weighted))],
                 weighted[, intersect(names(base), names(weighted))])
results$policy <- factor(results$policy,
  levels = c("Trace-full", "Weighted-full", "Reliability-full"))
write.csv(results, "paper1/output/weighted_trace_results.csv", row.names = FALSE)

schedule_matrix <- function(x) as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])
detection <- read.csv("paper1/output/engineering_detection_summary.csv")
output <- list()
for (scenario in unique(results$scenario)) {
  for (policy in levels(results$policy)) {
    x <- results[results$scenario == scenario & results$policy == policy, ]
    first <- x[x$stress_scale == 1, ]
    second <- x[x$stress_scale == 10, ]
    first <- first[order(first$replicate), ]
    second <- second[order(second$replicate), ]
    stopifnot(nrow(first) == 1000, nrow(second) == 1000,
              identical(first$replicate, second$replicate))
    difference <- vapply(1:1000, function(i) {
      max(abs(schedule_matrix(first$schedule[i]) - schedule_matrix(second$schedule[i])))
    }, numeric(1))
    d <- detection[detection$scenario == scenario & detection$policy == policy, ]
    stopifnot(nrow(d) == 1)
    output[[length(output) + 1]] <- data.frame(
      scenario = scenario, policy = policy,
      rmse_scale_1 = sqrt(mean(first$error^2)),
      rmse_scale_10 = sqrt(mean(second$error^2)),
      mcse_scale_1 = rmse_mcse(first$error), mcse_scale_10 = rmse_mcse(second$error),
      median_schedule_difference = median(difference),
      p90_schedule_difference = quantile(difference, 0.9),
      maximum_schedule_difference = max(difference),
      mean_seconds = mean(x$elapsed),
      median_detection_difference = d$median_detection_difference,
      p90_detection_difference = d$p90_detection_difference,
      maximum_detection_difference = d$maximum_detection_difference,
      percent_detection_over_1h = d$percent_detection_over_1h,
      percent_same_detection_stage = d$percent_same_detection_stage)
  }
}
output <- do.call(rbind, output)
numeric_columns <- vapply(output, is.numeric, logical(1))
output[numeric_columns] <- lapply(output[numeric_columns], round, digits = 6)
write.csv(with_model_version(output), "paper1/output/weighted_trace_summary.csv",
          row.names = FALSE)

lines <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
  "Scenario & Policy & Median $\\Delta t$ & 90th $\\Delta t$ & RMSE (scale 1) & RMSE (scale 10) & $>1$ h detection (\\%) & Max. detection $\\Delta t$ \\\\",
  "\\midrule")
for (i in seq_len(nrow(output))) {
  x <- output[i, ]
  lines <- c(lines, sprintf("%s & %s & %.3f & %.2f & %.3f & %.3f & %.1f & %.3f \\\\",
    x$scenario, x$policy, x$median_schedule_difference, x$p90_schedule_difference,
    x$rmse_scale_1, x$rmse_scale_10, x$percent_detection_over_1h,
    x$maximum_detection_difference))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/weighted_trace_table.tex")
print(output, row.names = FALSE)
