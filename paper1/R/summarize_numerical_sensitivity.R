source("paper1/R/model_version.R")

scenario_names <- c("Two-stress baseline", "Acceleration prior offset")
settings <- list(draws = c(16, 32, 64), grid = c(21, 31, 41))
schedule_matrix <- function(schedule) {
  matrix(as.numeric(unlist(strsplit(schedule, ";", fixed = TRUE))), nrow = 5)
}
output <- list()
for (type in names(settings)) {
  for (scenario_index in 1:2) {
    for (setting in settings[[type]]) {
      for (additional_starts in c(0, 4)) {
        if (additional_starts > 0 && setting != 41) next
        suffix <- if (additional_starts > 0) paste0("_starts_", additional_starts) else ""
        first_path <- paste0("paper1/output/sensitivity_", type, "_scenario_",
          scenario_index, "_scale_1", suffix, ".csv")
        second_path <- paste0("paper1/output/sensitivity_", type, "_scenario_",
          scenario_index, "_scale_10", suffix, ".csv")
        if (additional_starts > 0 && (!file.exists(first_path) || !file.exists(second_path))) next
        first <- read.csv(first_path)
        second <- read.csv(second_path)
        assert_model_version(first, first_path)
        assert_model_version(second, second_path)
        first <- first[first$sensitivity_value == setting, ]
        second <- second[second$sensitivity_value == setting, ]
        stopifnot(nrow(first) == 200, nrow(second) == 200,
                  identical(names(first), names(second)),
                  identical(sort(unique(first$replicate)), 1:200),
                  identical(sort(unique(second$replicate)), 1:200),
                  all(is.finite(first$error)), all(is.finite(second$error)),
                  all(is.finite(first$elapsed)), all(is.finite(second$elapsed)),
                  all(first$convergence == 0), all(second$convergence == 0))
        first <- first[order(first$replicate), ]
        second <- second[order(second$replicate), ]
        time_difference <- vapply(seq_len(nrow(first)), function(i) {
          max(abs(schedule_matrix(first$schedule[i]) - schedule_matrix(second$schedule[i])))
        }, numeric(1))
        output[[length(output) + 1]] <- data.frame(
          scenario = scenario_names[scenario_index], sensitivity_type = type,
          sensitivity_value = setting, additional_starts = additional_starts,
          replicates = nrow(first), median_time_difference = median(time_difference),
          p90_time_difference = quantile(time_difference, 0.9),
          maximum_time_difference = max(time_difference),
          rmse_scale_1 = sqrt(mean(first$error^2)),
          rmse_scale_10 = sqrt(mean(second$error^2)),
          mean_seconds = mean(c(first$elapsed, second$elapsed)),
          optimizer_warnings = sum(first$optimizer_convergence != 0) +
            sum(second$optimizer_convergence != 0))
      }
    }
  }
}
output <- do.call(rbind, output)
write.csv(with_model_version(output), "paper1/output/numerical_sensitivity_summary.csv",
          row.names = FALSE)

lines <- c("\\begin{tabular}{lllrrrrrr}", "\\toprule",
  "Scenario & Setting & Value & Extra starts & Median $\\Delta t$ & 90th $\\Delta t$ & RMSE (scale 1) & RMSE (scale 10) & Seconds \\\\",
  "\\midrule")
for (i in seq_len(nrow(output))) {
  x <- output[i, ]
  label <- if (x$sensitivity_type == "draws") "Posterior draws" else "Grid points"
  lines <- c(lines, sprintf("%s & %s & %d & %d & %.2f & %.2f & %.3f & %.3f & %.3f \\\\",
    x$scenario, label, x$sensitivity_value, x$additional_starts,
    x$median_time_difference, x$p90_time_difference, x$rmse_scale_1,
    x$rmse_scale_10, x$mean_seconds))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/numerical_sensitivity_table.tex")
print(output, row.names = FALSE)
