source("paper1/R/model_version.R")

for (index in 1:2) {
  for (scale in c(1, 2, 10)) {
    path <- paste0("paper1/output/weighted_trace_scale_", scale, "_scenario_", index, ".csv")
    x <- read.csv(path)
    assert_model_version(x, path)
    stopifnot(nrow(x) == 1000, identical(sort(unique(x$replicate)), 1:1000),
              all(x$policy == "Weighted-trace"), all(is.finite(x$error)),
              all(x$convergence == 0))
  }
}
summary <- read.csv("paper1/output/weighted_trace_summary.csv")
assert_model_version(summary, "weighted_trace_summary")
stopifnot(nrow(summary) == 6,
          all(table(summary$scenario, summary$policy) == 1),
          all(summary$percent_same_detection_stage == 100),
          all(summary$percent_detection_over_1h[summary$policy == "Trace-full"] > 0),
          all(summary$percent_detection_over_1h[summary$policy != "Trace-full"] == 0))
identity <- read.csv("paper1/output/parameterization_identity.csv")
stopifnot(max(identity$max_relative_weighted_difference) < 1e-4)

raw <- read.csv("paper1/output/engineering_detection_results.csv")
assert_model_version(raw, "engineering_detection_results")
stopifnot(nrow(raw) == 12000,
          all(table(raw$scenario, raw$policy, raw$stress_scale) == 1000))
groups <- interaction(raw$scenario, raw$replicate, drop = TRUE)
failure_range <- tapply(raw$failure_time, groups, function(x) max(x) - min(x))
observed_range <- tapply(raw$observed, groups, function(x) max(x) - min(x))
stopifnot(all(failure_range == 0), all(observed_range == 0))

table <- paste(readLines("paper1/manuscript/weighted_trace_table.tex", warn = FALSE),
               collapse = "\n")
for (i in seq_len(nrow(summary))) {
  x <- summary[i, ]
  expected <- sprintf("%s & %s & %.3f & %.2f & %.3f & %.3f & %.1f & %.3f",
    x$scenario, x$policy, x$median_schedule_difference, x$p90_schedule_difference,
    x$rmse_scale_1, x$rmse_scale_10, x$percent_detection_over_1h,
    x$maximum_detection_difference)
  if (!grepl(expected, table, fixed = TRUE)) {
    stop("Weighted-table mismatch at row ", i, ": ", expected)
  }
}
writeLines(c("RESS enhancement checks passed.",
             "Six weighted-trace outputs contain 1000 replicates each.",
             "Engineering outcomes use identical physical failure times across policies and codings.",
             "Weighted-trace identity, summary table, and detection results are consistent."),
           "paper1/output/ress_enhancement_validation.txt")
