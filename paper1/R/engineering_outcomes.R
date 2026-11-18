source("paper1/R/grid3d_sequential_inspection.R")

scenarios <- readRDS("paper1/output/revision_scenarios.rds")
parse_schedule <- function(x) as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])

for (index in 1:2) {
  check <- run_grid3d_replicate(1, scenarios[[index]], policies = "Full",
                                planning_samples = 16, grid_size = 21)
  saved <- read.csv(paste0("paper1/output/parameterization_scale_1_scenario_", index, ".csv"))
  saved <- saved[saved$replicate == 1 & saved$policy == "Full", ]
  stopifnot(identical(check$schedule, saved$schedule))
}

failure_cache <- lapply(scenarios, function(scenario) {
  lapply(1:1000, function(replicate) {
    set.seed(scenario$seed + replicate)
    lapply(scenario$stress, function(x) {
      rweibull(scenario$n, shape = 1 / scenario$sigma,
               scale = exp(scenario$alpha + scenario$beta * x))
    })
  })
})

first_detection <- function(failure_times, schedule) {
  failure_time <- min(unlist(failure_times))
  observed <- failure_time <= max(schedule)
  stage <- if (observed) which(schedule >= failure_time)[1] else NA_integer_
  list(observed = observed, failure_time = failure_time,
       detection_time = if (observed) schedule[stage] else NA_real_,
       detection_stage = stage)
}

policy_files <- rbind(
  data.frame(policy = c("Trace-full", "Full"), label = c("Trace-full", "Reliability-full"),
             prefix = "parameterization", stringsAsFactors = FALSE),
  data.frame(policy = "Weighted-trace", label = "Weighted-full",
             prefix = "weighted_trace", stringsAsFactors = FALSE)
)
raw <- list()
for (index in 1:2) {
  for (j in seq_len(nrow(policy_files))) {
    definition <- policy_files[j, ]
    for (scale in c(1, 10)) {
      path <- paste0("paper1/output/", definition$prefix, "_scale_", scale,
                     "_scenario_", index, ".csv")
      if (definition$policy == "Trace-full" && scale == 1) {
        path <- paste0("paper1/output/trace3d_scenario_", index, ".csv")
      }
      if (!file.exists(path)) next
      results <- read.csv(path)
      results <- results[results$policy == definition$policy, ]
      if (nrow(results) != 1000) next
      for (replicate in 1:1000) {
        detection <- first_detection(failure_cache[[index]][[replicate]],
                                     parse_schedule(results$schedule[replicate]))
        raw[[length(raw) + 1]] <- data.frame(
          scenario = scenarios[[index]]$name, replicate = replicate,
          policy = definition$label, stress_scale = scale,
          observed = detection$observed, failure_time = detection$failure_time,
          detection_time = detection$detection_time,
          detection_stage = detection$detection_stage)
      }
    }
  }
}
raw <- do.call(rbind, raw)
write.csv(with_model_version(raw), "paper1/output/engineering_detection_results.csv",
          row.names = FALSE)

summary <- list()
for (scenario in unique(raw$scenario)) {
  for (policy in unique(raw$policy)) {
    x <- raw[raw$scenario == scenario & raw$policy == policy & raw$stress_scale == 1, ]
    y <- raw[raw$scenario == scenario & raw$policy == policy & raw$stress_scale == 10, ]
    x <- x[order(x$replicate), ]
    y <- y[order(y$replicate), ]
    stopifnot(identical(x$replicate, y$replicate), identical(x$observed, y$observed),
              identical(x$failure_time, y$failure_time))
    keep <- x$observed
    difference <- abs(x$detection_time[keep] - y$detection_time[keep])
    pilot_detected <- !is.na(x$detection_stage) & x$detection_stage == 1
    post_pilot <- !pilot_detected & keep
    post_difference <- abs(x$detection_time[post_pilot] - y$detection_time[post_pilot])
    post_median <- if (length(post_difference)) median(post_difference) else NA_real_
    post_p90 <- if (length(post_difference)) quantile(post_difference, 0.9) else NA_real_
    post_maximum <- if (length(post_difference)) max(post_difference) else NA_real_
    summary[[length(summary) + 1]] <- data.frame(
      scenario = scenario, policy = policy, comparison = "1 vs 10",
      observed_replicates = sum(keep),
      undetected_replicates = sum(!keep),
      pilot_detected_replicates = sum(pilot_detected),
      post_pilot_replicates = sum(post_pilot),
      post_pilot_median_difference = post_median,
      post_pilot_p90_difference = post_p90,
      post_pilot_maximum_difference = post_maximum,
      post_pilot_percent_over_1h = 100 * mean(post_difference > 1),
      median_detection_difference = median(difference),
      p90_detection_difference = quantile(difference, 0.9),
      maximum_detection_difference = max(difference),
      percent_detection_over_1h = 100 * mean(difference > 1),
      percent_same_detection_stage = 100 * mean(x$detection_stage[keep] ==
                                                 y$detection_stage[keep]))
  }
}
summary <- do.call(rbind, summary)
write.csv(with_model_version(summary), "paper1/output/engineering_detection_summary.csv",
          row.names = FALSE)
print(summary, row.names = FALSE)
