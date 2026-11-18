source("paper1/R/grid3d_sequential_inspection.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
stress_scale <- as.numeric(Sys.getenv("PAPER1_STRESS_SCALE", "1"))
type <- Sys.getenv("PAPER1_SENSITIVITY_TYPE", "draws")
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "200"))
scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
scenario$stress <- scenario$stress * stress_scale
scenario$beta <- scenario$beta / stress_scale
scenario$prior_beta <- scenario$prior_beta / stress_scale
scenario$prior_sd[2] <- scenario$prior_sd[2] / stress_scale
scenario$x_use <- -1.5 * stress_scale

settings <- if (type == "draws") c(16, 32, 64) else if (type == "grid") c(21, 31, 41) else
  stop("Sensitivity type must be draws or grid")
requested_setting <- as.integer(Sys.getenv("PAPER1_SENSITIVITY_SETTING", "0"))
if (requested_setting > 0) settings <- requested_setting
additional_starts <- as.integer(Sys.getenv("PAPER1_ADDITIONAL_STARTS", "0"))
suffix <- if (additional_starts > 0) paste0("_starts_", additional_starts) else ""
output <- paste0("paper1/output/sensitivity_", type, "_scenario_", index,
                 "_scale_", stress_scale, suffix, ".csv")
log_output <- paste0("work/exploratory/logs/progress_sensitivity_", type, "_", index,
                     "_", stress_scale, suffix, ".txt")
remaining_settings <- NULL
if (requested_setting > 0 && identical(Sys.getenv("PAPER1_RESUME", "0"), "1") &&
    file.exists(output)) {
  previous <- read.csv(output)
  if (!"sensitivity_value" %in% names(previous)) {
    previous$sensitivity_value <- requested_setting
  }
  remaining_settings <- previous[previous$sensitivity_value != requested_setting, ]
  write.csv(previous[previous$sensitivity_value == requested_setting, ], output,
            row.names = FALSE)
}
results <- do.call(rbind, lapply(settings, function(setting) {
  planning_samples <- if (type == "draws") setting else 16
  grid_size <- if (type == "grid") setting else 21
  x <- run_with_checkpoints(replicates, function(replicate) {
    run_grid3d_replicate(replicate, scenario = scenario, policies = "Full",
      planning_samples = planning_samples, grid_size = grid_size,
      additional_starts = additional_starts)
  }, output, log_output, chunk_size = 25,
    resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
  x$sensitivity_type <- type
  x$sensitivity_value <- setting
  x$stress_scale <- stress_scale
  x$additional_starts <- additional_starts
  x
}))
if (!is.null(remaining_settings)) results <- rbind(remaining_settings, results)
write.csv(results, output, row.names = FALSE)
cat("Finished", type, scenario$name, "scale", stress_scale,
    "additional starts", additional_starts, "at", format(Sys.time()), "\n")
