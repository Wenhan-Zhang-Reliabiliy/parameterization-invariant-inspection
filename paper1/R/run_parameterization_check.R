source("paper1/R/grid3d_sequential_inspection.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
stress_scale <- as.numeric(Sys.getenv("PAPER1_STRESS_SCALE", "10"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
scenario$stress <- scenario$stress * stress_scale
scenario$beta <- scenario$beta / stress_scale
scenario$prior_beta <- scenario$prior_beta / stress_scale
scenario$prior_sd[2] <- scenario$prior_sd[2] / stress_scale
scenario$x_use <- -1.5 * stress_scale
output <- Sys.getenv("PAPER1_PARAMETERIZATION_OUTPUT",
  paste0("paper1/output/parameterization_scale_", stress_scale,
         "_scenario_", index, ".csv"))
log_output <- paste0("work/exploratory/logs/progress_parameterization_", stress_scale,
                     "_", index, ".txt")
policies <- if (identical(Sys.getenv("PAPER1_WEIGHTED_ONLY", "0"), "1")) "Weighted-trace" else
  if (identical(Sys.getenv("PAPER1_RELIABILITY_ONLY", "0"), "1")) "Full" else
    c("Trace-full", "Full")
results <- run_with_checkpoints(replicates, function(replicate) {
  run_grid3d_replicate(replicate, scenario = scenario,
    policies = policies, planning_samples = 16, grid_size = 21)
}, output, log_output, resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
results$stress_scale <- stress_scale
write.csv(results, output, row.names = FALSE)
cat("Finished", scenario$name, "stress scale", stress_scale,
    "at", format(Sys.time()), "\n")
