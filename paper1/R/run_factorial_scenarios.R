source("paper1/R/plugin_inspection.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
output <- Sys.getenv("PAPER1_FACTORIAL_OUTPUT",
                     paste0("paper1/output/factorial_scenario_", index, ".csv"))
log_output <- paste0("work/exploratory/logs/progress_factorial_", index, ".txt")
results <- run_with_checkpoints(replicates, function(replicate) {
  run_factorial_replicate(replicate, scenario = scenario,
                          planning_samples = 16, grid_size = 21)
}, output, log_output, resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
cat("Finished", scenario$name, "at", format(Sys.time()), "\n")
