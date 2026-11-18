source("paper1/R/lookahead_grid_inspection.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
scenario <- readRDS("paper1/output/scenarios.rds")[[index]]
output <- paste0("paper1/output/grid_outcome_", index, ".csv")
log_output <- paste0("work/exploratory/logs/progress_outcome_", index, ".txt")
results <- run_with_checkpoints(replicates, function(replicate) {
  run_lookahead_replicate(replicate, scenario = scenario, planning_samples = 12)
}, output, log_output, resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
cat("Finished", scenario$name, "at", format(Sys.time()), "\n")
