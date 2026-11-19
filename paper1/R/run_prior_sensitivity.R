source("paper1/R/grid_sequential_inspection.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
strength <- Sys.getenv("PAPER1_PRIOR_STRENGTH", "Current")
scenario <- readRDS("paper1/output/scenarios.rds")[[index]]
scenario$prior_sd <- switch(strength,
  Weak = c(0.4, 0.8, 0.25),
  Current = c(0.8, 0.8, 0.5),
  Diffuse = c(1.6, 0.8, 1.0),
  stop("Unknown prior strength")
)
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
output <- paste0("paper1/output/prior_", strength, "_scenario_", index, ".csv")
log_output <- paste0("work/exploratory/logs/progress_prior_", strength, "_", index, ".txt")
results <- run_with_checkpoints(replicates, function(replicate) {
  run_grid_replicate(replicate, scenario = scenario, planning_samples = 16, grid_size = 41)
}, output, log_output, resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
cat("Finished", strength, scenario$name, "at", format(Sys.time()), "\n")
