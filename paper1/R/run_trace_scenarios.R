source("paper1/R/checkpoint.R")

model <- Sys.getenv("PAPER1_MODEL", "single")
index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))

if (model == "single") {
  source("paper1/R/grid_sequential_inspection.R")
  scenario <- readRDS("paper1/output/scenarios.rds")[[index]]
  runner <- function(replicate) {
    run_grid_replicate(replicate, scenario = scenario,
      policies = c("Trace-1", "Trace-full"), planning_samples = 16, grid_size = 41)
  }
  default_output <- paste0("paper1/output/trace_scenario_", index, ".csv")
} else if (model == "joint") {
  source("paper1/R/grid3d_sequential_inspection.R")
  scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
  runner <- function(replicate) {
    run_grid3d_replicate(replicate, scenario = scenario, policies = "Trace-full",
                         planning_samples = 16, grid_size = 21)
  }
  default_output <- paste0("paper1/output/trace3d_scenario_", index, ".csv")
} else {
  stop("PAPER1_MODEL must be single or joint")
}

output <- Sys.getenv("PAPER1_TRACE_OUTPUT", default_output)
log_output <- paste0("work/exploratory/logs/progress_trace_", model, "_", index, ".txt")
results <- run_with_checkpoints(replicates, runner, output, log_output,
                                resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
cat("Finished", model, scenario$name, "at", format(Sys.time()), "\n")
