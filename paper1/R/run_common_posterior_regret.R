source("paper1/R/optimizer_diagnostics.R")
source("paper1/R/checkpoint.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
additional_starts <- as.integer(Sys.getenv("PAPER1_ADDITIONAL_STARTS", "0"))
label <- Sys.getenv("PAPER1_DIAGNOSTIC_LABEL", "main")
planning_samples <- as.integer(Sys.getenv("PAPER1_PLANNING_SAMPLES", "16"))
grid_size <- as.integer(Sys.getenv("PAPER1_GRID_SIZE", "21"))
scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
output <- paste0("paper1/output/common_regret_", label, "_scenario_", index,
                 "_starts_", additional_starts, ".csv")
log_output <- paste0("work/exploratory/logs/progress_common_regret_", label,
                     "_", index, "_", additional_starts, ".txt")
results <- run_with_checkpoints(replicates, function(replicate) {
  run_group_coding_replicate(replicate, scenario, planning_samples = planning_samples,
    grid_size = grid_size, additional_starts = additional_starts)
}, output, log_output, resume = identical(Sys.getenv("PAPER1_RESUME", "0"), "1"))
results$diagnostic_label <- label
write.csv(results, output, row.names = FALSE)
cat("Finished common-posterior regret", label, scenario$name,
    "additional starts", additional_starts, "at", format(Sys.time()), "\n")
