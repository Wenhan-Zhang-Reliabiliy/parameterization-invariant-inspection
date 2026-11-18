source("paper1/R/grid_sequential_inspection.R")

index <- as.integer(Sys.getenv("PAPER1_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
scenario <- readRDS("paper1/output/scenarios.rds")[[index]]
results <- do.call(rbind, lapply(seq_len(replicates), run_grid_replicate,
                                 scenario = scenario, planning_samples = 16,
                                 grid_size = 41))
write.csv(results, paste0("paper1/output/grid_scenario_", index, ".csv"),
          row.names = FALSE)
cat("Finished", scenario$name, "at", format(Sys.time()), "\n")
