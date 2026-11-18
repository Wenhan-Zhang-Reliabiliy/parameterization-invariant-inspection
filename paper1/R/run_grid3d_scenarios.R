source("paper1/R/grid3d_sequential_inspection.R")

index <- as.integer(Sys.getenv("PAPER1_REVISION_SCENARIO", "1"))
replicates <- as.integer(Sys.getenv("PAPER1_REVISION_REPLICATES", "1000"))
scenario <- readRDS("paper1/output/revision_scenarios.rds")[[index]]
results <- do.call(rbind, lapply(seq_len(replicates), run_grid3d_replicate,
                                 scenario = scenario, planning_samples = 16,
                                 grid_size = 21))
write.csv(results, paste0("paper1/output/grid3d_scenario_", index, ".csv"),
          row.names = FALSE)
cat("Finished", scenario$name, "at", format(Sys.time()), "\n")
