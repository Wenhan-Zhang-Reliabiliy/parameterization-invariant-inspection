source("paper1/R/lookahead_grid_inspection.R")

formals(lookahead_schedule)$candidates <- 12
formals(lookahead_schedule)$quadrature <- 7
scenario <- readRDS("paper1/output/scenarios.rds")[[1]]
results <- do.call(rbind, lapply(1:20, run_lookahead_replicate,
                                 scenario = scenario, planning_samples = 20))
write.csv(with_model_version(results), "paper1/output/grid_high_resolution.csv", row.names = FALSE)
cat("RMSE", sqrt(mean(results$error^2)), "bias", mean(results$error),
    "elapsed", mean(results$elapsed), "\n")
