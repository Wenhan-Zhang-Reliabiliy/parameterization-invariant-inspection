run_diagnostic <- function(index, label, replicates, additional_starts,
                           planning_samples, grid_size) {
  Sys.setenv(PAPER1_SCENARIO = index, PAPER1_DIAGNOSTIC_LABEL = label,
             PAPER1_REPLICATES = replicates,
             PAPER1_ADDITIONAL_STARTS = additional_starts,
             PAPER1_PLANNING_SAMPLES = planning_samples,
             PAPER1_GRID_SIZE = grid_size, PAPER1_RESUME = 0)
  source("paper1/R/run_common_posterior_regret.R")
}

for (index in 1:2) {
  run_diagnostic(index, "main", 1000, 0, 16, 21)
  run_diagnostic(index, "main", 200, 4, 16, 21)
  run_diagnostic(index, "grid41", 200, 0, 16, 41)
  run_diagnostic(index, "grid41", 200, 4, 16, 41)
  run_diagnostic(index, "grid31", 200, 0, 16, 31)
  run_diagnostic(index, "draws32", 200, 0, 32, 21)
  run_diagnostic(index, "draws64", 200, 0, 64, 21)
}
