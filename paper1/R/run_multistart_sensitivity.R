for (index in 1:2) {
  for (stress_scale in c(1, 10)) {
    Sys.setenv(PAPER1_SCENARIO = index, PAPER1_SENSITIVITY_TYPE = "grid",
               PAPER1_SENSITIVITY_SETTING = 41, PAPER1_REPLICATES = 200,
               PAPER1_STRESS_SCALE = stress_scale, PAPER1_ADDITIONAL_STARTS = 4,
               PAPER1_RESUME = 0)
    source("paper1/R/run_numerical_sensitivity.R")
  }
}
Sys.setenv(PAPER1_SENSITIVITY_SETTING = 0, PAPER1_ADDITIONAL_STARTS = 0)
