main_replicates <- as.integer(Sys.getenv("PAPER1_REPLICATES", "1000"))
replicates <- main_replicates
Sys.setenv(PAPER1_REPLICATES = main_replicates,
           PAPER1_REVISION_REPLICATES = main_replicates)
source("paper1/R/make_scenarios.R")
for (index in 1:6) {
  Sys.setenv(PAPER1_SCENARIO = index, PAPER1_MODEL = "single")
  source("paper1/R/run_grid_scenarios.R")
  source("paper1/R/run_grid_outcome.R")
  source("paper1/R/run_trace_scenarios.R")
}
for (index in 1:2) {
  Sys.setenv(PAPER1_REVISION_SCENARIO = index, PAPER1_SCENARIO = index,
             PAPER1_MODEL = "joint")
  source("paper1/R/run_grid3d_scenarios.R")
  source("paper1/R/run_trace_scenarios.R")
  source("paper1/R/run_factorial_scenarios.R")
  for (stress_scale in c(2, 10)) {
    Sys.setenv(PAPER1_STRESS_SCALE = stress_scale)
    source("paper1/R/run_parameterization_check.R")
  }
}
source("paper1/R/run_weighted_trace_experiments.R")
for (type in c("draws", "grid")) {
  for (index in 1:2) {
    Sys.setenv(PAPER1_SCENARIO = index, PAPER1_SENSITIVITY_TYPE = type,
               PAPER1_REPLICATES = 200)
    for (stress_scale in c(1, 10)) {
      Sys.setenv(PAPER1_STRESS_SCALE = stress_scale)
      source("paper1/R/run_numerical_sensitivity.R")
    }
  }
}
source("paper1/R/run_optimizer_diagnostics.R")
source("paper1/R/run_multistart_sensitivity.R")
Sys.setenv(PAPER1_REPLICATES = main_replicates)
for (index in c(1, 3)) {
  Sys.setenv(PAPER1_SCENARIO = index)
  for (strength in c("Weak", "Current", "Diffuse")) {
    Sys.setenv(PAPER1_PRIOR_STRENGTH = strength)
    source("paper1/R/run_prior_sensitivity.R")
  }
}
source("paper1/R/run_grid_refinement.R")
source("paper1/R/grid_posterior_validation.R")
source("paper1/R/mcmc_posterior_validation.R")
source("paper1/R/mcmc_group_validation.R")
source("paper1/R/make_mcmc_diagnostics.R")
source("paper1/R/check_grid_convergence.R")
source("paper1/R/check_parameterization_identity.R")
source("paper1/R/rebuild_manuscript.R")
