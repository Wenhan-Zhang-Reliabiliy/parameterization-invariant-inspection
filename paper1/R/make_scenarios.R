make_single_stress_scenario <- function(name, n, alpha, sigma, prior_alpha = alpha,
                                        prior_sigma = sigma, prior_beta = -1,
                                        prior_sd = c(0.8, 0.8, 0.5), seed) {
  list(name = name, n = n, alpha = alpha, beta = -1, sigma = sigma,
       prior_alpha = prior_alpha, prior_beta = prior_beta,
       prior_sigma = prior_sigma, prior_sd = prior_sd, seed = seed)
}

single_stress_scenarios <- list(
  make_single_stress_scenario("Baseline", 60, log(1200), 0.65, seed = 11000),
  make_single_stress_scenario("Small sample", 30, log(1200), 0.65, seed = 22000),
  make_single_stress_scenario("Sparse failures", 60, log(1800), 0.65, seed = 33000),
  make_single_stress_scenario("Small and sparse", 30, log(1800), 0.65, seed = 44000),
  make_single_stress_scenario("Shape prior offset", 60, log(1200), 0.50,
                              prior_sigma = 0.85, seed = 55000),
  make_single_stress_scenario("Location prior offset", 60, log(1200), 0.65,
                              prior_alpha = log(1600), seed = 66000)
)
saveRDS(single_stress_scenarios, "paper1/output/scenarios.rds")

make_joint_scenario <- function(name, beta, prior_beta, seed) {
  list(name = name, n = 30, alpha = log(1200), beta = beta, sigma = 0.65,
       stress = c(0, -0.5), prior_alpha = log(1200), prior_beta = prior_beta,
       prior_sigma = 0.65, prior_sd = c(0.8, 0.8, 0.5), seed = seed)
}
joint_scenarios <- list(
  make_joint_scenario("Two-stress baseline", -1, -1, 71000),
  make_joint_scenario("Acceleration prior offset", -1.3, -1, 72000)
)
saveRDS(joint_scenarios, "paper1/output/revision_scenarios.rds")
