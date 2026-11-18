source("paper1/R/model_version.R")

single <- read.csv("paper1/output/mcmc_posterior_validation.csv")
group <- read.csv("paper1/output/mcmc_group_validation.csv")
x <- rbind(
  data.frame(scenario = single$scenario, acceptance = single$acceptance, rhat = single$rhat),
  data.frame(scenario = group$scenario, acceptance = group$acceptance, rhat = group$rhat)
)
x$chains <- 2
x$iterations_per_chain <- 80000
x$warmup_per_chain <- 20000
x$thin <- 10
x$retained_total <- 12000
stopifnot(max(x$rhat) < 1.01)
write.csv(with_model_version(x), "paper1/output/mcmc_diagnostics.csv", row.names = FALSE)
print(x, row.names = FALSE)
