source("paper1/R/group_inspection.R")

run_group_mcmc <- function(times, counts, stress, prior_mean, prior_sd, fit,
                           iterations = 80000, burn = 20000, thin = 10, seed = 1) {
  set.seed(seed)
  root <- chol(fit$covariance)
  current <- fit$mean
  current_log_posterior <- group_log_posterior(current, times, counts, stress,
                                                prior_mean, prior_sd)
  samples <- matrix(NA_real_, nrow = floor((iterations - burn) / thin), ncol = 3)
  accepted <- 0
  output_index <- 1
  for (iteration in seq_len(iterations)) {
    candidate <- current + as.numeric(rnorm(3) %*% (2.38 / sqrt(3) * root))
    candidate_log_posterior <- group_log_posterior(candidate, times, counts, stress,
                                                    prior_mean, prior_sd)
    if (log(runif(1)) < candidate_log_posterior - current_log_posterior) {
      current <- candidate
      current_log_posterior <- candidate_log_posterior
      accepted <- accepted + 1
    }
    if (iteration > burn && (iteration - burn) %% thin == 0) {
      samples[output_index, ] <- current
      output_index <- output_index + 1
    }
  }
  list(samples = samples, acceptance = accepted / iterations)
}

validate_group_case <- function(scenario, replicate) {
  set.seed(scenario$seed + replicate)
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  prior_sd <- scenario$prior_sd
  times <- 200
  counts <- group_observed_counts(failure_times, times)
  fit <- fit_group_posterior(times, counts, scenario$stress, prior_mean, prior_sd)
  chains <- lapply(c(8181, 9181), function(seed) {
    run_group_mcmc(times, counts, scenario$stress, prior_mean, prior_sd, fit, seed = seed)
  })
  chain_targets <- lapply(chains, function(chain) {
    apply(chain$samples, 1, target_value, x_use = -1.5)
  })
  targets <- unlist(chain_targets)
  within <- mean(vapply(chain_targets, var, numeric(1)))
  between <- length(chain_targets[[1]]) * var(vapply(chain_targets, mean, numeric(1)))
  rhat <- sqrt(((length(chain_targets[[1]]) - 1) / length(chain_targets[[1]]) * within +
                  between / length(chain_targets[[1]])) / within)
  at_risk <- vapply(failure_times, function(x) sum(x > 200), numeric(1))
  plan <- plan_group_schedule(fit, 200, 1000, 4, at_risk, scenario$stress,
                              prior_mean, prior_sd, 4,
                              chains[[1]]$samples[seq(1, 6000, length.out = 256), ])
  laplace_targets <- apply(planning_draws(fit, 20000, 8181), 1, target_value, x_use = -1.5)
  laplace_plan <- plan_group_schedule(fit, 200, 1000, 4, at_risk, scenario$stress,
                                      prior_mean, prior_sd, 4, planning_draws(fit, 256, 8181))
  data.frame(scenario = scenario$name, replicate = replicate,
             acceptance = mean(vapply(chains, function(chain) chain$acceptance, numeric(1))),
             rhat = rhat,
             alpha_mean_mcmc = mean(chains[[1]]$samples[, 1]),
             beta_mean_mcmc = mean(chains[[1]]$samples[, 2]),
             logsigma_mean_mcmc = mean(chains[[1]]$samples[, 3]),
             target_mean_mcmc = mean(targets), target_var_mcmc = var(targets),
             target_mean_laplace = mean(laplace_targets),
             target_var_laplace = var(laplace_targets),
             plan = paste(round(plan, 2), collapse = ";"),
             laplace_plan = paste(round(laplace_plan, 2), collapse = ";"))
}

scenarios <- readRDS("paper1/output/revision_scenarios.rds")
indices <- as.integer(strsplit(Sys.getenv("PAPER1_REVISION_INDICES", "1,2"), ",")[[1]])
results <- do.call(rbind, lapply(indices, function(index) validate_group_case(scenarios[[index]], 1)))
write.csv(with_model_version(results), "paper1/output/mcmc_group_validation.csv", row.names = FALSE)
print(results, row.names = FALSE)
