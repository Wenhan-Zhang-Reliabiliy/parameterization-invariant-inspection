source("paper1/R/posterior_grid.R")

run_mcmc <- function(times, counts, prior_mean, prior_sd, fit, iterations = 80000,
                     burn = 20000, thin = 10, seed = 1) {
  set.seed(seed)
  index <- c(1, 3)
  proposal_covariance <- psd_matrix(fit$covariance[index, index])
  root <- chol(proposal_covariance)
  current <- fit$mean[index]
  current_log_posterior <- log_posterior(c(current[1], prior_mean[2], current[2]),
                                         times, counts, prior_mean, prior_sd)
  samples <- matrix(NA_real_, nrow = floor((iterations - burn) / thin), ncol = 2)
  accepted <- 0
  output_index <- 1
  for (iteration in seq_len(iterations)) {
    candidate <- current + as.numeric(rnorm(2) %*% (2.38 / sqrt(2) * root))
    candidate_log_posterior <- log_posterior(c(candidate[1], prior_mean[2], candidate[2]),
                                             times, counts, prior_mean, prior_sd)
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

validate_mcmc_case <- function(scenario, replicate) {
  set.seed(scenario$seed + replicate)
  failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                            scale = exp(scenario$alpha))
  times <- 200
  counts <- observed_counts(failure_times, times)
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  prior_sd <- c(0.8, 0.8, 0.5)
  fit <- fit_posterior(times, counts, prior_mean, prior_sd)
  chains <- lapply(c(8181, 9181), function(seed) {
    run_mcmc(times, counts, prior_mean, prior_sd, fit, seed = seed)
  })
  chain_targets <- lapply(chains, function(chain) {
    theta_samples <- cbind(chain$samples[, 1], prior_mean[2], chain$samples[, 2])
    apply(theta_samples, 1, target_value)
  })
  targets <- unlist(chain_targets)
  within <- mean(vapply(chain_targets, var, numeric(1)))
  between <- length(chain_targets[[1]]) * var(vapply(chain_targets, mean, numeric(1)))
  rhat <- sqrt(((length(chain_targets[[1]]) - 1) / length(chain_targets[[1]]) * within +
                  between / length(chain_targets[[1]])) / within)
  theta_samples <- cbind(chains[[1]]$samples[, 1], prior_mean[2], chains[[1]]$samples[, 2])
  plan_draws <- theta_samples[seq(1, nrow(theta_samples), length.out = 256), ]
  plan <- plan_schedule(fit, 200, 1000, 4, sum(failure_times > 200), prior_mean,
                        prior_sd, 4, plan_draws)
  data.frame(scenario = scenario$name, replicate = replicate,
             acceptance = mean(vapply(chains, function(chain) chain$acceptance, numeric(1))),
             rhat = rhat,
             alpha_mean = mean(theta_samples[, 1]),
             logsigma_mean = mean(theta_samples[, 3]),
             target_mean = mean(targets), target_var = var(targets),
             plan = paste(round(plan, 2), collapse = ";"))
}

scenarios <- readRDS("paper1/output/scenarios.rds")
indices <- as.integer(strsplit(Sys.getenv("PAPER1_GRID_INDICES", "1,3,6"), ",")[[1]])
results <- do.call(rbind, lapply(indices, function(index) validate_mcmc_case(scenarios[[index]], 1)))
write.csv(with_model_version(results), "paper1/output/mcmc_posterior_validation.csv", row.names = FALSE)
print(results, row.names = FALSE)
