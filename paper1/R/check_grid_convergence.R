source("paper1/R/optimizer_diagnostics.R")
source("paper1/R/grid_sequential_inspection.R")

scenario <- readRDS("paper1/output/scenarios.rds")[[1]]
set.seed(scenario$seed + 1)
failure_times <- rweibull(scenario$n, shape = 1 / scenario$sigma,
                          scale = exp(scenario$alpha))
prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
counts <- observed_counts(failure_times, 200)
at_risk <- sum(failure_times > 200)
single_fits <- lapply(c(41, 61, 101), function(size) {
  grid <- fit_grid_posterior(200, counts, prior_mean, scenario$prior_sd,
                             grid_size = size)
  draws <- sample_grid(grid, 256, 7717)
  target_draws <- apply(sample_grid(grid, 20000, 8181), 1, target_value)
  default_plan <- optimize_single_schedule(grid, 200, 1000, 4, at_risk,
    scenario$prior_sd, 4, draws, start_seed = 7717)
  multi_plan <- optimize_single_schedule(grid, 200, 1000, 4, at_risk,
    scenario$prior_sd, 4, draws, additional_starts = 4, start_seed = 7717)
  list(grid = grid, draws = draws, target_draws = target_draws,
       default_plan = default_plan, multi_plan = multi_plan, grid_size = size)
})
reference <- single_fits[[length(single_fits)]]
reference_values <- unlist(lapply(single_fits, function(x) {
  c(single_plan_objective(x$default_plan, reference$grid, 200, at_risk,
                          scenario$prior_sd, reference$draws),
    single_plan_objective(x$multi_plan, reference$grid, 200, at_risk,
                          scenario$prior_sd, reference$draws))
}))
best_reference <- min(reference_values)
single <- do.call(rbind, lapply(single_fits, function(x) {
  default_value <- single_plan_objective(x$default_plan, reference$grid, 200,
    at_risk, scenario$prior_sd, reference$draws)
  multi_value <- single_plan_objective(x$multi_plan, reference$grid, 200,
    at_risk, scenario$prior_sd, reference$draws)
  data.frame(scenario = scenario$name, replicate = 1, grid_size = x$grid_size,
    target_mean_grid = mean(x$target_draws), target_var_grid = var(x$target_draws),
    grid_plan = paste(round(x$default_plan, 2), collapse = ";"),
    multi_plan = paste(round(x$multi_plan, 2), collapse = ";"),
    default_regret = default_value - best_reference,
    multi_regret = multi_value - best_reference,
    default_multi_time_difference = max(abs(x$default_plan - x$multi_plan)))
}))
write.csv(with_model_version(single), "paper1/output/grid_convergence_single.csv",
          row.names = FALSE)

revision <- readRDS("paper1/output/revision_scenarios.rds")
check_group <- function(scenario, sizes) {
  set.seed(scenario$seed + 1)
  failure_times <- lapply(scenario$stress, function(x) {
    rweibull(scenario$n, shape = 1 / scenario$sigma,
             scale = exp(scenario$alpha + scenario$beta * x))
  })
  prior_mean <- c(scenario$prior_alpha, scenario$prior_beta, log(scenario$prior_sigma))
  counts <- group_observed_counts(failure_times, 200)
  fit <- fit_group_posterior(200, counts, scenario$stress, prior_mean, scenario$prior_sd)
  at_risk <- vapply(failure_times, function(x) sum(x > 200), numeric(1))
  fits <- lapply(sizes, function(size) {
    grid <- grid3d_posterior(200, counts, scenario$stress, prior_mean,
                             scenario$prior_sd, fit, grid_size = size)
    draws <- sample_grid3d(grid, 256, 7717)
    targets <- apply(sample_grid3d(grid, 20000, 8181), 1, target_value, x_use = -1.5)
    default_plan <- plan_group_schedule(grid, 200, 1000, 4, at_risk, scenario$stress,
      prior_mean, scenario$prior_sd, 4, draws, start_seed = 7717)
    multi_plan <- plan_group_schedule(grid, 200, 1000, 4, at_risk, scenario$stress,
      prior_mean, scenario$prior_sd, 4, draws, additional_starts = 4, start_seed = 7717)
    list(grid = grid, draws = draws, targets = targets, default_plan = default_plan,
         multi_plan = multi_plan, grid_size = size)
  })
  reference <- fits[[length(fits)]]
  values <- unlist(lapply(fits, function(x) {
    c(group_plan_objective(x$default_plan, reference$grid, 200, at_risk,
                           scenario$stress, scenario$prior_sd, reference$draws),
      group_plan_objective(x$multi_plan, reference$grid, 200, at_risk,
                           scenario$stress, scenario$prior_sd, reference$draws))
  }))
  best_reference <- min(values)
  do.call(rbind, lapply(fits, function(x) {
    default_value <- group_plan_objective(x$default_plan, reference$grid, 200,
      at_risk, scenario$stress, scenario$prior_sd, reference$draws)
    multi_value <- group_plan_objective(x$multi_plan, reference$grid, 200,
      at_risk, scenario$stress, scenario$prior_sd, reference$draws)
    data.frame(scenario = scenario$name, grid_size = x$grid_size,
      alpha_mean = x$grid$mean[1], beta_mean = x$grid$mean[2],
      logsigma_mean = x$grid$mean[3], target_mean = mean(x$targets),
      target_var = var(x$targets), plan = paste(round(x$default_plan, 2), collapse = ";"),
      multi_plan = paste(round(x$multi_plan, 2), collapse = ";"),
      default_regret = default_value - best_reference,
      multi_regret = multi_value - best_reference,
      default_multi_time_difference = max(abs(x$default_plan - x$multi_plan)))
  }))
}
group <- do.call(rbind, lapply(revision, check_group, sizes = c(21, 31, 41)))
write.csv(with_model_version(group), "paper1/output/grid_convergence_group.csv",
          row.names = FALSE)
print(single, row.names = FALSE)
print(group, row.names = FALSE)
