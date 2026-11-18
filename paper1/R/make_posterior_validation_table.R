source("paper1/R/model_version.R")

grid <- read.csv("paper1/output/grid_posterior_validation.csv")
single_mcmc <- read.csv("paper1/output/mcmc_posterior_validation.csv")
group_mcmc <- read.csv("paper1/output/mcmc_group_validation.csv")
group_grid <- read.csv("paper1/output/grid_convergence_group.csv")
group_grid <- group_grid[group_grid$grid_size == 21, ]

x <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  scenario <- grid$scenario[i]
  rbind(
    data.frame(scenario = scenario, method = "Laplace",
               target_mean = grid$target_mean_laplace[i],
               target_variance = grid$target_var_laplace[i]),
    data.frame(scenario = scenario, method = "Grid",
               target_mean = grid$target_mean_grid[i],
               target_variance = grid$target_var_grid[i]),
    data.frame(scenario = scenario, method = "MCMC",
               target_mean = single_mcmc$target_mean[i],
               target_variance = single_mcmc$target_var[i])
  )
}))
for (i in seq_len(nrow(group_mcmc))) {
  scenario <- group_mcmc$scenario[i]
  x <- rbind(x,
    data.frame(scenario = scenario, method = "Laplace",
               target_mean = group_mcmc$target_mean_laplace[i],
               target_variance = group_mcmc$target_var_laplace[i]),
    data.frame(scenario = scenario, method = "Grid3D",
               target_mean = group_grid$target_mean[i],
               target_variance = group_grid$target_var[i]),
    data.frame(scenario = scenario, method = "MCMC",
               target_mean = group_mcmc$target_mean_mcmc[i],
               target_variance = group_mcmc$target_var_mcmc[i]))
}
write.csv(x, "paper1/output/posterior_validation_summary.csv", row.names = FALSE)

lines <- c("\\begin{tabular}{llrr}", "\\toprule",
           "Scenario & Method & Posterior mean & Posterior variance \\\\",
           "\\midrule")
for (i in seq_len(nrow(x))) {
  lines <- c(lines, sprintf("%s & %s & %.3f & %.3f \\\\",
                            x$scenario[i], x$method[i], x$target_mean[i], x$target_variance[i]))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/posterior_validation_table.tex")
