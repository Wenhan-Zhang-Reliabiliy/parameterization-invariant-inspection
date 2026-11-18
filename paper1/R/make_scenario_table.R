source("paper1/R/sequential_inspection.R")

x <- data.frame(
  scenario = c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
               "Shape prior offset", "Location prior offset",
               "Two-stress baseline", "Acceleration prior offset"),
  groups = c(1, 1, 1, 1, 1, 1, 2, 2),
  n = c(60, 30, 60, 30, 60, 60, 30, 30),
  life = c(1200, 1200, 1800, 1800, 1200, 1200, 1200, 1200),
  sigma = c(0.65, 0.65, 0.65, 0.65, 0.50, 0.65, 0.65, 0.65),
  prior_life = c(1200, 1200, 1800, 1800, 1200, 1600, 1200, 1200),
  prior_sigma = c(0.65, 0.65, 0.65, 0.65, 0.85, 0.65, 0.65, 0.65),
  beta = c(-1, -1, -1, -1, -1, -1, -1, -1.3),
  prior_beta = c(-1, -1, -1, -1, -1, -1, -1, -1),
  use = c(-1, -1, -1, -1, -1, -1, -1.5, -1.5)
)
x$reliability <- vapply(seq_len(nrow(x)), function(i) {
  theta <- c(log(x$life[i]), x$beta[i], log(x$sigma[i]))
  exp(-exp(target_value(theta, x_use = x$use[i])))
}, numeric(1))
lines <- c("\\begin{tabular}{lrrrrrrrrrr}", "\\toprule",
  "Scenario & Groups & $n$ & $\\exp(\\alpha)$ & $\\sigma$ & Prior $\\exp(\\alpha)$ & Prior $\\sigma$ & $\\beta$ & Prior $\\beta$ & $x_u$ & Target $R$ \\\\",
  "\\midrule")
for (i in seq_len(nrow(x))) {
  lines <- c(lines, sprintf("%s & %d & %d & %d & %.2f & %d & %.2f & %.1f & %.1f & %.1f & %.3f \\\\",
                            x$scenario[i], x$groups[i], x$n[i], x$life[i], x$sigma[i],
                            x$prior_life[i], x$prior_sigma[i], x$beta[i], x$prior_beta[i],
                            x$use[i], x$reliability[i]))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/revision_scenarios_table.tex")
