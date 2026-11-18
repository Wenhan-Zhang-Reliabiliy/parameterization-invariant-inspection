source("paper1/R/format_numbers.R")

single <- read.csv("paper1/output/grid_convergence_single.csv")
group <- read.csv("paper1/output/grid_convergence_group.csv")
lines <- c("\\begin{tabular}{llrlrrr}", "\\toprule",
  "Scenario & Grid & Variance & Default plan & Default regret & Multi-start regret & $\\Delta t_{\\mathrm{MS}}$ \\\\",
  "\\midrule")
for (i in seq_len(nrow(single))) {
  lines <- c(lines, sprintf("Baseline & %d & %.3f & %s & %s & %s & %.2f \\\\",
    single$grid_size[i], single$target_var_grid[i], single$grid_plan[i],
    tex_scientific(single$default_regret[i]), tex_scientific(single$multi_regret[i]),
    single$default_multi_time_difference[i]))
}
lines <- c(lines, "\\midrule")
for (i in seq_len(nrow(group))) {
  lines <- c(lines, sprintf("%s & %d & %.3f & %s & %s & %s & %.2f \\\\",
    group$scenario[i], group$grid_size[i], group$target_var[i], group$plan[i],
    tex_scientific(group$default_regret[i]), tex_scientific(group$multi_regret[i]),
    group$default_multi_time_difference[i]))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/grid_convergence_table.tex")
