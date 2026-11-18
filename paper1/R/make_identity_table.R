source("paper1/R/format_numbers.R")

x <- read.csv("paper1/output/parameterization_identity.csv")
lines <- c("\\begin{tabular}{lllrr}", "\\toprule",
  "Scenario & Max. absolute difference & Max. relative difference & Trace at scale 1 & Trace at scale 10 \\\\",
  "\\midrule")
for (i in seq_len(nrow(x))) {
  lines <- c(lines, sprintf("%s & %s & %s & %.2f & %.2f \\\\",
    x$scenario[i], tex_scientific(x$max_absolute_utility_difference[i]),
    tex_scientific(x$max_relative_utility_difference[i]),
    x$trace_scale_1[i], x$trace_scale_10[i]))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
write_model_table(lines, "paper1/manuscript/parameterization_identity_table.tex")
