source("paper1/R/format_numbers.R")

high <- read.csv("paper1/output/grid_high_resolution.csv")
batch <- read.csv("paper1/output/grid_scenario_1.csv")
full <- batch[batch$scenario == "Baseline" & batch$policy == "Full" & batch$replicate <= 20, ]
macros <- c(
  sprintf("\\newcommand{\\HighResolutionOutcomeRMSE}{%.3f}", sqrt(mean(high$error^2))),
  sprintf("\\newcommand{\\HighResolutionOutcomeTime}{%.1f}", mean(high$elapsed)),
  sprintf("\\newcommand{\\HighResolutionBatchRMSE}{%.3f}", sqrt(mean(full$error^2)))
)
common_path <- "paper1/output/common_regret_summary.csv"
if (file.exists(common_path)) {
  common <- read.csv(common_path)
  main <- common[common$setting == "Grid 21, draws 16" &
                   common$additional_starts == 0 & common$replicates == 1000, ]
  if (nrow(main) == 2) {
    baseline <- main[main$scenario == "Two-stress baseline", ]
    offset <- main[main$scenario == "Acceleration prior offset", ]
    macros <- c(macros,
      sprintf("\\newcommand{\\MainRegretBaselineMedian}{%s}", tex_scientific(baseline$median_regret)),
      sprintf("\\newcommand{\\MainRegretOffsetMedian}{%s}", tex_scientific(offset$median_regret)),
      sprintf("\\newcommand{\\MainRegretMaxMedian}{%s}", tex_scientific(max(main$median_regret))),
      sprintf("\\newcommand{\\MainRegretBaselinePninety}{%s}", tex_scientific(baseline$p90_regret)),
      sprintf("\\newcommand{\\MainRegretOffsetPninety}{%s}", tex_scientific(offset$p90_regret)),
      sprintf("\\newcommand{\\MainRegretMaxPninetyVariancePercent}{%s}",
              tex_scientific(100 * max(expm1(main$p90_regret)))))
  }
}
prior_path <- "paper1/output/prior_sensitivity_reductions.csv"
if (file.exists(prior_path)) {
  prior <- read.csv(prior_path)
  current <- prior[prior$prior_strength == "Current", ]
  baseline <- current[current$scenario == "Baseline", ]
  sparse <- current[current$scenario == "Sparse failures", ]
  macros <- c(macros,
    sprintf("\\newcommand{\\PriorReductionMin}{%.1f}", min(prior$reduction)),
    sprintf("\\newcommand{\\PriorReductionMax}{%.1f}", max(prior$reduction)),
    sprintf("\\newcommand{\\PriorCurrentBaselineReduction}{%.2f}", baseline$reduction),
    sprintf("\\newcommand{\\PriorCurrentBaselineCI}{[%.2f, %.2f]}", baseline$lower, baseline$upper),
    sprintf("\\newcommand{\\PriorCurrentSparseReduction}{%.2f}", sparse$reduction),
    sprintf("\\newcommand{\\PriorCurrentSparseCI}{[%.2f, %.2f]}", sparse$lower, sparse$upper))
}
sensitivity_path <- "paper1/output/numerical_sensitivity_summary.csv"
if (file.exists(sensitivity_path)) {
  sensitivity <- read.csv(sensitivity_path)
  if ("optimizer_warnings" %in% names(sensitivity)) {
    macros <- c(macros, sprintf("\\newcommand{\\MultiStartOptimizerWarnings}{%d}",
      sum(sensitivity$optimizer_warnings[sensitivity$additional_starts > 0])))
  }
}
writeLines(macros, "paper1/manuscript/result_macros.tex")
