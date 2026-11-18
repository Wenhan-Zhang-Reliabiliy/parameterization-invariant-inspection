source("paper1/R/model_version.R")
source("paper1/R/mcse.R")

set.seed(20261117)
test_errors <- rnorm(500, mean = 0.2, sd = 0.7)
expected_mcse <- sqrt(var(test_errors^2) /
  (4 * length(test_errors) * mean(test_errors^2)))
stopifnot(abs(rmse_mcse(test_errors) - expected_mcse) < 1e-14)

read_versioned <- function(path) {
  x <- read.csv(path)
  assert_model_version(x, path)
  x
}

for (index in c(1, 3)) {
  main <- read_versioned(paste0("paper1/output/grid_scenario_", index, ".csv"))
  current <- read_versioned(paste0("paper1/output/prior_Current_scenario_", index, ".csv"))
  columns <- c("scenario", "replicate", "policy", "estimate", "truth", "error", "schedule")
  main <- main[order(main$replicate, main$policy), columns]
  current <- current[order(current$replicate, current$policy), columns]
  rownames(main) <- rownames(current) <- NULL
  stopifnot(identical(main, current))
}

criterion <- read.csv("paper1/output/criterion_summary.csv")
appendix <- read.csv("paper1/output/revision_performance_summary.csv")
policy_map <- c(Myopic = "Myopic", `Batch-full` = "Batch-full")
for (scenario in unique(appendix$scenario)) {
  for (policy in names(policy_map)) {
    a <- criterion[criterion$scenario == scenario & criterion$policy == policy, ]
    b <- appendix[appendix$scenario == scenario & appendix$policy == policy_map[[policy]], ]
    stopifnot(nrow(a) == 1, nrow(b) == 1, abs(a$rmse - b$rmse) < 1e-12)
  }
}

check_mcse <- function(results, summary, keys) {
  expected <- do.call(rbind, lapply(split(results, results[keys], drop = TRUE), function(x) {
    output <- x[1, keys, drop = FALSE]
    output$expected_rmse <- sqrt(mean(x$error^2))
    output$expected_mcse <- rmse_mcse(x$error)
    output
  }))
  comparison <- merge(summary, expected, by = keys)
  stopifnot(nrow(comparison) == nrow(summary),
            max(abs(comparison$rmse - comparison$expected_rmse)) < 1e-12,
            max(abs(comparison$rmse_mcse - comparison$expected_mcse)) < 1e-12)
}
check_mcse(read.csv("paper1/output/criterion_results.csv"), criterion,
           c("scenario", "policy"))
check_mcse(read.csv("paper1/output/revision_results.csv"), appendix,
           c("scenario", "policy"))
check_mcse(read.csv("paper1/output/factorial_results.csv"),
           read.csv("paper1/output/factorial_summary.csv"), c("scenario", "policy"))
parameterization_results <- read.csv("paper1/output/parameterization_results.csv")
parameterization_results$scale <- parameterization_results$stress_scale
check_mcse(parameterization_results, read.csv("paper1/output/parameterization_accuracy.csv"),
           c("scenario", "policy", "scale"))

prior_results <- do.call(rbind, lapply(c(1, 3), function(index) {
  do.call(rbind, lapply(c("Weak", "Current", "Diffuse"), function(strength) {
    x <- read_versioned(paste0("paper1/output/prior_", strength, "_scenario_", index, ".csv"))
    x$prior_strength <- strength
    x
  }))
}))
prior_policy_map <- c(Full = "Batch-full", `Two-step` = "Batch-2")
mapped <- match(prior_results$policy, names(prior_policy_map))
prior_results$policy[!is.na(mapped)] <- prior_policy_map[mapped[!is.na(mapped)]]
check_mcse(prior_results, read.csv("paper1/output/prior_sensitivity_summary.csv"),
           c("scenario", "prior_strength", "policy"))
prior_reductions <- read.csv("paper1/output/prior_sensitivity_reductions.csv")
canonical_reductions <- read.csv("paper1/output/criterion_reductions.csv")
canonical_reductions <- canonical_reductions[canonical_reductions$policy == "Batch-full" &
  canonical_reductions$baseline == "Myopic", ]
appendix_reductions <- read.csv("paper1/output/revision_paired_reductions.csv")
appendix_reductions <- appendix_reductions[appendix_reductions$policy == "Batch-full" &
  appendix_reductions$baseline == "Myopic", ]
canonical_reductions <- canonical_reductions[canonical_reductions$scenario %in%
  appendix_reductions$scenario, ]
for (scenario in canonical_reductions$scenario) {
  canonical_row <- canonical_reductions[canonical_reductions$scenario == scenario, ]
  appendix_row <- appendix_reductions[appendix_reductions$scenario == scenario, ]
  stopifnot(nrow(canonical_row) == 1, nrow(appendix_row) == 1,
    isTRUE(all.equal(canonical_row[, c("reduction", "lower", "upper")],
                     appendix_row[, c("reduction", "lower", "upper")],
                     check.attributes = FALSE)))
  if (scenario %in% c("Baseline", "Sparse failures")) {
    prior_row <- prior_reductions[prior_reductions$scenario == scenario &
      prior_reductions$prior_strength == "Current", ]
    stopifnot(nrow(prior_row) == 1,
      isTRUE(all.equal(canonical_row[, c("reduction", "lower", "upper")],
                       prior_row[, c("reduction", "lower", "upper")],
                       check.attributes = FALSE)))
  }
}
weak <- prior_reductions[prior_reductions$prior_strength == "Weak", ]
not_weak <- prior_reductions[prior_reductions$prior_strength != "Weak", ]
stopifnot(all(weak$lower < 0 & weak$upper > 0), all(not_weak$lower > 0))
prior_macros <- paste(readLines("paper1/manuscript/result_macros.tex", warn = FALSE),
                      collapse = "\n")
expected_prior_macros <- c(
  sprintf("\\newcommand{\\PriorReductionMin}{%.1f}", min(prior_reductions$reduction)),
  sprintf("\\newcommand{\\PriorReductionMax}{%.1f}", max(prior_reductions$reduction)))
stopifnot(all(vapply(expected_prior_macros, function(x) grepl(x, prior_macros, fixed = TRUE),
                     logical(1))))

prior_summary <- read.csv("paper1/output/prior_sensitivity_summary.csv")
prior_table <- paste(readLines("paper1/manuscript/prior_sensitivity_table.tex", warn = FALSE),
                     collapse = "\n")
for (scenario in c("Baseline", "Sparse failures")) {
  x <- prior_summary[prior_summary$scenario == scenario &
                       prior_summary$prior_strength == "Current", ]
  values <- vapply(c("Fixed", "Static", "Myopic", "Batch-full"), function(policy) {
    sprintf("%.3f", x$rmse[x$policy == policy])
  }, character(1))
  reduction <- prior_reductions[prior_reductions$scenario == scenario &
                                  prior_reductions$prior_strength == "Current", ]
  expected <- sprintf("Current & %s & %.1f", paste(values, collapse = " & "),
                      reduction$reduction)
  stopifnot(grepl(expected, prior_table, fixed = TRUE))
}

criterion_table <- paste(readLines("paper1/manuscript/criterion_table.tex", warn = FALSE),
                         collapse = "\n")
for (scenario in unique(criterion$scenario)) {
  values <- vapply(c("Trace-1", "Myopic", "Trace-full", "Batch-full"), function(policy) {
    sprintf("%.3f", criterion$rmse[criterion$scenario == scenario & criterion$policy == policy])
  }, character(1))
  stopifnot(grepl(paste0(scenario, " & ", paste(values, collapse = " & ")),
                  criterion_table, fixed = TRUE))
}

appendix_table <- paste(readLines("paper1/manuscript/revision_performance_table.tex", warn = FALSE),
                        collapse = "\n")
for (scenario in c("Baseline", "Small sample", "Sparse failures", "Small and sparse",
                   "Shape prior offset", "Location prior offset")) {
  values <- vapply(c("Fixed", "Static", "Myopic", "Batch-2", "Batch-full", "Outcome-2"),
                   function(policy) {
    x <- appendix[appendix$scenario == scenario & appendix$policy == policy, ]
    sprintf("%.3f (%.3f)", x$rmse, x$rmse_mcse)
  }, character(1))
  stopifnot(grepl(paste0(scenario, " & ", paste(values, collapse = " & ")),
                  appendix_table, fixed = TRUE))
}

common_path <- "paper1/output/common_regret_summary.csv"
if (file.exists(common_path)) {
  source("paper1/R/format_numbers.R")
  common <- read_versioned(common_path)
  main <- common[common$setting == "Grid 21, draws 16" &
                   common$additional_starts == 0 & common$replicates == 1000, ]
  stopifnot(nrow(main) == 2, max(main$maximum_identity_error) < 1e-4)
  for (index in 1:2) {
    default <- read_versioned(paste0("paper1/output/common_regret_main_scenario_",
                                    index, "_starts_0.csv"))
    multi_path <- paste0("paper1/output/common_regret_main_scenario_", index, "_starts_4.csv")
    if (file.exists(multi_path)) {
      multi <- read_versioned(multi_path)
      stopifnot(nrow(multi) == 200,
                identical(default$replicate[default$replicate <= 200], multi$replicate))
    }
  }
  macros <- paste(readLines("paper1/manuscript/result_macros.tex", warn = FALSE), collapse = "\n")
  for (scenario in main$scenario) {
    name <- if (scenario == "Two-stress baseline") "MainRegretBaselineMedian" else
      "MainRegretOffsetMedian"
    value <- tex_scientific(main$median_regret[main$scenario == scenario])
    stopifnot(grepl(sprintf("\\newcommand{\\%s}{%s}", name, value), macros, fixed = TRUE))
  }
  label_map <- c("Grid 21, draws 16" = "main", "Grid 31, draws 16" = "grid31",
                 "Grid 41, draws 16" = "grid41", "Grid 21, draws 32" = "draws32",
                 "Grid 21, draws 64" = "draws64")
  for (i in seq_len(nrow(common))) {
    x <- common[i, ]
    index <- if (x$scenario == "Two-stress baseline") 1 else 2
    path <- paste0("paper1/output/common_regret_", label_map[[x$setting]], "_scenario_",
                   index, "_starts_", x$additional_starts, ".csv")
    raw <- read_versioned(path)
    raw <- raw[raw$replicate <= x$replicates, ]
    stopifnot(max(abs(raw$regret - pmax(abs(raw$base_regret), abs(raw$scaled_regret)))) < 1e-12)
    raw$regret <- pmax(abs(raw$base_regret), abs(raw$scaled_regret))
    stopifnot(nrow(raw) == x$replicates,
              max(raw$objective_identity_error) <= x$maximum_identity_error + 1e-15,
              abs(median(raw$regret) - x$median_regret) < 1e-15)
  }
}

sensitivity_path <- "paper1/output/numerical_sensitivity_summary.csv"
if (file.exists(sensitivity_path)) {
  sensitivity <- read_versioned(sensitivity_path)
  stopifnot(nrow(sensitivity) == 14, all(sensitivity$replicates == 200),
            all(sensitivity$optimizer_warnings >= 0))
  macros <- paste(readLines("paper1/manuscript/result_macros.tex", warn = FALSE),
                  collapse = "\n")
  warning_macro <- sprintf("\\newcommand{\\MultiStartOptimizerWarnings}{%d}",
    sum(sensitivity$optimizer_warnings[sensitivity$additional_starts > 0]))
  stopifnot(grepl(warning_macro, macros, fixed = TRUE))
  for (index in 1:2) {
    main <- read_versioned(paste0("paper1/output/grid3d_scenario_", index, ".csv"))
    main <- main[main$policy == "Full" & main$replicate <= 200, ]
    main <- main[order(main$replicate), ]
    diagnostic <- read_versioned(paste0(
      "paper1/output/sensitivity_grid_scenario_", index, "_scale_1.csv"))
    diagnostic <- diagnostic[diagnostic$sensitivity_value == 21 &
                               diagnostic$replicate <= 200, ]
    diagnostic <- diagnostic[order(diagnostic$replicate), ]
    stopifnot(nrow(main) == 200, nrow(diagnostic) == 200,
              identical(main$error, diagnostic$error),
              identical(main$schedule, diagnostic$schedule))
  }
  for (i in seq_len(nrow(sensitivity))) {
    x <- sensitivity[i, ]
    index <- if (x$scenario == "Two-stress baseline") 1 else 2
    suffix <- if (x$additional_starts > 0) paste0("_starts_", x$additional_starts) else ""
    paths <- paste0("paper1/output/sensitivity_", x$sensitivity_type, "_scenario_", index,
                    "_scale_", c(1, 10), suffix, ".csv")
    raw <- do.call(rbind, lapply(paths, function(path) {
      z <- read_versioned(path)
      z[z$sensitivity_value == x$sensitivity_value, ]
    }))
    stopifnot(nrow(raw) == 400,
              sum(raw$optimizer_convergence != 0) == x$optimizer_warnings)
  }
}

manuscript <- paste(readLines("paper1/manuscript/manuscript.tex", warn = FALSE), collapse = "\n")
inputs <- regmatches(manuscript, gregexpr("\\\\input\\{[^}]+\\}", manuscript))[[1]]
inputs <- setdiff(sub("^\\\\input\\{", "", sub("\\}$", "", inputs)), "result_macros.tex")
expected_header <- paste("% Model version:", paper1_model_version)
table_paths <- file.path("paper1/manuscript", inputs)
stopifnot(all(vapply(table_paths, function(path) {
  identical(readLines(path, n = 1, warn = FALSE), expected_header)
}, logical(1))))
active_tables <- paste(unlist(lapply(file.path("paper1/manuscript", inputs),
                                    readLines, warn = FALSE)), collapse = "\n")
stopifnot(!grepl("Myopic|Batch-full|RMSE 1|RMSE 10", active_tables),
          !grepl("-0\\.0([^0-9]|$)", active_tables),
          !grepl("[0-9]e-[0-9]", active_tables))

writeLines(c(
  paste("Model version:", paper1_model_version),
  "Current-prior raw results are identical to the main results for Baseline and Sparse failures.",
  "Table 5 and Table A.17 use identical RMSE values for shared policies.",
  "All reported RMSE and Monte Carlo SE values match independent calculations from raw errors.",
  "Active tables contain no legacy policy labels, negative zero, or plain-text scientific notation.",
  if (file.exists(common_path)) "Fixed-pilot regret macros match the common-posterior outputs." else
    "Fixed-pilot regret outputs were not present for this validation run."
), "paper1/output/second_round_consistency.txt")
