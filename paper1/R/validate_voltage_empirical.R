source("paper1/R/voltage_empirical_model.R")

ratio_scale <- log(120 / 80)
first <- voltage_inputs(1)
second <- voltage_inputs(ratio_scale)
stopifnot(max(abs(second$historical$x - ratio_scale * first$historical$x)) < 1e-12,
          abs(second$x_use - ratio_scale * first$x_use) < 1e-12,
          max(abs(second$prior_mean - first$prior_mean * c(1, 1 / ratio_scale, 1))) < 1e-10,
          max(abs(second$prior_sd - first$prior_sd * c(1, 1 / ratio_scale, 1))) < 1e-12)

results <- read.csv("work/exploratory/voltage_case/voltage_empirical_results.csv")
stopifnot(nrow(results) == 6, all(is.finite(results$estimate)),
          all(results$convergence == 0), all(results$elapsed > 0))
for (i in seq_len(nrow(results))) {
  times <- as.numeric(strsplit(results$schedule[i], ";", fixed = TRUE)[[1]])
  stopifnot(length(times) == 5, all(diff(times) > 0), all(times <= 2500))
}

source_text <- paste(readLines("paper1/R/voltage_empirical_model.R", warn = FALSE),
                     collapse = "\n")
new_match <- regexpr("voltage_new_posterior <- function[\\s\\S]*?\\n\\}", source_text,
                     perl = TRUE)
new_body <- regmatches(source_text, new_match)
stopifnot(!grepl("voltage_exact_log_likelihood", new_body, fixed = TRUE))

summary <- read.csv("work/exploratory/voltage_case/voltage_empirical_summary.csv")
stopifnot(nrow(summary) == 3, identical(summary$policy,
                                        c("Trace-full", "Weighted-full", "Full")))
loo <- read.csv("work/exploratory/voltage_case/voltage_loo_summary.csv")
bootstrap <- read.csv("work/exploratory/voltage_case/voltage_bootstrap_summary.csv")
stopifnot(nrow(loo) == 3, all(loo$folds == 8), all(loo$all_converged),
          nrow(bootstrap) == 3, all(bootstrap$replicates == 200),
          all(bootstrap$convergence_failures == 0), all(bootstrap$undetected_pairs == 0))
table <- paste(readLines("paper1/manuscript/voltage_case_table.tex", warn = FALSE),
               collapse = "\n")
normalized_results <- results[results$coding == "normalized", ]
for (i in seq_len(nrow(normalized_results))) {
  x <- normalized_results[i, ]
  if (!grepl(gsub(";", ", ", x$schedule), table, fixed = TRUE)) {
    stop("Voltage schedule mismatch: ", x$schedule)
  }
            stopifnot(grepl(sprintf("%.3f", x$estimate), table, fixed = TRUE))
}
writeLines(c("Voltage empirical-prior experiment checks passed.",
             "Six rolling fits are finite and converged.",
             "Both codings have exactly transformed stress, target, and empirical prior.",
             "The new-data posterior does not reuse the historical likelihood."),
           "work/exploratory/voltage_case/validation.txt")
print(summary, row.names = FALSE)
