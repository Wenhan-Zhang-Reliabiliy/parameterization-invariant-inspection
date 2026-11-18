source("paper1/R/model_version.R")

summary <- read.csv("paper1/output/engineering_detection_summary.csv")
assert_model_version(summary, "engineering_detection_summary")
stopifnot(nrow(summary) == 6, all(summary$undetected_replicates == 0),
          all(summary$pilot_detected_replicates == c(927, 927, 927, 925, 925, 925)),
          all(summary$post_pilot_replicates == c(73, 73, 73, 75, 75, 75)))
trace <- summary[summary$policy == "Trace-full", ]
invariant <- summary[summary$policy != "Trace-full", ]
stopifnot(max(abs(trace$post_pilot_percent_over_1h - c(100, 98.66667))) < 1e-4,
          all(invariant$post_pilot_percent_over_1h == 0))

manuscript <- paste(readLines("paper1/manuscript/manuscript.tex", warn = FALSE),
                    collapse = "\n")
stopifnot(grepl("no additional observation", manuscript, ignore.case = TRUE),
          grepl("condition on $\\beta$ at its prior mean", manuscript, fixed = TRUE),
          grepl("B_{\\eta}=A^{-\\mathsf T}B_{\\theta}A^{-1}", manuscript, fixed = TRUE),
          grepl("\\eqref{eq:weightedtrace}", manuscript, fixed = TRUE),
          !grepl("survivors at 600 h|motorette|Arrhenius", manuscript,
                 ignore.case = TRUE))

observed_source <- readLines("paper1/R/sequential_inspection.R", warn = FALSE)
start <- which(grepl("^observed_counts <- function", observed_source))
end <- which(grepl("^}$", observed_source))
end <- end[end > start][1]
observed_body <- paste(observed_source[start:end], collapse = "\n")
stopifnot(grepl("breaks <- c(0, inspections, Inf)", observed_body, fixed = TRUE),
          !grepl("end_time", observed_body, fixed = TRUE))

if (file.exists("paper1/R/case_motorette.R")) {
  case_source <- paste(readLines("paper1/R/case_motorette.R", warn = FALSE), collapse = "\n")
  case_match <- regexpr("case_log_posterior <- function[\\s\\S]*?\\n\\}", case_source,
                        perl = TRUE)
  case_body <- regmatches(case_source, case_match)
  historical_match <- regexpr("historical_log_posterior <- function[\\s\\S]*?\\n\\}",
                              case_source, perl = TRUE)
  historical_body <- regmatches(case_source, historical_match)
  stopifnot(!grepl("exact_log_likelihood", case_body, fixed = TRUE),
            grepl("exact_log_likelihood", historical_body, fixed = TRUE))
}

writeLines(c("RESS review checks passed.",
             "Pilot-conditioned detection denominators and percentages match saved outputs.",
             "No undetected pairs remained at the final inspection.",
             "Terminal observation, conditional beta analysis, and full-precision invariance are described consistently.",
             "The motorette example is removed from the current manuscript; its local corrected implementation is checked when available."),
           "paper1/output/ress_review_validation.txt")
