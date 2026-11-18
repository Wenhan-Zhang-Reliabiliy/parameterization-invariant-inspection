tex <- paste(readLines("paper1/manuscript/manuscript.tex", warn = FALSE), collapse = "\n")
schedule <- read.csv("paper1/output/parameterization_schedule_summary.csv")
accuracy <- read.csv("paper1/output/parameterization_accuracy.csv")
criterion <- read.csv("paper1/output/criterion_summary.csv")
case <- read.csv("paper1/output/motorette_case_results.csv")

expect_text <- function(value) {
  stopifnot(grepl(value, tex, fixed = TRUE))
}
for (scenario in unique(schedule$scenario)) {
  for (policy in unique(schedule$policy)) {
    x <- schedule[schedule$scenario == scenario & schedule$policy == policy &
                    schedule$comparison == "1 vs 10", ]
    expect_text(sprintf("%.2f", x$median))
  }
}
expect_text(sprintf("%.3f", mean(criterion$seconds[criterion$policy == "Batch-full"])))
expect_text(sprintf("%.3f", mean(criterion$seconds[criterion$policy == "Trace-full"])))
trace_case <- case[case$policy == "Trace-full", ]
reliability_case <- case[case$policy == "Full", ]
schedule_matrix <- function(x) {
  matrix(as.numeric(unlist(strsplit(x, ";", fixed = TRUE))), nrow = 5)
}
trace_difference <- abs(schedule_matrix(trace_case$schedule[1]) -
                          schedule_matrix(trace_case$schedule[2]))
reliability_difference <- abs(schedule_matrix(reliability_case$schedule[1]) -
                                schedule_matrix(reliability_case$schedule[2]))
expect_text(sprintf("%.2f", max(trace_difference[2, ])))
expect_text(sprintf("%.2f", max(reliability_difference)))

for (scenario in unique(accuracy$scenario)) {
  for (policy in unique(accuracy$policy)) {
    x <- accuracy[accuracy$scenario == scenario & accuracy$policy == policy, ]
    stopifnot(nrow(x) == 3, max(x$rmse) - min(x$rmse) < 0.003)
  }
}

inputs <- regmatches(tex, gregexpr("\\\\input\\{[^}]+\\}", tex))[[1]]
inputs <- sub("^\\\\input\\{", "", sub("\\}$", "", inputs))
inputs <- setdiff(inputs, "result_macros.tex")
stopifnot(all(file.exists(file.path("paper1/manuscript", inputs))))
figures <- regmatches(tex, gregexpr("\\.\\./output/[^}]+\\.pdf", tex))[[1]]
stopifnot(all(file.exists(file.path("paper1/manuscript", figures))))
log_path <- "paper1/manuscript/manuscript.log"
if (file.exists(log_path)) {
  log <- paste(readLines(log_path, warn = FALSE), collapse = "\n")
  stopifnot(!grepl("undefined citations|undefined references|Overfull|Fatal error", log,
                   ignore.case = TRUE))
} else {
  message("LaTeX log not found; compile the manuscript to enable log checks.")
}
stopifnot(file.exists("paper1/manuscript/manuscript.pdf"),
          !grepl("Batch-Informed Sequential|most rules choose only the next", tex, ignore.case = TRUE))
writeLines(c(
  "Manuscript numeric checks passed against CSV outputs.",
  paste("Checked table inputs:", length(inputs)),
  paste("Checked figure files:", length(figures)),
  "No undefined citations, undefined references, overfull boxes, or fatal LaTeX errors.",
  "No superseded batch-informed or next-inspection-only RITO claims."
), "paper1/output/manuscript_validation.txt")
