paper1_model_version <- "weibull-cloglog-z0-v2"

with_model_version <- function(x) {
  x$model_version <- paper1_model_version
  x
}

assert_model_version <- function(x, path = "result object") {
  stopifnot("model_version" %in% names(x))
  if (any(x$model_version != paper1_model_version)) {
    stop(path, " contains results from a different or missing model version")
  }
  invisible(TRUE)
}

write_model_table <- function(lines, path) {
  writeLines(c(paste("% Model version:", paper1_model_version), lines), path)
}
