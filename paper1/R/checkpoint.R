source("paper1/R/model_version.R")

run_with_checkpoints <- function(replicates, run_replicate, output, log_output,
                                 chunk_size = 50, resume = FALSE) {
  completed <- NULL
  if (resume && file.exists(output)) {
    completed <- read.csv(output)
    assert_model_version(completed, output)
  }
  first <- if (is.null(completed) || nrow(completed) == 0) 1 else max(completed$replicate) + 1
  if (first > replicates) return(completed)
  chunks <- seq(first, replicates, by = chunk_size)
  for (start in chunks) {
    end <- min(start + chunk_size - 1, replicates)
    current <- with_model_version(do.call(rbind, lapply(start:end, run_replicate)))
    completed <- rbind(completed, current)
    write.csv(completed, output, row.names = FALSE)
    message <- paste(format(Sys.time()), "completed", end, "of", replicates, "replicates")
    cat(message, "\n", file = log_output, append = TRUE)
    flush.console()
  }
  completed
}
