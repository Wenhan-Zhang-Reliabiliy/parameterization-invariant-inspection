source("paper1/R/model_version.R")

rmse_mcse <- function(error) {
  n <- length(error)
  mse <- mean(error^2)
  if (n < 2 || mse <= 0) return(NA_real_)
  sd(error^2) / (2 * sqrt(n * mse))
}
