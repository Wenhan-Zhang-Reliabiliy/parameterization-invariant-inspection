source("paper1/R/model_version.R")

format_fixed <- function(x, digits) {
  rounded <- round(x, digits)
  rounded[abs(rounded) < 0.5 * 10^(-digits)] <- 0
  sprintf(paste0("%.", digits, "f"), rounded)
}

tex_scientific <- function(x, digits = 2) {
  vapply(x, function(value) {
    if (!is.finite(value)) return("NA")
    if (value == 0) return("$0$")
    exponent <- floor(log10(abs(value)))
    mantissa <- value / 10^exponent
    sprintf("$%.*f\\times10^{%d}$", digits, mantissa, as.integer(exponent))
  }, character(1))
}
