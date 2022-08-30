#' @importFrom stats fisher.test
fisher_test <- function(x) {
  tryres <- try({
    stats::fisher.test(x)
  }, silent = TRUE)
  if (inherits(tryres, "try-error")) {
    tryres <- try({
      stats::fisher.test(x, simulate.p.value = TRUE)
    }, silent = TRUE)
    if (inherits(tryres, "try-error"))
      return(NA)
  }
  return(tryres$p.value)
}

#' Get a credible interval for a fraction
#' @param frac the fraction
#' @param tot the denominator used to compute the fraction
#' @param limit the percent confidence limit (0-100)
#' @importFrom stats qbeta
#' @noRd
get_interval <- function(frac, tot, limit = 90) {
  tail <- (1 - limit / 100) / 2
  num <- frac * tot
  denom <- tot
  a_star <- num + 0.5 # a = 0.5
  b_star <- denom - num + 0.5 # b = 0.5
  lower <- (stats::qbeta(tail, a_star, b_star)) * 100
  upper <- (stats::qbeta(1 - tail, a_star, b_star)) * 100
  c(lower, upper)
}
