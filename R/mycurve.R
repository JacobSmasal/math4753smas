#' Title mycurve
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
mycurve = function(mu=0, sigma=1, a=1.96){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma), main = "Normal Distribution Curve")
  polygon(c(seq(mu-3*sigma, a, length.out=100), a), c(rep(0, 100), dnorm(seq(mu-3*sigma, a, length.out=100), mean=mu, sd=sigma)), col="lightblue")
  area <- integrate(function(x) dnorm(x, mean=mu, sd=sigma), -Inf, a)$value
  cat("Probability (P(X <= a)): ", area, "\n")
  list(mu = mu, sigma = sigma, area = area)
}

mycurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma), main = "Normal Distribution Curve")
  polygon(c(seq(mu-3*sigma, a, length.out=100), a), c(rep(0, 100), dnorm(seq(mu-3*sigma, a, length.out=100), mean=mu, sd=sigma)), col="lightblue")
  area <- integrate(function(x) dnorm(x, mean=mu, sd=sigma), -Inf, a)$value
  cat("Probability (P(X <= a)): ", area, "\n")
  list(mu = mu, sigma = sigma, area = area)
}
