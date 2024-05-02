#' Title ntickets
#'
#' @param N N variable
#' @param gamma gamma variable
#' @param p p variable
#'
#' @return plots and lists
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {
  # Function to calculate number of tickets using discrete distribution
  nd_func <- function(n) {
    sum(dbinom((n+1):N, N, p))
  }

  # Function to calculate number of tickets using normal approximation
  nc_func <- function(n) {
    1 - gamma - pnorm((n - N*p)/sqrt(N*p*(1-p)), lower.tail = FALSE)
  }

  # Calculate nd using discrete distribution
  nd <- sapply(0:(N-1), nd_func)

  # Calculate nc using normal approximation
  nc <- sapply(0:(N-1), nc_func)

  # Find n for discrete distribution
  n_discrete <- which.min(abs(N - nd))
  nd <- nd[n_discrete]

  # Find n for normal approximation
  n_continuous <- which.min(abs(N - nc))
  nc <- nc[n_continuous]

  # Create objective functions for both cases
  obj_func_discrete <- function(n) 1 - gamma - pbinom(n, N, p)
  obj_func_continuous <- function(n) 1 - gamma - pnorm((n - N*p)/sqrt(N*p*(1-p)), lower.tail = FALSE)

  # Plot objective functions vs n
  plot(0:(N-1), obj_func_discrete(0:(N-1)), type = "l", col = "blue", xlab = "n", ylab = "Objective function", main = "Objective function vs n (Discrete)")
  lines(0:(N-1), obj_func_continuous(0:(N-1)), col = "red")
  legend("topright", legend = c("Discrete", "Continuous"), col = c("blue", "red"), lty = 1)

  # Print named list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
