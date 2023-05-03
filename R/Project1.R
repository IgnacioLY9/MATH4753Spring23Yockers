#' @title ntickets
#'
#' @param N number of seats on the plane
#' @param gamma probability of overbooking
#' @param p probability a person shows up for the flight
#'
#'@importFrom graphics abline points
#'@importFrom stats optimize pbinom
#'
#' @return returns number of tickets to be sold using normal and binomial distributions
#' @export
#'
#' @examples
#' ntickets(N = 200, gamma = .02, p = .05)

ntickets <- function(N, gamma, p) {
  nprime <- N*10
  n <- N:nprime

  xlist <- which.min(abs(1-gamma-pbinom(N, n, p)))
  vert <- N + xlist - 1
  plot(1 - gamma - pbinom(N, n, p) ~ n, bg = "blue", pch = 21, type = "b", xlab = "n", ylab = "Objective",
       main = paste("Objective VS n to find optimal tickets sold\n(",vert, ") gamma = ", gamma, ", N = ",
                    N, " discrete", sep = ""), xlim = c(vert*.975, vert*1.073))
  abline(v=vert, col = "red", lwd = 2)
  abline(h=0, col = "red", lwd = 2)
  points(vert,1 - gamma - pbinom(N, vert, p), pch = 21, bg = "Red")
  points(vert,1 - gamma - pbinom(N, vert, p), pch = 21, bg = "Red")

  f <- function(x) {
    abs(1 - gamma - pnorm(N + .5, x * p, sqrt(x*p*(1-p))))
  }
  op <- optimize(f, interval = c(vert*.975, vert*1.073))
  curve(1 - gamma - pnorm(N + .5, x * p, sqrt(x*p*(1-p))), xlim = c(vert*.975, vert*1.073), xlab = "n", ylab = "Objective",
        main = paste("Objective VS n to find optimal tickets sold\n(",op$minimum, ") gamma = ", gamma, ", N = ",
                     N, " continuous", sep = ""))
  abline(v=op$minimum, col = "blue")
  abline(h=0, col = "blue")

  resultlist <- list(nd = vert, nc = op$minimum, N = N, p = p, gamma = gamma)
  resultlist
}
