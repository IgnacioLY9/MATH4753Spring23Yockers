#' Plot function
#'
#' @param x vector with horizontal values
#' @param y vector with vertical values
#' @param title main title of the graph
#' @param xlabel x label of the graph
#' @param ylabel y label of the graph
#'
#' @return a scatter plot with with points corresponding to ordered pairs made from the x and y vectors
#' @export
#'
#' @examples
#' \dontrun{fancyplot(spruce$BHDiameter, spruce$Height,
#'   title = "class", xlabel = "is", ylabel = "fun")}
fancyplot <- function(x, y, title, xlabel, ylabel) {
  plot(y ~ x, main = title, xlab = xlabel, ylab = ylabel, pch = 21, bg = "BLUE", xlim = c(0, 1.1*max(x)),
       ylim = c(0, 1.1*max(y)), cex = 1.2)
}
