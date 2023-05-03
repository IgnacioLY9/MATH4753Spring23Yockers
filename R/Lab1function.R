#' layeredbarplot
#'
#' @param x a vector
#' @param y a vector
#'
#'@importFrom graphics barplot
#'
#' @return a layered bar plot of x and y values
#' @export
#'
#' @examples
#' \dontrun{layeredbarplot(ddt$SPECIES, ddt$RIVER)}
layeredbarplot <- function (x, y) {
  tab <- table(x, y)
  barplot(tab)
}
