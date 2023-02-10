#' Title
#'
#' @param x vector containing data points
#'
#' @importFrom stats sd
#'
#' @return vector containing respecting z scores of elements in the input vector
#' @export
#'
#' @examples
#' findZ(1:20)
findZ <- function(x) {
  x-mean(x)/sd(x)
}
