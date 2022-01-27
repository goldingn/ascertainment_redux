#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param date_num
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
# make a fake temporal trend
random_trend <- function(dates, prob = 0.5, variance = 0.3) {
  plogis(qlogis(prob) + variance * sin(dates / runif(1, 50, 100) - runif(1, 50, 100)))
}
