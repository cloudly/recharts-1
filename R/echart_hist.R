#' Create an histogram
#'
#' @param a numeric vector
#' @rdname echart_hist
#' @export
#' @examples library(recharts)
#' echart_hist(rnorm(100))

#' @export
#' @rdname echart_hist
echart_hist = function(data, binwidth = NULL){
  if (!is.vector(data)) stop("Histogram only takes vectors.")
  if (!is.numeric(data)) stop ("Histogram needs a numeric vector.")
  if(is.null(binwidth)) {
    warning("Bin width is not specified. Default is Sturges's formula.")
    bar_hist = hist(data, plot = FALSE )
  } else{
    bar_hist = hist(data, plot = FALSE , binwidth = binwidth)
  }

  echart(bar_hist, ~breaks, ~counts,  type ="bar", barCategoryGap='0%')
}
