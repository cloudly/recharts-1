#' Create an ECharts widget
#'
#' Create an HTML widget for ECharts that can be rendered in the R console, R
#' Markdown documents, or Shiny apps. You can add more components to this widget
#' and customize options later. \code{eChart()} is an alias of \code{echart()}.
#' @param data a data object (usually a data frame or a list)
#' @rdname eChart
#' @export
#' @examples library(recharts)
#' ### scatter plot
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width)
#' echart(iris, ~ Sepal.Length, ~ Sepal.Width, series = ~ Species)
#'
#' # bar chart
#' bar_df = data.frame(
#'    date = rep(paste("day",1:10), 2),
#'    temperature = floor(rnorm(n = 20, mean = 20, sd = 10)),
#'    location = rep(c("NY","DC"), each = 10)
#'   )
#' echart(bar_df, ~date, ~temperature, ~location)
#'
#' #line chart
#' echart(bar_df, ~date, ~temperature, ~location, type="line")
echart = function(data, ...) {
  UseMethod('echart')
}

#' @export
#' @rdname eChart
echart.list = function(data, width = NULL, height = NULL, ...) {
  htmlwidgets::createWidget(
    'echarts', data, width = width, height = height, package = 'recharts'
  )
}

#' @param x the x variable
#' @param y the y variable
#' @export
#' @rdname eChart
echart.data.frame = function(
  data = NULL, x = NULL, y = NULL, series = NULL, type = 'auto',
  width = NULL, height = NULL, ...
) {

  xlab = autoArgLabel(x, deparse(substitute(x)))
  ylab = autoArgLabel(y, deparse(substitute(y)))

  x = evalFormula(x, data)
  y = evalFormula(y, data)
  if (type == 'auto') type = determineType(x, y)
<<<<<<< HEAD
  if (type == 'bar') {
    x = as.factor(x)
    if (is.null(y)) ylab = 'Frequency'
  }
=======

  # for histogram, change it to bar plot with no space between bars
  hist_indicator = 0 # need a tag here for histogram
  if (type == 'hist') {
    hist_indicator = 1 # use 1 for histograms
    type = 'bar'
  }
  # for bar plot, convert x  to factors
  if (type == 'bar' && !is.factor(x)) x = as.factor(x)
  if (type == 'bar' && !is.numeric(y)) stop("y must be numeric for bar plot.")
<<<<<<< HEAD
>>>>>>> + echart_hist() to generate simple histograms. I need to deal with parameters in data_bar() because of this so there should be a better way to handle these parameters. Any idea?
=======
>>>>>>> 21e97e21cece1099164de51398a02dce842a830c
>>>>>>> 5fbbb818e3e6798c0e6df841844478ceb4438484

  series = evalFormula(series, data)
  data_fun = getFromNamespace(paste0('data_', type), 'recharts')

<<<<<<< HEAD
=======
  # start axis from 0?
  if (is.numeric(x)) min_xaxis = ifelse( min(x) >0, 0, min(x))
  if (is.numeric(y)) min_yaxis = ifelse( min(y) >0, 0, min(y))

<<<<<<< HEAD
>>>>>>> + echart_hist() to generate simple histograms. I need to deal with parameters in data_bar() because of this so there should be a better way to handle these parameters. Any idea?
=======
>>>>>>> 21e97e21cece1099164de51398a02dce842a830c
>>>>>>> 5fbbb818e3e6798c0e6df841844478ceb4438484
  params = structure(list(
    # any better way here? only pass a parameter if it exists
    series = ifelse(hist_indicator ==1,
                    data_fun(x, y, series, barCategoryGap='0%'),
                    data_fun(x, y, series)),
    xAxis = list(), yAxis = list()
  ), meta = list(
    x = x, y = y
  ), TOJSON_ARGS = list(pretty = TRUE))

  if (!is.null(series)) {
    params$legend = list(data = levels(as.factor(series)))
  }

  chart = htmlwidgets::createWidget(
    'echarts', params, width = width, height = height, package = 'recharts',
    dependencies = getDependency(NULL)
  )

  chart %>% eAxis('x', name = xlab) %>% eAxis('y', name = ylab)
}

#' @export
#' @rdname eChart
echart.default = echart.data.frame

#' @export
#' @rdname eChart
eChart = echart
# from the planet of "Duo1 Qiao1 Yi1 Ge4 Jian4 Hui4 Si3" (will die if having to
# press one more key, i.e. Shift in this case)

determineType = function(x, y) {
  if (is.numeric(x) && is.numeric(y)) return('scatter')
<<<<<<< HEAD
  # when y is numeric, plot y against x; when y is NULL, treat x as a
  # categorical variable, and plot its frequencies
  if ((is.factor(x) || is.character(x)) && (is.numeric(y) || is.null(y)))
    return('bar')
  # FIXME: 'histogram' is not a standard plot type of ECharts
  # http://echarts.baidu.com/doc/doc.html
  if (is.numeric(x) && is.null(y)) return('histogram')
=======
  if (is.factor(x) && is.numeric(y)) return("bar")
  # use echart_hist() for histograms
  # if (is.numeric(x) && is.null(y)) return("histogram")
<<<<<<< HEAD
>>>>>>> + echart_hist() to generate simple histograms. I need to deal with parameters in data_bar() because of this so there should be a better way to handle these parameters. Any idea?
=======
>>>>>>> 21e97e21cece1099164de51398a02dce842a830c
>>>>>>> 5fbbb818e3e6798c0e6df841844478ceb4438484
  message('The structure of x:')
  str(x)
  message('The structure of y:')
  str(y)
  stop('Unable to determine the chart type from x and y automatically')
}

# not usable yet; see https://github.com/ecomfe/echarts/issues/1065
getDependency = function(type) {
  if (is.null(type)) return()
  htmltools::htmlDependency(
    'echarts-module', EChartsVersion,
    src = system.file('htmlwidgets/lib/echarts/chart', package = 'recharts'),
    script = sprintf('%s.js', type)
  )
}

getMeta = function(chart) {
  attr(chart$x, 'meta', exact = TRUE)
}
