#' Create an axis for a chart
#'
#' Add an axis to a chart.
#'
#' This function modified a few default options for the axis component in
#' ECharts: 1) \code{scale = TRUE} (was \code{FALSE} by default in ECharts); 2)
#' \code{axisLine$onZero = FALSE} (was \code{TRUE} in ECharts).
#' @export
#' @rdname axis
eAxis = function(
  chart, which = c('x', 'y'),
  type = c('value', 'category', 'time'), show = TRUE,
  position = c('bottom', 'top', 'left', 'right'),
  name = '', nameLocation = c('end', 'start'), nameTextStyle = emptyList(),
  boundaryGap = c(0, 0), min = NULL, max = NULL, scale = TRUE, splitNumber = NULL,
  axisLine = list(show = TRUE, onZero = FALSE), axisTick = list(show = FALSE),
  axisLabel = list(show = TRUE), splitLine = list(show = TRUE),
  splitArea = list(show = FALSE), data = list()
) {
  which = match.arg(which)
  odata = getMeta(chart)[[which]]  # original data along the axis
  if (missing(type)) type = axisType(odata, which)
  if (missing(position)) position = if (which == 'x') 'bottom' else 'left'
  if (missing(data) && type == 'category') {
    data = I(levels(as.factor(odata)))
  }

  x = chart$x
  i = paste0(which, 'Axis')
  o = list(
    type = match.arg(type), show = show, position = match.arg(position),
    name = name, nameLocation = match.arg(nameLocation), nameTextStyle = nameTextStyle,
    boundaryGap = boundaryGap, min = min, max = max, scale = scale,
    splitNumber = splitNumber, axisLine = axisLine, axisTick = axisTick,
    axisLabel = axisLabel, splitLine = splitLine, splitArea = splitArea, data = data
  )
  if (length(x[[i]])) {
    # only merge the arguments that are not missing, e.g. eAxis(min = 0) will
    # only override 'min' but will not override the 'name' attribute
    a = intersect(names(as.list(match.call()[-1])), names(o))#;browser()
    x[[i]] = mergeList(x[[i]], o[a])
  } else {
    x[[i]] = mergeList(x[[i]], o)
  }
  chart$x = x

  chart
}

#' @export
#' @rdname axis
eXAxis = function(chart, ...) {
  eAxis(chart, which = 'x', ...)
}

#' @export
#' @rdname axis
eYAxis = function(chart, ...) {
  eAxis(chart, which = 'y', ...)
}

axisType = function(data, which = c('x', 'y')) {
  if (is.numeric(data) || is.null(data)) return('value')
  if (is.factor(data) || is.character(data)) return('category')
  if (inherits(data, 'Date')) return('time')
  message('The structure of the ', which, ' variable:')
  str(data)
  stop('Unable to derive the axis type automatically from the ', which, ' variable')
}

#' Modify series properties
#'
#' Modify series properties
#'
#' @export

eSeries = function (chart,
                    which = 'all' , # wich should be series name or index? go with index now
                    stack = NULL, # binary T or F
                    barGap = NULL, # a number between 0 and 1
                    barCategoryGap = NULL, # a number between 0 and 1
                    barMinHeight = NULL,  #  a number >= 0
                    barWidth = NULL, # a number >= 0
                    barMaxWidth = NULL, # a number >= 0
                    symbol = NULL, # choose from 'circle' | 'rectangle' | 'triangle' | 'diamond' | 'emptyCircle' | 'emptyRectangle' | 'emptyTriangle' | 'emptyDiamond'| heart' | 'droplet' | 'pin' | 'arrow' | 'star'
                    symbolSize = NULL, # a number >= 0
                    showAllSymbol = NULL, # T or F
                    symbolRotate = NULL, # between -180 and 180
                    smooth = NULL,  # T or F
                    dataFilter = NULL,
                    large = NULL,  # T or F, use large scatter plot?
                    largeThreshold = NULL, # a number > 0 for large scatter plot
                    legendHoverLink= NULL, # highlight when hover on legend? T or F
                    ...){
  # change all series
  if (which == "all") series = chart$x$series else series = chart$x$series[which]

    # usage of stack: echart allows to stack any bar but here we stack for now.
    if (isTRUE(stack)) {
      series = lapply(list(series), function(x) {
       mergeList(x, eSeries_add_parameter(name = "stack", value = "grand total"))
      })
    }
    if (!is.null(barGap)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "barGap", value = percent_scale(barGap)))
    })
    if (!is.null(barCategoryGap)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "barCategoryGap", value = percent_scale(barCategoryGap)))

    })
    if (!is.null(barMinHeight) && barMinHeight >= 0) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "barMinHeight", value = barMinHeight))
    })
    if (!is.null(barWidth)  && barWidth >= 0) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "barWidth", value = barWidth))
    })
    if (!is.null(barMaxWidth)  && barMaxWidth >= 0) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "barMaxWidth", value = barMaxWidth))
    })
    # symbol also supports pictures? ignore for now.
    if (!is.null(symbol)  &&
        symbol %in% c('circle' , 'rectangle' , 'triangle' , 'diamond' , 'emptyCircle' ,
                      'emptyRectangle' , 'emptyTriangle' , 'emptyDiamond', 'heart' ,
                      'droplet' , 'pin' , 'arrow' , 'star')) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "symbol", value = symbol))
    })
    # also handle symbolsize for bubble plots?
    if (!is.null(symbolSize)  && symbolSize >= 0) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "symbolSize", value = symbolSize))
    })
    if (!is.null(symbolRotate)  && symbolRotate <= 180  && symbolRotate >= -180) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "symbolRotate", value = symbolRotate))
    })
    if (isTRUE(showAllSymbol)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "showAllSymbol", value = showAllSymbol))
    })
    if (isTRUE(smooth)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "smooth", value = smooth))
    })
    if (isTRUE(large)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "large", value = large))
    })
    if (!is.null(largeThreshold) && largeThreshold >= 0 ) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "largeThreshold", value = largeThreshold))
    })
    if (isTRUE(legendHoverLink)) series = lapply(series, function(x) {
      mergeList(x, eSeries_add_parameter(name = "legendHoverLink", value = legendHoverLink))
    })

  if (which == "all") chart$x$series = series  else chart$x$series[which] = series

  return(chart)
}

eSeries_add_parameter = function (name, value){
  temp = list(name = value)
  names(temp) = name
  return(temp)
}

percent_scale = function(number){
  if(! (number >=0 & number <=1)) stop("Number should be between 0 and 1.")
  paste0(number * 100, "%")
}

#' Add lines to graph
#'
#' Add lines to graph
#'
#' @export
eSeries_addline = function (chart, which = "all",
                            line_stat = NULL, # add a statistical line, choose from min, max, average
                            ...){

  if (which == "all") series = chart$x$series else series = chart$x$series[which]
  if (!is.null(line_stat)) series = lapply(series, function(x) {
    mergeList(x, echart_stat_line(stat = line_stat, name = line_stat))
  })

  if (which == "all") chart$x$series = series  else chart$x$series[which] = series

  return(chart)
}
