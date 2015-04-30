# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL, type = 'scatter') {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = type, data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(as.matrix(xy[[i]])))
  }
  obj
}

<<<<<<< HEAD
data_bar = function(x, y, series = NULL, type = 'bar') {

  # plot the frequencies of x when y is not provided
  if (is.null(y)) {

    if (is.null(series)) {
      y = table(x)
      return(list(list(type = type, data = unname(c(y)))))
    }

    y = table(x, series)
    nms = colnames(y)
    obj = list()
    for (i in seq_len(ncol(y))) {
      obj[[i]] = list(name = nms[i], type = type, data = unname(y[, i]))
    }
    return(obj)

  }

  # when y is provided, use y as the height of bars
  if (is.null(series)) {
    return(list(list(type = type, data = y)))
  }

  xy = tapply(y, list(x, series), function(z) {
    if (length(z) == 1) return(z)
    stop('y must only have one value corresponding to each combination of x and series')
  })
  nms = colnames(xy)
=======
data_bar = function(x, y, series = NULL,
                    name = NULL, #the series name?
                    stack = NULL,
                    barGap = NULL,
                    barCategoryGap = NULL,
                    barMinHeight = NULL,
                    barWidth = NULL,
                    barMaxWidth = NULL
                    ) {
  ### if no series passed... go with only one series.
  if (is.null(series)) {
    warning("No series specified for bar plot.")
    series_data = list(name = '', type = 'bar', data = y)
    if(!is.null(barCategoryGap)) series_data = c(series_data , barCategoryGap = barCategoryGap)
    if(!is.null(barMinHeight)) series_data = c(series_data , barMinHeight = barMinHeight)
    if(!is.null(barGap)) series_data = c(series_data , barGap = barGap)
    if(!is.null(barWidth)) series_data = c(series_data , barWidth = barWidth)
    if(!is.null(barMaxWidth)) series_data = c(series_data , barMaxWidth = barMaxWidth)
    return(list(series_data))
  }
  #otherwise, go with series.
  xy = split(as.data.frame(y), series)
  nms = names(xy)
<<<<<<< HEAD
>>>>>>> + echart_hist() to generate simple histograms. I need to deal with parameters in data_bar() because of this so there should be a better way to handle these parameters. Any idea?
=======
>>>>>>> 21e97e21cece1099164de51398a02dce842a830c
>>>>>>> 5fbbb818e3e6798c0e6df841844478ceb4438484
  obj = list()
  for (i in seq_len(ncol(xy))) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(xy[, i]))
  }
  obj

}

data_line = function(x, y, series = NULL) {
  if (is.numeric(x)) {
    return(data_scatter(x, y, series, type = 'line'))
  }
  if (is.null(series)) {
    return(list(list(type = 'line', data = y)))
  }
  data_bar(x, y, series, type = 'line')
}
