# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL) {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = 'scatter', data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'scatter', data = unname(as.matrix(xy[[i]])))
  }
  obj
}

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
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'bar', data = unname(unlist(xy[[i]])))
  }
  obj
}

data_line = function(x, y, series = NULL) {
  ### if no series passed... go with only one series.
  if (is.null(series)) {
    warning("No series specified for bar plot.")
    return(list(list(name = '', type = 'line', data = y)))
  }
  #otherwise, go with series.
  xy = y ### why no names?
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'line', data = unname(unlist(xy[[i]])))
  }
  obj
}
