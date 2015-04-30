# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL) {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(series_data = list(list(type = 'scatter', data = xy))))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'scatter', data = unname(as.matrix(xy[[i]])))
  }
  obj = list(series_data = obj)
  obj
}

data_bar = function(x, y, series = NULL) {
  ### if no series passed... go with only one series.
  if (is.null(series)) {
    warning("No series specified for bar plot.")
    return(list(
                series_data = list(list(name = '', type = 'bar', data = y)),
                xaxis = unique(x)
                )
           )
  }
  # otherwise, go with series.
  xy = y
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'bar', data = unname(unlist(xy[[i]])))
  }
  obj = list(series_data = obj, xaxis = unique(x))
  obj
}

data_line = function(x, y, series = NULL) {
  ### if no series passed... go with only one series.
  if (is.null(series)) {
    warning("No series specified for bar plot.")
    return(list(
      series_data = list(list(name = '', type = 'line', data = y)),
      xaxis = unique(x)
    )
    )
  }
  #otherwise, go with series.
  xy = y ### why no names?
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = 'line', data = unname(unlist(xy[[i]])))
  }
  obj = list(series_data = obj, xaxis = unique(x))
  obj
}

data_histogram = function(x, y = NULL, series = NULL, binwidth = NULL) {
  # ignore series information.
  if (!is.null(series)) {
    warning("Series information is ignored for histogram.")
  }
  # only x. do not understand y.
  if (!is.null(y)) {
    stop("y should be null for histogram.")
  }
  if (is.null(binwidth)) {
    warning("Bin width is not specified.")
    binwidth = diff(range(x))/20 # by default return 20 bins.
    }

  x_hist = hist(x, plot = F, breaks = diff(range(x))/binwidth)
  # only return bin and counts
  # x_bar = list(x_hist$breaks, x_hist$counts)
  obj = list(list(name = '', type = 'bar', data = unname(x_hist$counts)))
  obj = list(series_data = obj, xaxis = x_hist$breaks+binwidth/2)
  obj
}
