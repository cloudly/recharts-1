echart_stat_line = function ( stat, name = NULL){
  # stat is the statisitic of such hline,
  check_stat(stat)
  if (length(stat) > 1) stop("Only one stat each time.")
  list(markLine= list(data = list(type = stat, name = name )))
}


# a more general function to add any line
echart_abline_point = function ( start, end, name = NULL){
  # start is a vector with two values (x,y)
  # end is a vector with two values (x,y)
  if (!is.numeric(start) | !is.numeric(end)) stop("start and end should be numeric vectors.")
  if (length(start)!=2 | length(end) != 2) stop("start and end should be a vector of two numeric values.")
  if (Inf %in% c(start, end)) {
    warning("Cannot draw infinitie line.") # check if echart can handle Inf; if not, replace Inf

    }
  list(markLine =
         list(data =
                c(list( name = 'line start', value = 1, x = start[1], y = start[2]),
                               list( name = 'line end',  x = end[1], y = end[2]))
                      ))

}

# wrap of abline(); add line by intercept and slope
echart_abline = function ( intercept, slope, name = NULL,
                                 # put some defaults here, could be extended
                                 xmin = -100,
                                 xmax = 100,
                                 ymin = -100,
                                 ymax = 100){
  # start is a vector with two values (x,y)
  # end is a vector with two values (x,y)
  y_min_actual = xmin * slope + intercept
  y_max_actual = xmax * slope + intercept

  start = c(xmin, y_min_actual)
  end = c(xmax, y_max_actual)

  # ignore ymin and ymax for now
  echart_abline_point(start, end, name = name)
}

# wrap function for horizontal line
echart_hline = function (yintercept, name = NULL){
  if (!is.numeric(y)) stop("yintercept must be numeric.")
  if (length(yintercept)>1) stop("only one line each time.")
  echart_abline(intercept = yintercept, slope = 0)
}

# wrap function for vertical line

echart_vline = function (xintercept, name = NULL){
  if (!is.numeric(y)) stop("yintercept must be numeric.")
  if (length(xintercept)>1) stop ("one line each time.")
  start = c(xintercept, -Inf)
  end = c(xintercept, Inf)
  echart_abline_point(start, end, name = name)
}

#choose statistic
check_stat = function (stat) {
  if(!stat %in% c( "max", "min", "average")) stop("Statistic line should be either max, min or average.")
}
