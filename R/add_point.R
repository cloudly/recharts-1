# function to add points on plot
echart_point = function (x, y, name = NULL, value = NULL){
  if (!is.numeric(x) | !is.numeric(y)) { stop("x and y should be numeric.")}
  list(markPoint = list(data = list(name = name, value =  value, x = x, y = y)))
}

# add statistic point directly
echart_stat_point = function (stat, name = NULL, value = NULL){
  check_stat(stat)
  list(markPoint = list(data = list(name = name, type = stat)))
}

check_stat = function (stat){
  if (!stat %in% c("min","max","average")){
    stop("stat should be either min, max or avergae.")
  }
}

