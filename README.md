# recharts

[![Build Status](https://travis-ci.org/yihui/recharts.svg)](https://travis-ci.org/yihui/recharts)

This is a prototype of porting [ECharts](http://echarts.baidu.com) into R using **htmlwidgets**. I only spent a few days on it last year, and I hope more R users can contribute to it. To install this package:

```r
install.packages(
  'recharts',
  repos = c('http://yihui.name/xran', 'http://cran.rstudio.com')
)
```

Some "hello world" examples:

```r
library(recharts)
echart(iris, ~Sepal.Length, ~Sepal.Width)
echart(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)
# bar chart
bar_df = data.frame(
  date = rep(paste("day",1:10), 2),
  temperature = floor(rnorm(n = 20, mean = 20, sd = 10)),
  location = rep(c("NY","DC"), each = 10)
 )
echart(bar_df, ~date, ~temperature, ~location)
# line chart
echart(bar_df, ~date, ~temperature, ~location, type="line")
# histogram
echart_hist(rnorm(1000))
```

See the package vignette for more information if you want to contribute:

```r
vignette('design', package = 'recharts')
```

See https://github.com/taiyun/recharts for a similar project that we worked on before. Without the belssings of **htmlwidgets**, it is much more difficult to maintain that project. I hope this one can grow into a truly nice and exciting package.
