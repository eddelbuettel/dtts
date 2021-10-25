Alignment functions and other utilities for `data.table` time-series.

### Motivation

`data.table` allows the creation of time-series by creating a first
column with a temporal type. The functions in this package assume this
temporal type is `nanotime`.

All the standard `data.table` functions can be used with such a
time-series. The goal of this package is to consolidate this
functionality with functions that are specialized for time-series such
as alignment functions.

The alignment functions that are proposed in this package are
particularly versatile and allow to build higher-level functions quite
easily. And example of this is shown in the package itself with the
function `frequency`.


### Examples

The following function creates a time-series of 100 rows spaced every
1 seconds. It then aligns this time series on a time vector that is
spaced every 10 seconds and applies the column means to the
observations that are being aligned.

~~~ R
library(dtts); library(data.table)
one_second  <- 1e9
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
t2 <- nanotime(1:10 * one_second * 10)
align(dt1, t2, start=-10*one_second, func=function(x) colMeans(as.data.frame(x)))
~~~

### Status

The package is in the very early stages and is largely untested.

See the [issue tickets](https://github.com/eddelbuettel/dtts/issues)
for an up to date list of potentially desirable, possibly planned, or
at least discussed items.

### Installation

```r
remotes::install_github("lsilvest/dtts.utils")
```

### Author

Dirk Eddelbuettel, Leonardo Silvestri

### License

GPL (>= 2)
