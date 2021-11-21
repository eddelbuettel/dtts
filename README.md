# Alignment functions and other utilities for `data.table` time-series.

## Motivation

[`data.table`](https://CRAN.R-project.org/package=data.table) allows
the creation of time-series by creating a first column with a temporal
type. The functions in this package assume this temporal type is
[`nanotime`](https://CRAN.R-project.org/package=nanotime).

All the standard `data.table` functions can be used with such a
time-series. The goal of this package is to consolidate this
functionality with functions that are specialized for time-series such
as alignment functions.

The alignment functions that are proposed in this package are
particularly versatile and allow to build higher-level functions quite
easily. And example of this is shown in the package itself with the
function `frequency`.


## Creating a `data.table`-based time-series with a `nanotime` index

Three operations are necessary to create a `data.table`-based
time-series for use with the functions defined in this package:
1. Create the time index, i.e. a vector of `nanotime`
2. Create a `data.table` with the first column being the time index 
3. The name of the first column with the time index must be specified
   as a key

For instance, this code creates a time-series of 10 rows spaced every
hour with a data column `V1` containing random data:

~~~ R
library(data.table)
t1  <- nanotime(1:10 * as.nanoduration("01:00:00"))
dt1 <- data.table(index=t1, V1=runif(10))
setkey(dt1, index)
~~~


## Alignment functions

Alignment is the process of matching one time series to another. All
alignment functions in this package work in a similar way. For each
point in the vector `y` onto which `x` is aligned, a pair or arguments
called `start` and `end` define an interval around it. As an example
let's take `start` equal to -1 hour and `end` equal to 0 hour. This
means that a `y` of 2021-11-20 11:00:00 defines an interval from
2021-11-20 10:00:00 to 2021-11-20 11:00:00. The alignment process will
then use that interval to pick one or more points or a statistic on
that interval for the corresponding point in `y`.

Additionally to the arguments `start` and `end`, two other arguments,
booleans named `sopen` and `eopen` define if the start and end,
respectively, of the interval are open or not.

<img src="./inst/images/align_closest.svg">


### `align.idx`

This function takes two vectors of type `nanotime`. It aligns the
first one onto the second one and returns the indices of the first
vector that align with the second vector. There is no choice of
aggregation function here as this function works uniquely on
`nanotime` vectors and so there are no `data.table` columns on which
to operate, so the behavior here is to take the point in `x` that
falls in the interval and that is closest to the point of alignment in
`y`. The index of the point that falls in that interval is returned at
the position of the vector `y`. If no point exists in that interval
`NaN` is returned.



### `align`

This function takes a `data.table` and aligns is onto `y`, a vector of
`nanotime`. Like `align.idx`, it uses the arguments `start`, `end`,
`sopen` and `eopen` to define the intervals around the points in `y`. 

Instead of the result being an index, it is this time a new
`data.table` time-series with the first `nanotime` column being the
vector `y`, and the rows of this time-series are taken from the
`data.table` `x`. If no function is specified (i.e. `func` is `NULL`),
the function returns the row of the point in `x` that is in the
interval and that is closest to the point in `y` on which the
alignment is made. If `func` is defined, it receives for each point in
`y` all the rows in `x` that are in the defined interval. So `func`
must be a statistic that returns one row, but any number of
columns. Common examples are means (e.g. using `colMeans`), counts,
etc.

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

### `grid.align`

This function adds one more dimension to the function `align` in the
sense that instead of taking a vector `y`, it constructs it as a grid
that has as interval the value supplied in the argument `by`. The
interval is controllable (with arguments `ival_start`, `ival_end`,
`ival_sopen`, `ival_eopen`) but it is likely that in most cases the
default will be used which is the grid interval. As for `align`, the
caller can specifie `func`. Finally, note that `by` can be either a
`nanoduration` or a `nanoperiod`. In the latter case, as for the other
functions, the argument `tz` must be supplied so that the `nanoperiod`
interval can be anchored in a specific timezone.


### `frequency`

Frequency is yet one abtraction higher and is basically `grid.align`
with a default function which is the counting of the number of
elements in each interval.


## Status

The package currently proposes only a set of alignment
functions. "Moving" functions such as moving sum, moving average,
etc. are planned but not yet implemented.

See the [issue tickets](https://github.com/eddelbuettel/dtts/issues)
for an up to date list of potentially desirable, possibly planned, or
at least discussed items.

## Installation

```r
remotes::install_github("eddelbuettel/dtts.utils")
```

## Author

Dirk Eddelbuettel, Leonardo Silvestri

## License

GPL (>= 2)
