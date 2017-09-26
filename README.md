Interval type with nanosecond precision for R

### Motivation

Thanks to the package
[nanotime](https://cran.r-project.org/web/packages/nanotime/index.html)
R now has vector of time points with nanosecond precision. `nanoival`
is an interval type that allows complex subsetting into such a vector
of time, for example to keep only specific portions of a time point
vector.

The goal - not yet implemented - is to use `nanoival` to subset
time-series `data.table` objects.

### Demo

### Examples

Creating a `nanoival`, with the start time included ('+') and the end
time excluded ('-')

~~~ R
as.nanoival("+2012-03-01T21:21:00.000000001+00:00->2015-01-01T21:22:00.000000999+04:00-")
~~~

a `nanoival` can also be created with a pair of `nanotime` objects, a start
and an end, and optionally two logicals determining if the interval start(end) are open
or closed; by default the start is closed and end is open:

~~~ R
start <- nanotime("2012-03-01T21:21:00.000000001+00:00")
end <- nanotime("2013-03-01T21:21:00.000000001+00:00")
nanoival(start, end)
~~~

a vector of `nanotime` can be subsetted by an interval:

~~~ R
fmt <- "%Y-%m-%d %H:%M:%S"
one_second <- 1e9
a <- seq(nanotime("2012-12-12 12:12:12", fmt), length.out=10, by=one_second)
idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-", fmt),
         as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+", fmt))
a[idx]
~~~

`nanoival` also has the set operations `union`, `intersect`,
`setdiff`. All are defined for two `nanoival` arguments, and
additionally `intersect` and `setdiff` can have the first argument as
a `nanotime` and the result is then a `nanotime` instead of an
`nanoival`.

~~~ R
a <- seq(nanotime("2012-12-12 12:12:12", fmt), length.out=10, by=one_second)
i <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:18-", fmt)
setdiff(a, i)

i1 <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:17-", fmt)
i2 <- as.nanoival("+2012-12-12 12:12:16 -> 2012-12-12 12:12:18-", fmt)
union(i1, i2)
~~~

Finally, `intersect.idx` which gives back the indices of the intersection is defined.

~~~ R
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
intersect.idx(a, idx)
## which gives back:
## $x
## [1] 3 4 5 6 7 8
## 
## $y
## [1] 1 1 1 1 1 1
                                                                             
~~~

`union.idx` and `setdiff.idx` will be implemented in the future.

### Status

The package is in the very early stages and is largely untested.

See the [issue tickets](https://github.com/lsilvestri/nanoival/issues)
for an up to date list of potentially desirable, possibly planned, or
at least discussed items.

### Installation

```r
remotes::install_github("lsilvest/nanoival")
```

### Author

Leonardo Silvestri

### License

GPL (>= 2)
