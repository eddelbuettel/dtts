##' Get the index of the alignment of one vector onto another
##'
##' \code{align.idx} returns the index of the alignment of \code{x} on \code{y}
##'
##' In order to perform the alignment, intervals are created around
##' each elements in \code{y} using \code{start} and \code{end}. For
##' each such interval, the closest element in \code{x} is chosen. If
##' no element in \code{x} falls in the interval, then NaN is
##' returned.
##'
##' @param x the \code{nanotime} vector to align from
##' @param y the \code{nanotime} vector to align to
##' @param start scalar or vector of same length as \code{y} of type
##'     \code{integer64}; \code{start} is added to each element in
##'     \code{y} and it then defines the starting point of the
##'     interval under consideration for the alignement on that
##'     element of \code{y}
##' @param end scalar or vector of same length as \code{y} of type
##'     \code{integer64}; \code{start} is added to each element in
##'     \code{y} and it then defines the ending point of the interval
##'     under consideration for the alignement on that element of
##'     \code{y}
##' @return a vector of indices of the same length as \code{y}; this
##'     vector indexes into \code{x} and represents the points in
##'     \code{x} that are aligned with the points in \code{y}
##'
##' @examples
##' \dontrun{
##' align.idx(nanotime(c(10:14, 17:19)), nanotime(11:20), as.integer64(-1), as.integer64(0))
##' ## [1]  2  3  4  5  5 NA  6  7  8  8
##' }
align.idx <- function(x, y, start=as.integer64(0), end=as.integer64(0)) {
    ## validate parameter types LLL
    if (!inherits(x, "nanotime")) {
        stop("'x' must have class 'nanotime'")
    }
    if (!inherits(y, "nanotime")) {
        stop ("'y' must have class 'nanotime'")
    }
    
    .Call('_dtts_align_idx', x, y, as.integer64(start), as.integer64(end))
}


##' Align a \code{data.table} onto a \code{nanotime} vector
##'
##' \code{align} returns the subset of \code{data.table} \code{x} that
##' aligns on the temporal vector \code{y}
##'
##' For each element in \code{y}, intervals are created around this
##' element with \code{start} and \code{end}. All the elements of
##' \code{x} that fall within this interval are given as argument to
##' the function \code{func}. The function \code{func} show reduce
##' this \code{data.frame} to one unique row that will be associated
##' with the \code{nanotime} value in \code{y}.
##'
##' @param x the \code{data.table} time-series to align from
##' @param y the \code{nanotime} vector to align to
##' @param start scalar or vector of same length as \code{y} of type
##'     \code{integer64}; \code{start} is added to each element in
##'     \code{y} and it then defines the starting point of the
##'     interval under consideration for the alignement on that
##'     element of \code{y}
##' @param end scalar or vector of same length as \code{y} of type
##'     \code{integer64}; \code{start} is added to each element in
##'     \code{y} and it then defines the ending point of the interval
##'     under consideration for the alignement on that element of
##'     \code{y}
##' @param func a function taking one argument and which provides an
##'     arbitrary aggregation of its argument; if \code{NULL} then a
##'     function which takes the closest observation is used.
##' @return a \code{data.table} time-series of the same length as
##'     \code{y}; this is a subset of \code{x} with the
##'     \code{nanotime} index of \code{y}
##' 
##' @examples
##' \dontrun{
##' y <- nanotime((1:10)*1e9)
##' x <- data.table(index=nanotime((1:10)*1e9), data=1:10)
##' align(x, y, as.integer64(-1e9), as.integer64(1e9), colMeans)
##' }
align <- function(x, y, start=as.nanoduration(0), end=as.nanoduration(0), func=NULL) {
    ## validate parameter types LLL
    if (!is.data.table(x)) {
        stop("'x' must be a 'data.table'")
    }
    if (!inherits(y, "nanotime")) {
        stop ("'y' must have class 'nanotime'")
    }
    
    if (!is.null(func)) {
        if (!is.function(func)) {
            stop ("'func' must be a function")
        }
        ## data.table(index=y,
        ##            do.call(rbind, .Call('_dtts_align',
        ##                                 x[[1]],        # the index of the data.table
        ##                                 y,             # nanotime vector to align on
        ##                                 x,             # data.table data
        ##                                 start,
        ##                                 end,
        ##                                 func)))
    }
    else {
        ## res <- x[.Call('_dtts_align_idx', x[[1]], y, as.integer64(start), as.integer64(end))]
        ## res[[1]] <- y
        ## res
    }
}


setGeneric("grid.align", function(x, ...) standardGeneric("grid.align"))

##' Align a \code{data.table} onto a \code{nanotime} vector grid
##'
##' \code{grid.align} returns the subset of \code{data.table} \code{x}
##' that aligns on the grid defined by \code{by}, \code{start} and
##' \code{end}
##'
##' A grid defined by the parameter \code{by}, \code{start} and
##' \code{end} is created. The function then does a standard alignment
##' of \code{x} onto this grid (see the \code{align} function)
##'
##' @param x the \code{data.table} time-series to align from
##' @param by interval specified in nanoseconds
##' @param start scalar \code{nanotime} defining the start of the
##'     grid; by default the first element of \code{x} is taken.
##' @param end scalar \code{nanotime} defining the end of the grid; by
##'     default the last element of \code{x} is taken.
##' @param func a function taking one argument and which provides an
##'     arbitrary aggregation of its argument; if \code{NULL} then a
##'     function which takes the closest observation is used.
##' @return a \code{data.table} time-series of the same length as
##'     \code{y} with the aggregations computed by \code{func}
##' 
##' @examples
##' \dontrun{
##' one_second <- 1e9
##' one_minute <- 60 * one_second
##' x <- data.table(index=nanotime(cumsum(sin(seq(0.001, pi, 0.001)) * one_second)), 1)
##' grid.align(x, as.integer64(one_minute), sum)
##' }
setMethod("grid.align",
          signature("data.table"),
          function(x,                         # time-series
                   by,                        # the grid size
                   func,                      # function to apply on the subgroups
                   ival=by,                   # the interval size
                   start=x[[1]][1],           # start of the grid
                   end=tail(x[[1]], 1))       # time zone when using 'period'
          {
              ## if (typeof(by) == "nanoduration") {
              if (inherits(by, "integer64")) {
                  grid <- seq(start+by, end, by=by) # why do I need to qualify seq here???
                  if (tail(grid,1) < end) {
                      c(grid, tail(grid,1) + by)
                  }
              }
              ## else if (typeof(by) == "nanoperiod") {
              ##     if (is.null(tz)) stop("tz must be specified when 'by' is a nanoperiod")
              ##     grid <- seq(`+`(start,by,tz), end, by=by, tz=tz)
              ##     if (tail(grid,1) < end) {
              ##         c(--grid, `+`(tail(grid,1),by,tz))
              ##     }
              ## }
              ## else stop("invalid type for 'by', must be 'nanoduration' or 'nanoperiod'")
              else stop("invalid type for 'by', must be 'integer64'")
              
              align(x, grid, -ival, as.integer64(0), func=func)
          })
          

##' Return the number of observations per interval
##'
##' \code{frequency} returns the number of observations in
##' \code{data.table} \code{x} when subdivided in the specified
##' interval
##'
##' The interval specified in \code{by} is used to subdivide
##' \code{x}. The number of observations in \code{x} is then counted
##' for each interval, and the results are assigned to the end of each
##' interval.
##'
##' @param x the \code{data.table} time-series for which to calculate
##'     the frequency
##' @param by interval specified in nanoseconds
##' @param start scalar \code{nanotime} defining the start of the
##'     grid; by default the first element of \code{x} is taken.
##' @param end scalar \code{nanotime} defining the end of the grid; by
##'     default the last element of \code{x} is taken.
##' @return a \code{data.table} time-series with the number of
##'     observations in \code{x} that fall withing the intervals
##'     defined by \code{by}
##' 
##' @examples
##' \dontrun{
##' one_second <- 1e9
##' one_minute <- 60 * one_second
##' x <- data.table(index=nanotime(cumsum(sin(seq(0.001, pi, 0.001)) * one_second)), 1)
##' frequency(x, as.integer64(one_minute))
##' }
setMethod("frequency",
          signature("data.table"),
          function(x, by, ival=by, start=x[[1]][1], end=tail(x[[1]], 1))
              grid.align(x, by, function(y) if (is.null(y)) 0 else nrow(y), ival, start, end))
