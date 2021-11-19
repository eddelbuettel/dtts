setGeneric("align.idx", function(x, y, start, end, ...) standardGeneric("align.idx"))

align_idx_duration <- function(x,                         # time-series
                               y,                         # nanotime vector
                               start=as.nanoduration(0),
                               end=as.nanoduration(0),
                               sopen = FALSE,
                               eopen = TRUE)
{
    if (missing(start)) {
        start <- as.nanoduration(0)
    }
    if (missing(end)) {
        end <- as.nanoduration(0)
    }
    if (!is.logical(sopen)) {
        stop("'sopen' must be a 'logical'")
    }
    if (!is.logical(eopen)) {
        stop("'eopen' must be a 'logical'")
    }
    ## if (start==as.nanoperiod(0) && end==as.nanoperiod(0)) {
    ##     ## this is a case where the likely behavior should be to find equal alignment
    ##     ## TODO: ask Dirk what he thinks, not clear to me we want this...
    ##     if (missing(eopen)) {
    ##         eopen = FALSE
    ##     }
    ## }
    
    .Call('_dtts_align_idx_duration', sort(x), sort(y), start, end, sopen, eopen)
}

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
##'     \code{nanoduration}; \code{start} is added to each element in
##'     \code{y} and it then defines the starting point of the
##'     interval under consideration for the alignement on that
##'     element of \code{y}
##' @param end scalar or vector of same length as \code{y} of type
##'     \code{nanoduration}; \code{start} is added to each element in
##'     \code{y} and it then defines the ending point of the interval
##'     under consideration for the alignement on that element of
##'     \code{y}
##' @return a vector of indices of the same length as \code{y}; this
##'     vector indexes into \code{x} and represents the points in
##'     \code{x} that are aligned with the points in \code{y}
##'
##' @examples
##' \dontrun{
##' align.idx(nanotime(c(10:14, 17:19)), nanotime(11:20), as.nanoduration(-1), as.nanoduration(0))
##' ## [1]  2  3  4  5  5 NA  6  7  8  8
##' }
setMethod("align.idx", signature("nanotime", "nanotime", "nanoduration", "nanoduration"), align_idx_duration)
setMethod("align.idx", signature("nanotime", "nanotime", "missing", "missing"), align_idx_duration)
setMethod("align.idx", signature("nanotime", "nanotime", "missing", "nanoduration"), align_idx_duration)
setMethod("align.idx", signature("nanotime", "nanotime", "nanoduration", "missing"), align_idx_duration)


align_idx_period <- function(x,                         # time-series
                             y,                         # nanotime vector
                             start=as.nanoperiod(0),
                             end=as.nanoperiod(0),
                             sopen = FALSE,
                             eopen = TRUE,
                             tz)
{
    if (missing(start)) {
        start <- as.nanoperiod(0)
    }
    if (missing(end)) {
        end <- as.nanoperiod(0)
    }
    if (!is.logical(sopen)) {
        stop("'sopen' must be a 'logical'")
    }
    if (!is.logical(eopen)) {
        stop("'eopen' must be a 'logical'")
    }
    if (!is.character(tz)) {
        stop ("'tz' must be a 'character'")
    }
      
    .Call('_dtts_align_idx_period', sort(x), sort(y), start, end, sopen, eopen, tz)
}

setMethod("align.idx", signature("nanotime", "nanotime", "nanoperiod", "nanoperiod"), align_idx_period)
setMethod("align.idx", signature("nanotime", "nanotime", "missing", "nanoperiod"), align_idx_period)
setMethod("align.idx", signature("nanotime", "nanotime", "nanoperiod", "missing"), align_idx_period)



setGeneric("align", function(x, y, start, end, ...) standardGeneric("align"))


align_duration <- function(x,                         # time-series
                           y,                         # nanotime vector
                           start=as.nanoduration(0),
                           end=as.nanoduration(0), 
                           sopen = FALSE,
                           eopen = TRUE,
                           func=NULL)
{
    if (missing(start)) {
        start <- as.nanoduration(0)
    }
    if (missing(end)) {
        end <- as.nanoduration(0)
    }
    if (!inherits(x[[1]], "nanotime")) {
        stop("first column of `data.table` must be of type `nanotime`")
    }
    if (is.null(key(x)) || names(x)[1] != key(x)[1]) {
        stop("first column of `data.table` must be the first key")
    }
    if (!is.logical(sopen)) {
        stop("'sopen' must be a 'logical'")
    }
    if (!is.logical(eopen)) {
        stop("'eopen' must be a 'logical'")
    }    
    if (!is.null(func)) {
        if (!is.function(func)) {
            stop ("'func' must be a function")
        }
        res <- data.table(index=y,
                          do.call(rbind, .Call('_dtts_align_duration',
                                               x[[1]],        # the index of the data.table
                                               sort(y),       # nanotime vector to align on
                                               x,             # data.table data
                                               start,
                                               end,
                                               sopen,
                                               eopen,
                                               func)))
        setkeyv(res, key(x))
        res
    }
    else {
        ## if no function is supplied, make closest alignment:
        sorted_y <- sort(y)
        res <- x[.Call('_dtts_align_idx_duration', x[[1]], sorted_y, start, end, sopen, eopen)]
        res[[1]] <- sorted_y
        res
    }
}

align_duration_both_missing <- function(x, y) align_duration(x, y)

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
##'
##'
setMethod("align", signature("data.table", "nanotime", "nanoduration", "nanoduration"), align_duration)
setMethod("align", signature("data.table", "nanotime", "missing", "missing"), align_duration)
setMethod("align", signature("data.table", "nanotime", "nanoduration", "missing"), align_duration)
setMethod("align", signature("data.table", "nanotime", "missing", "nanoduration"), align_duration)



align_period <- function(x,                           # time-series
                         y,                           # nanotime vector
                         start=as.nanoperiod(0),
                         end=as.nanoperiod(0),
                         sopen = FALSE,
                         eopen = TRUE,
                         tz,
                         func=NULL)
{
    
    if (missing(start)) {
        start <- as.nanoperiod(0)
    }
    if (missing(end)) {
        end <- as.nanoperiod(0)
    }
    if (!inherits(x[[1]], "nanotime")) {
        stop("first column of `data.table` must be of type `nanotime`")
    }
    if (!is.logical(sopen)) {
        stop("'sopen' must be a 'logical'")
    }
    if (!is.logical(eopen)) {
        stop("'eopen' must be a 'logical'")
    }    
    if (!is.character(tz)) {
        stop ("'tz' must be a 'character'")
    }
    if (is.null(key(x)) || names(x)[1] != key(x)[1]) {
        stop("first column of `data.table` must be the first key")
    }
    if (!is.null(func)) {
        if (!is.function(func)) {
            stop ("'func' must be a function")
        }
        res <- data.table(index=y,
                          do.call(rbind, .Call('_dtts_align_period',
                                               x[[1]],        # the index of the data.table
                                               sort(y),       # nanotime vector to align on
                                               x,             # data.table data
                                               start,
                                               end,
                                               sopen,
                                               eopen,
                                               func,
                                               tz)))
        setkeyv(res, key(x))
        res
    }
    else {
        ## if no function is supplied, make closest alignment:
        sorted_y <- sort(y)
        res <- x[.Call('_dtts_align_idx_period', x[[1]], sorted_y, start, end, sopen, eopen, tz)]
        res[[1]] <- sorted_y
        res
    }
}


setMethod("align", signature("data.table", "nanotime", "nanoperiod", "nanoperiod"), align_period)
setMethod("align", signature("data.table", "nanotime", "nanoperiod", "missing"), align_period)
setMethod("align", signature("data.table", "nanotime", "missing", "nanoperiod"), align_period)



setGeneric("grid.align", function(x, by, ...) standardGeneric("grid.align"))

grid_align_duration <- function(x,                         # time-series
                                by,                        # the grid size
                                func,                      # function to apply on the subgroups
                                ival=by,                   # the interval size
                                start=x[[1]][1] + by,      # start of the grid
                                end=tail(x[[1]], 1))       # end of the grid
{
    grid <- seq(start, end, by=by)
    if (tail(grid,1) < end) {
        grid <- c(grid, tail(grid,1) + by)
    }
    
    align(x, grid, -ival, as.nanoduration(0), func=func)
}


grid_align_period <- function(x,                              # time-series
                              by,                             # the grid size
                              func,                           # function to apply on the subgroups
                              ival=by,                        # the interval size
                              start=plus(x[[1]][1], by, tz),  # start of the grid
                              end=tail(x[[1]], 1),            # end of the grid
                              tz)                             # time zone when using 'period'
{
    grid <- seq(start, end, by=by, tz=tz)
    if (tail(grid,1) < end) {
        grid  <- c(grid, plus(tail(grid,1), by, tz))
    }

    align(x, grid, -ival, as.nanoperiod(0), func=func, tz=tz)
}


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
##' x <- data.table(index=nanotime(cumsum(sin(seq(0.001, pi, 0.001)) * one_second)))
##' x <- x[, V2 := 1:nrow(x)]
##' setkey(x, index)
##' grid.align(x, as.nanoduration("00:01:00"), sum)
##' }
setMethod("grid.align", signature("data.table", "nanoduration"), grid_align_duration)
setMethod("grid.align", signature("data.table", "nanoperiod"),   grid_align_period)
 

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
##' one_second <- as.nanoduration("00:00:01")
##' one_minute <- 60 * one_second
##' x <- data.table(index=nanotime(cumsum(sin(seq(0.001, pi, 0.001)) * one_second)), 1)
##' frequency(x, one_minute)
##' }
setMethod("frequency",
          signature("data.table"),
          function(x, by, ival=by, start, end, tz) {
              if (missing(end)) {
                  end = tail(x[[1]], 1)
              }
              if (inherits(by, "nanoduration")) {
                  if (missing(start)) {
                      start = x[[1]][1] + by
                  }
                  grid.align(x, by, function(y) if (is.null(y)) 0 else nrow(y), ival, start, end)
              }
              else if (inherits(by, "nanoperiod")) {
                  if (missing(start)) {
                      start = plus(x[[1]][1], by, tz)
                      print(start)
                  }
                  grid.align(x, by, function(y) if (is.null(y)) 0 else nrow(y), ival, start, end, tz)
              }
              else {
                  stop("argument 'by' must be either 'nanoduration' or 'nanotime'")
              }
          })
