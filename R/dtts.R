
align_idx_duration <- function(x,                         # nanotime vector
                               y,                         # nanotime vector
                               start,
                               end,
                               sopen = FALSE,
                               eopen = TRUE,
                               bypass_x_check = FALSE,
                               bypass_y_check = FALSE)
{
    if (missing(start) && missing(end) && missing(sopen) && missing(eopen)) {
        eopen = FALSE                   # otherwise no interval is
                                        # defined and the result is
                                        # all NA, which is likely not
                                        # what the user intended
    }
    else {
        if (!is.logical(sopen)) {
            stop("'sopen' must be a 'logical'")
        }
        if (!is.logical(eopen)) {
            stop("'eopen' must be a 'logical'")
        }
    }
    if (missing(start)) {
        start <- as.nanoduration(0)
    }
    if (missing(end)) {
        end <- as.nanoduration(0)
    }
    if (!bypass_y_check & is.unsorted(y)) {
        stop("'y' must be sorted in ascending order")
    }
    if (!bypass_x_check & is.unsorted(x)) {
        stop("'x' must be sorted in ascending order")
    } 
    
    .align_idx_duration_cpp(x, y, start, end, sopen, eopen)
}

##' Get the index of the alignment of one vector onto another
##'
##' \code{align_idx} returns the index of the alignment of \code{x} on \code{y}
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
##'     \code{nanoduration} or \code{nanoperiod}; \code{start} is
##'     added to each element in \code{y} and it then defines the
##'     starting point of the interval under consideration for the
##'     alignment on that element of \code{y}
##' @param end scalar or vector of same length as \code{y} of type
##'     \code{nanoduration} or \code{nanoperiod}; \code{start} is
##'     added to each element in \code{y} and it then defines the
##'     ending point of the interval under consideration for the
##'     alignment on that element of \code{y}
##' @param sopen boolean scalar or vector of same lengths as \code{y}
##'     that indicates if the start of the interval is open or
##'     closed. Defaults to FALSE.
##' @param eopen boolean scalar or vector of same lengths as \code{y}
##'     that indicates if the end of the interval is open or
##'     closed. Defaults to TRUE.
##' @param tz scalar or vector of same length as \code{y} of type
##'     character. Only used when the type of \code{start} and
##'     \code{end} is \code{nanoperiod}. It defines the time zone for
##'     the definition of the interval.
##' @param bypass_x_check logical indicating if the sorting of
##'     \code{x} should be bypassed. This can provide a marginal
##'     speedup, but should be used carefully.
##' @param bypass_y_check logical indicating if the sorting of
##'     \code{y} should be bypassed. This can provide a marginal
##'     speedup, but should be used carefully.
##' @param ... further arguments passed to or from methods.
##' @return a vector of indices of the same length as \code{y}; this
##'     vector indexes into \code{x} and represent the closest point
##'     of \code{x} that is in the interval defined around each point
##'     in \code{y}
##'
##' @details When only \code{x} and \code{y} are specified, the
##'     default is to close the intervals so that the alignment simply
##'     picks up equal points. Note that it is possible to specify
##'     meaningless intervals, for instance with a \code{start} that
##'     is beyond \code{end}. In this case, the alignment will simply
##'     return NA for each element in \code{y}. In principle, the
##'     \code{start} and \code{end} are chosen to define an interval
##'     is the past, or around the points in \code{y}, but if they are
##'     both positive, they can define intervals in the future.
##' 
##' @rdname align_idx
##'
##' @examples
##' \dontrun{
##' align_idx(nanotime(c(10:14, 17:19)), nanotime(11:20))
##' ## [1]  2  3  4  5  NA NA  6  7  8  NA
##' }
setGeneric("align_idx", function(x, y, start, end, ...) standardGeneric("align_idx"))

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "nanoduration", "nanoduration"), align_idx_duration)

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "missing", "missing"), align_idx_duration)

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "missing", "nanoduration"), align_idx_duration)

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "nanoduration", "missing"), align_idx_duration)


align_idx_period <- function(x,                         # time-series
                             y,                         # nanotime vector
                             start=as.nanoperiod(0),
                             end=as.nanoperiod(0),
                             sopen = FALSE,
                             eopen = TRUE,
                             tz,
                             bypass_x_check = FALSE,
                             bypass_y_check = FALSE)
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
    if (!bypass_y_check & is.unsorted(y)) {
        stop("'y' must be sorted in ascending order")
    }
    if (!bypass_x_check & is.unsorted(x)) {
        stop("'x' must be sorted in ascending order")
    } 
      
    .align_idx_period_cpp(sort(x), sort(y), start, end, sopen, eopen, tz)
}


##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "nanoperiod", "nanoperiod"), align_idx_period)

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "missing", "nanoperiod"), align_idx_period)

##' @rdname align_idx
setMethod("align_idx", signature("nanotime", "nanotime", "nanoperiod", "missing"), align_idx_period)



##' @rdname align
setGeneric("align", function(x, y, start, end, ...) standardGeneric("align"))


align_duration <- function(x,                         # data.table time-series
                           y,                         # nanotime vector
                           start=as.nanoduration(0),
                           end=as.nanoduration(0), 
                           sopen = FALSE,
                           eopen = TRUE,
                           func = NULL)
{
    if (missing(start) && missing(end) && missing(sopen) && missing(eopen)) {
        eopen = FALSE               # otherwise no interval is
                                    # defined and the result is
                                    # all NA, which is likely not
                                    # what the user intended
    }
    if (missing(start)) {
        start <- as.nanoduration(0)
    }
    if (missing(end)) {
        end <- as.nanoduration(0)
    }
    if (!inherits(x[[1]], "nanotime")) {
        stop("first column of 'data.table' must be of type 'nanotime'")
    }
    if (is.null(key(x)) || names(x)[1] != key(x)[1]) {
        stop("first column of 'data.table' must be the first key")
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
                          do.call(rbind,
                                  .align_duration_cpp(x[[1]],        # the index of the data.table
                                                      sort(y),       # nanotime vector to align on
                                                      x,             # data.table data
                                                      start,
                                                      end,
                                                      sopen,
                                                      eopen,
                                                      func)))
        names(res)[1] <- names(x)[1]    # keep the original name of the index
        setkeyv(res, key(x))
        res
    }
    else {
        ## if no function is supplied, make closest alignment:
        sorted_y <- sort(y)
        res <- x[align_idx_duration(x[[1]], sorted_y, start, end, sopen, eopen, bypass_x_check=TRUE, bypass_y_check=TRUE)]
        res[[1]] <- sorted_y
        res
    }
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
##'     interval under consideration for the alignment on that
##'     element of \code{y}
##' @param end scalar or vector of same length as \code{y} of type
##'     \code{integer64}; \code{start} is added to each element in
##'     \code{y} and it then defines the ending point of the interval
##'     under consideration for the alignment on that element of
##'     \code{y}
##' @param sopen boolean scalar or vector of same lengths as \code{y}
##'     that indicates if the start of the interval is open or
##'     closed. Defaults to FALSE.
##' @param eopen boolean scalar or vector of same lengths as \code{y}
##'     that indicates if the end of the interval is open or
##'     closed. Defaults to TRUE.
##' @param tz scalar or vector of same length as \code{y} of type
##'     character. Only used when the type of \code{start} and
##'     \code{end} is \code{nanoperiod}. It defines the time zone for
##'     the definition of the interval.
##' @param func a function taking one argument and which provides an
##'     arbitrary aggregation of its argument; if \code{NULL} then a
##'     function which takes the closest observation is used.
##' @param ... further arguments passed to or from methods.
##' @return a \code{data.table} time-series of the same length as
##'     \code{y}; this is a subset of \code{x} with the
##'     \code{nanotime} index of \code{y}
##' 
##' @rdname align
##'
##' @examples
##' \dontrun{
##' y <- nanotime((1:10)*1e9)
##' x <- data.table(index=nanotime((1:10)*1e9), data=1:10)
##' align(x, y, as.nanoduration(-1e9), as.nanoduration(1e9), colMeans)
##' }
##'
##'
setMethod("align", signature("data.table", "nanotime", "nanoduration", "nanoduration"), align_duration)
##' @rdname align
setMethod("align", signature("data.table", "nanotime", "missing", "missing"), align_duration)
##' @rdname align
setMethod("align", signature("data.table", "nanotime", "nanoduration", "missing"), align_duration)
##' @rdname align
setMethod("align", signature("data.table", "nanotime", "missing", "nanoduration"), align_duration)


align_period <- function(x,                           # data.table time-series
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
        stop("first column of 'data.table' must be of type 'nanotime'")
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
        stop("first column of 'data.table' must be the first key")
    }
    if (!is.null(func)) {
        if (!is.function(func)) {
            stop ("'func' must be a function")
        }
        res <- data.table(index=y,
                          do.call(rbind,
                                  .align_period_cpp(x[[1]],        # the index of the data.table
                                                    sort(y),       # nanotime vector to align on
                                                    x,             # data.table data
                                                    start,
                                                    end,
                                                    sopen,
                                                    eopen,
                                                    func,
                                                    tz)))
        names(res)[1] <- names(x)[1]    # keep the original name of the index
        setkeyv(res, key(x))
        res
    }
    else {
        ## if no function is supplied, make closest alignment:
        sorted_y <- sort(y)
        res <- x[align_idx_period(x[[1]], sorted_y, start, end, sopen, eopen, tz, bypass_x_check=TRUE, bypass_y_check=TRUE)]
        res[[1]] <- sorted_y
        res
    }
}


##' @rdname align
setMethod("align", signature("data.table", "nanotime", "nanoperiod", "nanoperiod"), align_period)
##' @rdname align
setMethod("align", signature("data.table", "nanotime", "nanoperiod", "missing"), align_period)
##' @rdname align
setMethod("align", signature("data.table", "nanotime", "missing", "nanoperiod"), align_period)



##' @rdname grid_align
setGeneric("grid_align", function(x, by, ...) standardGeneric("grid_align"))

grid_align_duration <- function(x,                           # time-series
                                by,                          # the grid size
                                func=NULL,                   # function to apply on the subgroups
                                grid_start=x[[1]][1] + by,   # start of the grid
                                grid_end=tail(x[[1]], 1),    # end of the grid
                                ival_start=-by,              # the interval start
                                ival_end=as.nanoduration(0), # the interval end
                                ival_sopen=FALSE,            # the interval start open 
                                ival_eopen=TRUE)             # the interval end open
{
    if (!inherits(x[[1]], "nanotime")) {
        stop("first column of 'data.table' must be of type 'nanotime'")
    }
    grid <- seq(grid_start, grid_end, by=by)
    if (tail(grid,1) < grid_end) {
        grid <- c(grid, tail(grid,1) + by)
    }
    
    if (missing(ival_sopen) & missing(ival_eopen) & is.null(func)) {
        ## the intention is a closest align, so make sure the interval
        ## is closed at the end, as equality is considered the
        ## closest:
        ival_eopen <- FALSE
    }

    align(x, grid, ival_start, ival_end, ival_sopen, ival_eopen, func)
}


grid_align_period <- function(x,                                   # time-series
                              by,                                  # the grid size
                              func=NULL,                           # function to apply on the subgroups
                              grid_start=plus(x[[1]][1], by, tz),  # start of the grid
                              grid_end=tail(x[[1]], 1),            # end of the grid
                              ival_start=-by,                      # the interval start
                              ival_end=as.nanoperiod(0),           # the interval end
                              ival_sopen=FALSE,                    # the interval start open 
                              ival_eopen=TRUE,                     # the interval end open
                              tz)                                  # time zone when using 'period'
{
    if (!inherits(x[[1]], "nanotime")) {
        stop("first column of 'data.table' must be of type 'nanotime'")
    }
    grid <- seq(grid_start, grid_end, by=by, tz=tz)
    if (tail(grid,1) < grid_end) {
        grid  <- c(grid, plus(tail(grid,1), by, tz))
    }

    if (missing(ival_sopen) & missing(ival_eopen) & is.null(func)) {
        ## the intention is a closest align, so make sure the interval
        ## is closed at the end, as equality is considered the
        ## closest:
        ival_eopen <- FALSE
    }

    align(x, grid, ival_start, ival_end, ival_sopen, ival_eopen, tz, func)
}


##' Align a \code{data.table} onto a \code{nanotime} vector grid
##'
##' \code{grid_align} returns the subset of \code{data.table} \code{x}
##' that aligns on the grid defined by \code{by}, \code{start} and
##' \code{end}
##'
##' A grid defined by the parameter \code{by}, \code{start} and
##' \code{end} is created. The function then does a standard alignment
##' of \code{x} onto this grid (see the \code{align} function)
##'
##' @param x the \code{data.table} time-series to align from
##' @param by interval specified as a \code{nanoduration} or
##'     \code{nanoperiod}.
##' @param grid_start scalar \code{nanotime} defining the start of the
##'     grid; by default the first element of \code{x} is taken.
##' @param grid_end scalar \code{nanotime} defining the end of the grid; by
##'     default the last element of \code{x} is taken.
##' @param ival_start scalar of type \code{nanoduration} or
##'     \code{nanoperiod}; \code{ival_start} is added to each element
##'     of the grid and it then defines the starting point of the
##'     interval under consideration for the alignment onto that
##'     element.
##' @param ival_end scalar of type \code{nanoduration} or
##'     \code{nanoperiod}; \code{ival_end} is added to each element of
##'     the grid and it then defines the ending point of the interval
##'     under consideration for the alignment onto that element.
##' @param ival_sopen boolean scalar that indicates if the start of
##'     the interval is open or closed. Defaults to FALSE.
##' @param ival_eopen boolean scalar that indicates if the end of the
##'     interval is open or closed. Defaults to TRUE.
##' @param tz scalar of type character. Only used when the type of
##'     \code{by} and \code{end} is \code{nanoperiod}. It defines the
##'     time zone for the definition of the interval.
##' @param func a function taking one argument and which provides an
##'     arbitrary aggregation of its argument; if \code{NULL} then a
##'     function which takes the closest observation is used.
##' @param ... further arguments passed to or from methods.
##' @return a \code{data.table} time-series of the same length as
##'     \code{y} with the aggregations computed by \code{func}
##' 
##' @rdname grid_align
##'
##' @examples
##' \dontrun{
##' one_second <- 1e9
##' x <- data.table(index=nanotime(cumsum(sin(seq(0.001, pi, 0.001)) * one_second)))
##' x <- x[, V2 := 1:nrow(x)]
##' setkey(x, index)
##' grid_align(x, as.nanoduration("00:01:00"), sum)
##' }
setMethod("grid_align", signature("data.table", "nanoduration"), grid_align_duration)
##' @rdname grid_align
setMethod("grid_align", signature("data.table", "nanoperiod"),   grid_align_period)


##' Return the number of observations per interval
##'
##' \code{frequency} returns the number of observations in
##' \code{data.table} \code{x} for each interval specified by
##' \code{by}.
##'
##' @param x the \code{data.table} time-series for which to calculate
##'     the frequency
##' @param by interval specified as a \code{nanoduration} or
##'     \code{nanoperiod}.
##' @param grid_start scalar \code{nanotime} defining the start of the
##'     grid; by default the first element of \code{x} is taken.
##' @param grid_end scalar \code{nanotime} defining the end of the
##'     grid; by default the last element of \code{x} is taken.
##' @param tz scalar of type character. Only used when the type of
##'     \code{by} and \code{end} is \code{nanoperiod}. It defines the
##'     time zone for the definition of the interval.
##' @param ival_start scalar of type \code{nanoduration} or
##'     \code{nanoperiod}; \code{ival_start} is added to each element
##'     of the grid and it then defines the starting point of the
##'     interval under consideration for the alignment onto that
##'     element. This defaults to -\code{by} and most likely does not
##'     need to be overriden.
##' @param ival_end scalar of type \code{nanoduration} or
##'     \code{nanoperiod}; \code{ival_end} is added to each element of
##'     the grid and it then defines the ending point of the interval
##'     under consideration for the alignment onto that element. This
##'     defaults to 0 and most likely does not need to be overriden.
##' @param ival_sopen boolean scalar that indicates if the start of
##'     the interval is open or closed. Defaults to FALSE.
##' @param ival_eopen boolean scalar that indicates if the end of the
##'     interval is open or closed. Defaults to TRUE.
##' @return a \code{data.table} time-series with the number of
##'     observations in \code{x} that fall withing the intervals
##'     defined by the grid interval defined by \code{by}.
##' 
##' @examples
##' \dontrun{
##' one_second <- as.nanoduration("00:00:01")
##' one_minute <- 60 * one_second
##' x <- data.table(index=nanotime((1:100) * one_second), 1)
##' setkey(x, index)
##' frequency(x, one_minute)
##' }
setMethod("frequency",
          signature("data.table"),
          function(x, by, grid_start, grid_end, tz, ival_start=-by, ival_end, ival_sopen=FALSE, ival_eopen=TRUE)
          {
              if (missing(grid_end)) {
                  grid_end = tail(x[[1]], 1)
              }
              if (inherits(by, "nanoduration")) {
                  if (missing(grid_start)) {
                      grid_start = x[[1]][1] + by
                  }
                  if (missing(ival_end)) {
                      ival_end = as.nanoduration(0)
                  }
                  grid_align(x, by, nrow, grid_start, grid_end, ival_start, ival_end, ival_sopen, ival_eopen)
              }
              else if (inherits(by, "nanoperiod")) {
                  if (missing(grid_start)) {
                      grid_start = plus(x[[1]][1], by, tz)
                  }
                  if (missing(ival_end)) {
                      ival_end = nanoperiod(0)
                  }
                  grid_align(x, by, nrow, grid_start, grid_end, ival_start, ival_end, ival_sopen, ival_eopen, tz)
              }
              else {
                  stop("argument 'by' must be either 'nanoduration' or 'nanotime'")
              }
          })


##' @rdname ops
setGeneric("ops", function(x, y, op_string) standardGeneric("ops"))


##' Arithmetic operations on two \code{data.table} time-series
##'
##' \code{ops} returns the \code{y} time-series on which the \code{x}
##' time-series values are applied using the specified operator
##' \code{op}.
##'
##' @section Details:
##'
##' The n elements of the \code{x} time-series operand define a set of
##' n-1 intervals, and the value associated with each interval is
##' applied to all the observations in the \code{y} time-series
##' operand that fall in the interval. Note that the interval is
##' closed at the beginning and open at the end. The supported values
##' for \code{op} are "*", "/", "+", "-".
##'
##' There has to be one numeric column in \code{x} and \code{y}; there
##' has to be either a one to one correspondance between the number of
##' numeric columns in \code{x} and \code{y}, or there must be only
##' one numeric column in \code{x} that will be applied to all numeric
##' columns in \code{y}. Non-numeric columns must not appear in
##' \code{x}, whereas they will be skipped of they appear in \code{y}.
##' 
##' @param x the \code{data.table} time-series that determines the
##'     left operand
##' @param y the \code{data.table} time-series that determines the
##'     right operand \code{nanoperiod}.
##' @param op_string string defining the operation to apply; the
##'     supported values for \code{op} are "*", "/", "+", "-".
##'
##' @rdname ops
##' 
##' @examples
##' \dontrun{
##' one_second_duration  <- as.nanoduration("00:00:01")
##' t1 <- nanotime(1:2 * one_second_duration * 3)
##' t2 <- nanotime(1:4 * one_second_duration)
##' dt1 <- data.table(index=t1, data1 = 1:length(t1))
##' setkey(dt1, index)
##' dt2 <- data.table(index=t2, data1 = 1:length(t2))
##' setkey(dt2, index)
##' ops(dt1, dt2, "+")
##' }
setMethod("ops",
          signature("data.table", "data.table", "character"),
          function(x, y, op_string)
          {
              if (!inherits(x[[1]], "nanotime")) {
                  stop("first column of 'x' must be of type 'nanotime'")
              }
              if (!inherits(y[[1]], "nanotime")) {
                  stop("first column of 'y' must be of type 'nanotime'")
              }
              if (is.null(key(x)) || names(x)[1] != key(x)[1]) {
                  stop("first column of 'x' must be the first key")
              }
              if (is.null(key(y)) || names(y)[1] != key(y)[1]) {
                  stop("first column of 'y' must be the first key")
              }
              
              
              .ops(x, y, op_string)
          })
