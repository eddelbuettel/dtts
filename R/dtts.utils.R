## general comment LLL

## specific comments LLL
align.idx <- function(x, y, start=as.integer64(0), end=as.integer64(0)) {
    ## validate parameter types LLL
    if (!inherits(x, "nanotime")) {
        stop("'x' must have class 'nanotime'")
    }
    if (!inherits(y, "nanotime")) {
        stop ("'y' must have class 'nanotime'")
    }
    
    .Call('_align_idx', x, y, as.integer64(start), as.integer64(end))
}


## align_idx(nanotime(1:10), nanotime(11:20), as.integer64(-1), as.integer64(1))


align <- function(x, y, start=as.integer64(0), end=as.integer64(0), func=NULL) {
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
        data.table(index=y,
                   do.call(rbind, .Call('_align_func',
                                        x[[1]],        # the index of the data.table
                                        y,             # nanotime vector to align on
                                        x,             # data.table data
                                        as.integer64(start), # would be nice to get a duration type
                                        as.integer64(end),   # idem
                                        func)))
    }
    else {
        x[.Call('_align_idx', x[[1]], y, as.integer64(start), as.integer64(end))]
    }
}


## library(nanotime); library(data.table); library(dtts.utils)
## f <- function(x) x
## y <- nanotime((1:10)*1e9)
## x <- data.table(idx=nanotime((1:10)*1e9), data=1:10)
## align_func(x, y, as.integer64(-1e9), as.integer64(1e9), f)

setGeneric("grid.align", function(x, ...) standardGeneric("grid.align"))

setMethod("grid.align",
          signature("data.table"),
          function(x,                         # time-series
                   by,                        # the grid size
                   func,                      # "count", "min", "max", "median", "mean", "closest"
                   ival=by,                   # the interval size
                   start=x[[1]][1],           # start of the grid
                   end=tail(x[[1]], 1))       # time zone when using 'period'
          {
              ## if (typeof(by) == "duration") {
              if (inherits(by, "integer64")) {
                  grid <- nanoival::seq(start+by, end, by=by) # why do I need to qualify seq here???
                  if (tail(grid,1) < end) {
                      c(grid, tail(grid,1) + by)
                  }
              }
              ## else if (typeof(by) == "period") {
              ##     if (is.null(tz)) stop("tz must be specified when 'by' is a period")
              ##     grid <- seq(`+`(start,by,tz), end, by=by, tz=tz)
              ##     if (tail(grid,1) < end) {
              ##         c(--grid, `+`(tail(grid,1),by,tz))
              ##     }
              ## }
              ## else stop("invalid type for 'by', must be 'duration' or 'period'")
              else stop("invalid type for 'by', must be 'integer64'")
              
              align(x, grid, -ival, as.integer64(0), func=func)
          })
          

setMethod("frequency",
          signature("data.table"),
          function(x, by, ival=by, start=x[[1]][1], end=tail(x[[1]], 1))
              grid.align(x, by, function(y) if (is.null(y)) 0 else nrow(y), ival, start, end))
