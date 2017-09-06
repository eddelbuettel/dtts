

align_idx <- function(x, y, start, end) {
    ## validate parameter types LLL
    .Call('_align_idx', x, y, start, end)
}

## align_idx(nanotime(1:10), nanotime(11:20), as.integer64(-1), as.integer64(1))


align_func <- function(x, y, start, end, func) {
    ## validate parameter types LLL
    if (!is.data.table(x)) {
        stop("'x' must be a 'data.table'")
    }
    if (!inherits(y, "nanotime")) {
        stop ("'y' must have class 'nanotime'")
    }
    if (!is.function(func)) {
        stop ("'func' must be a function")
    }
    do.call(rbind, .Call('_align_func',
                         x[[1]],        # the index of the data.table
                         y,             # nanotime vector to align on
                         x,             # data.table data
                         as.integer64(start), # would be nice to get a duration type
                         as.integer64(end),   # idem
                         func))
}


## library(nanotime); library(data.table); library(dtts.utils)
## f <- function(x) x
## y <- nanotime((1:10)*1e9)
## x <- data.table(idx=nanotime((1:10)*1e9), data=1:10)
## align_func(x, y, as.integer64(-1e9), as.integer64(1e9), f)
