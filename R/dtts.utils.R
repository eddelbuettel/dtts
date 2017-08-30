

align_idx <- function(x, y, start, end) {
    ## validate parameter types LLL
    .Call('_align_idx', x, y, start, end)
}

## align_idx(nanotime(1:10), nanotime(11:20), as.integer64(-1), as.integer64(1))
