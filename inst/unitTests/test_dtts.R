

savedFormat <- NULL
one_second  <- 1e9

.setUp <- function() {
    savedFormat <- options()$nanotimeFormat
    options(nanotimeFormat="%Y-%m-%d %H:%M:%S")
}

.tearDown <- function() {
    options(nanotimeFormat=savedFormat)
}


## align.idx
## ---------
test_align.idx_equal <- function() {
    ## do the alignment with no interval, so require equality for alignment:
    t1 <- nanotime(1:100 * one_second)
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align.idx(t1, t2), 1:10 * 10)
}
test_align.idx_before <- function() {
    ## do the alignment with an interval before of 1 nanosecond
    t1 <- nanotime(1:100 * one_second * 2 + one_second)
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align.idx(t1, t2, start=-one_second), seq(4, 49, 5))
}
test_align.idx_after <- function() {
    ## do the alignment with an interval after of 1 nanosecond
    t1 <- nanotime(1:100 * one_second * 2 + one_second)
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align.idx(t1, t2, end=one_second), seq(5, 50, 5))
}

## align
## -----
test_align.equal <- function() {
    ## do the alignment with no interval, so require equality for alignment:
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align(dt1, t2), dt1[1:10 * 10])
}
test_align.before <- function() {
    ## do the alignment with an interval before of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second * 2 + one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align(dt1, t2, start=-one_second), dt1[seq(4, 49, 5)])
}
test_align.after <- function() {
    ## do the alignment with an interval after of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second * 2 + one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    checkEquals(align(dt1, t2, end=one_second), dt1[seq(5, 50, 5)])
}

## align func
## ----------
test_align.func.equal <- function() {
    ## do the alignment with only one 'x' row for each 'y' nanotime:
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    square_col1 <- function(x) if (is.null(x)) c(NaN, NaN, NaN) else { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)
}
test_align.func.multiple <- function() {
    ## test multiple 'x' rows for each 'y' nanotime:
}
test_align.func.variable_start <- function() {
    ## test using a non-scalar 'start' parameter
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func.variable_start_end <- function() {
    ## test using a non-scalar 'start' and 'end' parameters
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second * 10)
    square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func_missing <- function() {
    ## test where some groups have no rows (NULL)
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second * 10)
    square_col1 <- function(x) if (is.null(x)) data.table(NaN, NaN, NaN)
                               else { x[1,1] <- x[1,1] ^ 2; x }
    exp <- rbind(dt1[1:10 * 10], data.table(index=dt1$index[11], V1=NaN, V2=NaN, V3=NaN))
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func_error_dim <- function() {
    ## test when 'func' returns an incorrect number of columns
    library(nanotime);library(nanoival);library(dtts.utils);library(RUnit);library(data.table)
    one_second  <- 1e9
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second * 10)
    square_col1 <- function(x) if (is.null(x)) 1 else { x[1,1] <- x[1,1] ^ 2; x }
    checkException(align(dt1, t2, end=1, func=square_col1))
}
test_align.func_error_incorrect_function <- function() {
    ## test when 'func' returns an incorrect number of columns
    library(nanotime);library(nanoival);library(dtts.utils);library(RUnit);library(data.table)
    one_second  <- 1e9
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second * 10)
    f <- function(x) lkdsjfdsfsdfsdfds(x) # will not eval
    checkException(align(dt1, t2, end=1, func=f))
}

test_frequency <- function() {
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    res <- frequency(dt1, as.integer64(30*one_second))
    exp <- data.table(index=seq(dt1$index[1], by=30*one_second, length.out=3), V1=30)
    checkEquals(res, exp)
}
