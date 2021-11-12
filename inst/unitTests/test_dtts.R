savedFormat <- NULL
one_second_duration  <- as.nanoduration("00:00:01")
one_second_period    <- as.nanoperiod(one_second_duration)

.setUp <- function() {
    savedFormat <- options()$nanotimeFormat
    options(nanotimeFormat="%Y-%m-%d %H:%M:%S")
}

.tearDown <- function() {
    options(nanotimeFormat=savedFormat)
}


## align.idx
## ---------
test_align.idx_equal_duration <- function() {
    ## do the alignment with no interval, so require equality for alignment:
    t1 <- nanotime(1:100 * one_second_duration)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align.idx(t1, t2), 1:10 * 10)
}
test_align.idx_before_duration <- function() {
    ## do the alignment with an interval before of 1 nanosecond
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align.idx(t1, t2, start=-one_second_duration), seq(4, 49, 5))
}
test_align.idx_after_duration <- function() {
    ## do the alignment with an interval after of 1 nanosecond
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align.idx(t1, t2, end=one_second_duration), seq(5, 50, 5))
}
test_align.idx_before_period <- function() {
    ## do the alignment with an interval before of a 1 nanosecond period
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align.idx(t1, t2, start=-one_second_period, tz="America/New_York"), seq(4, 49, 5))
}
test_align.idx_after_period <- function() {
    ## do the alignment with an interval after of a 1 nanosecond period
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align.idx(t1, t2, end=one_second_period, tz="America/New_York"), seq(5, 50, 5))
}

## align
## -----
test_align_none.equal <- function() {
    ## do the alignment with no interval, so require equality for alignment:
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    setkey(dt1, index)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align(dt1, t2), dt1[1:10 * 10])
}
test_align_duration.before <- function() {
    ## do the alignment with an interval before of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    setkey(dt1, index)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align(dt1, t2, start=-one_second_duration), dt1[seq(4, 49, 5)])
}
test_align_duration.after <- function() {
    ## do the alignment with an interval after of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align(dt1, t2, start=as.nanoduration(0), end=one_second_duration), dt1[seq(5, 50, 5)])
}
test_align_period.before <- function() {
    ## do the alignment with an interval before of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align(dt1, t2, start=-one_second_period, tz="America/New_York"), dt1[seq(4, 49, 5)])
}
test_align_period.after <- function() {
    ## do the alignment with an interval after of 1 nanosecond
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    setkey(dt1, index)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    checkEquals(align(dt1, t2, end=one_second_period, tz="America/New_York"), dt1[seq(5, 50, 5)])
}
## for period alignment, check it more carefully on a timezone boundary:
test_align_period.before <- function() {
    cols <- 3
    t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
              by=as.nanoduration("01:00:00"))
    rows <- length(t1)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    setkey(dt1, index)
    t2 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
              by=as.nanoperiod("1d"), tz="America/New_York")
    ## notice below the transition over the day where we go to winter
    ## time that has 25 hours, hence the index that goes from 25 to
    ## 50:
    checkEquals(align(dt1, t2, start=-as.nanoperiod("1d"), tz="America/New_York"), dt1[c(1, 25, 50, 75)])
}


## align func
## ----------
test_align.func.equal <- function() {
    ## do the alignment with only one 'x' row for each 'y' nanotime:
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    setkey(dt1, index)
    t2 <- nanotime(1:10 * one_second_duration * 10)
    square_col1 <- function(x) if (is.null(x)) c(NaN, NaN, NaN) else { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=as.nanoduration(1), func=square_col1), exp)
}
test_align.func.multiple <- function() {
    ### LLL
    ## test multiple 'x' rows for each 'y' nanotime:
}
test_align.func.variable_start <- function() {
    ## test using a non-scalar 'start' parameter
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second_duration * 10)
    square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func.variable_start_end <- function() {
    ## test using a non-scalar 'start' and 'end' parameters
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second_duration * 10)
    square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func_missing <- function() {
    ## test where some groups have no rows (0 row data.table)
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second_duration * 10)
    square_col1 <- function(x) {
        if (nrow(x)==0) data.table(NaN, NaN, NaN)
        else { x[1,1] <- x[1,1] ^ 2; x }
    }
    exp <- rbind(dt1[1:10 * 10], data.table(index=dt1$index[11], V1=NaN, V2=NaN, V3=NaN))
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=1, func=square_col1), exp)    
}
test_align.func_error_dim <- function() {
    ## test when 'func' returns an incorrect number of columns
    one_second_duration  <- 1e9
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second_duration * 10)
    square_col1 <- function(x) if (nrow(x)==0) 1 else { x[1,1] <- x[1,1] ^ 2; x }
    checkException(align(dt1, t2, end=1, func=square_col1))
}
test_align.func_error_incorrect_function <- function() {
    ## test when 'func' cannot be called
    one_second_duration  <- 1e9
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:11 * one_second_duration * 10)
    f <- function(x) lkdsjfdsfsdfsdfds(x) # will not eval
    checkException(align(dt1, t2, end=1, func=f))
}


## for period alignment, check it more carefully on a timezone boundary:
test_align_func_period_before <- function() {
    ## what we are specifically looking for here is that the alignment
    ## will take the calendar day over the timezone boundary, so using
    ## `nrow` as `func` will yield the transition day to winter time
    ## as having 25 hours"
    cols <- 3
    t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-10T00:00:00 America/New_York"),
              by=as.nanoduration("01:00:00"))
    rows <- length(t1)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- seq(from=as.nanotime("2021-11-07T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
              by=as.nanoperiod("1d"), tz="America/New_York")
    expected <- data.table(index=t2, V1=c(24, 25, 24))
    checkEquals(align(dt1, t2, start=-as.nanoperiod("1d"), tz="America/New_York", func=nrow), expected)
}
test_align_func_period_after <- function() {
    ## what we are specifically looking for here is that the alignment
    ## will take the calendar day over the timezone boundary, so using
    ## `nrow` as `func` will yield the transition day to winter time
    ## as having 25 hours"
    cols <- 3
    t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-10T00:00:00 America/New_York"),
              by=as.nanoduration("01:00:00"))
    rows <- length(t1)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    t2 <- seq(from=as.nanotime("2021-11-07T00:00:00 America/New_York"),
              to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
              by=as.nanoperiod("1d"), tz="America/New_York")
    expected <- data.table(index=t2, V1=c(25, 24, 24))
    checkEquals(align(dt1, t2, end=as.nanoperiod("1d"), tz="America/New_York", func=nrow), expected)
}
test_align.func.equal_incorrect_dt <- function() {
    ## do the alignment with only one 'x' row for each 'y' nanotime:
    cols <- 3
    rows <- 100
    dt1 <- data.table(index="a", matrix(1:(rows*cols), rows, cols))
    t2 <- nanotime(1:10 * one_second_duration * 10)
    square_col1 <- function(x) if (is.null(x)) c(NaN, NaN, NaN) else { x[1,1] <- x[1,1] ^ 2; x }
    exp <- dt1[1:10 * 10]
    exp[,2] <- exp[,2] ^ 2
    checkEquals(align(dt1, t2, end=as.nanoduration(1), func=square_col1), exp)
}
## LLL also check that if dt1 is not ordered on its first column then we will throw and error

test_frequency <- function() {
    cols <- 3
    rows <- 100
    t1 <- nanotime(1:rows * one_second_duration)
    dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
    res <- frequency(dt1, as.integer64(30*one_second_duration))
    exp <- data.table(index=nanotime(seq(dt1$index[1], by=30*one_second_duration, length.out=3)), V1=30)
    checkEquals(res, exp)
}
