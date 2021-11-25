
suppressMessages({
    library(dtts)                           # in case test script gets run stand-alone
    library(data.table)
})

savedFormat <- NULL
one_second_duration  <- as.nanoduration("00:00:01")
one_second_period    <- as.nanoperiod(one_second_duration)

savedFormat <- options()$nanotimeFormat
options(nanotimeFormat="%Y-%m-%d %H:%M:%S")

## align.idx
## ---------
#test_align.idx_equal_duration <- function() {
## do the alignment with no interval, so require equality for alignment:
t1 <- nanotime(1:100 * one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align.idx(t1, t2, sopen=FALSE, eopen=FALSE), 1:10 * 10)
#}

#test_align.idx_before_duration <- function() {
## do the alignment with an interval before of 1 nanosecond
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align.idx(t1, t2, start=-one_second_duration), seq(4, 49, 5))
#}

#test_align.idx_after_duration <- function() {
## do the alignment with an interval after of 1 nanosecond
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align.idx(t1, t2, end=one_second_duration, eopen=FALSE), seq(5, 50, 5))
#}

#test_align.idx_before_period <- function() {
## do the alignment with an interval before of a 1 nanosecond period
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align.idx(t1, t2, start=-one_second_period, tz="America/New_York"), seq(4, 49, 5))
#}

#test_align.idx_after_period <- function() {
## do the alignment with an interval after of a 1 nanosecond period
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align.idx(t1, t2, end=one_second_period, eopen=FALSE, tz="America/New_York"), seq(5, 50, 5))
                                        #}
## all default arguments:
t1 <- nanotime(1:30 * one_second_duration)
t2 <- nanotime(1:3 * one_second_duration * 10)
expect_equal(align.idx(t1, t2), c(10,20,30))

## incorrect parameter types (nanoduration):
expect_error(align.idx(t1, t2, sopen="open"), "must be a 'logical'")
expect_error(align.idx(t1, t2, eopen="open"), "must be a 'logical'")
## incorrect parameter types (nanoperiod):
expect_error(align.idx(t1, t2, end=one_second_period, sopen="open"), "must be a 'logical'")
expect_error(align.idx(t1, t2, end=one_second_period, eopen="open"), "must be a 'logical'")
expect_error(align.idx(t1, t2, end=one_second_period, tz=3), "'tz' must be a 'character'")


## align
## -----

## with all default params:
rows <- 10
t1 <- seq(as.nanotime(0), by=one_second_duration, length.out=rows)
dt1 <- data.table(index=t1, v1=1:rows)
setkey(dt1, index)
t2 <- seq(as.nanotime(0), by=2*one_second_duration, length.out=rows/2)
expect_equal(align(dt1, t2), dt1[seq(1, rows, by=2)])

## do the alignment with no interval, so require equality for alignment:
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, eopen=FALSE), dt1[1:10 * 10])
#}

#test_align_duration.before <- function() {
## do the alignment with an interval before of 1 nanosecond
cols <- 3
rows <- 100
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, start=-one_second_duration), dt1[seq(4, 49, 5)])
#}

#test_align_duration.after <- function() {
## do the alignment with an interval after of 1 nanosecond
cols <- 3
rows <- 100
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, start=as.nanoduration(0), end=one_second_duration, eopen=FALSE), dt1[seq(5, 50, 5)])
#}

#test_align_period.before <- function() {
## do the alignment with an interval before of 1 nanosecond
cols <- 3
rows <- 100
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, start=-one_second_period, tz="America/New_York"), dt1[seq(4, 49, 5)])
#}

#test_align_period.after <- function() {
## do the alignment with an interval after of 1 nanosecond
cols <- 3
rows <- 100
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, end=one_second_period, eopen=FALSE, tz="America/New_York"), dt1[seq(5, 50, 5)])
#}

## for period alignment, check it more carefully on a timezone boundary:
#test_align_period.before <- function() {
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
expect_equal(align(dt1, t2, start=-as.nanoperiod("1d"), sopen=TRUE, eopen=FALSE, tz="America/New_York"), dt1[c(1, 25, 50, 74)])
#}

## incorrect parameter types (nanoduration):
expect_error(align(dt1, t2, sopen="open"), "must be a 'logical'")
expect_error(align(dt1, t2, eopen="open"), "must be a 'logical'")
expect_error(align(dt1, t2, func="a string instead of a function"), "must be a function")
## incorrect parameter types (nanoperiod):
expect_error(align(dt1, t2, start=-as.nanoperiod("1d"), sopen="open"), "must be a 'logical'")
expect_error(align(dt1, t2, start=-as.nanoperiod("1d"), eopen="open"), "must be a 'logical'")
expect_error(align(dt1, t2, start=-as.nanoperiod("1d"), func="a string instead of a function", tz="America/New_York"), "must be a function")
expect_error(align(dt1, t2, start=-as.nanoperiod("1d"), tz=complex(1)), "'tz' must be a 'character'")
expect_error(align(dt1[,2], t2, start=-as.nanoperiod("1d")), "first column of 'data.table' must be of type 'nanotime'")

## missing key (nanoduration):
t1 <- as.nanotime("2021-11-06T00:00:00 America/New_York")
dt1 <- data.table(index=t1, v1=0)
expect_error(align(dt1, t1), "first column of 'data.table' must be the first key")

## missing key (nanoperiod):
t1 <- as.nanotime("2021-11-06T00:00:00 America/New_York")
dt1 <- data.table(index=t1, v1=0)
expect_error(align(dt1, t1, start=as.nanoperiod(0), tz="America/New_York"), "first column of 'data.table' must be the first key")


## align func
## ----------
#test_align.func.equal <- function() {
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
expect_equal(align(dt1, t2, end=as.nanoduration(1), func=square_col1), exp)
#}

#test_align.func.multiple <- function() {
### LLL
## test multiple 'x' rows for each 'y' nanotime:
#}

#test_align.func.variable_start <- function() {
## test using a non-scalar 'start' parameter
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
exp <- dt1[1:10 * 10]
exp[,2] <- exp[,2] ^ 2
expect_equal(align(dt1, t2, end=as.nanoduration(1), func=square_col1), exp)
#}

#test_align.func.variable_start_end <- function() {
## test using a non-scalar 'start' and 'end' parameters
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
square_col1 <- function(x) { x[1,1] <- x[1,1] ^ 2; x }
exp <- dt1[1:10 * 10]
exp[,2] <- exp[,2] ^ 2
expect_equal(align(dt1, t2, end=as.nanoduration(1), func=square_col1), exp)
#}

#test_align.func.variable_start_end_overlapping <- function() {
## test using a non-scalar 'start' and 'end' parameters
cols <- 3
rows <- 100
t1 <- nanotime(0:(rows-1) * one_second_duration)
dt1 <- data.table(index=t1, matrix(0:(rows*cols-1), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:9 * one_second_duration * 10)

exp <- data.table(index=t2, V1=seq(9.5, 89.5, by=10))
exp[, V2 := V1 + 100]
exp[, V3 := V2 + 100]
setkey(exp, index)

## useful for testing:
## newColMeans <- function(x) {
##     print("this is x")
##     print(x)
##     colMeans(x)
## }

expect_equal(align(dt1, t2, start=-10*one_second_duration, end=10*one_second_duration, sopen=TRUE, eopen=TRUE, func=colMeans), exp)
#}

#test_align.func_missing <- function() {
## test where some groups have no rows (0 row data.table)
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:11 * one_second_duration * 10)
square_col1 <- function(x) {
    if (nrow(x)==0) data.table(NaN, NaN, NaN)
    else { x[1,1] <- x[1,1] ^ 2; x }
}
exp <- rbind(dt1[1:10 * 10], data.table(index=t2[11], V1=NaN, V2=NaN, V3=NaN))
exp[,2] <- exp[,2] ^ 2
setkey(exp, index)
expect_equal(align(dt1, t2, end=as.nanoduration(1), eopen=FALSE, func=square_col1), exp)
#}

#test_align.func_error_dim <- function() {
## test when 'func' returns an incorrect number of columns
one_second_duration  <- 1e9
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:11 * one_second_duration * 10)
square_col1 <- function(x) if (nrow(x)==0) 1 else { x[1,1] <- x[1,1] ^ 2; x }
expect_error(align(dt1, t2, end=as.nanoduration(1), func=square_col1))
#}

#test_align.func_error_incorrect_function <- function() {
## test when 'func' cannot be called
one_second_duration  <- 1e9
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:11 * one_second_duration * 10)
f <- function(x) lkdsjfdsfsdfsdfds(x) # will not eval
expect_error(align(dt1, t2, end=as.nanoduration(1), func=f))
#}


## for period alignment, check it more carefully on a timezone boundary:
#test_align_func_period_before <- function() {
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
setkey(dt1, index)
t2 <- seq(from=as.nanotime("2021-11-07T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
          by=as.nanoperiod("1d"), tz="America/New_York")
expected <- data.table(index=t2, V1=c(24, 25, 24))
setkey(expected, index)
expect_equal(align(dt1, t2, start=-as.nanoperiod("1d"), tz="America/New_York", func=nrow), expected)
#}

#test_align_func_period_after <- function() {
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
setkey(dt1, index)
t2 <- seq(from=as.nanotime("2021-11-07T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
          by=as.nanoperiod("1d"), tz="America/New_York")
expected <- data.table(index=t2, V1=c(25, 24, 24))
setkey(expected, index)
expect_equal(align(dt1, t2, end=as.nanoperiod("1d"), tz="America/New_York", func=nrow), expected)
#}

#test_align.func.equal_incorrect_dt <- function() {
## do the alignment with only one 'x' row for each 'y' nanotime:
cols <- 3
rows <- 100
dt1 <- data.table(index="a", matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
square_col1 <- function(x) if (is.null(x)) c(NaN, NaN, NaN) else { x[1,1] <- x[1,1] ^ 2; x }
exp <- dt1[1:10 * 10]
exp[,2] <- exp[,2] ^ 2
expect_error(align(dt1, t2, end=as.nanoduration(1), func=square_col1))
#}


## grid.align:
## ----------

## nanoduration
t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
          by=as.nanoduration("01:00:00"))
dt1 <- data.table(index=t1, V1=0:(length(t1)-1))
setkey(dt1, index)
exp <- data.table(index=as.nanotime(c("2021-11-07T00:00:00-04:00", "2021-11-07T23:00:00-05:00",
                                      "2021-11-08T23:00:00-05:00", "2021-11-09T23:00:00-05:00")),
                  V1=c(24, 48, 72, 73))
setkey(exp, index)
expect_equal(grid.align(dt1, by=as.nanoduration("24:00:00")), exp)

## nanoperiod
t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
          by=as.nanoduration("01:00:00"))
dt1 <- data.table(index=t1, V1=0:(length(t1)-1))
setkey(dt1, index)
# format(grid.align(dt1, by=as.nanoperiod("1d"), tz="America/New_York")$index, tz="America/New_York")
exp <- data.table(index=as.nanotime(c("2021-11-07T00:00:00-04:00", "2021-11-08T00:00:00-05:00",
                                      "2021-11-09T00:00:00-05:00")),
                  V1=c(24, 49, 73))
setkey(exp, index)
expect_equal(grid.align(dt1, by=as.nanoperiod("1d"), tz="America/New_York"), exp)

## this test to make the grid longer than t1:
t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-08T23:00:00 America/New_York"),
          by=as.nanoduration("01:00:00"))
dt1 <- data.table(index=t1, V1=0:(length(t1)-1))
setkey(dt1, index)
exp <- data.table(index=as.nanotime(c("2021-11-07T00:00:00-04:00", "2021-11-08T00:00:00-05:00",
                                      "2021-11-09T00:00:00-05:00")),
                  V1=c(24, 49, 72))
setkey(exp, index)
expect_equal(grid.align(dt1, by=as.nanoperiod("1d"), tz="America/New_York"), exp)

                                                                  
## frequency:
## ---------
#test_frequency_duration <- function() {
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
res <- frequency(dt1, by=as.nanoduration("00:00:30"))
exp <- tail(data.table(index=seq(dt1$index[1], by=30*one_second_duration, length.out=4), V1=30), -1)
exp <- rbind(exp, data.table(index=tail(exp$index,1)+30*one_second_duration, V1=10))
setkey(exp, index)
expect_equal(res, exp)
#}

## #test_frequency_start_end_duration <- function() {
cols <- 3
rows <- 100
t1 <- nanotime(0:(rows-1) * one_second_duration)
dt1 <- data.table(index=t1, matrix(0:(rows*cols-1), rows, cols))
setkey(dt1, index)
res <- frequency(dt1, by=as.nanoduration("00:00:30"), start=nanotime(0), end=nanotime(0) + 2*30*one_second_duration)
exp <- data.table(index=seq(dt1$index[1], by=30*one_second_duration, length.out=3), V1=c(0, 30, 30))
setkey(exp, index)
expect_equal(res, exp)
## #}


#test_frequency_period <- function() {
t1 <- seq(nanotime("2021-02-01 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
          by=as.nanoperiod("01:00:00"), tz="America/New_York")
dt1 <- data.table(index=t1, V1=1:length(t1))
setkey(dt1, index)
res <- frequency(dt1, by=as.nanoperiod("1d"), tz="America/New_York")

t2 <- seq(nanotime("2021-02-02 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
          by=as.nanoperiod("1d"), tz="America/New_York")
exp <- data.table(index=t2, V1=24)
exp[index=="2021-03-15T04:00:00+00:00", V1 := 23] # the dailight transition day
setkey(exp, index)
expect_equal(res, exp)
#}

## wrong type for 'by
expect_error(frequency(dt1, by=3), "argument 'by' must be either 'nanoduration' or 'nanotime'")

    
if (FALSE) {
    ## don't do this; must appear in vignette!

    t2 <- seq(nanotime("2021-02-02 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
              by=as.nanoperiod("1d"), tz="America/New_York")
    exp <- data.table(index=t2, V1=24)
    exp[index=="2021-03-15T04:00:00+00:00"] <- 1e9
}

options(nanotimeFormat=savedFormat)
