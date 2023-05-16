
suppressMessages({
    library(dtts)                           # in case test script gets run stand-alone
    library(nanotime)
    library(data.table)
})

savedFormat <- NULL
one_second_duration  <- as.nanoduration("00:00:01")
one_second_period    <- as.nanoperiod(one_second_duration)

savedFormat <- options()$nanotimeFormat
options(nanotimeFormat="%Y-%m-%d %H:%M:%S")

## align_idx
## ---------
#test_align_idx_equal_duration <- function() {
## do the alignment with no interval, so require equality for alignment:
t1 <- nanotime(1:100 * one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, sopen=FALSE, eopen=FALSE), 1:10 * 10)
#}

#test_align_idx_before_duration <- function() {
## do the alignment with an interval before of 1 nanosecond
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-one_second_duration), seq(4, 49, 5))
#}

#test_align_idx_after_duration <- function() {
## do the alignment with an interval after of 1 nanosecond
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, end=one_second_duration, eopen=FALSE), seq(5, 50, 5))
#}
## test align_idx duration with non-equal times, with NA at the end:
t1 <- nanotime(1:10 * one_second_duration * 2 + one_second_duration - 1)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-2*one_second_duration, eopen=FALSE), c(4, 9, rep(NA, 8)))

## test align_idx duration with non-equal times, with NA at the end:
t1 <- nanotime(1:10 * one_second_duration * 2 + one_second_duration - 1)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-2*one_second_duration), c(4, 9, rep(NA, 8)))

## test align_idx duration with non-equal times, with NA at the beginning:
t1 <- nanotime(1:10 * one_second_duration * 2 + 10 * one_second_duration - 1)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, sopen=TRUE, start=-2*one_second_duration), c(NA, 5, 10, rep(NA, 7)))

#test_align_idx_before_period <- function() {
## do the alignment with an interval before of a 1 nanosecond period
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-one_second_period, tz="America/New_York"), seq(4, 49, 5))
#}

## test align_idx period with non-equal times, with NA at the end:
t1 <- nanotime(1:10 * one_second_duration * 2 + one_second_duration - 1)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-as.nanoperiod("00:00:02"), eopen=FALSE, tz="America/New_York"), c(4, 9, rep(NA, 8)))

## test align_idx period with non-equal times, with NA at the end:
t1 <- nanotime(1:10 * one_second_duration * 2 + one_second_duration - 1)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-as.nanoperiod("00:00:02"), tz="America/New_York"), c(4, 9, rep(NA, 8)))

#test_align_idx_after_period <- function() {
## do the alignment with an interval after of a 1 nanosecond period
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align_idx(t1, t2, end=one_second_period, eopen=FALSE, tz="America/New_York"), seq(5, 50, 5))
                                        #}
## all default arguments:
t1 <- nanotime(1:30 * one_second_duration)
t2 <- nanotime(1:3 * one_second_duration * 10)
expect_equal(align_idx(t1, t2), c(10,20,30))

## incorrect parameter types (nanoduration):
expect_error(align_idx(t1, t2, sopen="open"), "must be a 'logical'")
expect_error(align_idx(t1, t2, eopen="open"), "must be a 'logical'")
## incorrect parameter types (nanoperiod):
expect_error(align_idx(t1, t2, end=one_second_period, sopen="open"), "must be a 'logical'")
expect_error(align_idx(t1, t2, end=one_second_period, eopen="open"), "must be a 'logical'")
expect_error(align_idx(t1, t2, end=one_second_period, tz=3), "'tz' must be a 'character'")


## align
## -----

## with all default params:
rows <- 10
t1 <- seq(as.nanotime(0), by=one_second_duration, length.out=rows)
dt1 <- data.table(idx=t1, v1=1:rows, key="idx")
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

# test align period after; do the alignment with an interval after of 1 nanosecond
cols <- 3
rows <- 100
t1 <- nanotime(1:100 * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
expect_equal(align(dt1, t2, end=one_second_period, eopen=FALSE, tz="America/New_York"), dt1[seq(5, 50, 5)])


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
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(an_index=t1, matrix(0:(rows*cols-1), rows, cols), key="an_index")
t2 <- nanotime(1:9 * one_second_duration * 10)
exp <- data.table(an_index=t2, V1=seq(9.5, 89.5, by=10), key="an_index")
exp[, V2 := V1 + 100]
exp[, V3 := V2 + 100]
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


# test align period before; make sure we have trailing NA:
cols <- 3
rows <- 40
t1 <- nanotime(1:rows * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
exp <- data.table(index=t2, V1=c(rep(1, 8), rep(0, 2)))
setkey(exp, index)
expect_equal(align(dt1, t2, start=-one_second_period, eopen=FALSE, func=nrow, tz="America/New_York"), exp)

# test align period before; make sure we have trailing NA; same as above with eopen==TRUE:
cols <- 3
rows <- 40
t1 <- nanotime(1:rows * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
exp <- data.table(index=t2, V1=c(rep(1, 8), rep(0, 2)))
setkey(exp, index)
expect_equal(align(dt1, t2, start=-one_second_period, func=nrow, tz="America/New_York"), exp)

# test align period before; make sure we have trailing NA; same as above with sopen==TRUE:
cols <- 3
rows <- 40
t1 <- nanotime(1:rows * one_second_duration * 2 + one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
t2 <- nanotime(1:10 * one_second_duration * 10)
exp <- data.table(index=t2, V1=0)
setkey(exp, index)
expect_equal(align(dt1, t2, start=-one_second_period, sopen=TRUE, eopen=FALSE, func=nrow, tz="America/New_York"), exp)


## grid_align:
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
expect_equal(grid_align(dt1, by=as.nanoduration("24:00:00")), exp)

## nanoduration error with dt
dt1 <- data.table(index=1:10, V1=1:10)
setkey(dt1, index)
expect_error(grid_align(dt1, by=as.nanoduration("24:00:00")), "first column of 'data.table' must be of type 'nanotime'")

## nanoperiod
t1 <- seq(from=as.nanotime("2021-11-06T00:00:00 America/New_York"),
          to=as.nanotime("2021-11-09T00:00:00 America/New_York"),
          by=as.nanoduration("01:00:00"))
dt1 <- data.table(index=t1, V1=0:(length(t1)-1))
setkey(dt1, index)
# format(grid_align(dt1, by=as.nanoperiod("1d"), tz="America/New_York")$index, tz="America/New_York")
exp <- data.table(index=as.nanotime(c("2021-11-07T00:00:00-04:00", "2021-11-08T00:00:00-05:00",
                                      "2021-11-09T00:00:00-05:00")),
                  V1=c(24, 49, 73))
setkey(exp, index)
expect_equal(grid_align(dt1, by=as.nanoperiod("1d"), tz="America/New_York"), exp)

## nanoduration error with dt
dt1 <- data.table(index=1:10, V1=1:10)
setkey(dt1, index)
expect_error(grid_align(dt1, by=as.nanoperiod("24:00:00")), "first column of 'data.table' must be of type 'nanotime'")

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
expect_equal(grid_align(dt1, by=as.nanoperiod("1d"), tz="America/New_York"), exp)

                                                                  
## frequency (aka grid_align with func = 'nrow':
## ---------
#test_frequency_duration <- function() {
cols <- 3
rows <- 100
t1 <- nanotime(1:rows * one_second_duration)
dt1 <- data.table(index=t1, matrix(1:(rows*cols), rows, cols))
setkey(dt1, index)
res <- dtts:::frequency(dt1, by=as.nanoduration("00:00:30"))
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
res <- dtts:::frequency(dt1, by=as.nanoduration("00:00:30"), grid_start=nanotime(0), grid_end=nanotime(0) + 2*30*one_second_duration)
exp <- data.table(index=seq(dt1$index[1], by=30*one_second_duration, length.out=3), V1=c(0, 30, 30))
setkey(exp, index)
expect_equal(res, exp)
## #}


#test_frequency_period <- function() {
t1 <- seq(nanotime("2021-02-01 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
          by=as.nanoperiod("01:00:00"), tz="America/New_York")
dt1 <- data.table(index=t1, V1=1:length(t1))
setkey(dt1, index)
res <- dtts:::frequency(dt1, by=as.nanoperiod("1d"), tz="America/New_York")

t2 <- seq(nanotime("2021-02-02 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
          by=as.nanoperiod("1d"), tz="America/New_York")
exp <- data.table(index=t2, V1=24)
exp[index=="2021-03-15T04:00:00+00:00", V1 := 23] # the dailight transition day
setkey(exp, index)
expect_equal(res, exp)
#}

## frequency wrong 'by' type:
expect_error(dtts:::frequency(dt1, by=3), "argument 'by' must be either 'nanoduration' or 'nanotime'")

## tests for when there are duplicate times in the vector to align onto:
## --------------------------------------------------------------------

## align_idx duplicates in t2:
t1 <- nanotime(1:100 * one_second_duration)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
expect_equal(align_idx(t1, t2, sopen=FALSE, eopen=FALSE), time_vec * 10)

## check the same but with duplicate in t1:
t1 <- nanotime(rep(1:10, each=2) * one_second_duration)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration)
# we don't expect the indices to be duplicates, because the indices represent a number for each element of t2:
expect_equal(align_idx(t1, t2, sopen=FALSE, eopen=FALSE), c(1, 3, 5, 5, 7, 9, 11, 13, 15, 15))

## check the same but with duplicate on open start boundary with no interval:
t1 <- nanotime(rep(1:10, each=2) * one_second_duration)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration)
# we don't expect the indices to be duplicates, because the indices represent a number for each element of t2:
expect_equal(align_idx(t1, t2, sopen=TRUE, eopen=FALSE), rep(NA_real_, length(t2)))

## idx.align, duplicates in t2, with interval:
t1 <- nanotime(1:100 * one_second_duration)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-one_second_duration, sopen=FALSE, eopen=FALSE), time_vec * 10)

## idx.align, duplicates in t2, with interval, sopen=TRUE:
t1 <- nanotime(1:100 * one_second_duration)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
expect_equal(align_idx(t1, t2, start=-one_second_duration, sopen=TRUE, eopen=FALSE), time_vec * 10)

## idx.align, duplicates in t2, with interval, sopen=FALSE, eopen=TRUE:
t1 <- nanotime(1:100 * one_second_duration)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
## since we have eopen TRUE, we have to fetch one index before, hence the '-1':
expect_equal(align_idx(t1, t2, start=-one_second_duration, sopen=FALSE, eopen=TRUE), time_vec * 10 - 1)

## align, duplicates in t2
t1 <- nanotime(1:100 * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=2)
setkey(dt2, index)
## since we have eopen TRUE, we have to fetch one index before, hence the '-1':
expect_equal(align(dt1, t2, start=-2*one_second_duration, func=sum), dt2)

## align, duplicates in t1/t2
t1 <- nanotime(rep(1:50, each=2) * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=c(rep(4, 6), rep(0, 4)))
setkey(dt2, index)
## since we have eopen TRUE, we have to fetch one index before, hence the '-1':
expect_equal(align(dt1, t2, start=-2*one_second_duration, func=sum), dt2)

## align, duplicates in t1/t2, sopen/eopen FALSE
t1 <- nanotime(rep(1:50, each=2) * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=c(rep(6, 6), rep(0, 4)))
setkey(dt2, index)
## since we have eopen TRUE, we have to fetch one index before, hence the '-1':
expect_equal(align(dt1, t2, start=-2*one_second_duration, sopen=FALSE, eopen=FALSE, func=sum), dt2)

## align, duplicates in t2:
t1 <- nanotime(rep(1:100, each=1) * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=0)
setkey(dt2, index)
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=TRUE, eopen=TRUE, func=sum), dt2)
dt2[, V1 := 1]
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=TRUE, eopen=FALSE, func=sum), dt2)
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=FALSE, eopen=TRUE, func=sum), dt2)
dt2[, V1 := 2]
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=FALSE, eopen=FALSE, func=sum), dt2)

## align, duplicates in t1/t2:
t1 <- nanotime(rep(1:50, each=2) * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=c(rep(0, 6), rep(0, 4)))
setkey(dt2, index)
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=TRUE, eopen=TRUE, func=sum), dt2)
dt2[, V1 := c(rep(2, 6), rep(0, 4))]
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=TRUE, eopen=FALSE, func=sum), dt2)
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=FALSE, eopen=TRUE, func=sum), dt2)
dt2[, V1 := c(rep(4, 6), rep(0, 4))]
expect_equal(align(dt1, t2, start=-one_second_duration, sopen=FALSE, eopen=FALSE, func=sum), dt2)

## align, duplicates in t1/t2, period:
t1 <- nanotime(rep(1:50, each=2) * one_second_duration)
dt1 <- data.table(index=t1, 1)
setkey(dt1, index)
time_vec <- c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8)
t2 <- nanotime(c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8) * one_second_duration * 10)
dt2 <- data.table(index=t2, V1=c(rep(0, 6), rep(0, 4)))
setkey(dt2, index)
expect_equal(align(dt1, t2, start=-one_second_period, sopen=TRUE, eopen=TRUE, func=sum, tz="UTC"), dt2)
dt2[, V1 := c(rep(2, 6), rep(0, 4))]
expect_equal(align(dt1, t2, start=-one_second_period, sopen=TRUE, eopen=FALSE, func=sum, tz="UTC"), dt2)
expect_equal(align(dt1, t2, start=-one_second_period, sopen=FALSE, eopen=TRUE, func=sum, tz="UTC"), dt2)
dt2[, V1 := c(rep(4, 6), rep(0, 4))]
expect_equal(align(dt1, t2, start=-one_second_period, sopen=FALSE, eopen=FALSE, func=sum, tz="UTC"), dt2)


## tests for unsorted calls to align_idx:
## -------------------------------------
## both unsorted:
x <- as.nanotime(10:1)
y <- as.nanotime(4:2)
expect_error(align_idx(x, y), "'y' must be sorted in ascending order")
## x only unsorted:
x <- as.nanotime(10:1)
y <- as.nanotime(2:4)
expect_error(align_idx(x, y), "'x' must be sorted in ascending order")
## y only unsorted:
x <- as.nanotime(1:10)
y <- as.nanotime(4:2)
expect_error(align_idx(x, y), "'y' must be sorted in ascending order")
## bypass x sorted:
x <- as.nanotime(1:10)
y <- as.nanotime(2:4)
expect_equal(align_idx(x, y, bypass_x_check=TRUE), c(2:4))
## bypass x sorted, x descending:
x <- as.nanotime(10:1)
y <- as.nanotime(2:4)
expect_equal(align_idx(x, y, bypass_x_check=TRUE), rep(NA_real_, 3))   # incorrect align as x is not sorted
## bypass y sorted, y descending:
x <- as.nanotime(1:10)
y <- as.nanotime(4:1)
expect_equal(align_idx(x, y, bypass_y_check=TRUE), c(4, rep(NA_real_, 3)))   # incorrect align as x is not sorted
## x only unsorted, period:
x <- as.nanotime(10:1)
y <- as.nanotime(2:4)
expect_error(align_idx(x, y, start=-as.nanoperiod("00:00:01"), tz="UTC"), "'x' must be sorted in ascending order")
## y only unsorted, period:
x <- as.nanotime(1:10)
y <- as.nanotime(4:2)
expect_error(align_idx(x, y, start=-as.nanoperiod("00:00:01"), tz="UTC"), "'y' must be sorted in ascending order")


## tests for 'ops' function:
## ------------------------
## 1 'x' col, 1 'y' col:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1 = 1)
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, data1 := c(2, 3, 3, 1)]
expect_equal(expected_dt, ops(dt1, dt2, "+"))
## 1 'x' col, 1 'y' col, ops=='-':
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1 = 1)
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, data1 := c(0, 1, 1, 1)]
expect_equal(expected_dt, ops(dt1, dt2, "-"))
## 1 'x' col, 1 'y' col, ops=='*':
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1 = 1)
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, data1 := c(1, 2, 2, 1)]
expect_equal(expected_dt, ops(dt1, dt2, "*"))
## 1 'x' col, 1 'y' col, ops=='/':
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1 = 1)
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, data1 := c(1, 2, 2, 1)]
expect_equal(expected_dt, ops(dt1, dt2, "/"))
## 1 'x' col, 3 'y' cols:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1=1, data2=2, data3=3)
setkey(dt2, index)
ops(dt1, dt2, "+")
expected_dt = copy(dt2)
expected_dt[, data1 := c(2, 3, 3, 1)]
expected_dt[, data2 := data1 + 1]
expected_dt[, data3 := data2 + 1]
expect_equal(expected_dt, ops(dt1, dt2, "+"))
## 3 'x' col, 3 'y' cols:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1=1, data2=2, data3=3)
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, data1 := c(2, 3, 3, 1)]
expected_dt[, data2 := c(5, 6, 6, 2)]
expected_dt[, data3 := c(8, 9, 9, 3)]
expect_equal(expected_dt, ops(dt1, dt2, "+"))
## no overlap -> no change
t1 <- nanotime(1:2 * one_second_duration * 10)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, data1 = 1:2)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1 = 1)
setkey(dt2, index)
expect_equal(dt2, ops(dt1, dt2, "+"))
## 3 'x' col, 3 'y' cols:, skip extra string cols in 'y':
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, d1 := c(2, 3, 3, 1)]
expected_dt[, d2 := c(5, 6, 6, 2)]
expected_dt[, d3 := c(8, 9, 9, 3)]
expect_equal(expected_dt, ops(dt1, dt2, "+"))
## same, but mix of int and double:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, d1=1:2, d2=3:4, d3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=as.integer(1), c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expected_dt = copy(dt2)
expected_dt[, d1 := c(2, 3, 3, 1)]
expected_dt[, d2 := c(5, 6, 6, 2)]
expected_dt[, d3 := c(8, 9, 9, 3)]
expect_equal(expected_dt, ops(dt1, dt2, "+"))
## error, non-numeric column in x:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, d1=1:2, c1="a", d2=3:4, d3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=as.integer(1), c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "all data columns of 'x' must be numeric")
## error, 2 cols in 'x', 3 cols in 'y'
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "'x' must have one numeric column or the same number as 'y'")
## error, no numerical columns in x:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1="a", c2="b")
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "'x' must have at least one numeric column")
## error, 3 cols in 'x', 2 cols in 'y'
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "'x' must have one numeric column or the same number as 'y'")
## error key check dt1:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "first column of 'x' must be the first key")
## error key check dt2:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
expect_error(ops(dt1, dt2, "+"), "first column of 'y' must be the first key")
## error key check first col of 'x' not nanotime:
t1 <- 1:2
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "first column of 'x' must be of type 'nanotime'")
## error key check first col of 'y' not nanotime:
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- 1:4
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, d1=1, c1="a", d2=2, c2="b", d3=3, c3="c")
setkey(dt2, index)
expect_error(ops(dt1, dt2, "+"), "first column of 'y' must be of type 'nanotime'")
## check unknown 'op':
t1 <- nanotime(1:2 * one_second_duration * 2)
t2 <- nanotime(1:4 * one_second_duration)
dt1 <- data.table(index=t1, c1=1:2, c2=3:4, c3=5:6)
setkey(dt1, index)
dt2 <- data.table(index=t2, data1=1, data2=2, data3=3)
setkey(dt2, index)
expect_error(ops(dt1, dt2, "sdf"), "unsupported operator 'sdf'")


if (FALSE) {
    ## don't do this; must appear in vignette!

    t2 <- seq(nanotime("2021-02-02 00:00:00 America/New_York"), nanotime("2021-04-01 00:00:00 America/New_York"),
              by=as.nanoperiod("1d"), tz="America/New_York")
    exp <- data.table(index=t2, V1=24)
    exp[index=="2021-03-15T04:00:00+00:00"] <- 1e9
}

options(nanotimeFormat=savedFormat)
