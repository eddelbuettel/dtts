# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

.align_duration_cpp <- function(x, y, xdata, start, end, sopen, eopen, func) {
    .Call(`_dtts_align_duration`, x, y, xdata, start, end, sopen, eopen, func)
}

.align_period_cpp <- function(x, y, xdata, start, end, sopen, eopen, func, tz) {
    .Call(`_dtts_align_period`, x, y, xdata, start, end, sopen, eopen, func, tz)
}

.align_idx_duration_cpp <- function(x, y, start, end, sopen, eopen) {
    .Call(`_dtts_align_idx_duration`, x, y, start, end, sopen, eopen)
}

.align_idx_period_cpp <- function(x, y, start, end, sopen, eopen, tz) {
    .Call(`_dtts_align_idx_period`, x, y, start, end, sopen, eopen, tz)
}

.ops <- function(xdata, ydata, op_string) {
    .Call(`_dtts_ops`, xdata, ydata, op_string)
}

