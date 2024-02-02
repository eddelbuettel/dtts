// define align functions

#include <chrono>
#include <iterator>
#include <cstdint>
#include <functional>
#include <vector>
#include <Rcpp.h>
#include <R_ext/Rdynload.h>
#include "nanotime/globals.hpp"
#include "nanotime/pseudovector.hpp"
#include "nanotime/period.hpp"


typedef nanotime::ConstPseudoVector<REALSXP, double, nanotime::duration> ConstPseudoVectorDuration;
typedef nanotime::ConstPseudoVector<CPLXSXP, Rcomplex> ConstPseudoVectorPrd;
typedef nanotime::ConstPseudoVector<LGLSXP,  std::int32_t> ConstPseudoVectorLgl;
typedef nanotime::ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> ConstPseudoVectorChar;


// for debug reasons...
// the following code from: https://stackoverflow.com/a/16692519
template<typename Clock, typename Duration>
std::ostream &operator<<(std::ostream &stream,
                         const std::chrono::time_point<Clock, Duration> &time_point) {
  const time_t time = Clock::to_time_t(time_point);
#if (__GNUC__ > 4 || defined(__WIN32) || ((__GNUC__ == 4) && __GNUC_MINOR__ > 8 && __GNUC_REVISION__ > 1))
  // Maybe the put_time will be implemented later?
  struct tm tm;
  // thanks to https://stackoverflow.com/a/38034148/143305 for the next test
  #if defined(__unix__)
  localtime_r(&time, &tm);
  #elif defined(_MSC_VER) || defined(__WIN32)
  localtime_s(&tm, &time);
  #endif
  return stream << std::put_time(&tm, "%c"); // Print standard date&time
#else
  char buffer[26];
  ctime_r(&time, buffer);
  buffer[24] = '\0';  // Removes the newline that is added
  return stream << buffer;
#endif
}


nanotime::duration abs_duration(nanotime::duration d)
{
  if (d >= d.zero())
    return d;
  return -d;
}


static Rcpp::NumericVector align_idx_helper_duration(const nanotime::dtime* x,
                                                     size_t xlen,
                                                     const nanotime::dtime* y,
                                                     size_t ylen,
                                                     const ConstPseudoVectorDuration& start,
                                                     const ConstPseudoVectorDuration& end,
                                                     const ConstPseudoVectorLgl& sopen,
                                                     const ConstPseudoVectorLgl& eopen) 
{
  Rcpp::NumericVector res(ylen);
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    auto ystart = y[iy] + start[iy];
    auto yend   = y[iy] + end[iy];
      
    // advance until we have a point in x that is in the interval
    // defined around yi:
    if (sopen[iy]) {
      while (ix < xlen && x[ix] <= ystart) ++ix;
    } else {
      while (ix < xlen && x[ix] < ystart) ++ix;
    }
    if (eopen[iy]) {
      if (ix >= xlen || x[ix] >= yend) {
        res[iy] = NA_REAL;
        continue;
      }
    } else {
      if (ix >= xlen || x[ix] > yend) {
        res[iy] = NA_REAL;
        continue;
      }
    }

    // find the closest point in the interval:
    if (eopen[iy]) {
      while (ix+1 < xlen && x[ix+1] < yend && abs_duration(x[ix] - y[iy]) > abs_duration(x[ix+1] - y[iy])) ++ix;
    } else {
      while (ix+1 < xlen && x[ix+1] <= yend && abs_duration(x[ix] - y[iy]) > abs_duration(x[ix+1] - y[iy])) ++ix;
    }
    res[iy] = ix + 1;     // +1 because of R numbering start convention
  }

  return res;
}



static Rcpp::NumericVector align_idx_helper_period(const nanotime::dtime* x,
                                                   size_t xlen,
                                                   const nanotime::dtime* y,
                                                   size_t ylen,
                                                   const ConstPseudoVectorPrd& start,
                                                   const ConstPseudoVectorPrd& end,
                                                   const ConstPseudoVectorLgl& sopen,
                                                   const ConstPseudoVectorLgl& eopen,
                                                   const ConstPseudoVectorChar& tz) 
{
  Rcpp::NumericVector res(ylen);
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    nanotime::period prd_start; memcpy(&prd_start, reinterpret_cast<const char*>(&start[iy]), sizeof(nanotime::period));
    nanotime::period prd_end;   memcpy(&prd_end,   reinterpret_cast<const char*>(&end[iy]),   sizeof(nanotime::period));
    auto ystart = nanotime::plus(y[iy], prd_start, std::string(tz[iy]));
    auto yend   = nanotime::plus(y[iy], prd_end,   std::string(tz[iy]));
      
    // advance until we have a point in x that is in the interval
    // defined around yi:
    if (sopen[iy]) {
      while (ix < xlen && x[ix] <= ystart) ++ix;
    } else {
      while (ix < xlen && x[ix] < ystart) ++ix;
    }
    if (eopen[iy]) {
      if (ix >= xlen || x[ix] >= yend) {
        res[iy] = NA_REAL;
        continue;
      }
    } else {
      if (ix >= xlen || x[ix] > yend) {
        res[iy] = NA_REAL;
        continue;
      }
    }

    // find the closest point in the interval:
    if (eopen[iy]) {
      while (ix+1 < xlen && x[ix+1] < yend && abs_duration(x[ix] - y[iy]) > abs_duration(x[ix+1] - y[iy])) ++ix;
    } else {
      while (ix+1 < xlen && x[ix+1] <= yend && abs_duration(x[ix] - y[iy]) > abs_duration(x[ix+1] - y[iy])) ++ix;
    }
    res[iy] = ix + 1;     // +1 because of R numbering start convention
  }

  return res;
}


static Rcpp::IntegerVector makeIndex(size_t start, size_t end) {
  Rcpp::IntegerVector res(end - start);
  size_t off = 0;
  for (size_t i=start; i<end; ++i) {
    res[off++] = i; 
  }
  return res;
}


template<class ForwardIt, class T>
ForwardIt lower_bound_sopen(ForwardIt first, ForwardIt last, const T& value)
{
    ForwardIt it;
    typename std::iterator_traits<ForwardIt>::difference_type count, step;
    count = std::distance(first, last);
 
    while (count > 0) {
        it = first; 
        step = count / 2; 
        std::advance(it, step);
        if (*it <= value) {     // '<=' rather than '<' as in the STL!
            first = ++it; 
            count -= step + 1; 
        }
        else
            count = step;
    }
    return first;
}


Rcpp::List align_func_duration(const nanotime::dtime* x,
                               size_t xlen,
                               const nanotime::dtime* y,
                               size_t ylen,
                               Rcpp::List xdata, 
                               const ConstPseudoVectorDuration& start,
                               const ConstPseudoVectorDuration& end,
                               const ConstPseudoVectorLgl& sopen,
                               const ConstPseudoVectorLgl& eopen,
                               const Rcpp::Function& func) 
{
  auto res = Rcpp::List::create();
  auto cols = makeIndex(2, XLENGTH(xdata)+1);
  typedef SEXP SUBSET_DT_FUN(SEXP,SEXP,SEXP); 
  SUBSET_DT_FUN *subsetDT = (SUBSET_DT_FUN *) R_GetCCallable("data.table", "DT_subsetDT" );
                     
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    auto ystart = y[iy] + start[iy];
    auto yend   = y[iy] + end[iy];
    
    // advance until we have a point in x that is in the interval
    // defined around yi:
    if (sopen[iy]) {
      auto iter = lower_bound_sopen(x + ix, x+xlen, ystart);
      ix = iter - x;
    } else {
      auto iter = std::lower_bound(x + ix, x+xlen, ystart);
      ix = iter - x;
    }

    if (eopen[iy]) {
      if (ix >= xlen || x[ix] >= yend) {
        const SEXP rows = Rcpp::IntegerVector::create(0);
        res.push_back(func(subsetDT(xdata, rows, cols))); // empty interval
        continue;
      }
    } else {
      if (ix >= xlen || x[ix] > yend) {
        const SEXP rows = Rcpp::IntegerVector::create(0);
        res.push_back(func(subsetDT(xdata, rows, cols))); // empty interval
        continue;
      }
    }
    auto first_ix = ix;

    // find the last point in the interval:
    if (sopen[iy]) {
      auto iter = lower_bound_sopen(x + ix, x+xlen, yend);
      ix = iter - x;
    } else {
      auto iter = std::lower_bound(x + ix, x+xlen, yend);
      ix = iter - x;
    }
    if (eopen[iy]) {
      while (ix < xlen && x[ix] < yend) ++ix;
    } else {
      while (ix < xlen && x[ix] <= yend) ++ix;
    } 

    const SEXP rows = makeIndex(first_ix+1, ix+1); // subsetDT is 1-based indexing
    res.push_back(func(subsetDT(xdata, rows, cols)));

    // reset ix to the first ix found, because the intervals
    // specified could overlap:
    ix = first_ix;
  }
  return res;
}


Rcpp::List align_func_period(const nanotime::dtime* x,
                             size_t xlen,
                             const nanotime::dtime* y,
                             size_t ylen,
                             Rcpp::List xdata, 
                             const ConstPseudoVectorPrd& start,
                             const ConstPseudoVectorPrd& end,
                             const ConstPseudoVectorLgl& sopen,
                             const ConstPseudoVectorLgl& eopen,
                             const Rcpp::Function& func,
                             const ConstPseudoVectorChar& tz) 
{
  auto res = Rcpp::List::create();
  auto cols = makeIndex(2, XLENGTH(xdata)+1);
  typedef SEXP SUBSET_DT_FUN(SEXP,SEXP,SEXP); 
  SUBSET_DT_FUN *subsetDT = (SUBSET_DT_FUN *) R_GetCCallable("data.table", "DT_subsetDT" );
                     
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    nanotime::period prd_start; memcpy(&prd_start, reinterpret_cast<const char*>(&start[iy]), sizeof(nanotime::period));
    nanotime::period prd_end;   memcpy(&prd_end,   reinterpret_cast<const char*>(&end[iy]),   sizeof(nanotime::period));
    auto ystart = nanotime::plus(y[iy], prd_start, std::string(tz[iy]));
    auto yend   = nanotime::plus(y[iy], prd_end,   std::string(tz[iy]));
    
    // advance until we have a point in x that is in the interval
    // defined around yi:
    if (sopen[iy]) {
      auto iter = lower_bound_sopen(x + ix, x+xlen, ystart);
      ix = iter - x;
    } else {
      auto iter = std::lower_bound(x + ix, x+xlen, ystart);
      ix = iter - x;
    }

    if (eopen[iy]) {
      if (ix >= xlen || x[ix] >= yend) {
        const SEXP rows = Rcpp::IntegerVector::create(0);
        res.push_back(func(subsetDT(xdata, rows, cols))); // empty interval
        continue;
      }
    } else {
      if (ix >= xlen || x[ix] > yend) {
        const SEXP rows = Rcpp::IntegerVector::create(0);
        res.push_back(func(subsetDT(xdata, rows, cols))); // empty interval
        continue;
      }
    }
    auto first_ix = ix;

    // find the last point in the interval:
    if (sopen[iy]) {
      auto iter = lower_bound_sopen(x + ix, x+xlen, yend);
      ix = iter - x;
    } else {
      auto iter = std::lower_bound(x + ix, x+xlen, yend);
      ix = iter - x;
    }
    if (eopen[iy]) {
      while (ix < xlen && x[ix] < yend) ++ix;
    } else {
      while (ix < xlen && x[ix] <= yend) ++ix;
    } 

    const SEXP rows = makeIndex(first_ix+1, ix+1); // subsetDT is 1-based indexing
    res.push_back(func(subsetDT(xdata, rows, cols)));

    // reset ix to the first ix found, because the intervals
    // specified could overlap:
    ix = first_ix;
  }
  return res;
}


// [[Rcpp::export(.align_duration_cpp)]]
Rcpp::List align_duration(const Rcpp::NumericVector& x,         // nanotime vector
                          const Rcpp::NumericVector& y,         // nanotime vector
                          const Rcpp::List xdata,               // DT
                          const Rcpp::NumericVector& start,     // duration
                          const Rcpp::NumericVector& end,       // duration
                          const Rcpp::LogicalVector& sopen,     // start open
                          const Rcpp::LogicalVector& eopen,     // end open
                          const Rcpp::Function func)            // function to apply (character)
{
  return align_func_duration(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                             x.size(),
                             reinterpret_cast<const nanotime::dtime*>(&y[0]),
                             y.size(),
                             xdata,
                             ConstPseudoVectorDuration(start),
                             ConstPseudoVectorDuration(end),
                             ConstPseudoVectorLgl(sopen),
                             ConstPseudoVectorLgl(eopen),
                             Rcpp::Function(func));
}


// [[Rcpp::export(.align_period_cpp)]]
Rcpp::List align_period(const Rcpp::NumericVector& x,         // nanotime vector
                        const Rcpp::NumericVector& y,         // nanotime vector
                        const Rcpp::List xdata,               // DT
                        const Rcpp::ComplexVector& start,     // period
                        const Rcpp::ComplexVector& end,       // period
                        const Rcpp::LogicalVector& sopen,     // start open
                        const Rcpp::LogicalVector& eopen,     // end open
                        const Rcpp::Function func,            // function to apply (character)
                        const Rcpp::CharacterVector tz)       // timezone
{
  return align_func_period(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                           x.size(),
                           reinterpret_cast<const nanotime::dtime*>(&y[0]),
                           y.size(),
                           xdata,
                           ConstPseudoVectorPrd(start),
                           ConstPseudoVectorPrd(end),
                           ConstPseudoVectorLgl(sopen),
                           ConstPseudoVectorLgl(eopen),
                           Rcpp::Function(func),
                           ConstPseudoVectorChar(tz));
}


// [[Rcpp::export(.align_idx_duration_cpp)]]
Rcpp::NumericVector align_idx_duration(const Rcpp::NumericVector& x,     // nanotime vector
                                       const Rcpp::NumericVector& y,     // nanotime vector
                                       const Rcpp::NumericVector& start, // duration
                                       const Rcpp::NumericVector& end,   // duration
                                       const Rcpp::LogicalVector& sopen, // start open
                                       const Rcpp::LogicalVector& eopen) // end open
{
  return align_idx_helper_duration(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                                   x.size(),
                                   reinterpret_cast<const nanotime::dtime*>(&y[0]),
                                   y.size(),
                                   ConstPseudoVectorDuration(start),
                                   ConstPseudoVectorDuration(end),
                                   ConstPseudoVectorLgl(sopen),
                                   ConstPseudoVectorLgl(eopen));
}


// [[Rcpp::export(.align_idx_period_cpp)]]
Rcpp::NumericVector align_idx_period(const Rcpp::NumericVector& x,     // nanotime vector
                                     const Rcpp::NumericVector& y,     // nanotime vector
                                     const Rcpp::ComplexVector& start, // period
                                     const Rcpp::ComplexVector& end,   // period
                                     const Rcpp::LogicalVector& sopen, // start open
                                     const Rcpp::LogicalVector& eopen, // end open
                                     const Rcpp::CharacterVector& tz)  // timezone
{
  return align_idx_helper_period(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                                 x.size(),
                                 reinterpret_cast<const nanotime::dtime*>(&y[0]),
                                 y.size(),
                                 ConstPseudoVectorPrd(start),
                                 ConstPseudoVectorPrd(end),
                                 ConstPseudoVectorLgl(sopen),
                                 ConstPseudoVectorLgl(eopen),
                                 ConstPseudoVectorChar(tz));
}



// this function takes two vectors and positional args and an op:
template<typename U, int RTYPE, typename F>
void applyv(U x, Rcpp::Vector<RTYPE>& y, size_t y_s, size_t y_e, F f) {
  for (auto iy=y_s; iy<y_e; ++iy) {
    y[iy] = f(x, y[iy]);
  }
}


void ops_helper(const nanotime::dtime* x,
                size_t xlen,
                const nanotime::dtime* y,
                size_t ylen,
                const Rcpp::NumericVector& xdata,
                Rcpp::NumericVector& ydata,
                std::function<double(double, double)> op)
{
  size_t ix = 0;
 
  // for each point in x, we try to find a matching point or set of
  // points in y:

  auto from_yiter = y;
  for (ix=0; ix < xlen; ix++) {
    auto to_yiter = std::lower_bound(from_yiter, y + ylen, x[ix]);
    if (to_yiter == y + ylen) continue;

    auto iy_s = from_yiter - y;
    auto iy_e = to_yiter - y;    
    applyv(xdata[ix], ydata, iy_s,  iy_e, op);
        
    from_yiter = to_yiter;
  }
}



static bool check_numeric(SEXP s) {
  return TYPEOF(s) == REALSXP || TYPEOF(s) == INTSXP;
}


static R_xlen_t get_nb_numeric_columns(Rcpp::List& l) {
  auto ncols_double = 0;
  for (auto i=1; i<l.size(); ++i) {
    if (check_numeric(l[i])) {
      ++ncols_double;
    }
  }
  return ncols_double;
}


// [[Rcpp::export(.ops)]]
Rcpp::List ops(Rcpp::List& xdata,
               Rcpp::List& ydata,
               Rcpp::String& op_string)
{
  // handle the translation of the op_string
  std::function<double(double, double)> op;
  if (op_string == "+") {
    op = std::plus<double>();
  } else if (op_string == "-") {
    op = std::minus<double>();
  } else if (op_string == "*") {
    op = std::multiplies<double>();
  } else if (op_string == "/") {
    op = std::divides<double>();
  } else {
    Rcpp::stop(std::string("unsupported operator '") + std::string(op_string) + "'");
  }
  
  // only work with doubles; require that except for the index, all
  // other columns of 'x' are numerics:
  auto x_ncols_numeric = get_nb_numeric_columns(xdata);
  if (x_ncols_numeric == 0) {
    Rcpp::stop("'x' must have at least one numeric column");
  } else if (x_ncols_numeric != xdata.size() - 1) {
    Rcpp::stop("all data columns of 'x' must be numeric");
  }
  // if one column, easy, apply it on all columns of 'ydata', but if
  // more than one, check we have the same number of numeric columns in
  // 'ydata':
  auto y_ncols_numeric = get_nb_numeric_columns(ydata);
  if (x_ncols_numeric != 1 && x_ncols_numeric != y_ncols_numeric) {
    Rcpp::stop("'x' must have one numeric column or the same number as 'y'");
  }
  

  Rcpp::NumericVector x = xdata[0];
  auto x_dt = reinterpret_cast<const nanotime::dtime*>(&x[0]);
  Rcpp::NumericVector y = ydata[0];
  auto y_dt = reinterpret_cast<const nanotime::dtime*>(&y[0]);
 
  Rcpp::List res = Rcpp::clone(ydata);

  // iterate through the rest of y columns and apply the ops:
  auto ix = 0;
  for (auto iy=1; iy<ydata.size(); ) {
    Rcpp::NumericVector xdata_col;
    if (xdata.size() == 2) {
      xdata_col = xdata[1];
    } else {
      ++ix;
      if (ix >= xdata.size()) {
        break;
      }
      xdata_col = xdata[ix];
    }

    // move to next numeric column of 'ydata':
    while (!check_numeric(ydata[iy]) && (iy < ydata.size())) {
     ++iy;
    }
    // if we got to the last columns it means we are done (not
    // reachable as we run out of x cols first, but in case that logic
    // changes, keep this check):
    if (iy == ydata.size()) {
      break;  // # nocov
    }

    Rcpp::NumericVector ydata_col = res[iy]; 
    ops_helper(x_dt,
               x.size(),
               y_dt,
               y.size(),
               xdata_col,
               ydata_col,
               op);
    // after cloning, strangely, it seems we get copies and not references to the
    // elements of the list; so we reassign 'ydata_col' back to res so res is
    // modified:
    res[iy] = ydata_col;

    ++iy;                       // increment for the next time round
  }
  
  return res;
}
