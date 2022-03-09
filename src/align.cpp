// define align functions

#include <chrono>
#include <iterator>
#include <cstdint>
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
#if (__GNUC__ > 4 || ((__GNUC__ == 4) && __GNUC_MINOR__ > 8 && __GNUC_REVISION__ > 1))
  // Maybe the put_time will be implemented later?
  struct tm tm;
  // thanks to https://stackoverflow.com/a/38034148/143305 for the next test
  #if defined(__unix__)
  localtime_r(&time, &tm);
  #elif defined(_MSC_VER)
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
      while (ix <= xlen && x[ix] <= ystart) ++ix;
    } else {
      while (ix <= xlen && x[ix] < ystart) ++ix;
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
      while (ix <= xlen && x[ix] <= ystart) ++ix;
    } else {
      while (ix <= xlen && x[ix] < ystart) ++ix;
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
  SUBSET_DT_FUN *subsetDT = (SUBSET_DT_FUN *) R_GetCCallable("data.table", "CsubsetDT" );
                     
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
  SUBSET_DT_FUN *subsetDT = (SUBSET_DT_FUN *) R_GetCCallable("data.table", "CsubsetDT" );
                     
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
  try {
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
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;         // not reached
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
  try {
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
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)");
  }
  return R_NilValue;         // not reached
}


// [[Rcpp::export(.align_idx_duration_cpp)]]
Rcpp::NumericVector align_idx_duration(const Rcpp::NumericVector& x,     // nanotime vector
                                       const Rcpp::NumericVector& y,     // nanotime vector
                                       const Rcpp::NumericVector& start, // duration
                                       const Rcpp::NumericVector& end,   // duration
                                       const Rcpp::LogicalVector& sopen, // start open
                                       const Rcpp::LogicalVector& eopen) // end open
{
  try {
    return align_idx_helper_duration(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                                     x.size(),
                                     reinterpret_cast<const nanotime::dtime*>(&y[0]),
                                     y.size(),
                                     ConstPseudoVectorDuration(start),
                                     ConstPseudoVectorDuration(end),
                                     ConstPseudoVectorLgl(sopen),
                                     ConstPseudoVectorLgl(eopen));
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
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
  try {
    return align_idx_helper_period(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                                   x.size(),
                                   reinterpret_cast<const nanotime::dtime*>(&y[0]),
                                   y.size(),
                                   ConstPseudoVectorPrd(start),
                                   ConstPseudoVectorPrd(end),
                                   ConstPseudoVectorLgl(sopen),
                                   ConstPseudoVectorLgl(eopen),
                                   ConstPseudoVectorChar(tz));
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


// // template <typename T, typename F>
// // void op_zts(const arr::Vector<nanotime::dtime>& x, 
// //             const arr::Vector<nanotime::dtime>& y, 
// //             const arr::Vector<T>& xdata, 
// //             arr::Vector<T>& ydata) 
// // {
// //   size_t ix = 0;

// //   if (xdata.size() != x.size()) throw std::out_of_range("'xdata' must have same size as 'x'");   

// //   // for each point in x, we try to find a matching point or set of
// //   // points in y:
// //   auto from_yiter = y.begin();
// //   for (ix=0; ix<x.size(); ix++) {
// //     auto to_yiter = std::lower_bound(from_yiter, y.end(), x[ix]);
// //     if (to_yiter == y.end()) continue;

// //     auto iy_s = from_yiter-y.begin();
// //     auto iy_e = to_yiter-y.begin();
// //     F::f(xdata[ix], ydata.begin() + iy_s, ydata.begin() + iy_e);
      
// //     from_yiter = to_yiter;
// //   }
// // }

