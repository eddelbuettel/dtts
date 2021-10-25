// define align functions

#include <chrono>
#include <cstdint>
#include <Rcpp.h>
#include <R_ext/Rdynload.h>
#include "nanotime/globals.hpp"
#include "nanotime/pseudovector.hpp"


// a thin wrapper that gives us the vector recycling behaviour of R:
// template <int T, typename U, typename R=U>
// struct PseudoVector {
//   PseudoVector(Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
//   inline R& operator[](R_xlen_t i) {
//     return i<sz ? *reinterpret_cast<R*>(&v[i]) : *reinterpret_cast<R*>(v[i%sz]);
//   }
//   inline R_xlen_t size() const { return sz; }
//   inline bool isScalar() const { return v.size()==1; }
// private:
//   Rcpp::Vector<T>& v;
//   const R_xlen_t sz;
// };
// template <int T, typename U, typename R=U>
// struct ConstPseudoVector {
//   ConstPseudoVector(const Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
//   inline const R& operator[](R_xlen_t i) const {
//     return i<sz ? *reinterpret_cast<const R*>(&v[i]) : *reinterpret_cast<const R*>(&v[i%sz]);
//   }
//   inline R_xlen_t size() const { return sz; }
//   inline bool isScalar() const { return v.size()==1; }
// private:
//     const Rcpp::Vector<T>& v;
//     const R_xlen_t sz;
// };


// character is a special case that needs a bit of help:
// template <>
// struct ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> {
//   ConstPseudoVector(const Rcpp::Vector<STRSXP>& v_p) : v(v_p), sz(v_p.size()) { }
//   inline const Rcpp::CharacterVector::const_Proxy operator[](R_xlen_t i) const { return i<sz ? v[i] : v[i%sz]; }
//   inline R_xlen_t size() const { return sz; }
//   inline bool isScalar() const { return v.size()==1; }
// private:
//     const Rcpp::Vector<STRSXP>& v;
//     const R_xlen_t sz;
// };


typedef nanotime::ConstPseudoVector<REALSXP, double, nanotime::duration> ConstPseudoVectorDuration;


// for debug reasons...
// the following code from: https://stackoverflow.com/a/16692519
template<typename Clock, typename Duration>
std::ostream &operator<<(std::ostream &stream,
                         const std::chrono::time_point<Clock, Duration> &time_point) {
  const time_t time = Clock::to_time_t(time_point);
#if __GNUC__ > 4 || \
    ((__GNUC__ == 4) && __GNUC_MINOR__ > 8 && __GNUC_REVISION__ > 1)
  // Maybe the put_time will be implemented later?
  struct tm tm;
  localtime_r(&time, &tm);
  return stream << std::put_time(&tm, "%c"); // Print standard date&time
#else
  char buffer[26];
  ctime_r(&time, buffer);
  buffer[24] = '\0';  // Removes the newline that is added
  return stream << buffer;
#endif
}


// namespace Global {
//   using dtime = std::chrono::system_clock::time_point;
//   using duration = dtime::duration;

//   template<typename T, typename U, typename R>
//   struct plus {
//     inline R operator()(const T& t, const U& u) const {
//       return t + u;
//     }
//   };
  
// }

// template <class Rep, class Period, class = std::enable_if_t<
//           std::chrono::duration<Rep, Period>::min() < std::chrono::duration<Rep, Period>::zero()>>
//           constexpr std::chrono::duration<Rep, Period> abs(std::chrono::duration<Rep, Period> d)
// {
//   return d >= d.zero() ? d : -d;
// }
  
  
// // wraps without cost either a scalar or a C vector so it can be used
// // with the same interface in the various templated functions
// template <typename T, typename U=T>
// struct PseudoVector {
//   PseudoVector(const U* v_p, size_t vlen_p, size_t sz_p=0) : 
//     v(v_p), vlen(vlen_p), scalar(vlen == 1), first_elt(v[0]), sz(sz_p) { }
//   inline const U operator[](size_t i) const { return scalar ? first_elt : v[i]; }
  
//   inline const T plus(T t, U u) const { return nanotime::plus<T,U,T>()(t, u); }
//   inline size_t size() const { return scalar ? sz : vlen; }
  
// private:
//   const U* v;
//   const size_t vlen;
//   const bool scalar;
//   const U first_elt;
//   const size_t sz;
// };


nanotime::duration abs_duration(nanotime::duration d)
{
  if (d >= d.zero())
    return d;
  return -d;
}


void align_idx_helper(const nanotime::dtime* x,
                      size_t xlen,
                      const nanotime::dtime* y,
                      size_t ylen,
                      const ConstPseudoVectorDuration& start,
                      const ConstPseudoVectorDuration& end,
                      Rcpp::NumericVector& res) 
{
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    auto ystart = y[iy] + start[iy];
    auto yend   = y[iy] + end[iy];
      
    // advance until we have a point in x that is in the interval
    // defined around yi:
    while (ix <= xlen && x[ix] < ystart) ++ix;
    if (ix >= xlen || x[ix] > yend) {
      res.push_back(NA_REAL);
      continue;
    }

    // find the closest point in the interval:
    while (ix+1 < xlen && x[ix+1] <= yend && abs_duration(x[ix] - y[iy]) > abs_duration(x[ix+1] - y[iy])) ++ix;
    res.push_back(ix + 1);     // +1 because of R numbering start convention
  }
}
  

static Rcpp::IntegerVector makeIndex(size_t start, size_t end) {
  Rcpp::IntegerVector res(end - start);
  size_t off = 0;
  for (size_t i=start; i<end; ++i) {
    res[off++] = i; 
  }
  return res;
}


Rcpp::List align_func(const nanotime::dtime* x,
                      size_t xlen,
                      const nanotime::dtime* y,
                      size_t ylen,
                      Rcpp::List xdata, 
                      const ConstPseudoVectorDuration& start,
                      const ConstPseudoVectorDuration& end,
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
    auto iter = std::lower_bound(x + ix, x+xlen, ystart);
    ix = iter - x;
      
    if (ix >= xlen || x[ix] >= yend) {
      const SEXP rows = Rcpp::IntegerVector::create(0);
      res.push_back(func(subsetDT(xdata, rows, cols))); // empty interval
      continue;
    }
    auto first_ix = ix;

    // find the last point in the interval:
    iter = std::lower_bound(x + ix, x+xlen, yend);
    ix = iter - x;
    while (ix < xlen && x[ix] < yend) ++ix;

    const SEXP rows = makeIndex(first_ix+1, ix+1); // subsetDT is 1-based indexing
    res.push_back(func(subsetDT(xdata, rows, cols)));

    // reset ix to the first ix found, because the intervals
    // specified could overlap:
    ix = first_ix;
  }
  return res;
}



// [[Rcpp::export]]
Rcpp::List align(const Rcpp::NumericVector& x, // nanotime vector
                 const Rcpp::NumericVector& y,         // nanotime vector
                 const Rcpp::List xdata,               // DT
                 const Rcpp::NumericVector& start,     // duration (or period?)
                 const Rcpp::NumericVector& end,       // duration (or period?)
                 const Rcpp::Function func)            // function to apply (character)
{
  try {
    return align_func(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                      x.size(),
                      reinterpret_cast<const nanotime::dtime*>(&y[0]),
                      y.size(),
                      xdata,
                      ConstPseudoVectorDuration(start),
                      ConstPseudoVectorDuration(end),
                      Rcpp::Function(func));
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;         // not reached
}


// [[Rcpp::export]]
Rcpp::NumericVector align_idx(Rcpp::NumericVector x,     // nanotime vector
                              Rcpp::NumericVector y,     // nanotime vector
                              Rcpp::NumericVector start, // duration (or period?)
                              Rcpp::NumericVector end)   // duration (or period?)
{
  try {
    Rcpp::NumericVector res;

    align_idx_helper(reinterpret_cast<const nanotime::dtime*>(&x[0]),
                     x.size(),
                     reinterpret_cast<const nanotime::dtime*>(&y[0]),
                     y.size(),
                     ConstPseudoVectorDuration(start),
                     ConstPseudoVectorDuration(end),
                     res);

    return res;
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

