// define align functions

#include <chrono>
#include <cstdint>
#include <Rcpp.h>


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


namespace Global {
  using dtime = std::chrono::system_clock::time_point;
  using duration = dtime::duration;

  template<typename T, typename U, typename R>
  struct plus {
    inline R operator()(const T& t, const U& u) const {
      return t + u;
    }
  };
  
}

template <class Rep, class Period, class = std::enable_if_t<
          std::chrono::duration<Rep, Period>::min() < std::chrono::duration<Rep, Period>::zero()>>
          constexpr std::chrono::duration<Rep, Period> abs(std::chrono::duration<Rep, Period> d)
{
  return d >= d.zero() ? d : -d;
}
  
  

  template <typename T, typename U=T>
  struct PseudoVector {
    PseudoVector(const U* v_p, size_t vlen_p, size_t sz_p=0) : 
      v(v_p), vlen(vlen_p), scalar(vlen == 1), first_elt(v[0]), sz(sz_p) { }
    inline const U operator[](size_t i) const { return scalar ? first_elt : v[i]; }
    
    inline const T plus(T t, U u) const { return Global::plus<T,U,T>()(t, u); }
    inline size_t size() const { return scalar ? sz : vlen; }

  private:
    const U* v;
    const size_t vlen;
    const bool scalar;
    const U first_elt;
    const size_t sz;
  };



template <typename TM, typename DS, typename DE>
void align_idx(const TM*& x,
               size_t xlen,
               const TM*& y,
               size_t ylen,
               const DS& start, 
               const DE& end,
               Rcpp::NumericVector& res) 
{
  size_t ix = 0, iy = 0;

  // for each point in y, we try to find a matching point or set of
  // points in x:
  for (iy=0; iy<ylen; iy++) {
    auto ystart = start.plus(y[iy], start[iy]);
    auto yend   = end.plus(y[iy], end[iy]);
      
    // advance until we have a point in x that is in the interval
    // defined around yi:
    while (ix <= xlen && x[ix] < ystart) ++ix;
    if (ix >= xlen || x[ix] > yend) {
      res.push_back(NA_REAL);
      continue;
    }

    // find the closest point in the interval:
    while (ix+1 < xlen && x[ix+1] <= yend && abs(x[ix] - y[iy]) > abs(x[ix+1] - y[iy])) ++ix;
    res.push_back(ix + 1);     // +1 because of R numbering start convention
  }
}
  
  
// /// This is the same algorithm as 'align_idx', except that instead
// /// of returning a vector of indices, the vector 'ydata' is modified
// /// in place with the data pulled from x.
// template <typename T, typename NANF, 
//           typename DS, typename DE>
// void align_closest(const arr::Vector<Global::dtime>& x, 
//                    const arr::Vector<Global::dtime>& y, 
//                    const arr::Vector<T>& xdata, 
//                    arr::Vector<T>& ydata, 
//                    const DS& start, 
//                    const DE& end) 
// {
//   size_t ix = 0, iy = 0;

//   if (xdata.size() != x.size()) throw std::out_of_range("'xdata' must have same size as 'x'");   

//   // for each point in y, we try to find a matching point or set of
//   // points in x:
//   for (iy=0; iy<y.size(); iy++) {
//     auto ystart = start.plus(y[iy], start[iy]);
//     auto yend   = end.plus(y[iy], end[iy]);
    
//     // advance until we have a point in x that is in the interval
//     // defined around yi:
//     while (ix < x.size() && x[ix] < ystart) ++ix;
//     if (ix >= x.size() || x[ix] > yend) {
//       ydata.push_back(NANF::f());
//       continue;
//     }

//     // find the closest point in the interval:
//     while (ix+1 < x.size() && x[ix+1] <= yend && tz::abs(x[ix] - y[iy]) > tz::abs(x[ix+1] - y[iy]))
//       ++ix;
//     ydata.push_back(xdata[ix]);
//   }
// }


// template <typename T, typename F,
//           typename DS, typename DE>
// void align_func(const arr::Vector<Global::dtime>& x, 
//                 const arr::Vector<Global::dtime>& y, 
//                 const arr::Vector<T>& xdata, 
//                 arr::Vector<T>& ydata, 
//                 const DS& start, 
//                 const DE& end) 
// {
//   size_t ix = 0, iy = 0;

//   if (xdata.size() != x.size()) throw std::out_of_range("'xdata' must have same size as 'x'");   

//   // for each point in y, we try to find a matching point or set of
//   // points in x:
//   for (iy=0; iy<y.size(); iy++) {
//     auto ystart = start.plus(y[iy], start[iy]);
//     auto yend   = end.plus(y[iy], end[iy]);
    
//     // advance until we have a point in x that is in the interval
//     // defined around yi:
//     auto iter = std::lower_bound(x.begin() + ix, x.end(), ystart);
//     ix = iter - x.begin();
      
//     if (ix >= x.size() || x[ix] >= yend) {
//       ydata.push_back(F::f(xdata.end(), xdata.end())); // empty interval
//       continue;
//     }
//     typename arr::Vector<T>::const_iterator istart(xdata, ix);
//     auto first_ix = ix;

//     // find the last point in the interval:
//     iter = std::lower_bound(x.begin() + ix, x.end(), yend);
//     ix = iter - x.begin();
//     while (ix < x.size() && x[ix] < yend) ++ix;
//     typename arr::Vector<T>::const_iterator iend(xdata, ix);

//     ydata.push_back(F::f(istart, iend));

//     // reset ix to the first ix found, because the intervals
//     // specified could overlap:
//     ix = first_ix;
//   }
// }

// template <typename T, typename F>
// void op_zts(const arr::Vector<Global::dtime>& x, 
//             const arr::Vector<Global::dtime>& y, 
//             const arr::Vector<T>& xdata, 
//             arr::Vector<T>& ydata) 
// {
//   size_t ix = 0;

//   if (xdata.size() != x.size()) throw std::out_of_range("'xdata' must have same size as 'x'");   

//   // for each point in x, we try to find a matching point or set of
//   // points in y:
//   auto from_yiter = y.begin();
//   for (ix=0; ix<x.size(); ix++) {
//     auto to_yiter = std::lower_bound(from_yiter, y.end(), x[ix]);
//     if (to_yiter == y.end()) continue;

//     auto iy_s = from_yiter-y.begin();
//     auto iy_e = to_yiter-y.begin();
//     F::f(xdata[ix], ydata.begin() + iy_s, ydata.begin() + iy_e);
      
//     from_yiter = to_yiter;
//   }
// }


RcppExport SEXP _align_idx(SEXP x,     // nanotime vector
                           SEXP y,     // nanotime vector
                           SEXP start, // duration (or period?)
                           SEXP end)   // duration (or period?)
{
  const Rcpp::NumericVector nvx(x);
  const Rcpp::NumericVector nvy(y);
  const Global::dtime* vx = reinterpret_cast<const Global::dtime*>(&nvx[0]);
  const Global::dtime* vy = reinterpret_cast<const Global::dtime*>(&nvy[0]);
  Rcpp::NumericVector res;
  const Rcpp::NumericVector nvstart(start);
  const Rcpp::NumericVector nvend(end);
  const Global::duration* sstart = reinterpret_cast<const Global::duration*>(&nvstart[0]);
  const Global::duration* send   = reinterpret_cast<const Global::duration*>(&nvend[0]);

  align_idx(vx, nvx.size(),
            vy, nvy.size(),
            PseudoVector<Global::dtime, Global::duration>(sstart, nvstart.size(), nvy.size()),
            PseudoVector<Global::dtime, Global::duration>(send,   nvend.size(),   nvy.size()),
            res);

  return res;
}

RcppExport SEXP _align_closest(SEXP x,
                               SEXP y,
                               SEXP xdata,
                               SEXP ydata,
                               SEXP start,
                               SEXP end,
                               SEXP func)
{
  

}

RcppExport SEXP _align_func(SEXP x,
                            SEXP y,
                            SEXP xdata,
                            SEXP ydata,
                            SEXP start,
                            SEXP end,
                            SEXP func) {
  

}


// the op is cool too, so do that... LLL
