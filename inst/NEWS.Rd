\name{NEWS}
\title{News for Package \pkg{dtts}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/dtts/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/dtts/issues/#1}{##1}}

\section{Changes in version 0.1.3 (2024-07-18)}{
  \itemize{
    \item Windows builds use \code{localtime_s} with LLVM (Tomas Kalibera in
    \ghpr{16})
    \item Tests code has been adjusted for an upstream change in
    \pkg{data.table} tests for \code{all.equal} (Michael Chirico in \ghpr{18}
    addressing \ghit{17})
  }
}

\section{Changes in version 0.1.2 (2024-01-31)}{
  \itemize{
    \item Update the one exported C-level identifier from \pkg{data.table}
    following its 1.5.0 release and a renaming
    \item Routine continuous integration updates
  }
}

\section{Changes in version 0.1.1 (2023-08-08)}{
  \itemize{
    \item A simplifcation was applied to the C++ interface glue code
    (\ghpr{9} fixing \ghit{8})
    \item The package no longer enforces the C++11 compilation standard
    (\ghpr{10})
    \item An uninitialized memory read has been correct (\ghpr{11})
    \item A new function \code{ops} has been added (\ghpr{12})
    \item Function names no longer start with a dot (\ghpr{13})
    \item Arbitrary index columns are now supported (\ghpr{13})
  }
}

\section{Changes in version 0.1.0 (2022-03-06)}{
  \itemize{
    \item Initial CRAN upload.
    \item Package is functional and provides examples.
  }
}
