\name{NEWS}
\title{News for Package \pkg{dtts}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/dtts/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/dtts/issues/#1}{##1}}

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
