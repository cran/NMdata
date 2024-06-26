% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnAppend.R
\name{fnAppend}
\alias{fnAppend}
\title{paste something before file name extension.}
\usage{
fnAppend(fn, x, pad0 = 0, sep = "_")
}
\arguments{
\item{fn}{The file name or file names to modify.}

\item{x}{A character string or a numeric to add to the file name}

\item{pad0}{In case x is numeric, a number of zeros to pad before
the appended number. This is useful if you are generating say
more than 10 files, and your counter will be 01, 02,..,
10,... and not 1, 2,...,10,...}

\item{sep}{The separator between the existing file name (until
extension) and the addition.}
}
\value{
A character (vector)
}
\description{
Append a file name like file.mod to file_1.mod or file_pk.mod. If
it's a number, we can pad some zeros if wanted. The separator
(default is underscore) can be modified.
}
