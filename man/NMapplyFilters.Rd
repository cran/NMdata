% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMapplyFilters.R
\name{NMapplyFilters}
\alias{NMapplyFilters}
\title{Translate filters in Nonmem and apply to data}
\usage{
NMapplyFilters(data, file, text, lines, invert = FALSE, as.fun, quiet)
}
\arguments{
\item{data}{An input data object. Could be read with NMreadCsv or
NMscanInput.}

\item{file}{Path to mod/lst file. Only one of file, text, or lines to be
given. See ?NMreadSection for understanding when to use, file, text, or
lines.}

\item{text}{The mod/lst as characters.}

\item{lines}{The mod/lst as character, line by line.}

\item{invert}{Invert the filters? This means read what Nonmem would
disregard, and disregard what Nonmem would read.}

\item{as.fun}{The default is to return data as a data.frame. Pass a function
(say tibble::as_tibble) in as.fun to convert to something else. If
data.tables are wanted, use as.fun="data.table". The default can be
configured using NMdataConf.}

\item{quiet}{Don't report information along the way if no warnings or
errors. Default is FALSE.}
}
\value{
data with filters applied
}
\description{
Translate filters in Nonmem and apply to data
}
\details{
This is not bulletproof. Nested conditions are not supported altogether.
}
\seealso{
Other Nonmem: 
\code{\link{NMextractText}()},
\code{\link{NMgenText}()},
\code{\link{NMreadSection}()},
\code{\link{NMreplaceDataFile}()},
\code{\link{NMwriteSection}()}
}
\concept{Nonmem}
\keyword{internal}
