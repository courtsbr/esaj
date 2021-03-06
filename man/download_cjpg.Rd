% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cjpg.R
\name{download_cjpg}
\alias{download_cjpg}
\title{Download results of a query on first degree lawsuits filed
in Brazilian Justice Courts}
\usage{
download_cjpg(query, path = ".", classes = "", subjects = "",
  courts = "", date_start = "", date_end = "", min_page = 1,
  max_page = 1, cores = 1, tj = "tjsp")
}
\arguments{
\item{query}{Character vector with search query}

\item{path}{Path to directory where to save HTMLs}

\item{classes}{Character vector with class IDs (see \code{\link[=cjpg_table]{cjpg_table()}})}

\item{subjects}{Character vector with subject IDs (see \code{\link[=cjpg_table]{cjpg_table()}})}

\item{courts}{Character vector with court IDs (see \code{\link[=cjpg_table]{cjpg_table()}})}

\item{date_start}{Lower bound for date}

\item{date_end}{Upper bound for date}

\item{min_page}{First page of results to download}

\item{max_page}{Last page of results to download}

\item{cores}{The number of cores to be used when downloading}

\item{tj}{TJ from which to get data (only works with TJSP for now)}
}
\value{
A character vector with the paths to the downloaded files
}
\description{
Downloads an HTML with the results obtained from
querying a dataset of all first degree lawsuits and then one
HTML for each page of results (at most \code{max_page} pages). \code{query}
should be the string to look for in the lawsuits and \code{clases},
\code{courts}, etc. should be the filtering parameters (make sure
to use \code{\link[=cjpg_table]{cjpg_table()}} to get lists of all valid codes for these
arguments).
}
\seealso{
\code{\link[=cjpg_table]{cjpg_table()}}, \code{\link[=browse_table]{browse_table()}}
}
