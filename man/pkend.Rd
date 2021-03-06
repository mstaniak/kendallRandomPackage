% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stable_kendall_distribution.R
\name{pkend}
\alias{pkend}
\title{CDF of Kendall stable distribution}
\usage{
pkend(m_alpha)
}
\arguments{
\item{m_alpha}{function giving moments of order alpha of step dist.}
}
\value{
function function giving values of CDF of Kendall stable distribution
}
\description{
CDF of Kendall stable distribution
}
\examples{
pKend <- pkend(function(x) 1)
# Step distribution: delta_{1}
pKendall <- pKend(1:10, 0.5)
# Values of CDF for arguments 1:10 and alpha = 0.5


}
