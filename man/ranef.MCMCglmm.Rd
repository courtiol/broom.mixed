% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMCglmm_tidiers.R
\name{ranef.MCMCglmm}
\alias{ranef.MCMCglmm}
\title{Extract random effects from an \code{MCMCglmm} object}
\usage{
\method{ranef}{MCMCglmm}(object, use = c("all", "mean"), ...)
}
\arguments{
\item{object}{An \code{MCMCglmm} model object to extract the effects from}

\item{use}{A character string indicating whether to extract
all posterior samples or the mean of the posteriors. Defaults to
"all".}

\item{\dots}{Arguments passed on to the worker function.}
}
\value{
A matrix of the fixed effects
}
\description{
Function designed to extract the random effects from an
\code{MCMCglmm} model object. Can either extract all samples from the
random effects posteriors or return the posterior means.
}
\examples{
\dontrun{
  # a simple MCMCglmm model
  data(PlodiaPO)
  m <- MCMCglmm(PO ~ 1, random= ~ FSfamily, data=PlodiaPO, pr=TRUE, verbose=FALSE)

  # only extract average fixed effects
  head(ranef(m, use = "mean"))

  # histogram of posterior samples of fixed effects
  hist(ranef(m)[1, ])
  # matches the mean
  rowMeans(ranef(m)[1:6, ])
}
}
\seealso{
\code{\link{fixef.MCMCglmm}}
}
