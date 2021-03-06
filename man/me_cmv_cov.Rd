% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/me_cmv_cov.R
\name{me_cmv_cov}
\alias{me_cmv_cov}
\title{Adjust a covariance matrix for Common Method Variance (CMV) and Quality of Sum score}
\usage{
me_cmv_cov(.medesign)
}
\arguments{
\item{.medesign}{An \code{medesign} object given by \code{\link{medesign}}}
}
\value{
The common-method-variance and quality corrected covariance matrix.
}
\description{
\code{me_cmv_cov} accepts an \code{medesign} object specified in
\code{\link{medesign}} and adjusts the covariance coefficients of
common method variables with the reliability and validity coefficients
from \code{me_data} as well as the quality of sum scores.
}
\examples{

set.seed(2131)

# measurement data extract from SQP for Spain, Round 7 in Spanish
me_data <-
data.frame(
  question = c("ppltrst", "polintr", "trstprl", "trstplt", "trstprt", "stfedu", "stfhlth"),
  reliability = c(0.737, 0.624, 0.812, 0.852, 0.858, 0.757, 0.76),
  validity = c(0.952, 0.964, 0.959, 0.965, 0.956, 0.838, 0.798),
  quality = c(0.702, 0.601, 0.779, 0.822, 0.821, 0.635, 0.607)
)

# Define a measurement error model with two sum scores
# and sharing a common method variance between ppltrst
# and the zpoltrst sum score
m1 <- "std(zserv) = stfhlth + stfedu;
       std(zpoltrst) = trstprl + trstplt + trstprt;
       ~~ .;
       ~ ppltrst + zpoltrst"

# Create measurement error design
mdes <-
  medesign(
    model_syntax = m1,
    .data = ess7es[me_data$question],
    me_data = me_data
  )

mdes

# Original covariance matrix
me_covariance(mdes$.data)

# Coefficients of covariance changes
# when adjusting for common method variance and
# quality of sum scores
me_cmv_cov(mdes)


}
\seealso{
\code{link{medesign}} and \code{\link{me_cmv_cor}} for the same
adjustment but for a correlation matrix.
}
