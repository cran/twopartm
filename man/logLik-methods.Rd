\name{logLik-methods}
\alias{logLik,twopartm-method}

\title{Method for Function \code{logLik} for Two-part Model Objects in Package \pkg{twopartm}}

\description{
  The \code{logLik} method for \code{\link{twopartm-class}} that extracts log-likelihood from a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{logLik}{twopartm}(object,\dots)
}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link[twopartm]{tpm}}.}
    \item{\dots}{arguments passed to \code{\link[stats]{logLik}} in the default setup.}
}

\details{
  The \code{logLik} method for \code{\link{twopartm-class}} returns an object of class \code{logLik}, including the log likelihood value with degree of freedom of a fitted two-part regression model object of class \code{twopartm}.
}

\value{
Returns an object of class \code{logLik} for model object \code{twopartm}.This is a number with at least one attribute, "df" (degrees of freedom), giving the number of (estimated) parameters in the two-part model.
}

\seealso{\code{\link{twopartm-class}}, \code{\link{glm}},\code{\link{logLik.lm}}, \code{\link[twopartm]{tpm}}
}


\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Harville, D.A. (1974). Bayesian inference for variance components using only error contrasts. \emph{Biometrika}, 61, 383–385. doi: 10.2307/2334370.
}


\author{Yajie Duan, Birol Emir, Griffith Bell and Javier Cabrera}


\examples{

##data about health expenditures, i.e., non-negative continuous response
data(meps,package = "twopartm")


##fit two-part model with the same regressors in both parts, with logistic
##regression model for the first part, and glm with Gamma family with log
##link for the second-part model
tpmodel = tpm(exp_tot~female+age, data = meps,link_part1 = "logit",
family_part2 = Gamma(link = "log"))

tpmodel

##summary information
summary(tpmodel)

##log-likehood
logLik(tpmodel)
}

\keyword{methods}
