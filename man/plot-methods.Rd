\name{plot-methods}
\alias{plot,twopartm,missing-method}

\title{Method for Function \code{plot} for Two-part Model Objects in Package \pkg{twopartm}}

\description{
  The \code{plot} method for \code{\link{twopartm-class}} that provides plot diagnostics for a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{plot}{twopartm,missing}(x, y, \dots)
}

\arguments{
    \item{x}{an object of class \code{twopartm}.}
    \item{y}{not used.}
    \item{\dots}{arguments passed to \code{\link[stats]{plot.lm}} in the default setup.}
}

\details{
  The \code{plot} method for \code{\link{twopartm-class}} returns the residual plot for the full two-part model, and also six plots for each part's glm model. Six plots are: a plot of residuals against fitted values, a Scale-Location plot of sqrt(| residuals |) against fitted values, a Normal Q-Q plot, a plot of Cook's distances versus row labels, a plot of residuals against leverages, and a plot of Cook's distances against leverage/(1-leverage). By default, the first three plots and the fifth one of each part's model are provided. The plots for each part's model could be selected by argument \code{which} of function \code{plot.lm} for glm model object.
}

\value{
Returns residual plot for the full two-part model, and plot diagnostics for each part's model from an object \code{twopartm}.
}

\seealso{\code{\link{twopartm-class}}, \code{\link{glm}},\code{\link{plot.lm}}, \code{\link[twopartm]{tpm}}
}


\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Belsley, D. A., Kuh, E. and Welsch, R. E. (1980). \emph{Regression Diagnostics}. New York: Wiley.

Cook, R. D. and Weisberg, S. (1982). \emph{Residuals and Influence in Regression}. London: Chapman and Hall.
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

##plots for two-part model
plot(tpmodel)

}

\keyword{methods}
