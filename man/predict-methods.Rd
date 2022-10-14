\name{predict-methods}
\alias{predict-methods}
\alias{predict,twopartm-method}

\title{Method for Function \code{predict} for Two-part Model Fits in Package \pkg{twopartm}}

\description{
  Obtains predictions and optionally estimates standard errors of those predictions from a fitted two-part model object of class \code{twopartm}.
}

\usage{
\S4method{predict}{twopartm}(object,newdata = NULL, se.fit = FALSE,
dispersion_part1 = NULL,dispersion_part2 = NULL,na.action = na.pass)
}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link{tpm}}.}
    \item{newdata}{optionally, a data frame in which to look for variables with which to predict. If omitted, the original observations are used.}
    \item{se.fit}{logical switch indicating if standard errors are required.}
    \item{dispersion_part1}{the dispersion of the GLM fit to be assumed in computing the standard errors for the first-part model. If omitted, that returned by \code{summary} applied to the first-part model object is used.}
    \item{dispersion_part2}{the dispersion of the GLM fit to be assumed in computing the standard errors for the second-part model. If omitted, that returned by \code{summary} applied to the second-part model object is used.}
    \item{na.action}{function determining what should be done with missing values in \code{newdata}. The default is to predict \code{NA}.}
}

\details{
  The predictive values and corresponding standard errors are on the scales of the response variable not considering the link functions. The predictive responses are calculated by multiplying the predicted probabilities of non-zero responses and the fitted means of non-zero values. The prediction standard errors are calculated using delta method combining prediction standard errors from the models of both parts. If \code{newdata} is omitted the predictions are based on the data used for the fit. In that case how cases with missing values in the original fit is determined by the \code{na.action} argument of that fit.
}

\value{
  If \code{se.fit = FALSE}, a vector or matrix of predictions.
  If \code{se.fit = TRUE}, a list with components
  \item{fit}{Predictions, as for \code{se.fit = FALSE}.}
  \item{se.fit}{Estimated standard errors.}
  \item{residual.scale_part1}{A scalar giving the square root of the
    dispersion used in computing the standard errors for the first-part model.}
  \item{residual.scale_part2}{A scalar giving the square root of the
    dispersion used in computing the standard errors for the second-part model.}
}

\seealso{\code{\link{twopartm-class}}, \code{\link[twopartm]{tpm}}, \code{\link[twopartm]{AME}}, \code{\link[twopartm]{margin}}, \code{\link{glm}},  \code{\link{predict.glm}}
}

\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical Models in S}. Wadsworth & Brooks/Cole.
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

##get prediction results with standard errors for the
##first 500 observations in the dataset
predict(tpmodel,newdata = meps[1:500,],se.fit = TRUE)


##data for count response
data("bioChemists")

##fit two-part model with the same regressors in both parts, with logistic
##regression model for the first part, and poisson regression model with
##default log link for the second-part model
tpmodel = tpm(art ~ .,data = bioChemists,link_part1 = "logit",
family_part2 = poisson)

tpmodel

##get predictive counts
predict(tpmodel)

}

\keyword{methods}
