\name{coef-methods}
\alias{coef,twopartm-method}

\title{Method for Function \code{coef} for Two-part Model Objects in Package \pkg{twopartm}}

\description{
  The \code{coef} method for \code{\link{twopartm-class}} that extracts model coefficients from a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{coef}{twopartm}(object,model = c("tpm","model1","model2"),\dots)
}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link[twopartm]{tpm}}.}
    \item{model}{character specifying for which part of the model the coefficients should be extracted. It could be either \dQuote{tpm} for the full two-part model, or \dQuote{model1}, \dQuote{model2} for the first-part model and the second-part model respectively.The default is \dQuote{tpm}.}
    \item{\dots}{arguments passed to \code{\link[stats]{coef}} in the default setup.}
}

\details{
  The \code{coef} method for \code{\link{twopartm-class}} by default return a list including two vectors of estimated coefficients for both parts models. By setting the \code{model} argument, the model coefficients for the corresponding model component can be extracted.
}

\value{
Coefficients extracted from the model object \code{twopartm}.

With argument \code{model == "tpm"} this will be a list of two numeric vectors of model coefficients for both parts models. With \code{model == "model1" | "model2"} it will be a numeric vector of coefficients for the selected part's model.

}

\seealso{\code{\link{twopartm-class}}, \code{\link{glm}},\code{\link{coef}}, \code{\link[twopartm]{tpm}}
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

##summary information
summary(tpmodel)

##estimated coefficients for both parts
coef(tpmodel)

##estimated coefficients for the first-part model
coef(tpmodel,model = "model1")

}

\keyword{methods}
