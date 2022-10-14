\name{residuals-methods}
\alias{residuals,twopartm-method}

\title{Method for Function \code{residuals} for Two-part Model Objects in Package \pkg{twopartm}}

\description{
  The \code{residuals} method for \code{\link{twopartm-class}} that extracts model residuals from a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{residuals}{twopartm}(object,model = c("tpm","model1","model2"),
type = c("deviance", "pearson", "working","response", "partial"))

}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link[twopartm]{tpm}}.}
    \item{model}{character specifying for which part of the model the residuals should be extracted. It could be either \dQuote{tpm} for the full two-part model, or \dQuote{model1}, \dQuote{model2} for the first-part model and the second-part model respectively.The default is \dQuote{tpm}.}
    \item{type}{if \code{model == "model1" | "model2"}, the type of residuals which should be returned. The alternatives are: \dQuote{response} (default), \dQuote{pearson}, \dQuote{working}, \dQuote{deviance}, and \dQuote{partial}. Can be abbreviated.}
}

\details{
  The \code{residuals} method for \code{\link{twopartm-class}} can compute raw response residuals (observed - fitted) for the full two-part model, or different types of residues from both parts models respectively. The references define the types of residuals: Davison & Snell is a good reference for the usages of each. The partial residuals are a matrix of working residuals, with each column formed by omitting a term from the model.
}

\value{
Returns a numerical vector of residuals, either for the full two-part model, or two separate part models from an object \code{twopartm}.
}

\seealso{\code{\link{twopartm-class}}, \code{\link{glm}},\code{\link{residuals.glm}}, \code{\link[twopartm]{tpm}},  \code{\link[twopartm]{predict-methods}}
}


\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Davison, A. C. and Snell, E. J. (1991). Residuals and diagnostics. \emph{Statistical Theory and Modeling}. In Honour of Sir David Cox, FRS, eds. Hinkley, D. V., Reid, N. and Snell, E. J., Chapman and Hall.
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

##response residues from the full two-part model
residuals(tpmodel)

##response residues from the first-part model
residuals(tpmodel,model = "model1")

##deviance residues from the second-part model
residuals(tpmodel,model = "model2",type = "deviance")


}

\keyword{methods}
