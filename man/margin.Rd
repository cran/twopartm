\name{margin}
\alias{margin}
\alias{margin,twopartm-method}

\title{Predictive Margins with CIs for Two-part Model Objects}

\description{
  Calculate predictive margins with CIs for variables from a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{margin}{twopartm}(object, newdata = NULL, term = NULL, value = NULL,
se = TRUE, se.method = c("delta","bootstrap"), CI = TRUE, CI.boots = FALSE,
level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50)

}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link[twopartm]{tpm}}.}
    \item{newdata}{optionally, a data frame in which to look for variables with which to calculate predictive margins. If omitted, the original observations are used.}
    \item{term}{A character vector with the names of variables for which to compute the predictive margins. The default (NULL) returns predictive margins for all variables.}
    \item{value}{A list of one or more named vectors, specifically values at which to calculate the predictive marginal effects. If omitted, for factor or logical variables, predictive margins at all the levels are calculated, and for numeric (and integer) variables, predictive margins at the mean values among observations are calculated.  Note: This does not calculate predictive margins for subgroups but rather for whole datasets; to obtain subgroup margins, subset data directly.}
    \item{se}{logical switch indicating if standard errors are required.}
    \item{se.method}{A character string indicating the type of estimation procedure to use for estimating variances of predictive margins. The default (“delta”) uses the delta method. The alternative is “bootstrap”, which uses bootstrap estimation.}
    \item{CI}{logical switch indicating if confidence intervals are required.}
    \item{CI.boots}{if \code{se.method == "bootstrap"}, logical switch indicating if confidence intervals are obtained by normal-based or by bootstrap quantiles.}
    \item{level}{A numeric value specifying the confidence level for calculating p-values and confidence intervals.}
    \item{eps}{A numeric value specifying the “step” to use when calculating numerical derivatives.}
    \item{na.action}{function determining what should be done with missing values in \code{newdata}. The default is to predict \code{NA}.}
    \item{iter}{if \code{se.method == "bootstrap"}, the number of bootstrap iterations.}

}

\details{
    Predictive margins are calculated by taking average values of predictive responses at specified levels for factor and logical variables, or specified values for continuous variables. If \code{value = NULL} (the default), for factor or logical variables, predictive margins at all the levels are calculated, and for numeric (and integer) variables, predictive margins at the mean values among observations are calculated. Otherwise, predictive margins at values specified in \code{value} are calculated.Margins are calculated based on the original observations used in the fitted two-part model, or the new data set that \code{newdata} inputs.

    The standard errors of predictive margins could be calculated using delta method or bootstrap method. The delta method considers the average Jacobian vectors among observations with respect to both models' parameters, assuming independence between models from two parts. The Jacobian vectors are approximated by numerical differentiations. The bootstrap method generates bootstrap samples to fit two-part models, and get variances and inverted bootstrap quantile CIs or normal-based CIs of predictive margins. If \code{se == T}, the returned data frames also have columns indicating z-statistics and p-values that are calculated by normal assumption and input \code{level}, and with CIs if \code{CI == T}.

    If there are two or more values or levels of variables to be concerned for predictive margins, the ratios between calculated predictive margins are calculated. If \code{se == T} and \code{CI == T}, CIs at levels specified by \code{level} of the ratios are calculated by Fieller’s theorem using the covariance matrices between predictive margins obtained by delta or bootstrap methods. Fieller's theorem is a statistical method to calculate a confidence interval for the ratio of two means.

}

\value{
    A list of data frames of estimated predictive margins for all independent variables in the fitted two-part model or the variables that \code{term} specifies, if \code{se == T}, with standard errors of AMEs, z-statistics and p-values that are calculated by normal assumption and input \code{level}, and with CIs if \code{CI == T}. If \code{values = NULL} (the default), for factor or logical variables, predictive margins at all the levels are returned, and for numeric (and integer) variables, predictive margins at the mean values among observations are returned. Otherwise, predictive margins at specified values are returned. If there are two or more values or levels of variables to be concerned for predictive margins, a data frame including ratios between calculated predictive margins is also returned, if \code{se == T} and \code{CI == T}, with CIs at levels specified by \code{level} of the ratios.
}

\seealso{\code{\link{twopartm-class}}, \code{\link[twopartm]{tpm}}, \code{\link[twopartm]{predict-methods}}, \code{\link[twopartm]{AME}}, \code{\link{glm}}
}


\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Leeper, T.J. (2017). Interpreting regression results using average marginal effects with R’s margins. \emph{Available at the comprehensive R Archive Network (CRAN)}, pp.1-32.

Leeper, T.J., Arnold, J. and Arel-Bundock, V. (2017). Package ‘margins’. \emph{accessed December}, 5, p.2019.

Fieller, E.C. (1954). Some problems in interval estimation. \emph{Journal of the Royal Statistical Society: Series B (Methodological)}, 16(2), pp.175-185.

O’Hagan, A., Stevens, J.W. and Montmartin, J. (2000). Inference for the cost-effectiveness acceptability curve and cost-effectiveness ratio. \emph{Pharmacoeconomics}, 17(4), pp.339-349.

}


\author{Yajie Duan, Birol Emir, Griffith Bell and Javier Cabrera}


\examples{

##data about health expenditures, i.e., non-negative continuous response
data(meps,package = "twopartm")


##fit two-part model with different regressors in both parts, with probit
##regression model for the first part, and glm with Gamma family with log
##link for the second-part model
tpmodel = tpm(formula_part1 = exp_tot~female+age, formula_part2 =
exp_tot~female+age+ed_colplus,data = meps,link_part1 = "logit",
family_part2 = Gamma(link = "log"))

tpmodel

summary(tpmodel)

##Predictive margins and corresponding ratios for all variables with
##standard errors and CIs.
margin(tpmodel)


##Predictive margins and corresponding ratios for female, age at
##20,40,60,80, and more than college education level, resepectively
margin(tpmodel,value = list(female = 1,age = c(50,70),ed_colplus = 1))



##data for count response
data("bioChemists")

##fit two-part model with the same regressors in both parts, with logistic
##regression model for the first part, and poisson regression model with
##default log link for the second-part model
tpmodel = tpm(art ~ .,data = bioChemists,link_part1 = "logit",
family_part2 = poisson)

tpmodel


##Predictive margins and corresponding ratios for variable "kid5"
##at 2,3, with standard errors by bootstrap methods,
##and CIs by bootstrap quantiles
margin(tpmodel,term = "kid5",value = list(kid5 = c(2,3)),
se.method = "bootstrap",CI.boots = TRUE,iter = 20)

##Predictive margins and corresponding ratios for variable "ment" at
##6,7,8, without standard errors and CIs
margin(tpmodel,term = "ment",value = list(ment = c(6,7,8)),se = FALSE)


##Predictive margins and corresponding ratios for all the levels of
##variable "mar", and for variable "phd" at 2.5,3.2, calculated on
##the first 500 observations, with standard errors and CIs
margin(tpmodel,newdata = bioChemists[1:500,],term = c("phd","mar"),
value = list(phd = c(2.5,3.2)))

}

\keyword{methods}

