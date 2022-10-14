\name{AME}
\alias{AME}
\alias{AME,twopartm-method}

\title{Average Marginal Effect (AME) with CIs for Two-part Model Objects}

\description{
  Calculate average marginal effects (AMEs) with CIs for variables from a fitted two-part regression model object of class \code{twopartm}.
}

\usage{
\S4method{AME}{twopartm}(object, newdata = NULL, term = NULL, at = NULL, se = TRUE,
se.method = c("delta","bootstrap"), CI = TRUE,CI.boots = FALSE,
level = 0.95,eps = 1e-7,na.action = na.pass, iter = 50)

}

\arguments{
    \item{object}{a fitted two-part model object of class \code{twopartm} as returned by \code{\link[twopartm]{tpm}}.}
    \item{newdata}{optionally, a data frame in which to look for variables with which to calculate average marginal effects. If omitted, the original observations are used.}
    \item{term}{A character vector with the names of variables for which to compute the average marginal effects. The default (NULL) returns average marginal effects for all variables.}
    \item{at}{A list of one or more named vectors, specifically values at which to calculate the average marginal effects. The specified values are fully combined (i.e., a cartesian product) to find AMEs for all combinations of specified variable values. These are used to modify the value of data when calculating AMEs across specified values. Note: This does not calculate AMEs for subgroups but rather for counterfactual datasets where all observations take the specified values; to obtain subgroup effects, subset data directly.}
    \item{se}{logical switch indicating if standard errors are required.}
    \item{se.method}{A character string indicating the type of estimation procedure to use for estimating variances of AMEs. The default (“delta”) uses the delta method. The alternative is “bootstrap”, which uses bootstrap estimation.}
    \item{CI}{logical switch indicating if confidence intervals are required.}
    \item{CI.boots}{if \code{se.method == "bootstrap"}, logical switch indicating if confidence intervals are obtained by normal-based or by bootstrap quantiles.}
    \item{level}{A numeric value specifying the confidence level for calculating p-values and confidence intervals.}
    \item{eps}{A numeric value specifying the “step” to use when calculating numerical derivatives.}
    \item{na.action}{function determining what should be done with missing values in \code{newdata}. The default is to predict \code{NA}.}
    \item{iter}{if \code{se.method == "bootstrap"}, the number of bootstrap iterations.}

}

\details{

    For factor variables, the average value of discrete first-differences in predicted outcomes are calculated as AME (i.e., change in predicted outcome when factor is set to a given level minus the predicted outcome when the factor is set to its baseline level).  If you want to use numerical differentiation for factor variables (which you probably do not want to do), enter them into the original modeling function as numeric values rather than factors. For logical variables, the same approach as factors is used, but always moving from \code{FALSE} to \code{TRUE}.

    For numeric (and integer) variables, the method calculates an instantaneous marginal effect using a simple \dQuote{central difference} numerical differentiation:
    \deqn{\frac{f(x + \frac{1}{2}h) - f(x - \frac{1}{2}h)}{dh}}{(f(x + 0.5h) - f(x - 0.5h))/(2h)}, where (\eqn{h = \max(|x|, 1) \sqrt{\epsilon}}{h = max(|x|, 1)sqrt(epsilon)} and the value of \eqn{\epsilon}{epsilon} is given by argument \code{eps}. Then AMEs are calculated by taking average values of marginal effects from all the observations.

    If \code{at = NULL} (the default), AMEs are calculated based on the original observations used in the fitted two-part model, or the new data set that \code{newdata} inputs. Otherwise, AMEs are calculated based upon modified data by the number of combinations of values specified in at.

    The standard errors of AMEs could be calculated using delta method or bootstrap method. The delta method for two-part model considers the difference between average Jacobian vectors for factor or logical variables, or the second-order partial derivatives of prediction with respect to both models' parameters, assuming independence between models from two parts. The Jacobian vectors and derivatives are approximated by numerical differentiations. The bootstrap method generates bootstrap samples to fit two-part models, and get variances and inverted bootstrap quantile CIs or normal-based CIs of AMEs. If \code{se == T}, the returned data frames also have columns indicating z-statistics and p-values that are calculated by normal assumption and input \code{level}, and with CIs if \code{CI == T}.

}

\value{
    A data frame of estimated average marginal effects for all independent variables in the fitted two-part model or the variables that \code{term} specifies, if \code{se == T}, with standard errors of AMEs, z-statistics and p-values that are calculated by normal assumption and input \code{level}, and with CIs if \code{CI == T}. If \code{at = NULL} (the default), then the data frame will have a number of rows equal to the number of concerned variables. Otherwise, a data list of AMEs of concerned variables, or a data frame of AMEs if there's only one interested variable, is returned and the number of rows in the data frame for each variable will be a multiple thereof based upon the number of combinations of values specified in at.
}

\seealso{\code{\link{twopartm-class}}, \code{\link[twopartm]{tpm}}, \code{\link[twopartm]{predict-methods}}, \code{\link[twopartm]{margin}}, \code{\link{glm}}
}


\references{
Belotti, F., Deb, P., Manning, W.G. and Norton, E.C. (2015). twopm: Two-part models.
\emph{The Stata Journal}, 15(1), pp.3-20.

Leeper, T.J. (2017). Interpreting regression results using average marginal effects with R’s margins. \emph{Available at the comprehensive R Archive Network (CRAN)}, pp.1-32.

Leeper, T.J., Arnold, J. and Arel-Bundock, V. (2017). Package "margins". \emph{accessed December}, 5, p.2019.
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

##AMEs for all variables with standard errors and CIs
AME(tpmodel)

##AMEs for variable "female" with standard errors and CIs at age
##40,and 60 respectively
AME(tpmodel,term = "female",at = list(age = c(40,60)))




##data for count response
data("bioChemists")

##fit two-part model with the same regressors in both parts, with logistic
##regression model for the first part, and poisson regression model with
##default log link for the second-part model
tpmodel = tpm(art ~ .,data = bioChemists,link_part1 = "logit",
family_part2 = poisson)

tpmodel


##AMEs for variable "phd" if all are women
AME(tpmodel,term = "phd",at = list(fem = "Women"))

##AMEs for variable "ment" when all are women and the numbers
##of children aged 5 or younger are 1,3, with standard errors
##by bootstrap methods, and CIs by bootstrap quantiles
AME(tpmodel,term = "ment",at = list(fem = "Women",kid5 = c(1,3)),
se.method = "bootstrap",CI.boots = TRUE,iter = 15)

}

\keyword{methods}

