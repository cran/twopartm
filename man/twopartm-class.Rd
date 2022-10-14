\docType{class}
\name{twopartm-class}
\alias{twopartm-class}
\title{Class \code{twopartm}}
\description{A fitted two-part regression model by \code{\link{tpm}}.}
\section{Slots}{
  \describe{
    \item{\code{formula_part1}}{Formula specified for the first-part model, i.e., the binomial model for indicators about any non-zero responses.}
    \item{\code{formula_part2}}{Formula specified for the second-part model, i.e., the glm model for non-zero responses.}
    \item{\code{data}}{Data set used to fit the two-part model. It's the same data set as the \code{data} argument in \link{tpm}.}
    \item{\code{n}:}{Number of observations used in the two-part model (with weights > 0).}
    \item{\code{n_part1}}{Number of of observations used in the first-part model (with weights > 0), i.e., the binomial model for indicators about any non-zero responses.}
    \item{\code{n_part2}}{Number of of observations used in the second-part model (with weights > 0), i.e., the glm model for non-zero responses.}
    \item{\code{data_model1}}{The model frame for the first-part model,  i.e., the binomial model for indicators about any non-zero responses.}
    \item{\code{data_model2}}{The model frame for the second-part model,  i.e., the glm model for non-zero responses.}
    \item{\code{model_part1}}{An object of class \code{glm} of the fitted first-part model,  i.e., the binomial model for indicators about any non-zero responses.}
    \item{\code{model_part2}}{An object of class \code{glm} of the fitted second-part model,  i.e.,  the glm model for non-zero responses.}
    \item{\code{link_part1}}{Character string describing the link function of the first-part model, i.e., the binomial model for indicators about any non-zero responses.}
    \item{\code{family_part2}}{The family object used in the second-part model,  i.e.,  the glm model for non-zero responses.}
    \item{\code{weights}}{A vector of weights used in the two-part model fitting, or NULL if no weights used.}
    \item{\code{fitted}}{Fitted mean values by the two-part model, obtained by multiplying the fitted probabilities of non-zero responses and the fitted means of non-zero responses.}
    \item{\code{residuals}}{A vector of raw residuals (observed - fitted).}
    \item{\code{loglik}}{Log-likelihood values of the fitted two-part model.}
    \item{\code{y}}{The response vector.}
  }
}

\seealso{\code{\link[twopartm]{tpm}}, \code{\link[twopartm]{AME}}, \code{\link[twopartm]{margin}}, \code{\link{glm}}
}

\author{Yajie Duan, Birol Emir, Griffith Bell and Javier Cabrera}
\examples{

##data about health expenditures, i.e., non-negative continuous response
data(meps,package = "twopartm")


##fit two-part model with the same regressors in both parts, with logistic regression model
##for the first part, and glm with Gamma family with log link for the second-part model
tpmodel = tpm(formula_part1 = exp_tot~female+age, formula_part2 = exp_tot~female+age+ed_colplus,
data = meps,link_part1 = "logit",family_part2 = Gamma(link = "log"))

##get the formula specified for the first-part model
tpmodel@formula_part1

##get the formula specified for the second-part model
tpmodel@formula_part2

##get the log-likelihood for the fitted two-part model
tpmodel@loglik

##get the fitted glm model for the first part
tpmodel@model_part1
##get the fitted glm model for the second part
tpmodel@model_part2


}

\keyword{classes}
