\name{FiellerRatio}
\alias{FiellerRatio}
\alias{FiellerRatio,numeric-method}

\title{Ratio of two Gaussian random variables with CI by Fieller's theorem}

\description{
  Calculate ratio of two Gaussian random variables with confidence intervals obtained by Fieller's theorem.
}

\usage{
FiellerRatio(xest,yest,V,alpha = 0.05)
}

\arguments{
  \item{xest}{an estimate of one Gaussian random variable as numerator.}
  \item{yest}{an estimate of one Gaussian random variable as denominator.}
  \item{V}{Covariance matrix of two estimates.}
  \item{alpha}{the alpha (significant) level of the confidence interval. The default value is 0.05.}
}

\details{
    Let X, Y be Gaussian random variables (or normally distributed estimators) with estimates \code{xest} and \code{yest}, and the ratio of interest E(X)/E(Y). An intuitive point–estimate for the ratio of interest is \eqn{xest/yest}. Fieller's theorem allows the calculation of a confidence interval for the ratio of two population means given estimates and covariance matrix.
}

\value{
    A numeric vector including the ratio of two estimates, and the bounds of its confidence interval, if the denominator is significantly different from zero. Otherwise, if the denominator is not significantly different from zero but the confidence set is exclusive, a numeric vector including the ratio of two estimates, and the bounds of its exclusive confidence set is returned.
}

\references{
  Cabrera, J. and McDougall, A. (2002). Statistical consulting. \emph{Springer Science & Business Media}.

  Franz, V. H. (2007). Ratios: A short guide to confidence limits and proper use. \emph{arXiv preprint arXiv:0710.2024}.

  Fieller, E. C. (1954). Some problems in interval estimation. \emph{Journal of the Royal Statistical Society: Series B (Methodological)}, 16(2), 175-185.

  Zerbe, G. O. (1978). On Fieller's theorem and the general linear model. \emph{The American Statistician}, 32(3), 103-105.

  Young, D. A., Zerbe, G. O., & Hay Jr, W. W. (1997). Fieller's theorem, Scheffé simultaneous confidence intervals, and ratios of parameters of linear and nonlinear mixed-effects models. \emph{Biometrics}, 838-847.
}


\author{Yajie Duan, Javier Cabrera and Birol Emir}


\examples{

## example data: bivariate Gaussian random variables
library(MASS)
out <- mvrnorm(1000, mu = c(10,3), Sigma = matrix(c(1,0.2,0.2,1),
ncol = 2))

##ratio with CI between two sample means
FiellerRatio(mean(out[,1]),mean(out[,2]),V = cov(out)/1000)



##case that the denominator is not significantly different from zero
##but the confidence set is exclusive
out <- mvrnorm(1000, mu = c(3,0.001), Sigma = matrix(c(1,0.2,0.2,1), ncol = 2))
FiellerRatio(mean(out[,1]),mean(out[,2]),V = cov(out)/1000)



##an example of calculating ratio of fitted parameters with CI in regression models
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data.frame(treatment, outcome, counts) # showing data
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
summary(glm.D93)

##obtain estimates and covariance matrix of concerned fitted parameters
xest <- as.numeric(coef(glm.D93)["outcome3"])
yest <- as.numeric(coef(glm.D93)["outcome2"])
V <- vcov(glm.D93)[c("outcome3","outcome2"),c("outcome3","outcome2")]

##ratio with CI between two fitted parameters
FiellerRatio(xest,yest,V)
}

\keyword{methods}

