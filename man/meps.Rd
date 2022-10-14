\name{meps}
\alias{meps}
\docType{data}
\title{A Sample of Medical Expenditure Panel Survey 2004 data}
\description{
  A sample of MEPS 2004 data including 19386 observations.
}
\usage{
data("meps")
}
\format{
  \describe{
    \item{duid}{Dwelling unit id}
    \item{dupersid}{Person id (unique) }
    \item{hieuidx}{Health insurance eligibility unit id}
    \item{wtdper}{Sampling weight for person}
    \item{age}{Age}
    \item{female}{Female}
    \item{race_bl}{Black}
    \item{race_oth}{Other race, non-white and non-black}
    \item{eth_hisp}{Hispanic}
    \item{famsize}{Size of responding annualized family}
    \item{ed_hs}{High school education}
    \item{ed_hsplus}{Some college education}
    \item{ed_col}{College education}
    \item{ed_colplus}{More than college education}
    \item{lninc}{ln(family income)}
    \item{reg_midw}{Midwest region}
    \item{reg_south}{South region}
    \item{reg_west}{West region}
    \item{anylim}{Any disability}
    \item{mcs12}{Mental health component of SF12}
    \item{pcs12}{Physical health component of SF12}
    \item{ins_mcare}{Medicare insurance}
    \item{ins_mcaid}{Medicaid insurance}
    \item{ins_unins}{Uninsured}
    \item{ins_dent}{Dental insurance, prorated}
    \item{exp_tot}{Total medical care expenses}
    \item{exp_dent}{Dental care expenses}
    \item{exp_self}{Total expenses paid by self or family}
    \item{ use_disch}{# hospital discharges}
    \item{use_los}{# nights in hospital}
    \item{use_dent}{# dental visits}
    \item{use_rx}{# prescriptions and refills}
  }
}
\details{
  This data set is taken from book Health econometrics using Stata (Vol. 3).
}
\source{
  found in Stata format at
  \url{https://www.stata-press.com/data/heus.html}
}
\references{
  Deb, P., Norton, E.C. and Manning, W.G. (2017). Health econometrics using Stata (Vol. 3). \emph{College Station, TX: Stata press}.
}
\keyword{datasets}
