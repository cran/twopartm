#Method "FiellerRatio"----
setGeneric(name = "FiellerRatio",
           def = function(xest,yest,V,alpha = 0.05) standardGeneric("FiellerRatio"))

#' @import MASS
#' @importFrom stats qt
#' @export
setMethod(f = "FiellerRatio",
          signature = signature(xest = "numeric"),
          definition = function(xest,yest,V,alpha = 0.05){

            #check inputs
            if(!is.numeric(xest) | !is.numeric(yest) | length(xest) != 1 | length(yest) != 1){
              stop("xest or yest should be the estimates of two variables.")
            }
            if(is.vector(V)| is.null(V)){
              stop("Covariance matrix of two estimates should be given.")
            }

            #ratio
            Q <- xest/yest

            #variances and covariances of estimates

            varx <- V[1,1]
            vary <- V[2,2]
            covxy <- V[1,2]


            z <- qnorm(1-alpha/2)

            #check whether the denominator yest is significantly different
            #from zero at significance level alpha
            if(yest^2/vary > z^2){
              m = xest*yest-z^2*covxy
              minB <- (m-sqrt(m^2-(yest^2-z^2*vary)*(xest^2-z^2*varx)))/(yest^2-z^2*vary)
              maxB <- (m+sqrt(m^2-(yest^2-z^2*vary)*(xest^2-z^2*varx)))/(yest^2-z^2*vary)

              #return ratio and CIs
              return(c(ratio=Q,min=minB,max=maxB))

            }else{
              # If the denominator is not significantly different from zero,
              # two situations can be discriminated by a z_unbounded value.

              z2_unb = yest^2/vary + (xest*vary-yest*covxy)^2/(vary*(varx*vary-covxy^2))

              if(z2_unb > z^2){

                #“unbounded/exclusive” case
                m = xest*yest-z^2*covxy
                B1 <- (m-sqrt(m^2-(yest^2-z^2*vary)*(xest^2-z^2*varx)))/(yest^2-z^2*vary)
                B2 <- (m+sqrt(m^2-(yest^2-z^2*vary)*(xest^2-z^2*varx)))/(yest^2-z^2*vary)

                minB <- min(B1,B2)
                maxB <- max(B1,B2)
                message("Notice: The denominator is not significantly different from zero, and the confidence set of the ratio is combining [-inf,bound1] and [bound2,inf].")
                return(c(ratio=Q,bound1=minB,bound2=maxB))

              }else{
                #“unbounded” case
                stop("The denominator is not significantly different from zero, and the confidence set of ratio does not exclude any value at all. (unbounded CI)")
              }

            }


          }
)



#function to output ratio with CI given margins and covariance
get_ratioCI <- function(value,res,SD_pre,level,se,CI){

  out_ratio = NULL

  for(i in seq_along(value[-1])){

    if(se == FALSE){
      res_ratio = data.frame(Contrast = paste(value[i+1],"-",value[1],sep = ""),
                             Ratio.of.Means = as.numeric(res$Margin[i+1]/res$Margin[1]))

    }else{

      #obatin ratio with CI
      ratio = FiellerRatio(res$Margin[i+1],res$Margin[1],SD_pre[c(1,i+1),c(1,i+1)],alpha = 1-level)

      res_ratio = data.frame(Contrast = paste(value[i+1],"-",value[1],sep = ""),
                             Ratio.of.Means = as.numeric(ratio[1]))
      #get CI
      if(CI == TRUE){
        res_ratio$CI.L = as.numeric(ratio[2])
        res_ratio$CI.U =  as.numeric(ratio[3])
      }

    }

    out_ratio = rbind(out_ratio,res_ratio)

  }
  #return a data frame including the ratio and CI
  out_ratio

}

