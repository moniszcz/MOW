library("entropy")

# arguments in eval function:
#   y - the response value as found in the formula
#   offset - the offset term, if any, found on the right hand side of the formula
#   parms - the vector or list (if any) supplied by the user as a parms argument to the call
#   wt - vector of weights

anova_eval <- function(y, wt, parms) {
  
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label= wmean, deviance=rss)
}


# arguments in split function:
#   y - vector or matrix of response values
#   wt - vector of weights
#   x -  of x values
#   parms - vector of user parameters, passed forward
#   continuous - if TRUE the x variable should be treated as continuous
#   goodness - the utility of the split, where larger numbers are better
#   direction - A vector of the same length with values of -1 and +1, where -1 suggests that values
#     with y < cutpoint be sent to the left side of the tree, and a value of +1 that values with y < cutpoint be sent to the right

anova_split <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]

    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    #goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    #list(goodness= goodness, direction=sign(lmean))
    rx <- rank(x[-1]) #use only the ranks of x, to preserve invariance
    fit <- smooth.spline(rx, goodness, df=2)
    list(goodness= predict(fit, rx)$y, direction=sign(lmean))
    
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
        direction = ux[ord])
    # list(goodness= (left.wt*lmean + right.wt*rmean)/sum(wt*y),
    #      direction = ux[ord])
  }
}


anova_split_cov <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness= goodness, direction=sign(lmean))
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
}



# arguments in init function:
#   y - vector or matrix of response values
#   offset - the offset term, if any, found on the right hand side of the formula
#   parms - vector of user parameters, passed forward
#   wt - vector of weights

anova_init <- function(y, offset, parms, wt) {
  if (!is.null(offset)) y <- y-offset
  list(y=y, parms=0, numresp=1, numy=1,
       summary= function(yval, dev, wt, ylevel, digits ) {
         paste("  mean=", format(signif(yval, digits)),
               ", MSE=" , format(signif(dev/wt, digits)),
               sep='')
       })
}

anova_init2 <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}