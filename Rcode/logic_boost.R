lb_init <- function(y, offset, parms, wt) {
  
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste("  mean=", format(signif(yval, digits)),", MSE=" , format(signif(dev/wt, digits)),sep ='')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

lb_eval <- function(indexes, wt, parms) {
  tree_number <- parms$tree_number
  lookup_table <- parms$lookup_table
  
  y_real = lookup_table$Y[indexes]
  rss <- sum(wt*(y_real-sum(y_real*wt)/sum(wt))^2)
  
  probs = lookup_table[[paste('P_', tree_number-1, sep='')]][indexes]
  
  probs_sum = sum(probs)
  y_sum = sum(y_real)
  
  y_pred = (y_sum - probs_sum) / sum(probs * (1 - probs))
  
  list(label = y_pred, deviance = rss)
}

lb_split <- function(indexes, wt, x, parms, continuous) {
  # Center y
  lookup_table <- parms$lookup_table
  tree_number <- parms$tree_number
  
  y = lookup_table[[paste('resid_', tree_number-1, sep='')]][indexes]
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
    list(goodness = goodness, direction = sign(lmean))
  } else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),direction = ux[ord])
  }
}