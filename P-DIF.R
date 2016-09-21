library(mirt)
library(mvtnorm)
PDIF = function(x, grOgniskowa, emp = TRUE) {
  stopifnot("MultipleGroupClass" %in% class(x),
            is.vector(grOgniskowa), is.vector(emp))
  stopifnot(x@Data$ngroups == 2,
            is.character(grOgniskowa) | is.numeric(grOgniskowa),
            length(grOgniskowa) == 1, length(emp) == 1,
            all(emp) %in% c(TRUE, FALSE))
  if (is.numeric(grOgniskowa)) {
    stopifnot(grOgniskowa %in% 1:2)
  } else {
    stopifnot(grOgniskowa %in% x@Data$groupNames)
    grOgniskowa = which(x@Data$groupNames == grOgniskowa)
  }
  grOdniesienia = setdiff(1:2, grOgniskowa)
  if (emp) {
    theta = fscores(x, "EAP")[as.numeric(x@Data$group) %in% grOgniskowa]
  } else {
    mCov = coef(x, simplify = TRUE)[[grOgniskowa]]$cov
    mCov[upper.tri(mCov)] = t(mCov)[upper.tri(mCov)]
    theta = rmvnorm(10^5, coef(x, simplify = TRUE)[[grOgniskowa]]$means, mCov)
  }

  pdif = as.numeric(rep(NA, x@Data$nitems))
  for (i in 1:length(pdif)) {
    pdif[i] = mean(expected.item(extract.item(x, i, grOgniskowa), theta)) -
      mean(expected.item(extract.item(x, i, grOdniesienia), theta))
  }
  return(pdif)
}
