as_factor_lbl <- function(x, ...) {
  # This function converts atomic/numeric variable to factors using
  # the `haven` package as_factor function, but preserves the label
  # attribute.
  require(haven)
  lbl <- attr(x,'label')
  fac <- as_factor(x)
  if (!(is.null(lbl))) attr(fac, 'label') <- lbl
  return(fac)
}