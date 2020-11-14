count_missings <- function(dr) {
  sapply(dr, FUN=function(col) { sum(is.na(col))})
}

## The function returns 0 for any number abs(x) <= 1. The log-10 of such x will be a negative #. 
##  log10 of such x will be a negative number, overwrapping with the artificially negated signed x, 
##  that's a negative number to begin with. 
signedLog10 <- function(x) {  
  ifelse(abs(x) <= 1, 0, sign(x) * log10(abs(x)))
}
