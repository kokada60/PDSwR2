count_missings <- function(dr) {
  sapply(dr, FUN=function(col) { sum(is.na(col))})
}