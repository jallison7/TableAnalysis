ta_chisq_p <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  x <- ta_chi_square(Table)
  df <- (rows - 1) * (cols - 1)
  p <- (1 - pchisq(x,df))
  options(digits = 3, scipen = 999)
  return(p)
}
