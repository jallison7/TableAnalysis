ta_chisq_p <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  x <- ta_chi_square(Table)
  df <- (rows - 1) * (cols - 1)
  p <- pchisq(x,df, lower.tail = FALSE)
  options(digits = 3, scipen = 999)
  return(p)
}
