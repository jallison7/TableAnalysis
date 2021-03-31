ta_lambda_col <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  N <- ta_add_totals(Table)[rows + 1, cols + 1]
  f <- array(dim = cols)
#find maximum value in each row
  for (i in 1:cols) {
    x <- Table[,i]
    f[i] <- max(x)
  }
  sumf <- sum(f)
  Fd <- max(ta_add_totals(Table)[1:rows,(cols + 1)])
  lambda <- (sumf - Fd) / (N - Fd)
return(lambda)
}
