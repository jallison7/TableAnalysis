ta_lambda_row <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  N <- ta_add_totals(Table)[rows + 1, cols + 1]
  f <- array(dim = rows)
  #find maximum value in each row
  for (i in 1:rows) {
    x <- Table[i,]
    f[i] <- max(x)
  }
  sumf <- sum(f)
  Fd <- max(ta_add_totals(Table)[rows + 1,1:cols])
  lambda <- (sumf - Fd) / (N - Fd)
  return(lambda)
}


