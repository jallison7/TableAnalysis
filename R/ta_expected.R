ta_expected <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  totals <- ta_add_totals(Table)
  expect <- array(dim = c(rows,cols))
  for (i in 1:rows) {
    for (j in 1:cols) {
      expect[i,j] <- (totals[i,cols + 1] * totals[rows + 1, j]) / totals[rows + 1, cols + 1]
    }
  }
  dimnames(expect) <- dimnames(Table)
  return(expect)
}

