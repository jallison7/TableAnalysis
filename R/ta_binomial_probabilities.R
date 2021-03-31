ta_binomial_probabilities <- function(Table) {
rows <- nrow(Table)
cols <- ncol(Table)
totals <- ta_add_totals(Table)
binomp <- as.data.frame(array(dim = c(rows, cols)))
expected <- ta_expected(Table)
N <- totals[rows + 1, cols + 1]
expectedp <- expected / N
for (i in 1:rows) {
  for (j in 1:cols) {
    if (Table[i,j] - expected[i,j] >= 0) {
      binomp[i,j] <- pbinom(Table[i, j], N, expectedp[i, j], lower.tail = FALSE)
    }
    else {
      binomp[i,j] <- 0 - pbinom(Table[i, j], N, expectedp[i, j])
    }
  }
}
dimnames(binomp) <- dimnames(Table)
return(binomp)
}
