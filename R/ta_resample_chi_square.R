ta_resample_chi_square <- function(Table, iterations) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  N <- ta_add_totals(Table)[rows+1, cols+1]
  expected <- ta_expected(Table)
  proportions <- as.data.frame(expected/N)
  prob <- stack(proportions)

  chi_square_resampling <- vector(mode = "numeric", length = iterations)

  for (i in 1:iterations) {
    Sim1 <- rmultinom(1,N,prob=prob$values)
    SimTable <- as.data.frame(cbind(Sim1,prob$ind))
    SimTable <- unstack(SimTable, form = V1 ~ V2)
    chi_square_resampling[i] <- ta_chi_square(SimTable)
  }
  return(as.data.frame(chi_square_resampling))
}

