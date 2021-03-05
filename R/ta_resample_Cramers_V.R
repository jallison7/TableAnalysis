ta_resample_Cramers_V <- function(Table, iterations) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  N <- ta_add_totals(Table)[rows+1, cols+1]
  proportions <- as.data.frame(Table/N)
  prob <- stack(proportions)

  Cramers_V_resampling <- vector(mode = "numeric", length = iterations)

  for (i in 1:iterations) {
    Sim1 <- rmultinom(1,N,prob=prob$values)
    SimTable <- as.data.frame(cbind(Sim1,prob$ind))
    SimTable <- unstack(SimTable, form = V1 ~ V2)
    Cramers_V_resampling[i] <- ta_Cramers_V(SimTable)
  }
  return(as.data.frame(Cramers_V_resampling))
}

