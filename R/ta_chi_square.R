ta_chi_square <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  Table <- ta_chi_square_cell_values(Table)
  row_total <- vector(mode = "numeric", rows)
    for (i in 1:rows) {
      row_total[i] <- sum(Table[i,1:cols])
    }
    X <- sum(row_total)
    options(digits = 3, scipen = 999)
  return(X)
}

