ta_chi_square_cell_values <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  expected <- ta_expected(Table)
  cell_values <- array(dim = c(rows,cols))
  for (i in 1:rows) {
    for (j in 1:cols) {
      cell_values[i,j] <- ((Table[i,j] - expected[i,j]) * (Table[i,j] - expected[i,j])) / expected[i,j]
    }
  }
  dimnames(cell_values) <- dimnames(Table)
  return(cell_values)
}
