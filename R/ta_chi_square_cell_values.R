ta_chi_square_cell_values <- function(Table) {
  expected <- ta_expected(Table)
  cell_values <- sqrt((Table - expected)^2 / expected)
  return(cell_values)
}


