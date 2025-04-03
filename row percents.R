row_percents <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  row_total <- vector(mode = "numeric", rows)
  column_total <- vector(mode = "numeric", cols + 1)
  for (i in 1:rows) {
    row_total[i] <- sum(Table[i,1:cols])
  }
  for (i in 1:cols){
    column_total[i] <- sum(Table[1:rows, i])
  }
  Table <- cbind(Table,row_total)
  cell_values <- array(dim = c(rows, cols + 1))
  for (i in 1:(rows)) {
      for (j in 1:(cols + 1)) {
      cell_values[i,j] <- (100 * (Table[i,j] / Table[i, (cols + 1)]))
    }
  }
  dimnames(cell_values) <- dimnames(Table)
  cell_values <- as.data.frame(cell_values)
  return(cell_values)
}


