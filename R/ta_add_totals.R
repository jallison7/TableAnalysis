ta_add_totals <- function (Table) {
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
column_total[cols+1] <- sum(column_total)
Table <- cbind(Table,row_total)
return(rbind(Table,column_total))
}

