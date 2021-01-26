ta_Cramers_V <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  chisq <- ta_chi_square(Table)
  Table_with_Totals <- ta_add_totals(Table)
  Grand_Total <- Table_with_Totals[rows+1,cols+1]
 return(round(sqrt(chisq/(Grand_Total*min(rows-1,cols-1))),3))
}


