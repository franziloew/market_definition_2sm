AddAdjustBox <- function(sg.table){
  after.location <- grep(" \\\\label\\{t_*", sg.table)
  sg.table <- append(sg.table,
                     "\\begin{adjustbox}{totalheight=\\textheight-2\\baselineskip}",
                     after = after.location)
  after.location <- grep("\\\\end\\{tabular\\}", sg.table)
  sg.table <- append(sg.table, "\\end{adjustbox}", after = after.location)
  return(sg.table)
}