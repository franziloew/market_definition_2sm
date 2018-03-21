substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

calculate_week = function(week, year) {
  date <- ymd(paste(year, 1, 1, sep="-"))
  week(date) = week
  return(date)
}