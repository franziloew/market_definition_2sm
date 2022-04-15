substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

calculate_week = function(week, year) {
  date <- lubridate::ymd(paste(year, 1, 1, sep="-"))
  lubridate::week(date) = week
  return(date)
}