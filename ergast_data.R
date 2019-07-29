

query_base <- "http://ergast.com/api/"
  
series <- "f1"
season <- "2019"
round <- "11"
query_laptimes <- paste0(query_base, series, '/', season, '/', round, '/', 'laps')

r <- httr::GET(url = query_laptimes)
httr::content(r)
response <- str(httr::content(r))


query_pitstops <- paste0(query_base, series, '/', season, '/', round, '/', 'pitstops')
r <- httr::GET(url = query_pitstops)