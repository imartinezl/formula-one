library(dplyr)
library(extrafont)
extrafont::font_import(prompt = F, pattern = "Formula")

# Driver Info -------------------------------------------------------------

driverId <- c("albon", "bottas","gasly","giovinazzi","grosjean","hamilton",
              "hulkenberg","kevin_magnussen","kubica","kvyat", "leclerc",
              "max_verstappen","norris","perez","raikkonen","ricciardo",
              "russell","sainz","stroll","vettel")
driverName <- c("ALB","BOT","GAS","GIO","GRO","HAM",
                "HUL","MAG","KUB","KVY","LEC",
                "VER","NOR","PER","RAI","RIC",
                "RUS","SAI","STR","VET")
driverColor <- c("#1E41FF","#00D2BE","#469BFF","#9B0000","#F0D787","#00D2BE",
                 "#FFF500","#F0D787","#FFFFFF","#469BFF","#DC0000",
                 "#1E41FF","#FF8700","#F596C8","#9B0000","#FFF500",
                 "#FFFFFF","#FF8700","#F596C8","#DC0000")

driverInfo <- data.frame(driverId, driverName, driverColor)

# Query Template ----------------------------------------------------------

"https://ergast.com/api/f1/2008/5/driverStandings"

series <- "f1"
season <- "2019"
round <- "11"
table <- 'driverStandings'
get_content <- function(series, season, round, table){
  query_base <- paste0("http://ergast.com/api/", series, '/', season, '/', round, '/')
  query <- paste0(query_base, table, '.json?limit=100')
  r <- httr::GET(url = query)
  return ( httr::content(r) )
}

d <- get_content(series, season, round, table)
standings <- lapply(1:10, function(round){
  d <- get_content(series, season, round, table)
  round_standings <- d$MRData$StandingsTable$StandingsLists[[1]]$DriverStandings %>% 
    lapply(data.frame, stringsAsFactors = F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(round = as.numeric(round),
                  position = as.numeric(position),
                  positionText = positionText %>% as.numeric(),
                  points = as.numeric(points),
                  wins = as.numeric(wins)
    )
  return(round_standings)
}) %>% dplyr::bind_rows()

standings <- standings %>% 
  dplyr::group_by(Driver.driverId, ) %>%
  dplyr::mutate(current_points = points,
                last_points = lag(points,1),
                race_points = current_points-last_points,
                race_points = ifelse(is.na(race_points), current_points, race_points),
                )

position <- c(1,2,3,4,5,6,7,8,9,10)
points_current <- c(25,18,15,12,10,8,6,4,2,1)
points_current_fastlap <- c(25,18,15,12,10,8,6,4,2,1)+1
points_new <- c(10,9,8,7,6,5,4,3,2,1)
point_system <- data.frame(position, points_current, points_current_fastlap, points_new)

standings %>% 
  merge(point_system, by='position') %>% 
  dplyr::mutate(point_diff = race_points - points_current,
                fast_lap = point_diff == 1) %>% View
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=round, y=race_points, color=Driver.code))


