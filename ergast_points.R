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
nrounds <- 17
standings <- lapply(1:nrounds, function(round){
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
  dplyr::group_by(Driver.driverId) %>%
  dplyr::mutate(current_points = points,
                last_points = lag(points,1),
                race_points = current_points-last_points,
                race_points = ifelse(is.na(race_points), current_points, race_points),
                fast_lap = race_points %in% c(26,19,16,13,11,9,7,5,3),
                race_points = ifelse(fast_lap, race_points-1, race_points)
  ) %>% dplyr::ungroup() %>% 
  merge(driverInfo, by.x="Driver.driverId",by.y="driverId")


## Query circuit information
circuit_info <- function(){
  query <- paste0("http://ergast.com/api/f1/2019/circuits.json?limit=100")
  r <- httr::GET(url = query)
  d <- httr::content(r)
  d$MRData$CircuitTable$Circuits %>% 
    lapply(data.frame, stringsAsFactors = F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(round = 1:n())
}
circuits <- circuit_info()

fast_lap_current <- 1
fast_lap_new <- 0
race_position <- c(1,2,3,4,5,6,7,8,9,10)
points_current <- c(25,18,15,12,10,8,6,4,2,1)
points_new <- c(10,9,8,7,6,5,4,3,2,1)
point_system <- data.frame(race_position, points_current, points_new)
data_to_plot <- standings %>% 
  merge(point_system, all.x=T, by.x='race_points', by.y='points_current', sort=F) %>% 
  dplyr::group_by(Driver.code) %>% 
  dplyr::arrange(round) %>% 
  dplyr::mutate( 
    points_current = ifelse(fast_lap, race_points+fast_lap_current, race_points),
    points_current_total = cumsum(points_current),
    points_new = ifelse(is.na(points_new), 0, points_new),
    points_new = ifelse(fast_lap, points_new+fast_lap_new, points_new),
    points_new_total = cumsum(points_new)
  ) %>% 
  dplyr::ungroup() 

data_to_plot %>% 
  tidyr::gather(key, value, points_current_total, points_new_total) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=round, y=value, color=Driver.driverId))+
  ggplot2::geom_point(ggplot2::aes(x=round, y=value, color=Driver.driverId))+
  ggplot2::geom_text(data = . %>% filter(round %% 3 == 0), 
                     ggplot2::aes(x=round, y=value*1.1, color=Driver.driverId, label=driverName))+
  ggplot2::facet_grid(rows=vars(key), scales="free")+
  ggplot2::scale_color_manual(values=driverColor)+
  ggplot2::scale_x_continuous(breaks=1:nrow(circuits), labels=circuits$Location.country)
  

