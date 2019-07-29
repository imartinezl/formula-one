
library(dplyr)

# Query Template ----------------------------------------------------------

series <- "f1"
season <- "2019"
round <- "11"
query_base <- paste0("http://ergast.com/api/", series, '/', season, '/', round, '/')

# Qualifying --------------------------------------------------------------

query_qualifying <- paste0(query_base, 'qualifying', '.json?limit=100')
r <- httr::GET(url = query_qualifying)
d <- httr::content(r)

df_qualifying <- lapply(d$MRData$RaceTable$Races[[1]]$QualifyingResults, data.frame, stringsAsFactors=F) %>% 
  dplyr::bind_rows() %>% 
  dplyr::rename(driverId = Driver.driverId) %>% 
  dplyr::select(position, driverId) %>% 
  dplyr::mutate(position = as.numeric(position),
                lap_number = 0)

# Lap Times ---------------------------------------------------------------

df_laptimes <- data.frame()
laptime_response <- function(r){
  e <- httr::content(r)
  lap <- e$MRData$RaceTable$Races[[1]]$Laps[[1]]
  lapply(lap$Timings,  data.frame, stringsAsFactors=F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(lap_number = lap$number)
}

lap <- 1
query_laptime <- paste0(query_base, 'laps/', lap, '.json')
r <- httr::GET(url = query_laptime)

while(r$status_code == 200 && httr::content(r)$MRData$total != "0"){
  df_laptimes <- dplyr::bind_rows(df_laptimes, laptime_response(r))
  lap <- lap + 1
  query_laptime <- paste0(query_base, 'laps/', lap, '.json')
  r <- httr::GET(url = query_laptime)
}

# Pit Stops ----------------------------------------------------------------

query_pitstops <- paste0(query_base, 'pitstops', '.json?limit=100')
r <- httr::GET(url = query_pitstops)
d <- httr::content(r)

df_pitstops <- lapply(d$MRData$RaceTable$Races[[1]]$PitStops, data.frame, stringsAsFactors=F) %>% 
  dplyr::bind_rows() %>% 
  dplyr::rename(lap_number = lap) %>% 
  dplyr::select(-time)

# Merge Datasets ----------------------------------------------------------

df <- merge(df_laptimes, df_pitstops, by=c("driverId","lap_number"), all.x = T) %>% 
  dplyr::mutate(lap_number = as.numeric(lap_number),
                position = as.numeric(position),
                stop = as.numeric(stop),
                duration = as.numeric(duration)) %>% 
  dplyr::bind_rows(df_qualifying) %>% 
  dplyr::arrange(lap_number, position)
saveRDS(df, "laptimes_data.rds")
df <- readRDS("laptimes_data.rds")

# Lap Times Visualization -------------------------------------------------

last_laps <- df %>% 
  dplyr::group_by(driverId) %>% 
  dplyr::summarise(last_lap = max(lap_number),
                   position = position[which(lap_number == last_lap)]) %>% 
  dplyr::ungroup()

df %>% 
  # dplyr::filter(driverId == "hamilton") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x = lap_number, y = position, color=driverId), size=0.5) +
  ggplot2::geom_point(data = . %>% filter(!is.na(stop)), ggplot2::aes(x = lap_number, y = position, color=driverId)) +
  ggplot2::geom_point(data = last_laps, ggplot2::aes(x = last_lap, y = position, color=driverId), shape=10) +
  ggplot2::scale_y_continuous(trans = "reverse")
 
