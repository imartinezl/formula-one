
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
driverColor <- c("#469BFF","#00D2BE","#1E41FF","#9B0000","#F0D787","#00D2BE",
                 "#FFF500","#F0D787","#FFFFFF","#469BFF","#DC0000",
                 "#1E41FF","#FF8700","#F596C8","#9B0000","#FFF500",
                 "#FFFFFF","#FF8700","#F596C8","#DC0000")

driverInfo <- data.frame(driverId, driverName, driverColor)

# Query Template ----------------------------------------------------------

series <- "f1"
season <- "2019"
round <- "8"
query_base <- paste0("http://ergast.com/api/", series, '/', season, '/', round, '/')

# Qualifying --------------------------------------------------------------

qualifying_file <- "data/qualifying_data_fr.rds"
if(file.exists(qualifying_file)){
  df_qualifying <- readRDS(qualifying_file)
}else{
  query_qualifying <- paste0(query_base, 'qualifying', '.json?limit=100')
  r <- httr::GET(url = query_qualifying)
  d <- httr::content(r)
  
  df_qualifying <- lapply(d$MRData$RaceTable$Races[[1]]$QualifyingResults, data.frame, stringsAsFactors=F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(driverId = Driver.driverId) %>% 
    dplyr::select(position, driverId) %>% 
    dplyr::mutate(position = as.numeric(position),
                  lap_number = 0)
  saveRDS(df_qualifying, qualifying_file)
}
# Lap Times ---------------------------------------------------------------

laptimes_file <- "data/laptimes_data_fr.rds"
if(file.exists(laptimes_file)){
  df_laptimes <- readRDS(laptimes_file)
}else{
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
  df_laptimes <- df_laptimes %>% 
    dplyr::mutate(lap_number = as.numeric(lap_number),
                  position = as.numeric(position))
  saveRDS(df_laptimes, laptimes_file)
}

# Pit Stops ----------------------------------------------------------------

pitstops_file <- "data/pitstops_data_fr.rds"
if(file.exists(pitstops_file)){
  df_pitstops <- readRDS(pitstops_file)
}else{
  query_pitstops <- paste0(query_base, 'pitstops', '.json?limit=100')
  r <- httr::GET(url = query_pitstops)
  d <- httr::content(r)
  
  df_pitstops <- lapply(d$MRData$RaceTable$Races[[1]]$PitStops, data.frame, stringsAsFactors=F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(lap_number = lap) %>% 
    dplyr::select(-time) %>% 
    dplyr::mutate(lap_number = as.numeric(lap_number),
                  stop = as.numeric(stop))
  saveRDS(df_pitstops, pitstops_file)
}

# Merge Datasets ----------------------------------------------------------

df <- merge(df_laptimes, df_pitstops, by=c("driverId","lap_number"), all.x = T) %>% 
  dplyr::bind_rows(df_qualifying)

df <- df_laptimes %>% 
  dplyr::bind_rows(df_qualifying)
nlaps <- max(df$lap_number)

# Lap Times Visualization -------------------------------------------------

last_laps <- df_laptimes %>% 
  dplyr::group_by(driverId) %>% 
  dplyr::summarise(last_lap = max(lap_number),
                   position = position[which(lap_number == last_lap)]) %>% 
  dplyr::ungroup() %>% 
  merge(driverInfo, by="driverId")

# Penalties ---------------------------------------------------------------

if(F){
  post <- list(grosjean=7, kevin_magnussen=8, hamilton=9, kubica=10, russell=11, raikkonen=12, giovinazzi=13)
  finish_lap <- nlaps+4
  finish_pos <- df %>% 
    dplyr::filter(lap_number == nlaps) %>% 
    dplyr::mutate(lap_number = finish_lap)
  for(n in names(post)){
    finish_pos$position[finish_pos$driverId ==  n] <- unlist(post[n])
  }
  df <- df %>% 
    dplyr::bind_rows(finish_pos)
  
  pre <- list(albon = 16, russell=17, kubica=18, norris=19)
  for(n in names(pre)){
    df$position[df$driverId ==  n & df$lap_number == 0] <- unlist(pre[n])
  }
}


dd <- lapply(unique(df$driverId), function(driver){
  df_s <- df %>% 
    dplyr::filter(driverId == driver) %>%
    dplyr::arrange(lap_number)
  
  lap_number <- c()
  position <- c()
  for(i in 1:(nrow(df_s)-1)){
    current_pos <- df_s$position[i]
    next_pos <- df_s$position[i+1]
    by <- 0.05
    x <- seq(df_s$lap_number[i], df_s$lap_number[i+1], by=by)
    x_ <- seq(-0.5, 0.5, length.out=length(x))
    a <- 10
    b <- current_pos - next_pos
    y <- current_pos - b/(1+exp(-a*x_))
    lap_number <- c(lap_number, x)
    position <- c(position, y)
  }
  data.frame(driverId=driver, lap_number, position, stringsAsFactors = F)
}) %>% dplyr::bind_rows()


# POSITION GRAPH ----------------------------------------------------------

dd %>% 
  merge(driverInfo, by="driverId") %>% 
  ggplot2::ggplot()+
  # Position Lines
  ggplot2::geom_line(ggplot2::aes(x = lap_number, y = position, color=driverId), size=0.5) +
  # Points and names every 10 laps
  ggplot2::geom_point(data = . %>% filter(lap_number %% 10 == 0),
                      ggplot2::aes(x = lap_number, y = position, color=driverId), shape=16, size=2) +
  ggplot2::geom_label(data = . %>% filter(lap_number %% 10 == 0),
                      ggplot2::aes(x = lap_number, y = position-0.3, label=driverName, fill=driverId), 
                      size=3, label.padding = grid::unit(0.15, "lines"), family="Formula1 Display-Bold") +
  # Points and names for drivers retiring before last lap
  ggplot2::geom_point(data = last_laps %>% filter(last_lap < nlaps),
                      ggplot2::aes(x = last_lap, y = position, color=driverId), shape=16, size=2) +
  ggplot2::geom_label(data = last_laps %>% filter(last_lap < nlaps),
                      ggplot2::aes(x = last_lap, y = position-0.3, label=driverName, fill=driverId),
                      size=3, label.padding = grid::unit(0.15, "lines"), family="Formula1Display-Bold") +
  # Points and names for drivers on the last lap
  ggplot2::geom_point(data = last_laps %>% filter(last_lap == nlaps),
                      ggplot2::aes(x = last_lap, y = position, color=driverId), shape=16, size=2) +
  ggplot2::geom_label(data = last_laps %>% filter(last_lap  == nlaps),
                      ggplot2::aes(x = last_lap, y = position-0.3, label=driverName, fill=driverId), 
                      size=3, label.padding = grid::unit(0.15, "lines"), family="Formula1 Display-Bold") +
  # Pit stops names
  ggplot2::geom_text(data = merge(df_laptimes, df_pitstops, by=c("driverId","lap_number"), all.y = T) %>%
                       merge(driverInfo, by="driverId"),
                     ggplot2::aes(x = lap_number, y = position-0.3, color=driverId, label=paste0("PIT",stop)),
                     size=3, family = "Formula1-Display-Regular") +
  # Color and continuous scales
  ggplot2::scale_color_manual(values=driverColor)+
  ggplot2::scale_fill_manual(values=driverColor)+
  ggplot2::scale_x_continuous(breaks=c(seq(0,nlaps,by=5),nlaps), expand = c(0.02,0.102))+
  ggplot2::scale_y_continuous(breaks=c(1,5,10,15,20), trans = "reverse") +
  # Theme
  ggplot2::theme(legend.position = "none",
                 panel.background = ggplot2::element_rect(fill = "#38383F"),
                 plot.background = ggplot2::element_rect(fill = "#38383F"),
                 text = ggplot2::element_text(family = "Formula1 Display-Regular", colour = "#dddddd"),
                 plot.title = ggplot2::element_text(family = "Formula1 Display-Bold", hjust = 0, colour = "#dddddd"),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(family = "Formula1 Display-Regular", size=10, colour = "#dddddd"),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(family = "Formula1 Display-Regular", size=10, colour = "#dddddd"),
                 # axis.ticks.y = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(), #ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#dddddd"),
                 panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank())+
  ggplot2::ggtitle("German Grand Prix 2019 - F1 Race") +
  ggplot2::ggsave(filename = paste0("test", Sys.time(), ".png"), device="png", dpi=400, width=16, height=8)





