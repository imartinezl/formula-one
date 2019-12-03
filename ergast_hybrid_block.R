library(dplyr)
library(extrafont)
extrafont::font_import(prompt = F, pattern = "Formula")

# Constructors Info -------------------------------------------------------------

constructorColor <- c(
  "mercedes"= "#00D2BE",
  "red_bull"= "#1E41FF",
  "ferrari"= "#DC0000",
  "haas"="#F0D787",
  "renault"= "#FFF500",
  "alfa"= "#9B0000",
  "racing_point"= "#F596C8",
  "toro_rosso"= "#469BFF",
  "mclaren"= "#FF8700",
  "williams"= "#FFFFFF",
  "force_india"="#F596C8",
  "lotus_f1"="#BA9D5C",
  "sauber"="#011E65",
  "caterham"="#04650d",
  "manor"="#B52A00",
  "marussia"="#B11007"
)

# Query Template ----------------------------------------------------------

"https://ergast.com/api/f1/2008/5/driverStandings"
get_content <- function(series, season, round, table){
  base <- c("http://ergast.com/api", series, season, round)
  query_base <- paste0(base[!is.na(base)], collapse = '/')
  query <- paste0(query_base, '/', table, '.json?limit=500')
  print(query)
  r <- httr::GET(url = query)
  return ( httr::content(r) )
}


# race results --------------------------------------------------------------

series <- "f1"
season <- "2019"
round <- NA
table <- "results"
seasons <- c("2014","2015","2016","2017","2018","2019")
results <- lapply(seasons, function(season){
  d <- get_content(series, season, round, table)
  lapply(d$MRData$RaceTable$Races, function(race){
    race$Results %>% 
      lapply(data.frame, stringsAsFactors = F) %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(season = race$season,
                    round = race$round)
  }) %>% dplyr::bind_rows()
}) %>% dplyr::bind_rows()


compute_blocks <- function(results, team_name, xref=0){
  team <- results %>% 
    dplyr::filter(Constructor.constructorId == team_name) %>% 
    dplyr::select(position, positionText, points, status, Time.millis,
                  Driver.driverId, Constructor.constructorId, season, round) %>% 
    dplyr::arrange(as.numeric(season), as.numeric(round)) %>% 
    dplyr::mutate(id = 1:n(),
                  win = position == "1",
                  podium = position == "2" | position == "3",
                  top6 = position == "4" | position == "5" | position == "6",
                  top10 = position == "7" | position == "8" | position == "9" | position == "10",
                  retired = is.na(Time.millis) & status != "Finished", #is.na(as.numeric(positionText)) is.na(Time.millis) & 
                  outpoints = points == "0" #& !retired
    ) %>% 
    dplyr::group_by(season, round) %>% 
    dplyr::mutate(retiredBoth = all(retired),
                  temp_win = paste0(position, collapse = ""),
                  win12 = temp_win == "12" | temp_win == "21") %>% 
    dplyr::select(-temp_win) %>% 
    dplyr::ungroup()
  
  x <- c()
  y <- c()
  s <- c()
  x_win <- xref; y_win <- 0
  x_podium <- xref; y_podium <- 10
  x_top6 <- xref; y_top6 <- 20
  x_top10 <- xref; y_top10 <- 30
  x_retired <- xref; y_retired <- 40
  x_outpoints <- xref; y_outpoints <- 50
  h <- 1
  w <- 1
  x_max <- xref + 10
  for (i in 1:nrow(team)) {
    # print(i)
    if(team$win[i]){
      x <- c(x, x_win)
      y <- c(y, y_win)
      s <- c(s, team$win12[i])
      x_win <- x_win + w
      if(x_win >= x_max){
        y_win <- y_win + h
        x_win <- xref
      }
    }
    if(team$podium[i]){
      x <- c(x, x_podium)
      y <- c(y, y_podium)
      s <- c(s, F)
      x_podium <- x_podium + w
      if(x_podium >= x_max){
        y_podium <- y_podium + h
        x_podium <- xref
      }
    }
    if(team$top6[i]){
      x <- c(x, x_top6)
      y <- c(y, y_top6)
      s <- c(s, F)
      x_top6 <- x_top6 + w
      if(x_top6 >= x_max){
        y_top6 <- y_top6 + h
        x_top6 <- xref
      }
    }
    if(team$top10[i]){
      x <- c(x, x_top10)
      y <- c(y, y_top10)
      s <- c(s, F)
      x_top10 <- x_top10 + w
      if(x_top10 >= x_max){
        y_top10 <- y_top10 + h
        x_top10 <- xref
      }
    }
    if(team$retired[i]){
      x <- c(x, x_retired)
      y <- c(y, y_retired)
      s <- c(s, team$retiredBoth[i])
      x_retired <- x_retired + w
      if(x_retired >= x_max){
        y_retired <- y_retired + h
        x_retired <- xref
      }
    }
    if(team$outpoints[i]){
      x <- c(x, x_outpoints)
      y <- c(y, y_outpoints)
      s <- c(s, F)
      x_outpoints <- x_outpoints + w
      if(x_outpoints >= x_max){
        y_outpoints <- y_outpoints + h
        x_outpoints <- xref
      }
    }
  }
  
  return(team %>% cbind(data.frame(x,y,s)))
}

teams_names <- c("mercedes", "ferrari", "red_bull", "williams")
n <- length(teams_names)
by <- 12
teams_xref <- seq(0,n*by,by=by)

a <- lapply(1:n, function(i){
  compute_blocks(results, teams_names[i], teams_xref[i])
}) %>% dplyr::bind_rows()

team_name <- "mercedes"
team <- compute_blocks(results, team_name, 10)

a %>% 
  ggplot2::ggplot()+
  ggplot2::geom_tile(ggplot2::aes(x=x, y=y, color=s, fill=Constructor.constructorId),
                     width=0.5, height=0.5, size=0.25)+
  ggplot2::coord_fixed(ratio=1) + #, xlim=c(-10,20))+
  ggplot2::scale_fill_manual(values=constructorColor)+
  ggplot2::scale_color_manual(values=c("black", "red"))#constructorColor)

