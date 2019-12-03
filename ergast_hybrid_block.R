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
  "sauber"="#536CC3",##536CC3 / #011E65
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


compute_blocks <- function(results, teams_id, xref=0, x_max=10, y_incr=15){
  team <- results %>% 
    dplyr::filter(Constructor.constructorId == teams_id) %>% 
    dplyr::select(position, positionText, points, status, Time.millis,
                  Driver.driverId, Constructor.constructorId, season, round) %>% 
    dplyr::arrange(as.numeric(season), as.numeric(round)) %>% 
    dplyr::mutate(id = 1:n(),
                  win = position == "1" & points != "0",
                  podium = (position == "2" | position == "3") & points != "0",
                  top6 = (position == "4" | position == "5" | position == "6") & points != "0",
                  top10 = (position == "7" | position == "8" | position == "9" | position == "10") & points != "0",
                  dnf = !(status %in% c("Finished","+1 Lap")) & points == "0",# & is.na(as.numeric(positionText)) 
                  outpoints = status %in% c("Finished","+1 Lap") & points == "0",
                  rare = !(win | podium | top6 | top10 | dnf | outpoints),
                  top10 = ifelse(rare & points != "0", T, top10),
    ) %>% 
    dplyr::group_by(season, round) %>% 
    dplyr::mutate(dnfBoth = all(dnf),
                  temp_win = paste0(position, collapse = ""),
                  win12 = temp_win == "12" | temp_win == "21") %>% 
    dplyr::select(-temp_win) %>% 
    dplyr::ungroup()
  
  # team[!(team$win | team$podium | team$top6 | team$top10 | team$outpoints | team$dnf),] %>% View
  
  x <- c()
  y <- c()
  s <- c()
  # y_incr <- 15
  x_dnf <- xref; y_dnf <- 0
  x_outpoints <- xref; y_outpoints <- y_dnf + y_incr
  x_top10 <- xref; y_top10 <- y_outpoints + y_incr
  x_top6 <- xref; y_top6 <- y_top10 + y_incr
  x_podium <- xref; y_podium <- y_top6 + y_incr
  x_win <- xref; y_win <- y_podium + y_incr
  h <- -1
  w <- 1
  x_max <- xref + x_max
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
    if(team$dnf[i]){
      x <- c(x, x_dnf)
      y <- c(y, y_dnf)
      s <- c(s, team$dnfBoth[i])
      x_dnf <- x_dnf + w
      if(x_dnf >= x_max){
        y_dnf <- y_dnf + h
        x_dnf <- xref
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


y_incr <- 15
x_max <- 10
# categories info
category_label <- c( "DNF", "Out of points", "Top 10", "Top 6", "Podiums (2nd and 3rd)","Wins")
category_y <- seq(0, (length(category_text)-1)*y_incr, by=y_incr)
categories <- data.frame(category_label, category_y)

# teams info
teams_id <- c("mercedes", "ferrari", "red_bull", "williams", "force_india", 
              "racing_point", "mclaren", "toro_rosso", "renault", "lotus_f1", "haas", 
              "alfa", "sauber", "caterham", "manor", "marussia")
teams_names <- sapply(teams_id, function(name){ results$Constructor.name[results$Constructor.constructorId == name][1]})
n <- length(teams_names)
x_sep <- 14
teams_xref <- seq(0,(n-1)*x_sep,by=x_sep)
teams_xmid <- teams_xref + x_max/2
teams_df <- data.frame(teams_id, teams_names, teams_xref, teams_xmid, row.names = NULL)

a <- lapply(1:n, function(i){
  print(i)
  compute_blocks(results, teams_id[i], teams_xref[i], x_max, y_incr)
}) %>% dplyr::bind_rows()

# team_name <- "alfa"
# team <- compute_blocks(results, team_name, 10)

font_family <- "Formula1-Display-Regular"
font_family_bold <- "Formula1-Display-Bold"
font_color <- "#FFFFFF"
background_color <- "#222222"
a %>% 
  ggplot2::ggplot()+
  ggplot2::geom_tile(ggplot2::aes(x=x, y=y, color=s, fill=Constructor.constructorId),
                     width=0.5, height=0.5, size=0)+
  ggplot2::geom_text(data=teams_df, ggplot2::aes(x=teams_xmid, y=max(category_y), label=teams_names), 
                     size=2, hjust=0.5, vjust=0,
                     family=font_family, color=font_color, check_overlap = T)+
  ggplot2::geom_text(data=categories, ggplot2::aes(x=0, y=category_y, label=category_label),
                     size=2, hjust=1, vjust=0,
                     family=font_family, color=font_color, check_overlap = T)+
  ggplot2::coord_fixed(ratio=1, xlim=c(-15,200), ylim=c(-30, 100) )+
  ggplot2::scale_fill_manual(guide=F, values=constructorColor)+
  ggplot2::scale_color_manual(guide=F, values=c("black", "red"))+#constructorColor)
  ggplot2::theme_void()+
  ggplot2::theme(
    text = ggplot2::element_text(family=font_family, color=font_color),
    plot.background = ggplot2::element_rect(fill=background_color),
    panel.background = ggplot2::element_rect(fill=background_color)
    # legend.title = ggplot2::element_blank(),
    # legend.text = ggplot2::element_text(size=14),
    # legend.key = ggplot2::element_rect(fill = background_color, color=background_color),
    # legend.key.height = ggplot2::unit(1.5, "lines"),
    # legend.key.width = ggplot2::unit(0.7, "lines"),
    # legend.spacing.x = ggplot2::unit(0.5, "cm"),
    # legend.spacing.y = ggplot2::unit(0, "cm"),
    # legend.position = "right"
  )

