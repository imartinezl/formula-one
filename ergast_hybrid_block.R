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


compute_blocks <- function(results, teams_id, xref=0, x_max=10, y_incr=15, w=1, h=-1){
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
  # h <- -0.1
  # w <- 0.1
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


y_incr <- 12
x_max <- 10
# categories info
category_label <- c( "DNF", "Out of points", "Top 10", "Top 6", "Podiums","Wins")
category_ref <- seq(0, (length(category_label)-1)*y_incr, by=y_incr) + 0.5
category_mid <- category_ref - y_incr/2 + 1
category_x <- c(-3, -3, -3, -4.5, -6, -7.5)
category_y <- c(category_ref[1], category_ref[2], category_ref[6], category_ref[6], category_ref[6], category_ref[6])
category_yend <- category_ref-y_incr+2
categories <- data.frame(category_label, category_ref, category_mid, category_x, category_y, category_yend)

# teams info
teams_id <- c("mercedes", "ferrari", "red_bull", "williams", "force_india", 
              "racing_point", "mclaren", "toro_rosso", "renault", "lotus_f1", "haas", 
              "alfa", "sauber", "caterham", "manor", "marussia")
teams_names <- sapply(teams_id, function(name){ results$Constructor.name[results$Constructor.constructorId == name][1]})
n <- length(teams_names)
x_sep <- 11
teams_xref <- seq(0,(n-1)*x_sep,by=x_sep)
teams_xmid <- teams_xref + x_max/2
teams_df <- data.frame(teams_id, teams_names, teams_xref, teams_xmid, row.names = NULL)

a <- lapply(1:n, function(i){
  print(i)
  compute_blocks(results, teams_id[i], teams_xref[i], x_max, y_incr, w=1, h=-1)
}) %>% dplyr::bind_rows()

# team_name <- "alfa"
# team <- compute_blocks(results, team_name, 10)

top <- max(category_ref)
bottom <- -y_incr
font_family <- "Formula1-Display-Regular"
font_family_bold <- "Formula1-Display-Bold"
font_color <- "#FFFFFF"
background_color <- "#111111"
p <- a %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=x, y=y, color=s, fill=Constructor.constructorId), size=3, stroke=0, shape=22)+
  ggplot2::geom_point(data= . %>% dplyr::filter(s), ggplot2::aes(x=x, y=y), size=2, shape=4)+
  ggplot2::scale_fill_manual(guide=F, values=constructorColor)+
  ggplot2::scale_color_manual(guide=F, values=c("#111111", "#f6f6f6"))+
  ggnewscale::new_scale_color()+
  ggplot2::geom_text(data=teams_df, 
                     ggplot2::aes(x=teams_xmid-0.5, y=top+4, 
                                  label = stringr::str_wrap(teams_names, 8), color=teams_id), 
                     size=3.5, hjust=0.5, vjust=0, angle=0, family=font_family_bold)+
  ggplot2::geom_segment(data=teams_df, #color="white",
                        ggplot2::aes(x = teams_xref-0.5, xend=teams_xref+x_max-0.5, color=teams_id, y=top+2.5, yend=top+2.5))+
  ggplot2::scale_color_manual(guide=F, values=constructorColor)+
  ggplot2::geom_text(data=categories, ggplot2::aes(x=-15, y=category_mid, label=category_label),
                     size=3.5, hjust=0.5, vjust=0.5, family=font_family_bold, color=font_color, check_overlap = T)+
  ggplot2::geom_segment(data=categories, color="white",
                        ggplot2::aes(x = category_x, xend=category_x, y=category_y, yend=category_yend))+
  # ggplot2::geom_text(label="F1 Hybrid Era 2014-2019", x=-20, y=max(category_y)+10, hjust=0, size=7,
                     # family=font_family_bold, color=font_color, check_overlap = T)+
  ggplot2::coord_fixed(ratio=1, xlim=c(-15,170), ylim=c(-15, top+10) )+
  ggplot2::theme_void()+
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill=background_color),
    panel.background = ggplot2::element_rect(fill=background_color)
  ) + ggplot2::ggsave(filename="demo_block.png",device="png", dpi=600, width=20, height=10)


png("test-annotate-cairo.png", width = 18, height = 8,units = "in", bg = background_color, res=300, type="cairo")
print(p)
dev.off()

