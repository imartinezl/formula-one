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


# constructorStandings ----------------------------------------------------

series <- "f1"
season <- "2019"
round <- "last"
table <- "constructorStandings"
seasons <- c("2014","2015","2016","2017","2018","2019")
constructorStandings <- lapply(seasons, function(season){
  d <- get_content(series, season, round, table)
  d$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings %>% 
    lapply(data.frame, stringsAsFactors = F) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(position = as.numeric(position),
                  positionText = positionText %>% as.numeric(),
                  points = as.numeric(points),
                  wins = as.numeric(wins)
    )
}) %>% dplyr::bind_rows()

nraces <- constructorStandings$wins %>% sum()
npoints <- constructorStandings$points %>% sum()
constructorStandings_stats <- constructorStandings %>% 
  dplyr::group_by(Constructor.name, Constructor.constructorId) %>% 
  dplyr::summarise(
    total_wins = sum(wins),
    total_points = sum(points)
  ) %>% 
  dplyr::ungroup()


# qualifying --------------------------------------------------------------

series <- "f1"
season <- "2019"
round <- NA
table <- "qualifying"
qualifying <- lapply(seasons, function(season){
  d <- get_content(series, season, round, table)
  lapply(d$MRData$RaceTable$Races, function(race){
  race$QualifyingResults %>% 
    lapply(data.frame, stringsAsFactors = F) %>% 
    dplyr::bind_rows()
  }) %>% dplyr::bind_rows()
}) %>% dplyr::bind_rows()

npoles <- sum(qualifying$position == "1")
qualifying_stats <- qualifying %>% 
  dplyr::group_by(Constructor.name, Constructor.constructorId) %>% 
  dplyr::summarise(total_poles = sum(position == "1")) %>% 
  dplyr::ungroup()


# PLOTS -------------------------------------------------------------------

constructorStandings_stats %>% 
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x=reorder(Constructor.name, -total_points), 
                                 y=total_points, 
                                 fill=Constructor.constructorId))+
  ggplot2::scale_fill_manual(guide=F, values=constructorColor)+
  ggplot2::labs(x="Constructor", y="Points")+
  hrbrthemes::theme_ipsum_rc()+
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(hjust=1, vjust=1, angle=45)
  )+
  ggplot2::ggtitle("Total points")

constructorStandings_stats %>% 
  dplyr::arrange(total_points) %>% 
  dplyr::mutate(xmax = cumsum(total_points),
                xmin = lag(xmax,1),
                xmin = ifelse(is.na(xmin), 0, xmin),
                ymin=9, ymax=10, x=(xmin+xmax)/2,
                angle = (x/npoints*360)
                ) %>%  
  ggplot2::ggplot()+
  ggplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                  fill=Constructor.constructorId))+
  # ggplot2::geom_text(data=. %>% filter(total_points>10),
  #                    hjust=0, family="Formula1 Display-Regular", size=2,
  #                    ggplot2::aes(x=x, y=11, label=Constructor.name, angle=180-angle))+
  ggplot2::coord_polar(theta = "x", start = 3*pi/2, direction = 1, clip = "on")+
  ggplot2::scale_fill_manual(name="Constructor", drop = T,
                             labels = constructorStandings_stats$Constructor.name,
                             values = constructorColor)+
  ggplot2::labs(x="Constructor", y="Points")+
  ggplot2::scale_y_continuous(limits=c(0,NA))+
  ggplot2::scale_x_continuous(expand = c(0.02, 0.02))+
  ggplot2::theme_void()+
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill="#aabbcc")
  )


w <- 0.75
top <- 10
sep <- 0.75
font_family <- "Formula1-Display-Regular"
font_family_bold <- "Formula1-Display-Bold"
font_color <- "#FFFFFF"
background_color <- "#111111"
filter <- constructorStandings_stats$total_points>10
constructorStandings_stats %>% 
  merge(qualifying_stats) %>% 
  dplyr::arrange(total_points) %>% 
  dplyr::mutate(points_rel = total_points/npoints,
                points_per = paste0(round(points_rel*100),'%'),
                points_xmax = cumsum(total_points)/npoints,
                points_xmin = lag(points_xmax,1),
                points_xmin = ifelse(is.na(points_xmin), 0, points_xmin),
                points_x = (points_xmin+points_xmax)/2,
                points_ymin = top-w, points_ymax = top, 
                points_angle = (points_x*330),
                points_angle = ifelse(points_angle>90 & points_angle<270, points_angle-180, points_angle),
                # points_angle = ifelse(points_angle>270, points_angle+90, points_angle)
  ) %>% 
  dplyr::arrange(total_wins) %>%
  dplyr::mutate(wins_rel = total_wins/nraces,
                wins_per = paste0(round(wins_rel*100),'%'),
                wins_xmax = cumsum(total_wins)/nraces,
                wins_xmin = lag(wins_xmax,1),
                wins_xmin = ifelse(is.na(wins_xmin), 0, wins_xmin),
                wins_x=(wins_xmin+wins_xmax)/2,
                wins_ymin=top-2*w-sep, wins_ymax=top-w-sep, 
                wins_angle = (wins_x*330),
                wins_angle = ifelse(wins_angle>90, wins_angle-180, wins_angle),
                
  ) %>% 
  dplyr::arrange(total_poles) %>%
  dplyr::mutate(poles_rel = total_poles/npoles,
                poles_per = paste0(round(poles_rel*100),'%'),
                poles_xmax = cumsum(total_poles)/npoles,
                poles_xmin = lag(poles_xmax,1),
                poles_xmin = ifelse(is.na(poles_xmin), 0, poles_xmin),
                poles_x=(poles_xmin+poles_xmax)/2,
                poles_ymin=top-3*w-2*sep, poles_ymax=top-2*w-2*sep,
                poles_angle = (poles_x*330),
                poles_angle = ifelse(poles_angle>90, poles_angle-180, poles_angle),
  ) %>% 
  dplyr::filter(total_points > 10) %>%
  ggplot2::ggplot()+
  ggplot2::geom_rect(ggplot2::aes(xmin=points_xmin, xmax=points_xmax, 
                                  ymin=points_ymin, ymax=points_ymax,
                                  fill=Constructor.constructorId),
                     )+
  ggplot2::geom_rect(ggplot2::aes(xmin=wins_xmin, xmax=wins_xmax, 
                                  ymin=wins_ymin, ymax=wins_ymax,
                                  fill=Constructor.constructorId))+
  ggplot2::geom_rect(ggplot2::aes(xmin=poles_xmin, xmax=poles_xmax, 
                                  ymin=poles_ymin, ymax=poles_ymax,
                                  fill=Constructor.constructorId))+
  ggplot2::geom_text(data=. %>% filter(points_rel*100>2.5),
                     family=font_family_bold, size=4, hjust=0.5,
                     ggplot2::aes(x=points_x, y=top-w/2, label=points_per, angle=-points_angle,
                                  color=Constructor.constructorId=="williams"))+
  ggplot2::geom_text(data=. %>% filter(wins_rel*100>1),
                     family=font_family_bold, color=font_color, size=4, hjust=0.5,
                     ggplot2::aes(x=wins_x, y=top-3*w/2-sep, label=wins_per, angle=-wins_angle))+
  ggplot2::geom_text(data=. %>% filter(poles_rel*100>1),
                     family=font_family_bold, color=font_color, size=4, hjust=0.5,
                     ggplot2::aes(x=poles_x, y=top-5*w/2-2*sep, label=poles_per, angle=-poles_angle))+
  # ggplot2::geom_text(data=. %>% filter(total_points>10),
  #                    hjust=0, family=font_family, size=2,
  #                    ggplot2::aes(x=x, y=11, label=Constructor.name, angle=180-angle))+
  ggplot2::geom_text(label="Points ", x=-1/9, y=top-w/2, hjust=1, angle=35, 
                     family=font_family, color=font_color, check_overlap = T)+
  ggplot2::geom_text(label="Wins ", x=-1/9, y=top-3*w/2-sep, hjust=1, angle=35, 
                     family=font_family, color=font_color, check_overlap = T)+
  ggplot2::geom_text(label="Poles ", x=-1/9, y=top-5*w/2-2*sep, hjust=1, angle=35, 
                     family=font_family, color=font_color, check_overlap = T)+
  # ggplot2::geom_text(label=" Points", x=0, y=top-w/2, hjust=0, angle=0, 
  #                    family=font_family, color=font_color, check_overlap = T)+
  # ggplot2::geom_text(label=" Wins", x=0, y=top-3*w/2-sep, hjust=0, angle=0, 
  #                    family=font_family, color=font_color, check_overlap = T)+
  # ggplot2::geom_text(label=" Poles", x=0, y=top-5*w/2-2*sep, hjust=0, angle=0, 
  #                    family=font_family, color=font_color, check_overlap = T)+
  ggplot2::geom_text(label="F1\nHybrid Era\n2014-2019", x=0, y=0, hjust=0.5, angle=0, size=7,
                     family=font_family_bold, color=font_color, check_overlap = T)+
  ggplot2::coord_polar(theta = "x", start = pi, direction = 1, clip = "off")+
  ggplot2::scale_fill_manual(name="",
                             labels=constructorStandings_stats$Constructor.name[filter],
                             values=constructorColor[constructorStandings_stats$Constructor.constructorId[filter]])+
  ggplot2::scale_color_manual(guide=F, values=c("#FFFFFF","#000000"))+
  ggplot2::scale_y_continuous(limits=c(0,NA))+
  ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(add=c(0,1/9)))+
  ggplot2::theme_void()+
  ggplot2::theme(
    text = ggplot2::element_text(family=font_family, color=font_color),
    plot.background = ggplot2::element_rect(fill=background_color),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = background_color, color=background_color),
    legend.key.height = ggplot2::unit(1, "lines"),
    legend.key.width = ggplot2::unit(0.5, "lines"),
    legend.spacing.x = ggplot2::unit(0.5, "cm"),
    legend.spacing.y = ggplot2::unit(0, "cm")
  )+
  ggplot2::ggsave(filename="demo.png",device="png", dpi=100, width=10, height=10)

