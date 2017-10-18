# This script is intendend to setup analysis in R for Hawaii analysis using
# Rmarkdown or just an R script.
require(knitr)
library(webshot)
library(tidyverse)
library(leaflet)

# Set working directory
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

opts_knit$set(progress=TRUE)

opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, print=FALSE, verbose=TRUE)
opts_chunk$set(results="asis")
opts_chunk$set(cache=TRUE,cache.path="cache/")
opts_chunk$set(fig.path="figure/",dev=c("png", "svg"))
opts_chunk$set(debug = function(before, options, envir) {
  if (!before) {
    message(
      paste(names(envir), as.list(envir),
            sep = " = ", collapse = "\n"))
  }
})


#===================#
# Utility Functions #
#===================#
library(leaflet)
library(rlang)
### Summarize df by worms and a variable variable and output a table.
summarize_worms_by <- function(df, variable) {
  summary <- df %>% dplyr::group_by(UQ(rlang::sym(variable)), worms_on_sample) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(worms_on_sample, n, fill = 0) %>%
    dplyr::mutate(Total = (`?` + `No` + `Tracks` + `Yes`),
                  Yes_Rate = round((`Yes` / `Total`), 3),
                  Yes_Track_Rate = round(((Yes + Tracks)/Total), 3),
                  Loss_Rate = round(`?`/`Total`, 3)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!variable := as.character(UQ(rlang::sym(variable))))
  
  total <- summary %>% 
    dplyr::summarize(!!variable := "Total",
                     `?` = sum(`?`),
                     No = sum(No),
                     Yes = sum(Yes),
                     Tracks = sum(Tracks),
                     Total = sum(Total)) %>%
    dplyr::mutate(
      Yes_Rate = round((sum(Yes)/sum(Total)), 3),
      Yes_Track_Rate= round(sum(Yes+Tracks)/sum(Total), 3),
      Loss_Rate = round(sum(`?`) / sum(Total), 3))
  
  rbind(summary, total)
}


# Draw a gallery from records in df.
gallery <- function(df) {
  knitr::asis_output(paste0("<img class='img-thumbnail' src='", df$photo_url_thumb, "' />"))
}


# Map a df
map_collection <- function(df, color_use) {
  
  WIDTH <- 20
  HEIGHT <- 20
  anchor_diff = -20
  popup_anchor_x = 0.001
  
  icos <- iconList(
    red = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/red.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    lred = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/lred.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    yellow = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/yellow.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    green = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/green.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    grey = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/grey.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    orange = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/orange.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    blue = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/blue.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    lblue = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/lblue.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    ),
    grey = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/grey.svg"),
      iconWidth = WIDTH, iconHeight = HEIGHT,
      popupAnchorX = popup_anchor_x, popupAnchorY = anchor_diff,
      iconAnchorX = WIDTH/2, iconAnchorY = HEIGHT
    )
  )
  df <- dplyr::filter(df, !is.na(df[[color_use]])) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(s_label = stringr::str_split(s_label, ",")) %>%
    dplyr::mutate(s_label = paste0("<ul>",
                                   paste0(purrr::map(s_label,
                                                     function(x) {
                                                       paste0("<li>", x, "</li>") 
                                                     }
                                   ), collapse=""),
                                   "</ul>",
                                   collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(substrate=ifelse(is.na(substrate), "", substrate))
  
  #print(df)
  
  attach(df)
  leaflet::leaflet(data = df, width = "100%") %>% 
    leaflet::addTiles( 
      paste0( 
        "https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=", 
        jsonlite::read_json("thunderforest.json")$key)  
    ) %>%
    leaflet::addMarkers(~longitude,
                        ~latitude,
                        popup = glue::glue("<h2>{c_label}</h2><hr />
                                           <strong>worms on sample:</strong> {worms_on_sample}<br />
                                           <strong>approximate number of worms:</strong> {approximate_number_of_worms}<br />
                                           <strong>substrate:</strong> {substrate}<br />
                                           <strong>landscape</strong> {landscape}<br /><br />
                                           <a href='{photo_url}'><img style='width: 150px;' src='{photo_url_thumb}'></a>"),
                        popupOptions(maxWidth = 500),
                        icon = icos[ df[[color_use]] ] )
  
  #htmlwidgets::saveWidget(m, tempfile(), selfcontained = FALSE)
  #webshot::webshot("temp.html", file = "map.png",
  #        cliprect = "viewport", vwidth = 1000, vheight = 1000)
}


#======#
# Data #
#======#

# Plot gridsect
plot_gridsect <- function(grid_num) {
  angles = list(A = 1,
                B = 2,
                C = 3,
                D = 4,
                E = 5,
                F = 6)
  
  mm <- df %>%
    dplyr::filter(grid_num == grid_num) %>%
    dplyr::select(c_label,
                  s_label,
                  latitude,
                  longitude,
                  grid_num,
                  gridsect,
                  gridsect_direction,
                  gridsect_radius) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(x = gridsect_radius * (sin( (angles[[gridsect_direction]] - 1) * (pi/3) )),
                  y = gridsect_radius * (cos( (angles[[gridsect_direction]] - 1) * (pi/3) )),
                  label = paste0(gridsect_direction, gridsect_radius))
  
  ggplot(mm, aes(x = x, y = y, label = label)) +
    geom_point() +
    geom_text(nudge_x = 0.1, nudge_y = 0.1) +
    theme_void()
}

# Elevation Plot

elevation_plot <- function(df) {
  library(elevatr)
  m <- df %>% dplyr::mutate(NR = row_number()) %>%
    dplyr::left_join(., 
                     dplyr::bind_rows(
                       lapply(1:(nrow(m) - 1), function(NR) {
                         ff <- geosphere::gcIntermediate(m[NR,c("longitude", "latitude")],
                                                         m[NR+1,c("longitude", "latitude")],
                                                         10,
                                                         sp = F,
                                                         addStartEnd = T)
                         row.names(ff) <- NULL
                         
                         as.data.frame(ff) %>%
                           dplyr::mutate(lead_lon = dplyr::lead(lon),
                                         lead_lat = dplyr::lead(lat)) %>%
                           dplyr::rowwise() %>%
                           dplyr::mutate(NR = NR,
                                         dist = geosphere::distHaversine(
                                           c(lon, lat),
                                           c(lead_lon, lead_lat)
                                         )
                           )
                       })
                     )
    ) %>%
    dplyr::filter(!is.na(dist)) %>%
    dplyr::rename(x = lon, y = lat) %>%
    dplyr::mutate(cumulative_distance = cumsum(dist)) %>%
    dplyr::group_by(c_label) %>%
    dplyr::mutate(collection_point = (row_number() == 1)) %>%
    dplyr::ungroup
  
  # Fetch elevation
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  xy_elev <- get_elev_point(m %>% 
                              dplyr::select(x,y) %>%
                              as.data.frame(),
                            prj=prj_dd,
                            src="epqs")
  
  # Merge elevation back in.
  m <- dplyr::bind_cols(m, xy_elev %>% as.data.frame())
  
  collection_m = m %>% dplyr::filter(collection_point)
  
  ggplot(m) +
    geom_line(aes(x = cumulative_distance, y = elevation)) +
    geom_point(aes(x = cumulative_distance, y = elevation), data = collection_m)
}





load("data/fulcrum/df.Rda")
