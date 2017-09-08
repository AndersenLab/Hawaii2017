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
