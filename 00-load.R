library(tidyverse)
library(lubridate)
library(geosphere)
library(googlesheets)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Team
RAPTORS <- c("dec@u.northwestern.edu",
             "daehan.lee@northwestern.edu",
             "erik.andersen@northwestern.edu",
             "stefanzdraljevic2018@u.northwestern.edu")



filter_box <- function(longitude, latitude, coords) {
  between(longitude, coords[1], coords[3]) &
    between(latitude, coords[2], coords[4]) &
    !is.na(longitude)
}

FtoC <- function(F) {
  (F - 32)*(5/9)
}

# Read in C-labels
sc <- readr::read_csv("data/fulcrum/sample_collection.csv") %>%
  dplyr::mutate(c_label = stringr::str_to_upper(c_label)) %>%
  dplyr::filter(project != "Worm Meeting Collection") %>%
  dplyr::rename(sampled_by = created_by) %>%
  dplyr::select(-updated_at,
                -system_created_at,
                -system_updated_at,
                -date) %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(created_at, tz = "HST")) %>%
  dplyr::mutate(date = lubridate::date(created_at)) %>%
  dplyr::select(-created_at) %>%
  # Label substrate moisture issue (moisture meters were on potentially wrong settings at times before 2017-08-09)
  dplyr::mutate(substrate_moisture_ = ifelse(substrate_moisture_ == -1, NA, substrate_moisture_)) %>%
  dplyr::mutate(substrate_moisture_issue = !(lubridate::ymd(date) %within% lubridate::interval("2017-08-09", "2017-08-31"))) %>%
  # Two observations had a C > 50; Clearly wrong.
  dplyr::mutate(substrate_temperature_c = ifelse(substrate_temperature_c == 100, NA, substrate_temperature_c)) %>%
  # Fix Fahrenheit observations
  dplyr::mutate(substrate_temperature_c = ifelse(substrate_temperature_c > 35,
                                                 FtoC(substrate_temperature_c),
                                                 substrate_temperature_c)) %>%
  # Fix substrate_temp_c when C < 9
  dplyr::mutate(substrate_temperature_c = ifelse(substrate_temperature_c < 9,
                                                 NA,
                                                 substrate_temperature_c)) %>%
  # Fix ambient temp F to C
  dplyr::mutate(ambient_temperature_c = ifelse(ambient_temperature_c > 50,
                                               FtoC(ambient_temperature_c),
                                               ambient_temperature_c)) %>%
  # Fix mispelling
  dplyr::mutate(substrate = ifelse(substrate == "Millipeed",
                                   "Millipede",
                                   substrate))

# Read in S-labels
po <- readr::read_csv("data/fulcrum/plating_out.csv") %>%
  dplyr::select(c_label_id = c_label,
                po_id = fulcrum_id,
                po_created_at = system_created_at,
                po_created_by = created_by,
                worms_on_sample,
                approximate_number_of_worms,
                males_observed,
                dauers_on_sample,
                approximate_number_of_worms,
                po_date = date,
                po_time = time)

# Read in data from photos
# comm <- paste0("exiftool -coordFormat '%+.6f' -csv -ext jpg ",
#                getwd(),
#                "/data/photos/id/*")

# Exif Data
# exif <- readr::read_csv(pipe(comm)) %>%
#   dplyr::mutate(SourceFile = stringr::str_replace(basename(SourceFile), ".jpg", "")) %>%
#   dplyr::select(sample_photo = SourceFile,
#                 altitude = GPSAltitude,
#                 latitude = GPSLatitude,
#                 longitude = GPSLongitude,
#                 ExposureTime,
#                 Artist,
#                 Aperture,
#                 BrightnessValue,
#                 PhotoDate = DateCreated,
#                 FOV) %>%
#   dplyr::mutate(altitude =  as.numeric(stringr::str_replace(altitude, " m", ""))) %>%
#   dplyr::mutate(FOV =  as.numeric(stringr::str_replace(FOV, " deg", ""))) %>%
#   dplyr::group_by(sample_photo) %>%
#   # Only retain data from one sample photo.
#   dplyr::distinct(.keep_all=T)
# save(file = "data/exif.Rda", exif)
load("data/fulcrum/exif.Rda")

# Join Data
df <- dplyr::full_join(po, sc, by = c("c_label_id" = "fulcrum_id")) %>%
  dplyr::rename(record_latitude = latitude, record_longitude = longitude) %>%
  dplyr::select(c_label,
                everything(),
                -c_label_id,
                -sample_photo_url) %>%
  dplyr::left_join(exif) %>%
  # In rare cases, lat/lon not with photo; fix.
  dplyr::mutate(latitude = ifelse(is.na(latitude), record_latitude, latitude)) %>%
  dplyr::mutate(longitude = ifelse(is.na(longitude), record_longitude, longitude)) %>%
  dplyr::mutate(ambient_temperature_c = as.numeric(ambient_temperature_c)) %>%
  dplyr::mutate(ambient_temperature_c = ifelse(ambient_temperature_c > 70,
                                               ((5/9)*(ambient_temperature_c-32)),
                                               ambient_temperature_c)) %>%
  dplyr::mutate_at(.vars = vars(dplyr::starts_with("gps")),
                   .funs = funs(as.numeric)) %>%
  dplyr::mutate(team = ifelse(sampled_by %in% RAPTORS, "RAPTORS", "MOANA")) %>%
  dplyr::mutate(worms_on_sample = ifelse(is.na(worms_on_sample), "?", worms_on_sample)) %>%
  dplyr::filter(!is.na(c_label)) %>%
  dplyr::select(-assigned_to,
                -status,
                -Artist) %>%
  # Calculate the Haversine distance
  dplyr::rowwise() %>%
  dplyr::mutate(gps_err = geosphere::distHaversine(c(longitude, latitude),
                                                   c(record_longitude, record_latitude))) %>%
  dplyr::ungroup()

# Generate dataset mapping C-labels to S-labels
po_slabels <- readr::read_csv("data/fulcrum/plating_out_s_labeled_plates.csv") %>%
  dplyr::select(fulcrum_parent_id, s_label) %>%
  dplyr::left_join(df, by = c("fulcrum_parent_id" = "po_id")) %>%
  dplyr::select(c_label,
                s_label,
                worms_on_sample,
                males_observed,
                dauers_on_sample,
                approximate_number_of_worms,
                po_date,
                po_time,
                longitude,
                latitude,
                substrate_temperature_c,
                substrate_moisture_,
                ambient_humidity_,
                ambient_temperature_c,
                sampled_by)

issues <- list(
# C-labels with no S-labels
c_label_no_slabel = po_slabels %>%
  dplyr::filter(is.na(s_label)) %>%
  dplyr::select(c_label),
dup_c_label = df %>%
  dplyr::group_by(c_label) %>%
  dplyr::filter(n() > 1) %>% dplyr::select(c_label, po_id) %>% 
  dplyr::distinct(.keep_all=T),
# S-labels with no C-labels
s_label_no_clabel = po_slabels %>%
  dplyr::filter(is.na(c_label)) %>%
  dplyr::select(s_label)
)

po_slabels <- po_slabels %>% 
              dplyr::filter(!is.na(c_label), !is.na(s_label))

# Add Nested S-labels
df <- dplyr::left_join(df, 
                       po_slabels %>%
                         dplyr::select(c_label, s_label) %>%
                         dplyr::group_by(c_label) %>%
                         dplyr::summarize(s_label_cnt = length(s_label), 
                                          s_label = paste0(s_label, collapse = ","))) %>%
  dplyr::select(c_label, s_label, s_label_cnt, everything(), -po_id)

 
# Samples collected that were never processed.
issues[["c_label_never_processed"]] = df %>% 
  dplyr::filter(worms_on_sample == "?") %>%
  dplyr::select(c_label)


# Trail coordinates

# Create Island Column
df$island <- "?"
df[filter_box(df$longitude, df$latitude, c(-158.3617,21.1968,-157.5117,21.7931)), "island"] <- "Oahu"
df[filter_box(df$longitude, df$latitude, c(-159.9362, 21.6523, -159.1782, 22.472)), "island"] <- "Kauai"
df[filter_box(df$longitude, df$latitude, c(-157.327, 21.0328, -156.685, 21.2574)), "island"] <- "Molokai"
df[filter_box(df$longitude, df$latitude, c(-156.7061, 20.4712, -155.9289, 21.0743)), "island"] <- "Maui"
df[filter_box(df$longitude, df$latitude, c(-156.1346, 18.6619, -154.6985, 20.4492)), "island"] <- "Big Island"

# Fix errant GPS locations from team Moana and Erik
df[df$island == "BIG_ISLAND" & df$team == "MOANA" , c("latitude", "longitude")] <- NA
df[df$island == "?", c("latitude", "longitude")] <- NA
df[df$island == "BIG_ISLAND" & df$team == "MOANA" & !is.na(df$island),"island"] <- "MAUI"

# Create Trail Column
df$location <- NA

df[filter_box(df$longitude, df$latitude, c(-157.72537,21.303309,-157.71919,21.32122)), "location"] <- "Kuliouou Ridge Trail"
df[filter_box(df$longitude, df$latitude, c(-158.0192352613,21.5014265529,-158.0145925283,21.5041245046)), "location"] <- "Wahiawa Botanical Garden"
df[filter_box(df$longitude, df$latitude, c(-157.8598800302,21.3149311581,-157.855797708,21.3182194587)), "location"] <- "Foster Community Garden"
df[filter_box(df$longitude, df$latitude, c(-157.7829487403,21.3569863645,-157.7752268314,21.3655295525)), "location"] <- "Maunawili Demonstration Trail"
df[filter_box(df$longitude, df$latitude, c(-157.8014534712,21.3322593,-157.798127532,21.3427719396)), "location"] <- "Manoa Falls Trail"
df[filter_box(df$longitude, df$latitude, c(-157.8135502338,21.3779082884,-157.7915561199,21.3970691079)), "location"] <- "Ho'omaluhia Botanical Garden"
df[filter_box(df$longitude, df$latitude, c(-159.613624,22.167098,-159.575601,22.226422)), "location"] <- "Na Pali Coast State Wilderness Park"




# Add photo URL
df <-df %>% dplyr::rowwise() %>%
  dplyr::group_by(c_label) %>%
  dplyr::mutate(photo = paste0(c_label,
                              ".",
                              stringr::str_to_lower(stringr::str_replace_all(substrate, "[^[:alnum:]]", "_")),
                              ".1.jpg"),
                photo_url = paste0("https://storage.googleapis.com/elegansvariation.org/photos/hawaii2017/",
                                   c_label,
                                   ".",
                                   stringr::str_to_lower(stringr::str_replace_all(substrate, "[^[:alnum:]]", "_")),
                                   ".1.jpg"),
                photo_url_thumb =  paste0("https://storage.googleapis.com/elegansvariation.org/photos/hawaii2017/",
                                          c_label,
                                          ".",
                                          stringr::str_to_lower(stringr::str_replace_all(substrate, "[^[:alnum:]]", "_")),
                                          ".1.thumb.jpg")) %>%
  dplyr::ungroup()
# 
# photo_comms <- df %>% dplyr::mutate(sample_photo = str_split(sample_photo, ",")) %>%
#   dplyr::select(-s_label) %>%
#   dplyr::select(c_label, sample_photo, substrate) %>%
#   tidyr::unnest() %>%
#   dplyr::group_by(c_label) %>%
#   dplyr::mutate(comm = paste0("cp ../data/photos/id/",
#                               sample_photo,
#                               ".jpg",
#                               " ",
#                               "../data/photos/c/",
#                               c_label,
#                               ".",
#                               stringr::str_to_lower(str_replace_all(substrate, "[^[:alnum:]]", "_")),
#                               ".",
#                               dplyr::row_number(c_label),
#                               ".jpg")) %>%
# 
#   dplyr::select(-c_label, comm)
# 
# writeLines(photo_comms$comm, con = file("scripts/rename_photos.sh"))

# redefine
cso <- po_slabels

# Fold in variables from df to the cso data frame
cso <- cso %>% dplyr::left_join(
                                df %>% dplyr::select(c_label,
                                     substrate,
                                     landscape,
                                     sky_view,
                                     photo_url,
                                     photo_url_thumb,
                                     altitude,
                                     team,
                                     island,
                                     location,
                                     date,
                                     time,
                                     FOV),
                                 by = "c_label"
                                )

# df for google sheet
# df %>% dplyr::mutate(s_label = stringr::str_split(s_label, ",")) %>%
#        tidyr::unnest() %>%
#        dplyr::select(c_label,
#                      s_label,
#                      island,
#                      date,
#                      po_date,
#                      approximate_number_of_worms) #%>%
#        dplyr::filter(!is.na(s_label)) %>% excel(.)

# Merge in blast data; Take top hit
blast_results <- readr::read_tsv("data/sanger/blast_results.tsv") %>%
                 dplyr::group_by(s_plate) %>%
                 dplyr::filter(row_number() == 1)

#==============================#
# Load manual curation results #
#==============================#
cso <- cso %>% dplyr::left_join(blast_results, by = c("s_label" = "s_plate")) %>%
               dplyr::left_join(
                 gs_key("1bavR10CEyvWt2zBSNBz-ADXx06b1mDFmuvaaM8Uobi4") %>%
                   gs_read("Full") %>%
                   dplyr::select(s_label,
                                 `genotyped wave 1 (8/22)`,
                                 `genotyped wave 2 (8/25)`,
                                 `genotyped wave 3 (9/8)`,
                                 pcr_rhpositive,
                                 SpeciesID,
                                 Notes)
                 ,
                 by = "s_label"
               )

save(file = "data/fulcrum/df.Rda", df, cso)
