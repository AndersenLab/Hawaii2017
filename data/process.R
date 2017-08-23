library(tidyverse)


setwd("/Users/dancook/coding/git/Hawaii2017/data/Fulcrum_Export_71d98212-bb77-4a5c-af44-e82601dbfdb4/")

po <- readxl::read_excel("plating_out/plating_out.xlsx") %>%
      dplyr::select(c_label,
                    po_created_at = system_created_at,
                    worms_on_sample,
                    approximate_number_of_worms,
                    males_observed,
                    po_date = date,
                    po_time = time)

sc <- readxl::read_excel("sample_collection/sample_collection.xlsx")


exif <- readr::read_csv(pipe("exiftool -coordFormat '%+.6f' -csv /Users/dancook/coding/git/Hawaii2017/data/Fulcrum_Export_220bb466-5547-4223-b4f6-d2ee7eb3e1aa/*.jpg")) %>%
        dplyr::mutate(SourceFile = stringr::str_replace(basename(SourceFile), ".jpg", "")) %>%
        dplyr::select(sample_photo = SourceFile,
                      GPSAltitude,
                      GPSLatitude,
                      GPSLongitude) %>%
        dplyr::mutate(GPSAltitude =  as.numeric(stringr::str_replace(GPSAltitude, " m", "")))


#===================#
# Sample Colleciton #
#===================#

sc$c_label <- stringr::str_to_upper(sc$c_label)

sc %>% dplyr::filter(substrate_moisture_ > 0) %>%
ggplot(.) +
  geom_histogram(aes(x = substrate_moisture_)) +
  labs(x = "Substrate Moisture (%)", y = "Count") +
  theme_bw()

sc %>% dplyr::filter(substrate_temperature_c > 0) %>%
  ggplot(.) +
  geom_histogram(aes(substrate_temperature_c)) +
  labs(x = "Substrate Tempurature °C", y = "Count") +
  theme_bw()

#=============#
# Plating Out #
#=============#


#======#
# Join #
#======#

df <- dplyr::left_join(sc, po, by = c("fulcrum_id" = "c_label")) %>%
      dplyr::filter(!is.na(worms_on_sample)) %>%
      dplyr::mutate(ambient_temperature_c = ifelse(ambient_temperature_c > 70,
                                                   ((5/9)*(ambient_temperature_c-32)),
                                                   ambient_temperature_c)) %>%
      dplyr::mutate_each(funs(as.numeric), dplyr::starts_with("gps"))

#=================#
# Worms on Sample #
#=================#

# Substrate Moisture
df %>% dplyr::filter(substrate_moisture_ > 0) %>%
ggplot(.) +
  geom_histogram(aes(x = substrate_moisture_, fill = worms_on_sample)) +
  labs(x = "Substrate Moisture (%)", y = "Count") +
  theme_bw()

# Substrate Temperature
ggplot(df) +
  geom_histogram(aes(x = substrate_temperature_c, fill = worms_on_sample)) +
  labs(x = "Substrate Tempurature °C", y = "Count") +
  theme_bw()

# Ambient Humidity
ggplot(df) +
  geom_histogram(aes(x = ambient_humidity, fill = worms_on_sample)) +
  labs(x = "Substrate Humidity (%)", y = "Count") +
  theme_bw()

# Ambient Temperature
ggplot(df) +
  geom_histogram(aes(x = ambient_temperature_c, fill = worms_on_sample)) +
  labs(x = "Substrate Humidity (°C)", y = "Count") +
  theme_bw()

# Skyview
# Substrate Temperature
table(df$worms_on_sample, df$sky_view)

#=============================#
# Approximate Number of Worms #
#=============================#

# Substrate Moisture
dfn <- df %>% dplyr::filter(!is.na(approximate_number_of_worms))
dfn %>% dplyr::filter(substrate_moisture_ > 0) %>%
  ggplot(.) +
  geom_histogram(aes(x = substrate_moisture_, fill = approximate_number_of_worms)) +
  labs(x = "Substrate Moisture (%)", y = "Count") +
  theme_bw()

# Substrate Temperature
ggplot(dfn) +
  geom_histogram(aes(x = substrate_temperature_c, fill = approximate_number_of_worms)) +
  labs(x = "Substrate Tempurature °C", y = "Count") +
  theme_bw()

# Ambient Humidity
ggplot(dfn) +
  geom_histogram(aes(x = ambient_humidity_, fill = approximate_number_of_worms)) +
  labs(x = "Substrate Humidity (%)", y = "Count") +
  theme_bw()

# Ambient Temperature
ggplot(dfn) +
  geom_histogram(aes(x = ambient_temperature_c, fill = approximate_number_of_worms)) +
  labs(x = "Substrate Humidity (°C)", y = "Count") +
  theme_bw()


#================#
# Males observed #
#================#

vars = list("ambient_temperature_c" = "Ambient Temperature (C)",
            "ambient_humidity_" = "Ambient Humidity (%)")

lapply(names(vars), function(k) { 
  v = vars[[k]]
  ggplot(dfn) +
    geom_histogram(aes_(x = as.name(k), fill = quote(males_observed))) +
    labs(x = v, y = "Count") +
    theme_bw()
})



# ==========================#
# GPS / Altitude Comparison #
#===========================#

df_photo <- dplyr::left_join(df, exif, by = c("sample_photo"))

ggplot(df_photo) +
  geom_point(aes(x = gps_altitude, y = GPSAltitude))



ggplot(df_photo) +
  geom_point(aes(x = latitude, y = GPSLatitude))


ggplot(df_photo) +
  geom_point(aes(x = longitude, y = GPSLongitude))




ggplot(df_photo) +
  geom_histogram(aes(x =  GPSAltitude, fill = approximate_number_of_worms)) 
