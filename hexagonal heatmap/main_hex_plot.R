# encoding: UTF-8
# AIzaSyABMFSHi7sHOvtCpMbeuNrh9wtEsw-x1Jc

# https://juliasilge.com/blog/nyc-airbnb/
# http://varianceexplained.org/r/sliced-ml/?utm_source=pocket_mylist

library(tidyverse)
library(readr)

df <- read_csv("Desktop/DS/web/personalweb/hexagonal heatmap/listings.csv")
View(df)

dim(df)
names(df)

df$price <- df$price + 1 # avoid observations with price = 0 

euros <- scales::dollar_format(suffix = "€", prefix = "")

df %>%
  ggplot(aes(price , fill = neighbourhood_group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  scale_x_log10(labels = euros ) +
  labs(fill = NULL, x = "price per night")


df %>%
  ggplot(aes(longitude, latitude, color = log(price))) +
  geom_point(alpha = 0.2, size = 0.5) +
  scale_color_viridis_c()


# Hexagonal heatmap of 2d bin counts
# https://ggplot2.tidyverse.org/reference/geom_hex.html 


hex_heatmap <- df %>%
  ggplot(aes(longitude, latitude, z = log(price))) +
  stat_summary_hex(alpha = 0.8, bins = 70) +
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)")

str(hex_heatmap)
hex_heatmap$layers

df %>%
  ggplot(aes(longitude, latitude, z = log(price))) +
  stat_summary_hex(alpha = 0.8, bins = 50) +
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)")

# ggplot’s stat_density2d, which uses MASS::kde2d on the backend to estimate the density using a bivariate normal kernel.


density_heatmap <- ggplot(data = df, aes(x = longitude, y = latitude)) +
  stat_density2d( alpha = .5, geom = "polygon") + 
  scale_fill_viridis_c() #+ 
  #theme_bw()

ggplot(data = df, aes(x = longitude, y = latitude)) +
  geom_density_2d_filled(alpha = 0.5) 



################################################################################
library(ggthemes)
library(ggmap)

# Geocoding API
# https://console.cloud.google.com/google/maps-apis

register_google(key = "your API here")

geocode("Barcelona, Spain", source = "google")

mapa_bcn <- 
  get_map(location = "Barcelona, Spain",
          zoom = 13, # 10 corresponds to city
          color = "color",
          maptype = "roadmap", # “terrain”, “satellite”, “roadmap”, “hybrid”
          scale = 1,
          source= "google")

mapa_bcn_minimalist <- get_googlemap(center = 'Barcelona, Spain', zoom = 13, maptype = "roadmap", 
                                     style = "&style=feature:all|element:labels|visibility:off" )

mapa_bcn_minimalist_zoom <- get_googlemap(center = 'Barcelona, Spain', zoom = 14, maptype = "roadmap", 
                                          style = "&style=feature:all|element:labels|visibility:off" )
#'feature:administrative.country|element:labels|visibility:off


# density: where the listings are 

points_map <- mapa_bcn %>% 
  ggmap() +
  geom_point(data = df, aes(x = longitude, y = latitude), color = 'darkblue', size = 0.1, alpha = 0.2) +
  theme_map()
# 424 points ommited (out of 17079)

mapa_bcn_minimalist %>% 
  ggmap() +
  geom_point(data = df, aes(x = longitude, y = latitude), color = 'darkblue', size = 0.1, alpha = 0.2) +
  theme_map()
# 424 points ommited (out of 17079)

mapa_bcn_minimalist %>% 
  ggmap() +
  geom_point(data = df, aes(x = longitude, y = latitude), color = 'blue', size = 0.1)
# 424 points ommited (out of 17079)

# mapa_bcn %>% 
#   ggmap(data = df, aes(x = longitude, y = latitude)) +
#   stat_density2d( alpha = .5, geom = "polygon") + 
#   scale_fill_viridis_c() 


# mapa_bcn %>% 
#   ggmap() +
#   stat_density2d( data = df, aes(x = longitude, y = latitude), 
#                   alpha = .5, geom = "polygon", fill= ..level.. ) + 
#   scale_fill_viridis_c() 


################################################################################



aggregated_lat_lon <- df %>%
  group_by(latitude = round(latitude, 3),
           longitude = round(longitude, 3)) %>%
  summarize(price = mean(price),
            n = n()) %>%
  filter(n >= 5)


agg_freq_map <- ggmap(mapa_bcn_minimalist) +
  geom_point(aes(longitude, latitude, size = n, color = price ),
             data = aggregated_lat_lon) +
  scale_color_gradient2(low = "#00008B", high = "darkred", midpoint = 2 ,
                        trans = "log10", labels = euros) +
  scale_size_continuous(range = c(1, 3)) +
  theme_map() +
  labs(color = "Price",
       size = "# of listings") 


agg_points_map <- ggmap(mapa_bcn_minimalist) +
  geom_point(aes(longitude, latitude, size = n, color = price ),
             data = aggregated_lat_lon) +
  scale_color_gradient2(low = "#00008B", high = "darkred", midpoint = 2 ,
                        trans = "log10", labels = euros) +
  scale_size_continuous(range = c(1, 3)) +
  theme_map() +
  labs(color = "Price",
       size = "# of listings") 


agg_hex_map <- ggmap(mapa_bcn_minimalist) +
  coord_cartesian() + 
  stat_summary_hex(data= df, aes(longitude, latitude, z = log(price)),
                   alpha = 0.8, bins = 70) + 
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)")

# Warning messages:
# 1: Removed 424 rows containing non-finite values (stat_summary_hex). 
# 2: Removed 4 rows containing missing values (geom_hex). 

sum( df$price == 0 )

#############################################################################
# IMPORTANT PLOTS

# plots_to_save <- list(hex_heatmap, density_heatmap, points_map, agg_freq_map, agg_hex_map) | no works
names_to_save <- paste0( c("hex_heatmap", "density_heatmap", "points_map", "agg_freq_map", "agg_hex_map"), ".png" )

ggsave(filename = names_to_save[1], plot = hex_heatmap, device= "png", width = 14, height = 10, units = "cm")
ggsave(filename = names_to_save[2], plot = density_heatmap, device= "png", width = 14, height = 10, units = "cm")
ggsave(filename = names_to_save[3], plot = points_map, device= "png", width = 14, height = 10, units = "cm")
ggsave(filename = names_to_save[4], plot = agg_freq_map, device= "png", width = 14, height = 10, units = "cm")
ggsave(filename = names_to_save[5], plot = agg_hex_map, device= "png", width = 14, height = 10, units = "cm")

# mapply(ggplot2::ggsave, filename = names_to_save, plot = plots_to_save, MoreArgs = list(width = 14, height = 10, units = "cm" ) )   


hex_heatmap
density_heatmap
points_map
agg_freq_map
agg_hex_map 

# Part de l'eixample i de primera linia de costa de la platja del Bogatell





