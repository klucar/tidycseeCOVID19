# library(ggmap)
# library(threejs)
# library(tidyverse)
# library(tidycseeCOVID19)
#
# data("covid19_daily")
# covid19 <- tibble::as_tibble(covid19_daily)
#
# plot_data <- covid19 %>% group_by(Country) %>%
#   summarise(confirmed_country_total = sum(Confirmed)) %>%
#   right_join(covid19, by = c("Country" = "Country")) %>%
#   mutate(Confirmed_pct = Confirmed / sum(Confirmed))
#
# plot_data <- covid19 %>%
#   group_by(Latitude, Longitude) %>%
#   summarise(Confirmed_sum = sum(Confirmed))
#
# # Plot the data on the globe
# #earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
# globe <- globejs(
#                  lat = plot_data$Latitude,
#                  long = plot_data$Longitude,
#                  val = 20* log10(plot_data$Confirmed_sum),
#                  color = 'red',
#                  pointsize = 0.5,
#                  atmosphere = TRUE)
