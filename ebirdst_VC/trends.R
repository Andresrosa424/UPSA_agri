rm(list=ls());gc() 


library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)


set_ebirdst_access_key("3t1ddtda3uh3", overwrite = TRUE)
ebirdst_download_status("Upland Sandpiper")
trends_UPSA <- ebirdst_download_trends("Upland Sandpiper")
ebirdst_data_dir()


# shows all species with st data available
trends_runs <- ebirdst_runs %>% 
  filter(has_trends) %>% 
  select(species_code, common_name,
         trends_season, trends_region,
         trends_start_year, trends_end_year,
         trends_start_date, trends_end_date,
         rsquared, beta0)
#subset to UPSA available data for viewing
upsa <- trends_runs %>% 
  filter(common_name == "Upland Sandpiper") %>% 
  select(trends_start_year, trends_end_year, 
         trends_start_date, trends_end_date)


### Mackey-Strimas trends ###

long_trend <- read.csv("C:/Users/ROSALESA/OneDrive - University of Saskatchewan/17_Analysis/Chapter1/eBird trend data/prairie analysis/PHJV_trends.csv")
str(long_trend)

#create col for confidence interval non zero, 1 is True, 0 is False
long_trend$abd_ppy_nonzero <- NA
long_trend$abd_ppy_nonzero <- as.factor(long_trend$abd_ppy_nonzero)

#ifelse statement to label that pixel if overlapping zero
long_trend$abd_ppy_nonzero <- ifelse(long_trend$abd_ppy_lower > 0 | long_trend$abd_ppy_upper < 0, 1, 0)

#subset only significant trends with high confidence
sig_trends <- subset(long_trend, abd_ppy_nonzero == "1")

#apply compund interest to trend to get cumulative trend 
#abd_trend: the median estimated cumulative change in relative abundance over the trend time period
#equation:abd_trend <- 100 * ((1 + abd_ppy / 100)^(2021 - 2007) - 1)

sig_trends$abd_trend <- NA
sig_trends$abd_trend <- 100 * ((1 + sig_trends$abd_ppy / 100)^(2021 - 2007) - 1)

write.csv(sig_trends, "C:/Users/ROSALESA/OneDrive - University of Saskatchewan/17_Analysis/Chapter1/eBird trends/prairie analysis/PHJVsig_abd_trends.csv", row.names = FALSE)




#########
#create object with trends 
trends <- load_trends("uplsan")


##### produce trends vector for arcpro #####


trends_sf <- st_as_sf(trends, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)

# be sure to modify the path to the file to save the file to directory of 
# your choice on your hard drive
write_sf(trends_sf, "ebird-trends_sagthr_2022.gpkg",
         layer = "upsa_trends")

# make a map
ggplot(trends_upsa) +
  geom_sf(aes(color = abd_ppy), size = 2) +
  scale_color_gradient2(low = "#CB181D", high = "#2171B5",
                        limits = c(-4, 4), 
                        oob = scales::oob_squish) +
  guides(color = guide_colorbar(title.position = "left", barheight = 15)) +
  labs(title = "Sagebrush species breeding trends (2012-2022)",
       color = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.title = element_text(angle = 90))

trends_sagebrush_species <- load_trends(sagebrush_species)

# calculate mean trend for each cell
trends_UPSA <- trends_upsa %>% 
  group_by(srd_id, latitude, longitude) %>% 
  summarize(n_species = n(),
            abd_ppy = mean(abd_ppy, na.rm = TRUE),
            .groups = "drop")
print(trends_UPSA)

# convert the points to sf format
uppies <- trends_UPSA %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

write_sf(uppies, "ebird-uppies_trends.gpkg",
         layer = "upsa_trends")

library(maps)

north_america <- map_data("world", region = c("USA", "Canada"))


# make a map
ggplot(uppies) +
  geom_sf(aes(color = abd_ppy), size = 2) +
  scale_color_gradient2(low = "#CB181D", high = "#2171B5",
                        limits = c(-4, 4), 
                        oob = scales::oob_squish) +
  guides(color = guide_colorbar(title.position = "left", barheight = 15)) +
  labs(title = "Upland Sandpiper breeding trends (2012-2022)",
       color = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.title = element_text(angle = 90))

#best so far
ggplot(uppies) +
  geom_sf(aes(color = abd_ppy), size = 2) +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "black") +
  scale_color_gradient2(low = "#CB181D", high = "#2171B5",
                        limits = c(-4, 4), 
                        oob = scales::oob_squish) +
  guides(color = guide_colorbar(title.position = "left", barheight = 15)) +
  labs(title = "Upland Sandpiper breeding trends (2012-2022)",
       color = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.title = element_text(angle = 90), plot.background = element_rect(fill = "transparent", color = NA)) +
  coord_sf(xlim = c(-165, -60), ylim = c(35, 72))


ggplot(uppies) +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "black") +
  scale_color_gradient2(low = "#CB181D", high = "#2171B5",
                        limits = c(-4, 4), 
                        oob = scales::oob_squish) +
  geom_sf(aes(color = abd_ppy), size = 2) +
  guides(color = guide_colorbar(title.position = "left", barheight = 15)) +
  labs(color = "Relative abundance trend [% change / year]") +
  theme_bw() +
  theme(legend.title = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  theme(panel.grid = element_blank(),
  plot.background = element_blank())+
  coord_sf(xlim = c(-165, -60), ylim = c(35, 72)) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="right") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8))

use_git()
