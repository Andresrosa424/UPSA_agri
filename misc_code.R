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




## Calculate Abundance for each year (2012-2022)

Use the equation 

$Ay = A2017 × (1 + r)^{y-2017}$
  
  - Ay is the abundance in year y.
- A2017  is the abundance in 2017 (median year).
- r is the annual percent change expressed as a decimal.
- y is the year of interest.

```{r}

#utrends_st <- utrends_st %>%
# bind_cols(map_dfc(2012:2022, 
# ~ tibble(!!paste0("abd_", .x) := utrends_st$abd * 
# (1 + utrends_st$abd_ppy) ^ (.x - 2017))))




```


# install packages from CRAN (unless installed)
pckgs_needed <- c(
  "tidyverse",
  "brms",
  "rstan",
  "rstanarm",
  "remotes",
  "tidybayes",
  "bridgesampling",
  "shinystan",
  "mgcv"
)
pckgs_installed <- installed.packages()[,"Package"]
pckgs_2_install <- pckgs_needed[!(pckgs_needed %in% pckgs_installed)]
if(length(pckgs_2_install)) {
  install.packages(pckgs_2_install)
} 

# install additional packages from GitHub (unless installed)
if (! "aida" %in% pckgs_installed) {
  remotes::install_github("michael-franke/aida-package")
}
if (! "faintr" %in% pckgs_installed) {
  remotes::install_github("michael-franke/faintr")
}
if (! "cspplot" %in% pckgs_installed) {
  remotes::install_github("CogSciPrag/cspplot")
}

# load the required packages
x <- lapply(pckgs_needed, library, character.only = TRUE)
library(aida)
library(faintr)
library(cspplot)

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the CSP-theme for plotting
theme_set(theme_csp())

# global color scheme from CSP
project_colors = cspplot::list_colors() |> pull(hex)
# names(project_colors) <- cspplot::list_colors() |> pull(name)

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
}



#calcualte cumulative trend in abd
utrends_folds <- utrends_folds %>%
  mutate(abd_ppy = round(abd_ppy, 7)) %>%
  mutate(abd_trend = 100 * ((1 + abd_ppy / 100)^(2022 - 2012) - 1)) %>%
  group_by(srd_id) %>% 
  mutate(trend_stdev = sd(abd_trend))

#subset SD
trend_sd <- utrends_folds %>%
  dplyr::select(srd_id, trend_stdev) %>%
  distinct()
#filter trend CI width
utrends <- raw_utrends %>%
  mutate(CI_width = abd_trend_upper - abd_trend_lower) %>%
  left_join(trend_sd, by = "srd_id") %>%
  rename(CI_stdev = trend_stdev)

ggplot(utrends, aes(x = CI_width)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal()

ggplot(utrends, aes(x = CI_stdev)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal()

#utrends <- utrends %>% 
filter(!(srd_id >= 126707 & srd_id <= 171704)) %>% 
  filter(CI_width <= 50) %>%
  filter(CI_stdev <= 20)

### Linear Models

LM 1

```{r}
lm_global <- lm(abd_trend ~ FO_mean + GR_mean +  shdi_mean  + simp_mean + simp_mean*shdi_mean + abd + srd_id + Core_distance, data = NA_model)
summary(lm_global)
plot(lm_global)
vif(lm_global)

```

LM2

```{r}
lm_global2 <- lm(abd_trend ~  GR_mean +  shdi_mean  + simp_mean + abd + srd_id + Core_distance, data = NA_model)
summary(lm_global2)
plot(lm_global2)
vif(lm_global2)

```

### General Linear Models

glm 1

```{r}
glm_global <- glm(abd_trend ~ FO_mean + GR_mean + ED_mean +  shdi_mean + simp_mean + abd + simp_mean*shdi_mean + srd_id + Core_distance, family = "gaussian", data = NA_model)
summary(glm_global)
plot(glm_global)
vif(glm_global)

```

GLM 2 GLM global model removing non-informative parameters from glm 1 (mean simp and abd)

```{r}
glm_global2 <- glm(abd_trend ~ FO_mean + GR_mean + ED_mean +  shdi_mean + abd + srd_id + Core_distance, family = "gaussian", data = NA_model)
summary(glm_global2)
plot(glm_global2)
vif(glm_global2)


```

### GLM Polynomial models

polynomial global

```{r}
poly_global <- glm(abd_trend ~ FO_mean + GR_mean + ED_mean + I(ED_mean^2) + shdi_mean + simp_mean + I(simp_mean^2) + I(simp_mean^3) + abd + srd_id + shdi_mean*simp_mean + Core_distance , data = NA_model)
summary(poly_global)

```

```{r}
poly_global2 <- glm(abd_trend ~ FO_mean + GR_mean +  shdi_mean + simp_mean + I(simp_mean^2) + I(simp_mean^3) + abd + srd_id + shdi_mean*simp_mean +  Core_distance , data = NA_model)
summary(poly_global2)
```

```{r}
poly_global3 <- glm(abd_trend ~ FO_mean + GR_mean  + simp_mean + I(simp_mean^2) + I(simp_mean^3) + abd + srd_id + shdi_mean*simp_mean +  Core_distance , data = NA_model)
summary(poly_global3)
```
### Global models AIC

```{r, echo=FALSE}
globals <- AIC(null_model, lm_global, lm_global2, glm_global, glm_global2, poly_global, poly_global2, poly_global3, gam_global, gam_global2, gam_global3)

globals <- globals %>% dplyr::arrange(AIC)
globals
```