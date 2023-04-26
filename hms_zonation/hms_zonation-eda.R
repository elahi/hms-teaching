################################################################################
##' @title EDA, hms zonation transects
##' @author Robin Elahi
##' @contact elahi@stanford.edu
##' @date 2023-04-25
##' @log 
################################################################################

#### File paths ####
here::i_am("hms_zonation/hms_zonation-eda.R")
library(here)
folder <- "hms_zonation"
file_name <- "hms_zonation-eda"

#### Packages ####
library(tidyverse)
theme_set(theme_bw())

#### Functions ####

#### Data ####
d <- read_csv(here(folder, "data","hms_zonation_data_230422.csv")) %>% 
  select(-DISTANCE, -HEIGHT) %>% 
  rename(DISTANCE = DISTANCE2) %>% 
  rename(HEIGHT = HEIGHT2) %>% 
  mutate(year = as.character(YEAR))
names(d)

# Remove data w/o tidal heights
# These two quadrats had no data for sessile or mobile taxa
d <- d %>% filter(!is.na(HEIGHT))

#### Elevation change ####

d %>% 
  ggplot(aes(DISTANCE, HEIGHT, color = SITE)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ year)

d %>% 
  ggplot(aes(DISTANCE, HEIGHT, color = year)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ SITE)

# 2013 is really bad, remove it
d <- d %>% 
  filter(year != "2013")

# Plot Height v Distance
d %>% 
  ggplot(aes(DISTANCE, HEIGHT, color = year)) + 
  geom_line(alpha = 0.7) +
  facet_wrap(~ SITE) + 
  labs(x = "Distance along transect (m)", y = "Elevation (m above MLLW)")

ggsave(file = here(folder, "figs", paste(file_name, "-plot_height_distance.png", sep = "")), 
       height = 3, width = 5)

# Summarise start and end distance along transect by year, site
transect_df <- d %>% 
  group_by(year, SITE) %>% 
  summarize(min_height = min(HEIGHT), 
            max_height = max(HEIGHT), 
            min_distance = min(DISTANCE), 
            max_distance = max(DISTANCE))

transect_df

transect_df %>% 
  ggplot(aes(SITE, min_height, color = year)) +
  geom_point()

transect_df %>% 
  ggplot(aes(SITE, max_height, color = year)) +
  geom_point()

d %>% 
  ggplot(aes(HEIGHT, SUM_SESSILE, color = SITE)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ year)

d %>% 
  ggplot(aes(HEIGHT, ENDOCLADIA, color = SITE)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ year)

d %>% 
  ggplot(aes(HEIGHT, SUM_MOBILE, color = SITE)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~ year)
