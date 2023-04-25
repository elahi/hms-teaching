################################################################################
##' @title Random points, HM1
##' @author Robin Elahi
##' @contact elahi@stanford.edu
##' @date 2023-04-21
##' @log 
################################################################################

##' Polygon created in maps.google.com
##' Corners estimated from two years of DIMES transects at HM1
##' Visually created a rectangle to encompass these two sets of points, along with features visible in map

#### File paths ####
here::i_am("dimes/dimes_HM1_random_2023.R")
library(here)
folder <- "dimes"
file_name <- "dimes_HM1_random_2023"

#### Packages ####
library(tidyverse)
library(sf)
theme_set(theme_bw())

#### Functions ####

#### Data ####
kml1 <- st_read(here(folder, "data","HM1.kml")) 
str(kml1)
names(kml1)
kml1
plot(kml1)
plot(st_geometry(kml1))
attributes(kml1)

#### Get points ####
N <- 30

# Random
set.seed(1)
my_points <- st_sample(kml1, size = N, type = "random")
plot(st_geometry(kml1))
plot(my_points, add = TRUE, pch = 3, col = "red")

# Regular
my_points <- st_sample(kml1, size = N, type = "regular")
plot(my_points, add = TRUE, pch = 21, col = "gray")


# Export for student testing
set.seed(5)
my_points <- st_sample(kml1, size = 10, type = "random")
plot(st_geometry(kml1))
plot(my_points, add = TRUE, pch = 3, col = "red")

# Create dataframe and export
# Note: column names are for use with gpx output as described here:
# https://www.gpsvisualizer.com/convert_input
head(my_points)
str(my_points)
points_df <- do.call(rbind, st_geometry(my_points)) %>% 
  as_tibble() %>% 
  setNames(c("lon", "lat")) %>% 
  mutate(name = 1:n(), 
         desc = "Random")

# Reorder columns
points_df <- points_df %>% 
  select(name, desc, lat, lon) 
points_df
write.csv(points_df, here(folder, "data_output", "hm1_10random_2023.csv"))
