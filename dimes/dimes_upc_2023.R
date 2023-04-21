################################################################################
##' @title Analysis of DIMES upc data, 2023 
##' @author Robin Elahi
##' @contact elahi@stanford.edu
##' @date 2023-04-21
##' @log 
################################################################################

#### File paths ####
here::i_am("dimes/dimes_upc_2023.R")
library(here)
folder <- "dimes"
file_name <- "dimes_upc_2023"

#### Packages ####
library(tidyverse)
theme_set(theme_bw())
#### Functions ####

#### Data ####
d <- read_csv(here(folder, "data","dimes_hms_upc_raw_230421.csv")) %>% 
  mutate(year = as.character(year))
names(d)

#### Summarise tidal heights and plot ####

# Boxplot
d %>% 
  ggplot(aes(transect_ht, quad_est_m, color = year)) +
  geom_boxplot() +
  labs(x = "Transect", y = "Elevation (m above MLLW)")

# Summarize tidal heights
elev_df <- d %>% 
  group_by(year, transect_ht) %>% 
  summarise(mean = mean(quad_est_m), 
            sd = sd(quad_est_m), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 1.96*se)
elev_df

# Plot mean and interval
elev_df %>% 
  ggplot(aes(transect_ht, mean, color = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.1) +
  labs(x = "DIMES transect at HMS", y = "Elevation (m above MLLW)", caption = "mean and 95%CI")

ggsave(file = here(folder, "figs", paste(file_name, "-plot_mean_elev.png", sep = "")), 
       height = 3, width = 4)

#### Summarise tidal heights and plot ####

# Boxplot
d %>% 
  ggplot(aes(transect_ht, Shell/16*100, color = year)) +
  geom_boxplot() +
  labs(x = "Transect", y = "Percent cover")

# Summarize 
shell_df <- d %>% 
  group_by(year, transect_ht) %>% 
  summarise(mean = mean(Shell/16*100, na.rm = TRUE), 
            sd = sd(Shell/16*100, na.rm = TRUE), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 1.96*se)
shell_df

# Plot mean and interval
shell_df %>% 
  ggplot(aes(transect_ht, mean, color = year)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.1) +
  labs(x = "DIMES transect at HMS", y = "Shell cover (%)", caption = "mean and 95%CI")

ggsave(file = here(folder, "figs", paste(file_name, "-plot_mean_shell.png", sep = "")), 
       height = 3, width = 4)


