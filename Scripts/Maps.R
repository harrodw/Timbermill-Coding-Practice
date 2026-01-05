################################################################################
# Author: Will Harrod
# Title: Making maps in R
# Created: 11/08/2025
################################################################################

# 1) Prep ######################################################################

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)
library(leaflet)
library(usmapdata)
library(extrafont)

# Load fonts (takes a few minutes)
font_import(prompt = FALSE)

# Path to spatial data
ras_path <- "C:\\Users\\willh\\Documents\\NCSU\\Data\\Spatial_Data\\"

# Add the turbines dataset
turbines_csv <- read.csv(paste0(ras_path, "USGS_Turbines\\all_turbines.csv"))

# Convert to an sf
turbines <- st_as_sf(turbines_csv, coords = c("xlong", "ylat")) %>% 
  # Create a new column for turbine capacity in MW
  mutate(MW = t_cap/1000) %>% 
  # Add a new column so they appear in the legend
  mutate(t.loc = "Turbine Locations") %>% 
  # Project to WGS 1984
  st_set_crs("WGS84")

# Add in the us states
us <- us_map(regions = "states", exclude = c("AK", "HI", "DC", "PR")) %>% 
  # select(full) %>% 
  rename(State = full)

# Add the wind generation capasity by state data 
state_cap <- read.csv("Data\\state_annual_wind_generation_2001_2025.csv")
# View
head(state_cap, n = nrow(state_cap))
glimpse(state_cap)

# pull out the years
names <- state_cap %>% 
  slice(4) %>% 
  pivot_longer(cols = everything(), names_to = "X", values_to = "Year") %>% 
  mutate(Year = case_when(Year == "source key" ~ "State", TRUE ~ Year)) %>% 
  slice_tail(n = 25) %>% 
  pull(Year)
# View
names

# Convert to a longer format
state_cap_long <- state_cap %>% 
  # Remove unnecessary columns
  select(-X, -X.1) %>% 
  # Asign the year as column names
  set_names(nm = names) %>% 
  # Remove unnessesary rows
  filter(!State %in% c("description", "Wind : United States")) %>% 
  arrange(`2001`) %>% 
  slice_tail(n = 48) %>% 
  # Replace blanks with 0
  mutate(across(.cols = everything(), ~ case_when(. == "--" ~ "0", TRUE ~ .))) %>% 
  # Pivot longer
  pivot_longer(cols = -State, names_to = "Year", values_to = "Capasity") %>% 
  # Remove extra characters from state names and switch chr to numeric
  mutate(State = str_remove(State, "Wind : "),
         Capasity = as.numeric(Capasity, ","),
         Year = as.numeric(Year)) 

# View
glimpse(state_cap_long)

# Pull out the years 
years <- state_cap_long %>% 
  arrange(Year) %>% 
  distinct(Year) %>% 
  pull(Year)

# Number of years 
n.years <- length(years)

# Define the minimum and maximum capacity
min_cap <- min(state_cap_long %>%  filter(Capasity > 0) %>% pull(Capasity))
max_cap <- max(state_cap_long %>% filter(State != "Texas") %>%  pull(Capasity))
texas_cap <- max(state_cap_long$Capasity)

# View
min_cap
max_cap
texas_cap

# View as a histogram
state_cap_long %>% 
  ggplot(aes(x = Capasity)) +
  geom_histogram() +
  facet_wrap(~Year)

# Path to the figures
fig_path <- "C:\\Users\\willh\\Documents\\NCSU\\Figures\\"

# Number of breaks
nbreaks <- 5

# Define palette breaks 
breaks <- c(min_cap, 500, 10000, 20000, 30000, texas_cap)
# View
breaks

# Define a palette
palette <- c("lightgray", hcl.colors(n = length(breaks) -1, palette = "Viridis"))

# 2) Map of state capacity by year ######################################################################

# Itterate over years
for (i in 1:n.years) {

# Define a year
y <- years[i]

# pull out each state's capasity for that year
state_cap_year <- state_cap_long %>% 
  filter(Year == y)

# Join with the states polygon
state_cap_poly <- us %>% 
  left_join(state_cap_year, by = "State") %>% 
  filter(Capasity > 0)

# Make a layer of states with no wind capasity
no_wind_states <- us %>% 
  left_join(state_cap_year, by = "State") %>% 
  filter(Capasity == 0) %>% 
  mutate(Capasity = "No Wind Capasity")

# Filter the turbine locations by year
trbn_year <- turbines %>% 
  filter(p_year <= y) 

# Make a plot of the turbines over the us
tbn_map_dot <- tm_shape(state_cap_poly) +
  # Map the states with wind
  tm_polygons(
    col = "Capasity",           
    palette = "viridis",
    border.col = "gray15",
    lwd = 0.4,
    border.alpha = 0.8,
    style = "cont",
    breaks = breaks,
    title = "Wind Capacity (GW hours)",
    alpha = 0.7
    ) +
  # Map the states with no wind
  tm_shape(no_wind_states) +
  tm_polygons(col = "Capasity",
              border.col = "gray15",
              lwd = 0.4,
              border.alpha = 0.8,
              palette = "gray90",
              title = "",
              alpha = 0.6) +
# Map the tubine loocations
  tm_shape(trbn_year) +
  tm_dots(
    col = "t.loc",
    palette = "gray20",
    alpha = 0.8,
    border.col = "white",
    border.lwd = 0.1,
    size = 0.1,
    title = "",
    legend.show = TRUE
    ) +
  tm_layout(
    title = as.character(y),
    frame = FALSE,  
    frame.lwd = NA,
    inner.margins = c(0.1, 0.1, 0.1, 0.1),
    title.size = 0.9,
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.text.fontfamily = "Arial",
    legend.outside = TRUE
  ) 
# Call the map
tbn_map_dot

# Save the map
tmap_save(tbn_map_dot, paste0(fig_path, "wind_map_", y, ".png"))

} # end loop


# 3) Turbine height by year ##########################################################

# View the data frame for turbines
glimpse(turbines_csv)

# View the hub heightss
sort(unique(turbines_csv$t_hh))
hist(turbines_csv$p_year)

# Make the plot
hh_year <- turbines_csv %>%
  filter(!is.na(p_year) & !is.na(t_hh)) %>% 
  mutate(Year = as.numeric(p_year)) %>% 
  ggplot() +
  geom_jitter(aes(x = Year, y = t_hh), 
             color = "skyblue", 
             size = 0.5, 
             alpha = 0.6,
             width = 1, 
             height =8
             ) +
  geom_smooth(aes(x = Year, y = t_hh),
              method = "lm", 
              color = "skyblue4", 
              linewidth = 2.5,
              alpha = 0.8
              ) +
  annotate(geom = "text",
            x = 1987,
           y = 80,
           label = "n = 76,051",,
           family = "Open Sans",
           color = "black",
           size = 6
          ) +
  theme_classic() + 
  scale_y_continuous(breaks = c(0, 40, 80, 120)) + 
  labs(
    x = "Project Year",
    y = "Turbine Hub Height (m)"
  ) +
  theme(
    plot.title = element_text(size = 18, family = "Open Sans"),
    axis.title.y = element_text(size = 18, family = "Open Sans"),
    axis.title.x = element_text(size = 18, family = "Open Sans"),
    axis.text = element_text(size = 18, family = "Open Sans"),
    legend.text = element_text(size = 18, family = "Open Sans"),
    legend.position = "none",
    legend.title = element_blank()
  )

# View the plot
hh_year

# Save the plot  
ggsave(plot = hh_year,
       file = paste0(fig_path, "rotor_diameter_plot.png"),
       width = 200,
       height = 150,
       units = "mm",
       dpi = 300)

# 4) energy consumption by source through time ######################################

# US data ----------------------------------------------------------------------------
# Add in US the data 
energy_wide <- read.csv("Data\\Renewable_Energy_Production_and_Consumption_by_Source_eia.csv")

# View
glimpse(energy_wide)

# Convertion factor for Tbtu to GWH
cnv_fct <- 2.93

# Convert to a more usable format
energy_long <- energy_wide %>% 
  # Replace not available with 0
  mutate(across(.cols = c("Solar.Energy.Consumption", "Wind.Energy.Consumption", "Biofuels.Consumption"),
                ~as.numeric(case_when(. == "Not Available" ~ "0", TRUE ~ .)))) %>% 
  # Converte character months to dates
  mutate(Year = as.numeric(str_sub(Month, start = 1, end = 4)),
         Month = case_when(str_detect(Month, "January")  ~ 1,
                           str_detect(Month, "February") ~ 2,
                           str_detect(Month, "March") ~ 3,
                           str_detect(Month, "April") ~ 4,
                           str_detect(Month, "May") ~ 5,
                           str_detect(Month, "June") ~ 6,
                           str_detect(Month, "July") ~ 7,
                           str_detect(Month, "August") ~ 8,
                           str_detect(Month, "September") ~ 9,
                           str_detect(Month, "October") ~ 10,
                           str_detect(Month, "November") ~ 11,
                           str_detect(Month, "December") ~ 12,
                           TRUE ~ NA
                                      )) %>% 
  # Make a true date Column
  mutate(Date = ym(paste0(Year, Month))) %>% 
  # Change the order
  select(Date, everything(), -Year, -Month) %>% 
  # Pivot longer
  pivot_longer(-Date, names_to = "Source", values_to = "Power") %>% 
  # Simplyfy power names
  mutate(Source = str_remove(Source, ".Consumption")) %>%
  mutate(Source = str_remove(Source, ".Energy")) %>% 
  mutate(Source = str_remove(Source, ".Power")) %>% 
  # Remove the "total biofuels catagory
  filter(Source != "Total.Biomass") %>% 
  mutate(Source = case_when(Source == "Biofuels" ~ "Agricultural Biofuels", TRUE ~ Source)) %>% 
  # Convert to GWH
  mutate(Power = Power*cnv_fct,
         Source = factor(Source, levels = c("Hydroelectric",
                                            "Wood",
                                            "Waste",
                                            "Agricultural Biofuels",
                                            "Geothermal",
                                            "Wind", 
                                            "Solar"
                                            )))

# View
glimpse(energy_long)

# View the order of all power soruces
sort(unique(energy_long$Source))

# Global data ------------------------------------------------------------------
# Ember data 
ember_full <- read.csv("Data\\ember_yearly_energy.csv")
# View
glimpse(ember_full)
count(ember_full, Subcategory, Variable)
count(ember_full, Ember.region, Area.type)
ember_full %>% filter(Ember.region == "") %>%  count(Area)

# clean the EMBER data 
ember_renewable <- ember_full %>% 
  filter(
           Variable %in% c("Hydro", "Solar", "Wind", "Bioenergy", "Coal", "Gas", "Nuclear") &
           Category == "Electricity generation" &
           Unit == "TWh" &
           Area.type == "Region") %>% 
  filter(Area == "World") %>% 
  select(Year, Variable, Value) %>% 
  mutate(Variable = case_when(Variable == "Hydro" ~ "Hydroelectric",
                              # Variable == "Other Renewables" ~ "Other Renewables",
                              TRUE ~ Variable)) %>% 
  mutate(Variable = factor(Variable, levels = c(
                                                "Wind", 
                                                "Solar", 
                                                "Hydroelectric", 
                                                "Bioenergy", 
                                                "Coal",
                                                "Gas", 
                                                "Nuclear")),
         Value = replace_na(Value, 0)) %>% 
  arrange(Year, Variable) 

# View
glimpse(ember_renewable)

# make a custom pallete
custom_pallet <- c(
  "skyblue", 
  "goldenrod1",
  "blue4",
  "green4",
  "gray20",
  "orchid4",
  "darkorange1"
  ) 

# correct length?
length(unique(ember_renewable$Variable)) == length(custom_pallet)

# Label heights
lab_hgt <- ember_renewable %>% 
  filter(Year == 2024) %>% 
  mutate(Label.Height = case_when(Variable == "Nuclear" ~  Value + 150,
                                  Variable == "Solar" ~ Value - 50,
                                  TRUE ~ Value + 50), 
         Label.Year = case_when(Variable == "Wind" ~ 2025,
                                Variable == "Solar" ~ 2025,
                                Variable == "Hydroelectric" ~ 2026.3,
                                Variable == "Bioenergy" ~ 2025.7,
                                Variable == "Gas" ~ 2025,
                                Variable == "Coal" ~ 2025,
                                Variable == "Nuclear" ~ 2025.3,
                                TRUE ~ 2024)
         ) %>% 
  select(Variable, Label.Year, Label.Height)

# Plot
eng_gen_time <- ember_renewable %>%
  group_by(Variable) %>% 
  left_join(lab_hgt, by = "Variable") %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = Year, 
                y = Value, 
                col = Variable,
                linewidth = Variable,
                alpha = Variable
  )) +
  geom_text(aes(x = Label.Year, 
                y = Label.Height,
                label = Variable, 
                col = Variable, 
                alpha = Variable,
                size = Variable),
            size = 4) +
  scale_alpha_manual(values = c(1, rep(0.4, 6))) +
  scale_color_manual(values = custom_pallet) +
  scale_linewidth_manual(values = c(1.7, rep(1.2, 6))) +
  scale_y_continuous(labels = function(x){paste(x, "TWh")}) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2029, by = 6), limits = c(2000, 2028)) +
  labs(
    x = "Year",
    y = "",
    main = "Gloabl Annual Electricity Generation"
  ) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 18, family = "Open Sans"),
    axis.title.y = element_text(size = 18, family = "Open Sans"),
    axis.title.x = element_text(size = 18, family = "Open Sans"),
    axis.text = element_text(size = 16, family = "Open Sans"),
    legend.text = element_text(size = 18, family = "Open Sans"),
    legend.position = "none",
    legend.title = element_blank()
  )

# View the plot
eng_gen_time

# Save the plot  
ggsave(plot = eng_gen_time,
       file = paste0(fig_path, "energy_consumption_plot.png"),
       width = 200,
       height = 170,
       units = "mm",
       dpi = 300)

# 5) Map of wind speeds at different heights ###################################  

# Path to the rasters
ras_path <- "C:\\Users\\willh\\OneDrive\\Documents\\NCSU\\Data\\Spatial_Data\\us-wind-data\\"

# Add the rasters
wind_40m <- rast(paste0(ras_path, "wtk_conus_40m_mean_masked.tif"))
wind_80m <- rast(paste0(ras_path, "wtk_conus_80m_mean_masked.tif"))

# Project the us rasters
wind_40m_prj <- project(wind_40m, crs(us))
wind_80m_prj <- project(wind_80m, crs(us))

# clip onshore only
wind_40m_clp <- mask(wind_40m_prj, us)
wind_80m_clp <- mask(wind_80m_prj, us)

# Make the maps
# 40m
wind_40m_map <- tm_shape(wind_40m_clp) +
  tm_raster(
  palette = "magma",
  style = "cont",
  title = ""
) +
  tm_shape(us) +
  tm_borders(
    lwd = 1,
    col = "gray30",
    alpha = 0.5
  ) +
  tm_layout(
    title = "40m Wind Speed (m/s)",
    frame = FALSE,  
    frame.lwd = NA,
    legend.title.size = 1.8,
    legend.text.size = 0.8,
    legend.outside = TRUE
  ) 
# 80m
wind_80m_map <- tm_shape(wind_80m_clp) +
  tm_raster(
    palette = "magma",
    style = "cont",
    title = ""
  ) +
  tm_shape(us) +
  tm_borders(
    lwd = 1,
    col = "gray30",
    alpha = 0.5
  ) +
  tm_layout(
    title = "80m Wind Speed (m/s)",
    frame = FALSE,  
    frame.lwd = NA,
    legend.title.size = 1.8,
    legend.text.size = 0.8,
    legend.outside = TRUE
  ) 

# View the maps
wind_40m_map
wind_80m_map

# Save the maps
tmap_save(wind_40m_map, paste0(fig_path, "wind_speed_40m_map_.png"))
tmap_save(wind_80m_map, paste0(fig_path, "wind_speed_80m_map_.png"))
