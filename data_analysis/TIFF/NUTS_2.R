library(here) # use for here()
library(rgdal) # readOGR()
library(tidyverse) # glimpse()
library(broom) # tidy()
library(ggplot2) # ggplot()

rm(list=ls())  # Clear rstudio environemnt
cat("\014") # Clear console
gc() # Free up unused memory
dev.off(dev.list()["RStudioGD"]) # Clear all plots

# SOURCE OF SHAPEFILE: httpshttps://geoportal.statistics.gov.uk/datasets/ons::nuts-level-2-january-2018-full-clipped-boundaries-in-the-united-kingdom/about

# Website used to reduce size of shape file: https://mapshaper.org/
# The original is way larger than needed which makes plotting slow
# I've shrunk it down to 

shapefile_location <- "data_analysis/NUTS_Level_2_0.03"

shapefile_name <- 'NUTS_Level_2_0.03'

# Define path to shapefile and look inside, you need to copy the shapefile title
list.files(here(shapefile_location))

# Read shapefile (note, dsn is the folder path, layey is the name of the shape file WITHOUT .shp)
UK_Shape <- readOGR( 
  dsn= here(shapefile_location),
  layer=shapefile_name)

# plot() is a quicker way than ggplot to test the data
# plot(UK_Shape)

# UK_Shape.df <- as(UK_Shape, "data.frame")
# 
# write_csv(UK_Shape.df, "/Users/dylan/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Global_Agriculture/Assessment/Y4 - S2/Dissertation/Data Analysis/iMac_v2/gadm40_GBR_2.csv")

# Check the size of the shape file - less than 1Mb will load quite quickly, >50 will take forever...
# humanReadable(object.size(UK_Shape), units="auto")

# Check the name of the column that includes the region data 
glimpse(UK_Shape)

# Using ggplot requires that the data be in the form of a table, use tidy() to do this. Also include the region data (above).
UK_Shape_Tidy <- tidy(UK_Shape, region = "nuts218nm")

# The code below was included to pull out a list of regions, don't think this is needed anymore
# UK_Regions <- unique(UK_Shape_Tidy[c("id")])
# write_csv(UK_Regions, "/Users/dylan/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Global_Agriculture/Assessment/Y4 - S2/Dissertation/Data Analysis/COMBINED DATA/regions.csv")

# Make a simle plot of the data...
ggplot() +
  geom_polygon(data = UK_Shape_Tidy, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void()

# Read in the output of the calculations:
NUTS_2_Data <- read_csv(here("data_analysis/TIFF_by_NUTS.csv"))

NUTS_2_TIFF <- NUTS_2_Data %>% select(Subregion, TIFF_per_ha)

UK_Shape_Tidy = UK_Shape_Tidy %>%
  left_join(. , NUTS_2_TIFF, by=c("id"="Subregion"))

UK_Shape_Tidy %>%
  ggplot( aes(x=TIFF_per_ha)) +
  geom_histogram(bins=20, fill='skyblue', color='#69b3a2') + scale_x_log10()

ggplot() +
  geom_polygon(data = UK_Shape_Tidy, aes(fill = TIFF_per_ha, x = long, y = lat, group = group)) +
  theme_void()

# ggsave("UK_Map.png",
#        plot = UK_Map,
#        width = 13,
#        height = 20,
#        units = "cm",
#        bg = "transparent",
#        device='png',
#        dpi=700)


world_map <- ggplot(data = UK_Shape_Tidy, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.25) +
  theme_void() +
  geom_polygon(aes(fill = TIFF_per_ha), colour = "black", size = 0.08) +
  scale_fill_gradient(
    low = "#6600cc",
    high = "#ffff00",
    guide = "legend",
    na.value = "grey",
    name="Total\nIncome\nFrom\nFarming\n£/ha",
    breaks = c(12,100,250,500,750,1000),
    labels = c(10,100,250,500,750,1000)) +
  
  theme(
    text = element_text(color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(size=15),
    legend.text = element_text(size=15),
    
    # legend.position="none",
    legend.position = c(0.9, 0.6),
    
    legend.title.align = 0)

world_map

