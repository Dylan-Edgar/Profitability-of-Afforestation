library(tidyverse)
library(here)
library(knitr) # used for kable()
library(kableExtra) # used for kable()
library(janitor)
library(viridis)
library(rgdal) # readOGR()
library(broom) # tidy()

rm(list=ls())  # Clear the rstudio environemnt (data frames etc.)
cat("\014") # Clear the console (just makes things look nice and new and tidy)
gc() # Free up unused memory (RStudio starts hoarding memory if running for a long time... apparently)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # Clear all plots (and silence the error if there are none to clear)

# ---------------------------------------------------------------------------------
# Calculate the 100 year TIFF
# ---------------------------------------------------------------------------------

# Import TIFF data
TIFF_by_NUTS <- read_csv(here("data_analysis/TIFF/TIFF_by_NUTS.csv"))

# Select only the important bits
TIFF_by_NUTS <- TIFF_by_NUTS[1:3]

# clean the column names
TIFF_by_NUTS <- clean_names(TIFF_by_NUTS)

# Add a column representing the total income over 100 years
TIFF_by_NUTS <- TIFF_by_NUTS %>%
  add_column("tiff_100" = TIFF_by_NUTS$tiff_ha * 100)

# ---------------------------------------------------------------------------------

# Set nominal a carbon price, we'll play around with this later
Carbon_Price <- 50

# ---------------------------------------------------------------------------------
# Calculate the PIU / ha for all projects collectively
# ---------------------------------------------------------------------------------

# Import the BP WCC project data
BP_data <- read_csv(here("data_analysis/WCC/BP_Summary.csv"))

# Select only the important data
PIU_Collective <- BP_data[,c(1,5,6)]

# Clean the column names
PIU_Collective <- clean_names(PIU_Collective)

# Calculate the total number of PIU
Total_PIU <- colSums(PIU_Collective[,2])

# Pull out unique area values (each value is repeated once for every PIU vintage)
Total_Area <- unique(PIU_Collective[,3])

# Calculate the total area
Total_Area <- colSums(Total_Area)

# Calculate PIU per hectare for all projects collectively...
PIU_per_ha <- Total_PIU / Total_Area

# ---------------------------------------------------------------------------------
# Calculate the PIU / ha for each project individually
# ---------------------------------------------------------------------------------

# Pull in only the columns we need
PIU_Individual <- BP_data[,c(1,5,2)]

# clean the column names
PIU_Individual <- clean_names(PIU_Individual)

# pivot the table so that data is sorted by project site
PIU_Individual <- pivot_wider(PIU_Individual, names_from = project_site, values_from = piu_project)

# Create a new object containing the sum of PIUs for each project site
PIU_Individual <- PIU_Individual %>% summarise_if(is.numeric, sum, na.rm = TRUE)

# Pivot it back again
PIU_Individual <- pivot_longer(PIU_Individual, !years_since_start_date, names_to = "project_site", values_to = "total_piu")

# Remove the first nonsense column
PIU_Individual <- PIU_Individual[,2:3]

# Pull out a list of areas
Area_data <- unique(BP_data[,c(1,6)])

# clean the names again
Area_data <- clean_names(Area_data)

# Join the two data sets up
PIU_Individual <- left_join(PIU_Individual, Area_data, by = "project_site")

# Calculate PIU per hectare
PIU_Individual <- PIU_Individual %>%
  add_column("piu_per_ha" = PIU_Individual$total_piu / PIU_Individual$area)

# Save this for later use (plotting broadleaf ratio against PIU/ha)
PIU_broadleaf <- PIU_Individual

# Calculate total income at multiple carbon prices
PIU_Individual <- PIU_Individual %>%
  add_column("10" = PIU_Individual$piu_per_ha * 10,
             "20" = PIU_Individual$piu_per_ha * 20,
             "30" = PIU_Individual$piu_per_ha * 30,
             "40" = PIU_Individual$piu_per_ha * 40,
             "50" = PIU_Individual$piu_per_ha * 50,
             "60" = PIU_Individual$piu_per_ha * 60,
             "70" = PIU_Individual$piu_per_ha * 70,
             "80" = PIU_Individual$piu_per_ha * 80)

# Pull out total PIT per project data
PIU_Individual_Total <- PIU_Individual[,1:4]

# And remove the data from the larger data set since it isn't needed for plotting
PIU_Individual <- PIU_Individual[,c(1,5:ncol(PIU_Individual))]

# pivot it back again to make ready for plotting
PIU_Individual <- PIU_Individual %>%
  pivot_longer(!project_site, names_to = "carbon_price", values_to = "total_income")

# Add another column to be used as 'group' - not sure why, but it doesn't work if you use name as group... 
# following this: https://www.data-to-viz.com/caveat/spaghetti.html
PIU_Individual <- PIU_Individual %>%
  mutate(project_site2=project_site)

# Make the column "carbon_price" back into an integer (becomes a 'char' for some reason)
PIU_Individual <- PIU_Individual %>% mutate(across(carbon_price, as.double))

# Set the primary colour to be used by the plot (this was included so that all plot colours could be changed at once)
Primary_Colour <- "#6600cc"

# make the plot
PIU_Individual_plot <- PIU_Individual %>%
  ggplot(aes(x=carbon_price, y=total_income)) +
  geom_line( data=PIU_Individual %>% dplyr::select(-project_site), aes(group=project_site2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=project_site), color=Primary_Colour, size=0.5 )+
  scale_colour_viridis_d() +
  theme_light() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()) +
  labs(x = "Price received per unit sold (£)", y = "Total income received over 100 years (£/ha)") +
  facet_wrap(~project_site)

# plot the plot
PIU_Individual_plot

# ---------------------------------------------------------------------------------
# Trees by project
# ---------------------------------------------------------------------------------

# Read the data, sourced from: https://mer.markit.com/br-reg/public/master-project.jsp?project_id=103000000000757
trees_by_project <- read_csv(here("data_analysis/WCC/BP_Tree_Data.csv"))

# clean the names
trees_by_project <- clean_names(trees_by_project)

# need to sort the names out...
unique_trees <- as_tibble(unique(trees_by_project$species))

# There must be a better way to do this, but I wasn't sure, and sometimes it's easier just to work hard not smart...
trees_by_project[trees_by_project == "Aspen"] <- "Aspen"
trees_by_project[trees_by_project == "Birch"] <- "Birch"
trees_by_project[trees_by_project == "Rowan"] <- "Rowan"
trees_by_project[trees_by_project == "Scots pine"] <- "Scots Pine"
trees_by_project[trees_by_project == "Alder"] <- "Alder"
trees_by_project[trees_by_project == "Ash"] <- "Ash"
trees_by_project[trees_by_project == "Downy birch"] <- "Birch"
trees_by_project[trees_by_project == "Downy Birch"] <- "Birch"
trees_by_project[trees_by_project == "Gean / wild cherry"] <- "Cherry"
trees_by_project[trees_by_project == "Oak (sessile)"] <- "Oak"
trees_by_project[trees_by_project == "Sessile Oak"] <- "Oak"
trees_by_project[trees_by_project == "Willow"] <- "Willow"
trees_by_project[trees_by_project == "Wych Elm"] <- "Elm"
trees_by_project[trees_by_project == "Bird Cherry"] <- "Cherry"
trees_by_project[trees_by_project == "Common Alder"] <- "Alder"
trees_by_project[trees_by_project == "Scots Pine"] <- "Scots Pine"
trees_by_project[trees_by_project == "Ash (MB)"] <- "Ash"
trees_by_project[trees_by_project == "Common Alder (MB)"] <- "Alder"
trees_by_project[trees_by_project == "Downy Birch (MB)"] <- "Birch"
trees_by_project[trees_by_project == "Rowan (MB)"] <- "Rowan"
trees_by_project[trees_by_project == "Sessile Oak (MB)"] <- "Oak"
trees_by_project[trees_by_project == "Wild Cherry (MB)"] <- "Cherry"
trees_by_project[trees_by_project == "Wych Elm (MB)"] <- "Elm"
trees_by_project[trees_by_project == "Birch (betula sp.)"] <- "Birch"
trees_by_project[trees_by_project == "Hybrid Poplar (Populus serotina/trichocarpa)"] <- "Poplar"
trees_by_project[trees_by_project == "Oak (MB)"] <- "Oak"
trees_by_project[trees_by_project == "Oak (Quercus sp.)"] <- "Oak"
trees_by_project[trees_by_project == "Oak (XB)"] <- "Oak"
trees_by_project[trees_by_project == "Scots pine (Pinus sylvestris)"] <- "Scots Pine"
trees_by_project[trees_by_project == "Silver Birch (MB)"] <- "Birch"
trees_by_project[trees_by_project == "Alder(MB)"] <- "Alder"
trees_by_project[trees_by_project == "Alder(XB)"] <- "Alder"
trees_by_project[trees_by_project == "Ash(MB)"] <- "Ash"
trees_by_project[trees_by_project == "Ash(XB)"] <- "Ash"
trees_by_project[trees_by_project == "Birch (XB)"] <- "Birch"
trees_by_project[trees_by_project == "Birch(MB)"] <- "Birch"
trees_by_project[trees_by_project == "Bird Cherry(MB)"] <- "Cherry"
trees_by_project[trees_by_project == "Hybrid Poplar"] <- "Poplar"
trees_by_project[trees_by_project == "Oak"] <- "Oak"
trees_by_project[trees_by_project == "Rowan(MB)"] <- "Rowan"
trees_by_project[trees_by_project == "Rowan(XB)"] <- "Rowan"
trees_by_project[trees_by_project == "Silver Birch (XB)"] <- "Birch"
trees_by_project[trees_by_project == "Downy Birch(MB)"] <- "Birch"
trees_by_project[trees_by_project == "Downy Birch(XB)"] <- "Birch"
trees_by_project[trees_by_project == "Sessile Oak(MB)"] <- "Oak"
trees_by_project[trees_by_project == "Sessile Oak(XB)"] <- "Oak"
trees_by_project[trees_by_project == "Bird Cherry (MB)"] <- "Cherry"
trees_by_project[trees_by_project == "Common Alder(MB)"] <- "Alder"
trees_by_project[trees_by_project == "Hybrid Larch"] <- "Larch"
trees_by_project[trees_by_project == "Sitka Spruce"] <- "Sitka"
trees_by_project[trees_by_project == "Birch (silver)"] <- "Birch"
trees_by_project[trees_by_project == "Birch downy (Betula pubescens)"] <- "Birch"
trees_by_project[trees_by_project == "Elm (MB)"] <- "Elm"
trees_by_project[trees_by_project == "Oak sessile (Quercus petraea)"] <- "Oak"
trees_by_project[trees_by_project == "Oak, Pedunculate (Quercus robur)"] <- "Oak"
trees_by_project[trees_by_project == "Wild cherry (gean)"] <- "Cherry"
trees_by_project[trees_by_project == "Willow (grey)"] <- "Willow"
trees_by_project[trees_by_project == "Aspen (MB)"] <- "Aspen"
trees_by_project[trees_by_project == "Birch (silver and downy)"] <- "Birch"
trees_by_project[trees_by_project == "Bird cherry"] <- "Cherry"
trees_by_project[trees_by_project == "Gean"] <- "Cherry"
trees_by_project[trees_by_project == "Oak (sessile and pedunculate)"] <- "Oak"
trees_by_project[trees_by_project == "Oak (sessile and pedunculate"] <- "Oak"
trees_by_project[trees_by_project == "Willow (MB)"] <- "Willow"
trees_by_project[trees_by_project == "Birch, Black/Downy(Betula pubescens)"] <- "Birch"
trees_by_project[trees_by_project == "Birch, Silver (Betula pendula)"] <- "Birch"
trees_by_project[trees_by_project == "Birch,Black/downy (Betula pubescens)"] <- "Birch"
trees_by_project[trees_by_project == "bird cherry (MB)"] <- "Cherry"
trees_by_project[trees_by_project == "Oak, Sessile (Quercus petraea)"] <- "Oak"
trees_by_project[trees_by_project == "Scots Pine (Pinus sylvestris s sp. scotica)"] <- "Scots Pine"
trees_by_project[trees_by_project == "Willow (salix sp.)"] <- "Willow"
trees_by_project[trees_by_project == "Willow spp."] <- "Willow"
trees_by_project[trees_by_project == "Alder (Common)"] <- "Alder"
trees_by_project[trees_by_project == "Birch (downy)"] <- "Birch"
trees_by_project[trees_by_project == "Birch(Downy)"] <- "Birch"
trees_by_project[trees_by_project == "Oak (Sessile)"] <- "Oak"
trees_by_project[trees_by_project == "Cherry"] <- "Cherry"
trees_by_project[trees_by_project == "Elm"] <- "Elm"
trees_by_project[trees_by_project == "Grey willow"] <- "Willow"
trees_by_project[trees_by_project == "Birch (downy/silver)"] <- "Birch"
trees_by_project[trees_by_project == "Common alder"] <- "Alder"
trees_by_project[trees_by_project == "Hazel"] <- "Hazel"
trees_by_project[trees_by_project == "Mixed broadleaves"] <- "Mixed Broadleaf"
trees_by_project[trees_by_project == "Mixed conifers"] <- "Mixed Conifers"
trees_by_project[trees_by_project == "Oak (robur/petraea)"] <- "Oak"
trees_by_project[trees_by_project == "other broadleaves"] <- "Mixed Broadleaf"
trees_by_project[trees_by_project == "Other Conifer"] <- "Mixed Conifers"
trees_by_project[trees_by_project == "other conifers"] <- "Mixed Conifers"
trees_by_project[trees_by_project == "Silver birch"] <- "Birch"
trees_by_project[trees_by_project == "Wild cherry/gean"] <- "Cherry"

# For some reason the rows below won't rename, so do it manually...
trees_by_project[191,2] <- "Birch"
trees_by_project[192,2] <- "Oak"
trees_by_project[55,2] <- "Oak"

unique_trees_2 <- as_tibble(unique(trees_by_project$species))

# Select only the relevant columns
trees_by_project <- trees_by_project[,c(1,2,7)]

# Sort by project again so you can make some spot checks
trees_by_project <- trees_by_project[order(trees_by_project$project_site), ]

# Combine each species within a given area
trees_by_project <- as_tibble(aggregate(x = trees_by_project$area_ha,
                                        by = list(project_site = trees_by_project$project_site,
                                                  species = trees_by_project$species),
                                        FUN = sum))

# Sort by project again so you can make some spot checks
# E.g., The total area of Birch in project Abernethy should be 3.93 + 3.48 + 0.484 = 7.894 (expect some rounding error)
trees_by_project <- trees_by_project[order(trees_by_project$project_site),]

# Make a plot
trees_by_project_absolute_plot <- ggplot(trees_by_project, aes(fill=species, y=x, x=project_site)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T,
                     name="Species")+
  ylab("Area (ha)") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# plot the plot
trees_by_project_absolute_plot

# save the data as a new dataframe as trees_by_project is about to get modified further
trees_by_project_absolute_data <- trees_by_project

# Could be interesting to compare broadleaf to conifer...

trees_by_project[trees_by_project == "Aspen"] <- "Broadleaf"
trees_by_project[trees_by_project == "Birch"] <- "Broadleaf"
trees_by_project[trees_by_project == "Rowan"] <- "Broadleaf"
trees_by_project[trees_by_project == "Alder"] <- "Broadleaf"
trees_by_project[trees_by_project == "Ash"] <- "Broadleaf"
trees_by_project[trees_by_project == "Elm"] <- "Broadleaf"
trees_by_project[trees_by_project == "Cherry"] <- "Broadleaf"
trees_by_project[trees_by_project == "Oak"] <- "Broadleaf"
trees_by_project[trees_by_project == "Willow"] <- "Broadleaf"
trees_by_project[trees_by_project == "Poplar"] <- "Broadleaf"
trees_by_project[trees_by_project == "Hazel"] <- "Broadleaf"
trees_by_project[trees_by_project == "Mixed Broadleaf"] <- "Broadleaf"

trees_by_project[trees_by_project == "Scots Pine"] <- "Conifer"
trees_by_project[trees_by_project == "Larch"] <- "Conifer"
trees_by_project[trees_by_project == "Sitka"] <- "Conifer"
trees_by_project[trees_by_project == "Mixed Conifers"] <- "Conifer"

# Make a plot
trees_by_project_percentage_plot <- ggplot(trees_by_project, aes(fill=species, y=x, x=project_site)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T,
                     name="",
                     guide = guide_legend( keyheight = unit(3, units = "mm"),
                                           keywidth=unit(12, units = "mm"),
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow=2)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Conifer vs Broadleaf") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# plot the plot
trees_by_project_percentage_plot

# ---------------------------------------------------------------------------------
# Scatter plot showing total income VS broadleaf %
# ---------------------------------------------------------------------------------

# https://r-graph-gallery.com/scatterplot.html

# aggregate data for each project by species (i.e., add up all the broadleaf etc)
trees_by_project_percentage_data <- as_tibble(aggregate(x = trees_by_project$x,
                                                        by = list(project_site = trees_by_project$project_site,
                                                                  species = trees_by_project$species),
                                                        FUN = sum))

# rename x, that isn't very informative
trees_by_project_percentage_data <- rename(trees_by_project_percentage_data, area = x)

# Pivot the data to seperate out broadleaf and conifer data
trees_by_project_percentage_data <- pivot_wider(trees_by_project_percentage_data, names_from = species, values_from = area)

# Change NA values to 0
trees_by_project_percentage_data[is.na(trees_by_project_percentage_data)] <- 0

# calculate the total area of all species
trees_by_project_percentage_data <- trees_by_project_percentage_data %>%
  add_column("total_tree_area" = trees_by_project_percentage_data$Broadleaf + trees_by_project_percentage_data$Conifer)

# calculate the percentage of broadleaf 
trees_by_project_percentage_data <- trees_by_project_percentage_data %>%
  add_column("broadleaf_percentage" = trees_by_project_percentage_data$Broadleaf / trees_by_project_percentage_data$total_tree_area)

# Pull in the piu/ha data calculated earlier
PIU_broadleaf

# join em up
trees_by_project_ratio_piu <- left_join(trees_by_project_percentage_data, PIU_broadleaf, by = "project_site")

# Create the scatter plot...
scatter <- ggplot(trees_by_project_ratio_piu, aes(x=broadleaf_percentage, y=piu_per_ha)) +
  geom_point() +
  theme_light() +
  ylab("PIU per Hectare") +
  xlab("Ration of Confier to Broadleaf, where 1 = 100% Broadleaf") +
  geom_smooth(method=lm , color=Primary_Colour, se=TRUE) # se=TRUE adds the confidence interval

# Well that's now very enlightening...
scatter

# ---------------------------------------------------------------------------------
# Previous land use and soil type
# ---------------------------------------------------------------------------------

# Make a table showing all the data

presentable_data <- trees_by_project_ratio_piu[,c(1, 7, 6, 8, 5)]

presentable_data <- presentable_data %>%
  add_column("broadleaf_percent" = presentable_data$broadleaf_percentage * 100)

presentable_data <- presentable_data[,c(1, 2, 3, 4, 6)]

BP_data_presentable <- unique(BP_data[,c(1,9,10)])

BP_data_presentable <- clean_names(BP_data_presentable)

presentable_data <- left_join(presentable_data, BP_data_presentable, by = "project_site")

presentable_data <- presentable_data[order(presentable_data$piu_per_ha),]

presentable_data_average <- presentable_data %>% summarise_if(is.numeric, mean, na.rm = TRUE)

presentable_data <- presentable_data %>%
  add_row("project_site" = "AVERAGE",
          "area" = presentable_data_average$area,
          "total_piu" = presentable_data_average$total_piu,
          "piu_per_ha" = presentable_data_average$total_piu / presentable_data_average$area,
          "broadleaf_percent" = presentable_data_average$broadleaf_percent,
          "previous_landuse" = "--",
          "soil_group" = "--",)

presentable_data

Presentable_WCC_BP_data <- presentable_data %>%
  rename("Project Site" = project_site,
         "Area (ha)" = area,
         "Total Sequestration (tCO2)" = total_piu,
         "Sequestration per Hectare (tCO2/ha)" = piu_per_ha,
         "Percent Broadleaf (%)" = broadleaf_percent,
         "Previous Landuse" = previous_landuse,
         "Soil Group" = soil_group)

summary(presentable_data)

# ---------------------------------------------------------------------------------
# Plot a map showing the project sites
# ---------------------------------------------------------------------------------
  
  # THIS SECTION IS CACHED SO WILL NOT UPDATE IF CHANGED (change to 'cache = FALSE')
  
  
  # rm(list=ls())  # Clear rstudio environemnt
  # cat("\014") # Clear console
  # gc() # Free up unused memory
  # dev.off(dev.list()["RStudioGD"]) # Clear all plots
  
  # SOURCE OF SHAPEFILE: httpshttps://geoportal.statistics.gov.uk/datasets/ons::nuts-level-2-january-2018-full-clipped-boundaries-in-the-united-kingdom/about
  
# Website used to reduce size of shape file: https://mapshaper.org/
# The original is way larger than needed which makes plotting slow
# I've shrunk it down to 

shapefile_location <- "data_analysis/Shapefiles/Scotland/Constituencies 2.5 percent"

shapefile_name <- 'Constituencies 2.5 percent'

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
UK_Shape_Tidy <- tidy(UK_Shape, region = "spc16nm")

BP_Project_Location <- read_csv(here("data_analysis/WCC/BP_Project_Location.csv"))

BP_Project_Map <- ggplot() +
  geom_polygon(data = UK_Shape_Tidy, aes(fill = NA, x = long, y = lat, group = group),
               fill = "grey") +
  coord_fixed(1) +
  theme_void() +
  scale_fill_viridis(trans = "log",
                     option = "D", # Sets the colour scale. Choose A-H. Only D really works without borders.
                     breaks=c(12,100,250,500,750,1000),
                     name="Total Income\nfrom Farming\n(£/ha/year)",
                     guide = guide_legend( keyheight = unit(3, units = "mm"),
                                           keywidth=unit(12, units = "mm"),
                                           label.position = "bottom",
                                           title.position = 'top',
                                           nrow=6)) + # Controls the key row length, 1 = horizontal, n = vertical.
  
  # The code below adds all the labels to the map.
  # I wasn't sure how to do this automatically so did it manually.
  # Reference data can be found in the dataframe "BP_Project_Location" above...
  
  annotate(geom="point", x=283700, y=668600, size=3, shape=6, colour='black') +
  annotate(geom="point", x=50000, y=1200000, size=2, shape=6, colour='black') +
  annotate(geom="text", x=70000, y=1200000, label="Crossrigg & Drumbow", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=246000, y=710900, size=3, shape=2, colour='black') +
  annotate(geom="point", x=50000, y=1175000, size=2, shape=2, colour='black') +
  annotate(geom="text", x=70000, y=1175000, label="Loch Katrine, Glen Fin & Inversnaid", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=342500, y=826000, size=3, shape=3, colour='black') +
  annotate(geom="point", x=50000, y=1150000, size=2, shape=3, colour='black') +
  annotate(geom="text", x=70000, y=1150000, label="Darroch Wids, x3 ", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=175000, y=820900, size=3, shape=1, colour='black') +
  annotate(geom="point", x=50000, y=1025000, size=2, shape=1, colour='black') +
  annotate(geom="text", x=70000, y=1025000, label="Kinloch Hills", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=297000, y=807300, size=3, shape=4, colour='black') +
  annotate(geom="point", x=50000, y=1100000, size=2, shape=4, colour='black') +
  annotate(geom="text", x=70000, y=1100000, label="Glenmore Forest Park & Abernethy", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=239500, y=569500, size=3, shape=5, colour='black') +
  annotate(geom="point", x=50000, y=1075000, size=2, shape=5, colour='black') +
  annotate(geom="text", x=70000, y=1075000, label="Barclye", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=234700, y=827500, size=3, shape=9, colour='black') +
  annotate(geom="point", x=50000, y=1050000, size=2, shape=9, colour='black') +
  annotate(geom="text", x=70000, y=1050000, label="Corrimony", size = 2.5, hjust = 0) +
  
  annotate(geom="point", x=294000, y=702600, size=3, shape=10, colour='black') +
  annotate(geom="point", x=50000, y=1125000, size=2, shape=10, colour='black') +
  annotate(geom="text", x=70000, y=1125000, label="Glen Sherup, Glen Quey & Geordies Wood", size = 2.5, hjust = 0)

BP_Project_Map

# ---------------------------------------------------------------------------------
# HEAT  MAPS
# ---------------------------------------------------------------------------------


PIU_Individual
