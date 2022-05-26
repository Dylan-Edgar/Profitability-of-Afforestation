library(wordcountaddin) # Library used to insert word count on to the title page.
library(bookdown) # used for 'output:bookdown::pdf_book' so that I can cross reference things
library(here) # use for here()
library(tidyverse) # read_csv()
library(knitr) # used for kable()
library(kableExtra) # used for kable()
library(rgdal) # readOGR()
library(broom) # tidy()
library(ggplot2) # ggplot()
library(plotly)
library(scales)
library(janitor) # tidy()
library(viridis)
library(janitor)

rm(list=ls())  # Clear the rstudio environemnt (data frames etc.)
cat("\014") # Clear the console (just makes things look nice and new and tidy)
gc() # Free up unused memory (RStudio starts hoarding memory if running for a long time... apparently)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # Clear all plots (and silence the error if there are none to clear)

# Import TIFF data
TIFF_by_NUTS <- read_csv(here("data_analysis/TIFF/TIFF_by_NUTS.csv"))

# Select only the important bits
TIFF_by_NUTS <- TIFF_by_NUTS[1:3]

# clean the column names
TIFF_by_NUTS <- clean_names(TIFF_by_NUTS)

# Add a column representing the total income over 100 years
TIFF_by_NUTS <- TIFF_by_NUTS %>%
  add_column("tiff_100" = TIFF_by_NUTS$tiff_ha * 100)

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

TIFF_by_NUTS <- TIFF_by_NUTS %>%
  add_column("50" = TIFF_by_NUTS$tiff_100 / 50,
             "100" = TIFF_by_NUTS$tiff_100 / 100,
             "200" = TIFF_by_NUTS$tiff_100 / 200,
             "252" = TIFF_by_NUTS$tiff_100 / 252,
             "300" = TIFF_by_NUTS$tiff_100 / 300,
             "400" = TIFF_by_NUTS$tiff_100 / 400,
             "500" = TIFF_by_NUTS$tiff_100 / 500)

TIFF_by_NUTS <- TIFF_by_NUTS[,c(1,2,5,6,7,8,9,10,11)]

TIFF_Scot <- TIFF_by_NUTS[order(TIFF_by_NUTS$region), ]

TIFF_Scot <- TIFF_Scot[31:35,]

TIFF_Scot <- TIFF_Scot[,2:8]

TIFF_Scot <- pivot_longer(TIFF_Scot, !nuts_level_2_subregion, names_to = "seq_rate", values_to = "carbon_price")

TIFF_Scot <- TIFF_Scot[order(TIFF_Scot$nuts_level_2_subregion, decreasing = FALSE), ]

barplot(height=TIFF_Scot$carbon_price,
        names=TIFF_Scot$nuts_level_2_subregion,
        density=c(0,20,40,20,20,200),
        angle=c(0,45,90,135,180,90),
        col="#6600cc",
        ylab = "Unit Price for Afforestation to become more Profitable (£/tCO2e)",
        xlab = "Region",
        legend = unique(TIFF_Scot$seq_rate))

summary(TIFF_by_NUTS)
