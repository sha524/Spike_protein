################ Research Project ################


##### What does this R script contain #####

#Library of packages needed for this analysis

#Contains the code for how to calculate and plot the number of sequences per day

#This script uses an old data set, cog_metadata_28_1_22.csv
#cog_metadata_28_1_22.csv did not contain the full data set from GISAID


##### Set up #####

#Reading in the data
sarscov2_data <- read.csv("./data/cog_metadata_28_1_22.csv")

#Loading in library functions
library(ggplot2)
library(Rmisc)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(pgirmess)
library(dplyr)
library(stringr)
library(devtools)
library(usethis)
library(cowplot)
library(mgcv)
library(factoextra)
library(maps)
library(mapproj)
library(sf)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(Rtsne)


##### Number of sequences per day #####

#Ensure the sample date column is in date format
sarscov2_data$sample_date <- as.Date(sarscov2_data$sample_date)

#Count the number of sequences per day
#sarscov2_data %>% starts the pipe operator, with sarscov2_data as the 1st input to the next function
#in the pipeline
sequences_per_day <- sarscov2_data %>%
  group_by(sample_date) %>% #Groups data by the sample_date column
  summarise(num_sequences = n_distinct(sequence_name)) #Counts unique sequences per day

#Visualisation of the results
ggplot(sequences_per_day, aes(x = sample_date, y = num_sequences)) +
  geom_point() +
  xlab("Date") +
  ylab("Number of sequences") +
  theme_classic()




