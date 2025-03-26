################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.tx


##### What does this R script contain #####

#Creating the map to show to the cluster distribution in the UK

#Two ways:
#map_data
#geom_sf() function




##### Map #####

#Creating a map using map_data
UK_map <- map_data("world") %>%
  filter(region == "UK") %>%
  select(-region)
view(UK_map)

ggplot(UK_map, aes(x = long, y = lat, group = group)) +
  geom_polygon()

#To create a map of the UK with borders
#Need to use the sf package
#ggthemes package
#rnaturalearth package

#Using the ne_states() function from the rnaturalearth package
#to select the 4 countries of the UK
UK_map2 <- ne_states(country =  "United Kingdom", returnclass = "sf")

gpplot(UK_map2) +
  geom_sf()








