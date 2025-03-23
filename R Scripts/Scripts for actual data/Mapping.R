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
  filter(region == "United Kingdom")


ggplot(map_data, aes)