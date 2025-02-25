################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.tx

###### What does this R script contain ######

#Extracting the country of origin




###### Extracting the country of origin ######

#Using str_extract to extract the country, that submitted the sequence
UK_sequences_df <- UK_sequences_df %>%
  mutate(Country = str_extract(Sequence_Information, pattern = "England|Wales|Scotland"))


#Count the number of sequences from each country
UK_sequences_df %>%
  count(Country)

