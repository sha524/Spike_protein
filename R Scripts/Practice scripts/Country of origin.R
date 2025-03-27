################ Research Project #################

#Going to the use the practice data
#Only the first 100 sequences

###### What does this R script contain ######

#Extracting the country of origin from the sequence information


###### Data setup ######

#First 100 sequences
practice_sequences <- UK_sequences_df %>%
  sample_n(100)


###### Extracting the country of origin ######
practice_sequences <- practice_sequences %>%
  mutate(Country = str_extract(Sequence_Information, pattern = "England|Wales|Scotland"))
  