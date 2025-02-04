################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Going to use the pivot_wider() function

#Transforming data from long format to wide format data, by spreading
#the values across multiple columns



###### Setup #####

#Using the UK_sequences_df

#Going to select just the Sequence_Information column and the Mutation column
Sequence_Mutation_columns <- UK_sequences_df %>%
  select(Sequence_Information, Mutations)


#Going to use separate_longer_delim()
#Function splits the mutations based on the delimiter |
#and makes a new row for each split
Mutations_split <- Sequence_Mutation_columns %>%
  separate_longer_delim(Mutations, delim = "|")

#Most common mutations
most_common_mutations_UK <- Mutations_split %>%
  count(Mutations) %>%
  arrange(desc(n)) %>%
  head(10)

#Joining the two tables together
#semi_join() filters for observations that match the second table
#Removing all the 
combined_data <- Mutations_split %>%
  semi_join(most_common_mutations_UK, by = "Mutations")
  

#Using pivot_wider() to transform the data from long format to wide format
wide_data_UK <- combined_data %>%
  pivot_wider(names_from = "Mutations", values_from = "Mutations")



