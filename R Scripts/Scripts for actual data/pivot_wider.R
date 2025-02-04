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
