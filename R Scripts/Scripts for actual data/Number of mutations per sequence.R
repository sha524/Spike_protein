################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain ######

#Plot of the number of mutations per day for the actual data

#Any further analysis




###### Set up ######

#UK_seqs_msa_0522_spike_mutations.txt


###### Plotting the number of mutations per sequence ######

#File is very large
#Going to look at the top 20 sequences with the most mutations

most_number_of_mutations <- UK_sequences_df %>%
  arrange(desc(Number_of_mutations)) %>%
  head(100)

ggplot(UK_sequences_df, aes(x = Sequence_Information, y = Number_of_mutations)) +
  geom_point()
