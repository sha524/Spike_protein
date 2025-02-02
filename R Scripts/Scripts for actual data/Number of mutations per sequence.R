################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain ######

#Create two new columns, one for the number of syn mutations and one for
#the number of non mutations

#Plot of the number of mutations per day for the actual data

#Any further analysis




###### Set up ######

#UK_seqs_msa_0522_spike_mutations.txt



###### Data manipulation ######

#Going to count the number of nonsynonymous mutations per sequence
#Going to count the number of synonymous mutations per sequence
UK_sequences_df <- UK_sequences_df %>%
  mutate(non_mutations = str_count(Mutations, pattern = "non")) %>%
  mutate(syn_mutations = str_count(Mutations, pattern = "syn")) %>%
  relocate(Sample_date, .after = syn_mutations)


###### Plotting the number of mutations per sequence ######

#File is very large
#Going to look at the top 20 sequences with the most mutations

most_number_of_mutations <- UK_sequences_df %>%
  arrange(desc(Number_of_mutations)) %>%
  head(20)

ggplot(UK_sequences_df, aes(x = Sequence_Information, y = Number_of_mutations)) +
  geom_point()




