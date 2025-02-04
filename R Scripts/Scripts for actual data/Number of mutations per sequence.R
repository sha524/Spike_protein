################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain ######

#Create two new columns, one for the number of syn mutations and one for
#the number of non mutations

#Plot of the top 20 sequences with the most mutations per sequence



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

#Plot of the sequences with the most mutations
ggplot(most_number_of_mutations, aes(x = Sequence_Information, y = Number_of_mutations,
                                     fill = Sequence_Information)) +
  geom_col() +
  ylim(0, 300) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 7)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black")) +
  ylab("Number of mutations per sequence") +
  xlab("Sequence Information")



#Plot top 10 sequences with the most mutations, non vs syn
top_10_most_mutations <- most_number_of_mutations %>%
  head(10)



  
  








