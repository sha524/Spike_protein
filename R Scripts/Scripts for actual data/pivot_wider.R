################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Most common mutations

#Going to use the pivot_wider() function

#Transforming data from long format to wide format data, by spreading
#the values across multiple columns



###### Setup #####

#Using the UK_sequences_df

#Going to select just the Sequence_Information column and the Mutation column
Sequence_Mutation_columns <- UK_sequences_df %>%
  select(Sequence_Information, Mutations)




###### Data manipulation #####

#Going to use separate_longer_delim()
#Function splits the mutations based on the delimiter |
#and makes a new row for each split
Mutations_split <- Sequence_Mutation_columns %>%
  separate_longer_delim(Mutations, delim = "|")



###### Most common mutations #####
most_common_mutations_UK <- Mutations_split %>%
  count(Mutations) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(non_vs_syn = str_extract(Mutations, pattern = "non")) %>%
  mutate(non_vs_syn = ifelse(is.na(non_vs_syn), "syn", "non"))


#Visualisation of the most common mutations
ggplot(most_common_mutations_UK, aes(x = Mutations, y = n, fill = non_vs_syn)) +
  geom_col() +
  xlab("Mutations") +
  ylab("Frequency") +
  scale_fill_manual(values = c(non = "blue", syn = "red")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 12),
        axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none",
        panel.spacing = unit(c(2, 2, 2, 2), "cm"))





###### pivot_wider() #####

#Joining the two tables together
#semi_join() filters for observations that match the second table
#Removing all the 
combined_data <- Mutations_split %>%
  semi_join(most_common_mutations_UK, by = "Mutations")
  

#Using pivot_wider() to transform the data from long format to wide format
wide_data_UK <- combined_data %>%
  pivot_wider(names_from = "Mutations", values_from = "Mutations")


#Need to change all the NA's to 0's
#Need to change all the values where the mutation is present to 1's
no_na_UK <- wide_data_UK %>%
  select(-Sequence_Information) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, 1)))

#Going to recombine with the Sequence_Information column
#Selecting the sequence information column to then recombine with the mutation data
sequence_information_column_2 <- wide_data_UK %>%
  select(Sequence_Information)

#completed_wide_data_UK contains the sequence information
#and the most common mutations
#vectorised the data. 1's represent where the mutation is present
#0's represent where the data is not present
completed_wide_data_UK <- data.frame(c(sequence_information_column_2, no_na_UK))



###### wide data for clustering analysis #####

#Going to select just the mutations column
#Only contains the unique individual mutations
mutations_column <- Mutations_split %>%
  count(Mutations) %>%
  select(Mutations)

#Going to join the data using full_join()
full_join_data <- mutations_column %>%
  full_join(Mutations_split, by = "Mutations") %>%
  relocate(Mutations, .after = Sequence_Information)















