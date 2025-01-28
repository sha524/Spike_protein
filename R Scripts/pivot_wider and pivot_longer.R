#Going to the use the practice data
#Only the first 100 sequences


###### What does this R script contain ######

#Attemping to get the data in pivot_wider format, wide data




###### Data Manipulation ######

#Going to select just the Sequence Information and Mutations columns
#Will just be easier to work with

view(practice_spike_df)
sequence_mutations_data <- practice_spike_df %>%
  select(Sequence_Information, Mutations) %>%
view()

#Using the separate_longer_delim() function to split the mutations by |
#separate_longer_delim() then makes a new row for that split
#from the tidyr package
separated_rows <- sequence_mutations_data %>%
  separate_longer_delim(Mutations, delim = "|") %>%
  view()


###### pivot_wider() ######

#Using pivot_wider to get wide data
wide_data <- separated_rows %>%
  pivot_wider(names_from = "Mutations", values_from = "Mutations") %>%
  view()

#Need to replace all NA values with 0's and all
#values where a mutation is present with 1's
no_na <- wide_data %>%
  select(-Sequence_Information) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "0", "1"))) %>%
  view()


###### Completed data ######

#Need to recombine the vectorised data with the sequence information
#Going to look to use a join()

view(wide_data)
sequence_information_column <- wide_data %>%
  select(Sequence_Information) %>%
  view()

#complete_wide_data contains the vectorised data
#1's represent where the mutation is present and 0's show where
#a mutation is not present
complete_wide_data <- data.frame(c(sequence_information_column, no_na))
view(complete_wide_data)







