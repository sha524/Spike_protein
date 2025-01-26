#Going to the use the practice data
#Only the first 100 sequences


###### What does this R script contain ######

#Attemping to get the data in pivot_wider format, wide data




###### Data Manipulation ######

##Going to select just the Sequence Information and Mutations columns
##Will just be easier to work with

view(practice_spike_df)
sequence_mutations_data <- practice_spike_df %>%
  select(Sequence_Information, Mutations) %>%
view()

sequence_mutations_data %>%
  


