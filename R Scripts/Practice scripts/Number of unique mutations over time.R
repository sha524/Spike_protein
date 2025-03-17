################ Research Project #################

#Going to the use the practice data
#Only the first 100 sequences

##### What does this R script contain #####

#The number of individual mutations

#Plot the number of unique mutations over time

#Each individual mutation associated with a the date it was
#first sequenced



##### Set up #####

#practice_spike_df

#Going to split the mutations up using separate_longer_delim() function
#Use distinct() function to keep only the unique/distinct rows from the
#data frame

practice_split <- practice_spike_df %>%
  #Selecting the Sequence_information, Mutations and Sample_Date columns
  select(Sequence_Information, Mutations, Sample_Date) %>%
  separate_longer_delim(Mutations, delim = "|") %>%
  distinct(Mutations, .keep_all = TRUE) %>%
  #grouping all the sequences by their sample date
  group_by(Sample_Date) %>%
  #Using n() to count the number of dates that were the same
  summarise(count = n()) %>%
  arrange(desc(count))

#How many distinct unique mutations are there in this data set?
n_distinct(practice_split$Mutations)
#192 unique mutations in the practice_spike_df data

practice_split %>%
  count(Sample_Date)


##### Visualisation #####
ggplot(practice_split, aes(x = Sample_Date, y = count)) +
  geom_point()


