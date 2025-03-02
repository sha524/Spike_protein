################# Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt



###### What does this R script contain ######

#The number of individual mutations

#Plot the number of unique mutations over time

#Each individual mutation associated with a the date it was
#first sequenced



###### Set up ######

#UK_sequences_df

#Going to split the mutations using the separate_longer_delim() function
#Use distinct function to keep only the unique/distinct rows

#Will first take a random sample just to test
UK_sequences_df %>%
#Taking a random sample of 1000 values
#sample_n(5000) %>%
  select(Sequence_Information, Mutations, Sample_date, year) %>%
  separate_longer_delim(Mutations, delim = "|") %>%
#distinct() function used to keep on the first time a unique
#mutations appeared
  distinct(Mutations, .keep_all = TRUE) %>%
  group_by(Sample_date, year) %>%
  summarise(count = n()) %>%
#plotting the number of unique mutations over time
  ggplot(aes(x = Sample_date, y = count, colour = as.factor(year))) +
  geom_point() +
  xlab("Sample date") +
  ylab("Number of novel mutations") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2021-01-01",
                                  "2022-01-01", "2023-01-01", "2024-01-01")),
                     labels = c("2020", "2021", "2022", "2023", "2024")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(name = "year",
                    values = c("2020" = "red",
                               "2021" = "blue",
                               "2022" = "green",
                               "2023" = "yellow",
                               "2024" = "purple"))
  


###### Visualisation ######
ggplot(test, aes(x = Sample_date, y = year)) +
  geom_point()



