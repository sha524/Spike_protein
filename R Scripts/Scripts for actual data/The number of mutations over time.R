################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Plotting the number of mutations over time

#Want to see if the number of mutations increases or decreases over time




###### Setup #####

#Using the UK_sequences_df



###### Plotting the number of mutations over time #####
ggplot(UK_sequences_df, aes(x = Sample_date, y = Number_of_mutations)) +
  geom_point()


