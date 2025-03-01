################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Plotting the number of mutations over time

#Want to see if the number of mutations increases or decreases over time




###### Setup #####

#Using the UK_sequences_df

#Adding an extra column to practice_spike_df
#to separate the sequences by year
practice_spike_df <- practice_spike_df %>%
  mutate(year = year(as.Date(Sample_Date)))
  
  
#Adding a new column to the UK_sequences_df
UK_sequences_df <- UK_sequences_df %>%
  mutate(year = year(as.Date(Sample_date))) %>%
  na.omit()
  


###### Plotting the number of mutations over time #####

#Going to try and plot the practice data set first
#The number of mutations over time
#practice_spike_df
ggplot(practice_spike_df, aes(x = Sample_Date, y = Number_of_Mutations, colour = year)) +
  geom_point() +
  facet_wrap(~ year)

UK_sequences_df %>%
  sample_n(1000) %>%
  ggplot(aes(x = Sample_date, y = Number_of_mutations, fill = year)) +
  geom_point()



#Using a linear model here
#Need to check the assumptions
model <- lm(Number_of_Mutations ~ Sample_Date, data = practice_spike_df)
#summary(model)
#p-value < 2.2e-16
#Therefore, the predictor variable is statistically significant
#Sample date has an effect on the number of mutations


#Currently crashing out
#Going to limit the number of values
num_mutations_time <- UK_sequences_df %>%
  tail(10000)
  
#Number of mutations over time for the main data set
ggplot(UK_sequences_df, aes(x = Sample_date, y = Number_of_mutations)) +
  geom_smooth()

linear_UK <- lm(Number_of_mutations ~ Sample_date, data = UK_sequences_df)
gam_UK <- gam(Number_of_mutations ~ Sample_date, data = UK_sequences_df)

