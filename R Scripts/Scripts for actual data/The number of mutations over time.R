################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Plotting the number of mutations over time

#Want to see if the number of mutations increases or decreases over time




###### Setup #####

#Using the UK_sequences_df





###### Plotting the number of mutations over time #####

#Going to try and plot the practice data setv first
#The number of mutations over time
#practice_spike_df
ggplot(practice_spike_df, aes(x = Sample_Date, y = Number_of_Mutations)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Using a linear model here
#Need to check the assumptions
model <- lm(Number_of_Mutations ~ Sample_Date, data = practice_spike_df)
#summary(model)
#p-value < 2.2e-16
#Therefore, the predictor variable is statistically significant
#Sample date has an effect on the number of mutations

#Number of mutations over time for the main data set
ggplot(UK_sequences_df, aes(x = Sample_date, y = Number_of_mutations)) +
  geom_point()



