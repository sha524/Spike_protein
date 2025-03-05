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
  mutate(year = year(as.Date(Sample_Date))) %>%
  na.omit()
  
  
#Adding a new column to the UK_sequences_df
UK_sequences_df <- UK_sequences_df %>%
  mutate(year = year(as.Date(Sample_date))) %>%
  na.omit()
  


###### Plotting the number of mutations over time #####

#Going to try and plot the practice data set first
#The number of mutations over time
#practice_spike_df
ggplot(practice_spike_df, aes(x = Sample_Date, y = Number_of_Mutations, colour = year)) +
  geom_point()

UK_sequences_df %>%
  sample_n(1000) %>%
  ggplot(aes(x = as.Date(Sample_date), y = Number_of_mutations, colour = year)) +
  geom_point(alpha = 0.7)

#Violin plot by year
UK_sequences_df %>%
  # sample_n(5000) %>%
  ggplot(aes(x = as.factor(year), y = Number_of_mutations, fill = as.factor(year))) +
  geom_violin() +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", width = 1.2,
               linewidth = 0.3) +
  # geom_jitter(size = 0.3, alpha = 0.1) +
  ylab("Number of mutations") +
  xlab("Year") +
  scale_fill_manual(values = c("2020" = "grey", "2021" = "blue",
                               "2022" = "red", "2023" = "green",
                               "2024" = "purple")) +
  # scale_colour_manual(values = c("2020" = "grey", "2021" = "blue",
  #                                "2022" = "red", "2023" = "green",
  #                                "2024" = "purple")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 15),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")


#Box plot by year
UK_sequences_df %>%
  # sample_n(10000) %>%
  ggplot(aes(x = as.factor(year), y = Number_of_mutations, fill = as.factor(year))) +
  geom_boxplot(outlier.alpha = 0.5, coef = 0)






#Using a linear model here
#Need to check the assumptions
model <- lm(Number_of_Mutations ~ Sample_Date, data = practice_spike_df)
#summary(model)
#p-value < 2.2e-16
#Therefore, the predictor variable is statistically significant
#Sample date has an effect on the number of mutations


# Currently crashing out
# Going to limit the number of values
# num_mutations_time <- UK_sequences_df %>%
#   tail(10000)
# 
# ggplot(num_mutations_time, aes(x = Sample_date, y = Number_of_mutations)) +
#   geom_smooth()


  
#Number of mutations over time for the main data set
# ggplot(UK_sequences_df, aes(x = Sample_date, y = Number_of_mutations)) +
#   geom_smooth()
# 
# linear_UK <- lm(Number_of_mutations ~ Sample_date, data = UK_sequences_df)
# gam_UK <- gam(Number_of_mutations ~ Sample_date, data = UK_sequences_df)

