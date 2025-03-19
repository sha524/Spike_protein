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
violin_plot <- UK_sequences_df %>%
  # sample_n(5000) %>%
  ggplot(aes(x = as.factor(year), y = Number_of_mutations, fill = as.factor(year))) +
  geom_violin() +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", width = 1,
               linewidth = 0.45) +
  # geom_jitter(size = 0.3, alpha = 0.1) +
  ylab("Number of mutations per sequence") +
  xlab("Year") +
  scale_fill_manual(values = c("2020" = "grey", "2021" = "blue",
                               "2022" = "red", "2023" = "green",
                               "2024" = "purple")) +
  # scale_colour_manual(values = c("2020" = "grey", "2021" = "blue",
  #                                "2022" = "red", "2023" = "green",
  #                                "2024" = "purple")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))



# #Box plot by year
# UK_sequences_df %>%
#   # sample_n(10000) %>%
#   ggplot(aes(x = as.factor(year), y = Number_of_mutations, fill = as.factor(year))) +
#   geom_boxplot(outlier.alpha = 0.5, coef = 0)




###### Statistics #####

#Going to use a significance level of 0.05

#Summarising the data set
summary_mutations_UK <- UK_sequences_df %>%
  group_by(year) %>%
  summarise(mean_mutations = mean(Number_of_mutations),
            median_mutations = median(Number_of_mutations),
            max_mutations = max(Number_of_mutations),
            min_mutations = min(Number_of_mutations),
            var_mutations = var(Number_of_mutations))

#Plotting the median values
median_values <- ggplot(summary_mutations_UK, aes(x = year, y = median_mutations)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "dodgerblue") +
  xlab("Year") +
  ylab("Median number of mutations per sequence") +
  ylim(0, 80) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))


##### Final figure #####
plot_grid(violin_plot, median_values, labels = c("A", "B"))



##### Statistics #####

#Question:    Does the number of mutations
#             per sequence increase with each year?


#Hypothesis testing:    H0: Mean number of mutations per sequence over time is the same
#(ANOVA)                H1: Mean number of mutations per sequence is not the same
                      
#Hypothesis testing:    H0: Median number of mutations per sequence over time is the same
#(Kruskal-Wallis)       H1: Median number of mutations per sequence over time is not the same 


#Answer: Can reject the H0 hypothesis, the median number of mutations over time
#is not the same

#Using a Spearman's rank correlation to test for correlation
#The data is not normally distributed
UK_sequences_df %>%
  select(Number_of_mutations, year) %>%
  arrange(year) %>%
  summarise(correlation = cor(year, Number_of_mutations, method = "spearman"))
#correlation value of 0.895 


### ANOVA model ###
#Does 
#Going to use an one-way ANOVA test
mod_mutations <- UK_sequences_df %>%
  select(Number_of_mutations, year) %>%
  arrange(year) %>%
  mutate(year = as.factor(year)) %>%
  aov(Number_of_mutations ~ year, data = .)
summary(mod_mutations)
#Reporting the result:
#There was a significant effect of the year on the number of mutations per sequence
#(ANOVA: F = 9451290; d.f. = 4, p = 2e-16)

#Checking the assumptions
#Testing for normality
hist(mod_mutations$residuals)
shapiro.test(sample(residuals(mod_mutations), 5000))
#Having to take a random sample of 5000 values, as
#shapiro-wilks test has a p-value of 2e-16 < 0.05
#residuals are not normally distributed
plot(mod_mutations, which = 1)

#An ANOVA test is not the correct statistical test to use here
#This is because the data does not follow a normal distribution


### Kruskal Wallis test ###
kruskal_mod <- UK_sequences_df %>%
  select(Number_of_mutations, year) %>%
  arrange(year) %>%
  mutate(year = as.factor(year))
kruskal.test(kruskal_mod$Number_of_mutations ~ kruskal_mod$year)
#Reporting the the results:
#There was a significant difference in the number of mutations per sequence
#compared to each year (Kruskal-Wallis: χ2 = 1558575, d.f. = 4, p = 2.2e-16)


#Need to work out which years actually differ
#Post-hoc test needed, kruskalmc()
kruskalmc(kruskal_mod$Number_of_mutations, kruskal_mod$year, probs = 0.05)
#Post-hoc analysis showed that the number of mutations per sequence,
#is significantly different between each year





###### Statistics (Regression Modelling) #####


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

