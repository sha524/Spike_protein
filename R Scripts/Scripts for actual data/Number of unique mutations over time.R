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

##### Plot of all the data #####

#Will first take a random sample just to test
novel_plot <- UK_sequences_df %>%
#Taking a random sample of 5000 values
# sample_n(5000) %>%
  select(Sequence_Information, Mutations, Sample_date, year) %>%
  separate_longer_delim(Mutations, delim = "|") %>%
#distinct() function used to keep the first time a unique
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
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  scale_colour_manual(name = "year",
                    values = c("2020" = "red",
                               "2021" = "blue",
                               "2022" = "green",
                               "2023" = "yellow",
                               "2024" = "purple"))
  


###### Novel mutations ######

#How many unique mutations are there?
unique_mutations <- UK_sequences_df %>%
  select(Mutations) %>%
#Separating the mutations by |
  separate_longer_delim(Mutations, delim = "|") %>%
#Removing duplicates
  distinct(Mutations) %>%
#Using count and arrange to ensure there are no repeats
  count(Mutations) %>%
  arrange(desc(n))


#What date had the most number of novel mutations?
#2021-12-11

#First created a data frame to work from
#novel_mutations contains the sample date and the number of novel mutations for that day
novel_mutations <- UK_sequences_df %>%
  select(Mutations, Sample_date, year) %>%
  separate_longer_delim(Mutations, delim = "|") %>%
  distinct(Mutations, .keep_all = TRUE) %>%
  group_by(Sample_date, year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


#Mean and median number of novel mutations for a day
summary_novel_mutations <- novel_mutations %>%
  ungroup() %>%
  select(count) %>%
  summarise(mean = mean(count),
            median = median(count))
#Large discrepancies between the mean and median values

#Mean and median number of novel mutations for each year
summary_year_novel <- novel_mutations %>%
  group_by(year) %>%
  summarise(mean = mean(count),
            median = median(count))
#Large discrepancies between the mean and median values


##### Visualisation of the median values #####
median_novel_plot <- summary_year_novel %>%
  na.omit() %>%
  ggplot(aes(x = as.factor(year), y = median, fill = as.factor(year))) +
  geom_col() +
  xlab("Year") +
  ylab("Median number of novel mutations") +
  scale_fill_manual(values = c("2020" = "red", "2021" = "blue", "2022" = "green",
                               "2023" = "yellow", "2024" = "purple")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face= "bold", size = 14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 10),
        legend.position = "none",
        plot.margin = unit(c(2, 2, 2, 2), "cm"))


##### Final figure #####
plot_grid(novel_plot, median_novel_plot, ncol = 2,
          labels = c("A", "B"))




##### Statistical tests ###### 

#Question:  Does the number of novel mutations
#           change over time


#Hypothesis testing:    H0: Mean number of novel mutations per year is the same
#(ANOVA)                H1: Mean number of novel mutations per year is not the same

#Answer:

novel_mod <- UK_sequences_df %>%
  #Selecting the Mutations, Sample_date and year columns
  select(Mutations, Sample_date, year) %>%
  #Getting the individual
  separate_longer_delim(Mutations, delim = "|") %>%
  #Keeping only the first occurence of the mutation
  distinct(Mutations, .keep_all = TRUE) %>%
  arrange(year) %>%
  mutate(year = as.factor(year)) %>%
  group_by(year, Sample_date) %>%
  #Counting the number of novel mutations on a date
  summarise(count = n()) %>%
  aov(count ~ year, data = .)
summary(novel_mod)

#Reporting the resukt:
#There was a significant effect of the sample year on the number of novel mutations
#for that date (ANOVA: F = 40.62, d.f. = 4, p < 2e-16)

#Need to check the assumptions of an ANOVA model
#Checking for normality
hist(novel_mod$residuals)
#Residuals do not look normally distributed
shapiro.test(novel_mod$residuals)
#p-value< 2e-16
#Not normally distributed
#Need to use a non-parametric test


#Kruskal-Wallis test
#Performed as above for the ANOVA test
Kruskal_novel <- UK_sequences_df %>%
  select(Mutations, Sample_date, year) %>%
  separate_longer_delim(Mutations, delim = "|") %>%
  distinct(Mutations, .keep_all = TRUE)






