################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain ######

#Generated the main data frame to work from

#Table of sequence information and mutations

#Counting the number of mutations per sequence

#Extract the date using a regular expression

#Calculate the number of sequences per day

#Plot the number of sequences per day

#Additional more in-depth analysis:
#Includes separation of each year into separate groups
#What day had the most and least number of sequences per day


###### Set up ######

#Reading the data in
UK_sequences <- read.table("./data/UK_seqs_msa_0522_spike_mutations.txt")


###### Creating the new data frame to work from ######

#Odd rows in UK_sequences contain the sequence information data
#Even rows in UK_sequences contain the mutation data
#Going index the odd and even rows

#Odd rows
UK_odd_rows <- seq_len(nrow(UK_sequences)) %% 2
UK_data_odd_rows <- UK_sequences[UK_odd_rows == 1, ]

#Even rows
UK_even_rows <- seq_len(nrow(UK_sequences)) %% 2
UK_data_even_rows <- UK_sequences[UK_even_rows == 0, ]

#Combining the two tables to make the tibble
UK_sequences_df <- tibble(UK_data_odd_rows, UK_data_even_rows)

#Need to change the column headings
colnames(UK_sequences_df)[1] <- "Sequence_Information"
colnames(UK_sequences_df)[2] <- "Mutations"


###### Counting the number of mutations per sequence ######
UK_sequences_df$Number_of_mutations <- str_count(UK_sequences_df$Mutations, pattern = "\\|") + 1


###### Extracting the data using a regular expression ######
UK_sequences_df$Sample_date <- str_extract(UK_sequences_df$Sequence_Information,
                                           pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")



###### Dealing with NA values ######

#Shows the exact locations of the NA values
which(is.na(UK_sequences_df), arr.ind = TRUE)

#The outputs that there are lots of missing NA values
#in the Sample_date column

UK_sequences_df[38879, ]

#Output:
#hCoV-19/England/NORT-YNNTWN9/2022|EPI_ISL_11743052
#Date is not the format provided by the regular expression

#Going to remove the NA values
UK_sequences_df <- UK_sequences_df %>%
  na.omit()

###### Calculate the number of sequences per day ######

#Need to check the date first
UK_sequences_df$Sample_date <- as.Date(UK_sequences_df$Sample_date)

#Going to use the count() verb to count the number of sequences
#Going to use na.omit remove any NA values
actual_sequences_per_day <- UK_sequences_df %>%
  count(Sample_date) %>%
  na.omit()


###### Plot the number of sequences per day ######
ggplot(actual_sequences_per_day, aes(x = Sample_date, y = n, colour = Sample_date)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, linewidth = 1.5, colour = "blue") +
  geom_rect(aes(xmin = as.Date("2020-05-01"), xmax = as.Date("2020-09-01"),
            ymin = 0, ymax = 16000, fill = "red"), alpha = .005,
            colour = "yellow", linetype = "dashed") +
  geom_rect(aes(xmin = as.Date("2020-09-01"), xmax = as.Date("2020-10-01"),
                ymin = 0, ymax = 16000, fill = "blue"), alpha = 0.005,
            colour = "red", linetype = "dashed") +
  geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date("2020-11-01"),
                ymin = 0, ymax = 16000, fill = "green"), alpha = 0.005,
            colour = "blue", linetype = "dashed") +
  geom_rect(aes(xmin = as.Date("2020-11-01"), xmax = as.Date("2021-11-09"),
                ymin = 0, ymax = 16000, fill = "yellow"), alpha = 0.005,
            colour = "purple", linetype = "dashed") +
  geom_rect(aes(xmin = as.Date("2021-11-09"), xmax = as.Date("2024-12-29"),
                ymin = 0, ymax = 16000, fill = "purple"), alpha = 0.005,
            colour = "green", linetype = "dashed") +
  annotate("text", x = as.Date("2020-05-10"), y = 15810,
           label = expression(beta), size = 5, colour = "black",) +
  annotate("text", x = as.Date("2020-09-18"), y = 15810,
           label = expression(alpha), size = 5, colour = "black") +
  annotate("text", x = as.Date("2020-10-23"), y = 15810,
           label = expression(delta), size = 5, colour = "black") +
  annotate("text", x = as.Date("2021-11-01"), y = 15810,
           label = expression(gamma), size = 5, colour = "black") +
  annotate("text", x = as.Date("2024-12-20"), y = 15810,
           label = expression(omicron), size = 5, colour = "black") +
  ylab("Number of sequences per day") +
  xlab("Sample date") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(colour = "Sample date") +
  scale_fill_manual(name = "SARS-CoV-2 strain", 
                    values = c("red", "blue", "green", "yellow", "purple"))


#Using a generalised linear model (gam)
#A type of regression model for non-linear relationships
#between predictor variables and their response variables
#Regression looks at the relationship between two variables



###### More in-depth analysis of the number of sequences per day ######

#What day had the most sequences?
max_day <- actual_sequences_per_day %>%
  arrange(desc(n)) %>%
  head(1)

#What day had the least sequences?
min_day <- actual_sequences_per_day %>%
  arrange(desc(n)) %>%
  tail(1)

#What year had the most sequences
#Going to look to use a regular expression to group each year together
#2020, 2021, 2022, 2023 and 2024
#year_2020 <- tibble(str_extract_all(actual_sequences_per_day$Sample_date, pattern = "2020.*"))

colnames(year_2020)[1] <- "Year_of_sample_date"
year_2020$Year_of_sample_date <- as.Date(year_2020$Year_of_sample_date)

#Separating the sample dates by year
#Each column contains a different date
separate_years <- actual_sequences_per_day %>%
  mutate(year_2020 = str_extract(Sample_date, pattern = "2020.*")) %>%
  mutate(year_2021 = str_extract(Sample_date, pattern = "2021.*")) %>%
  mutate(year_2022 = str_extract(Sample_date, pattern = "2022.*")) %>%
  mutate(year_2023 = str_extract(Sample_date, pattern = "2023.*")) %>%
  mutate(year_2024 = str_extract(Sample_date, pattern = "2024.*"))

#Going to place each year into its on tibble
#Will then look to recombine

#year 2020
year_2020 <- separate_years %>%
  select(year_2020, n) %>%
  arrange(desc(n)) %>%
  na.omit()
  
#year 2021
year_2021 <- separate_years %>%
  select(year_2021, n) %>%
  arrange(desc(n)) %>%
  na.omit()

#year 2022
year_2022 <- separate_years %>%
  select(year_2022, n) %>%
  arrange(desc(n)) %>%
  na.omit()

#year 2023
year_2023 <- separate_years %>%
  select(year_2023, n) %>%
  arrange(desc(n)) %>%
  na.omit()

#year 2023
year_2024 <- separate_years %>%
  select(year_2024, n) %>%
  arrange(desc(n)) %>%
  na.omit()






