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
  ylab("Number of sequences per day") +
  xlab("Sample date") +
  theme_bw()








