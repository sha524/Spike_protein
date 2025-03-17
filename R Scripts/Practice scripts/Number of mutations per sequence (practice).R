################ Research Project #################

#Going to the use the practice data
#Only the first 100 sequences


###### What does this R script contain ######

#Similar script to practice script - Date and the number of sequences per day.R

#Creating a table of sequence information and mutations for the practice data

#Counting the number of mutations per sequence

#Extracting the date using a regular expression

#All performed on the practice data

#Plotted the number of mutations per sequence



###### Set up ######

#Reading in the data
spike_mutation_data_practice <- read.table("./data/test_UK_seqs_msa_0522_spike_mutations.txt")


###### Creating the data frame ######

### Odd Rows ###
odd_rows <- seq_len(nrow(spike_mutation_data_practice)) %% 2
data_odd_rows <- spike_mutation_data_practice[odd_rows == 1, ]


### Even Rows ###
even_rows <- seq_len(nrow(spike_mutation_data_practice)) %% 2
data_even_rows <- spike_mutation_data_practice[even_rows == 0, ]


### Combining odd and even rows into a data frame ###
practice_spike_df <- data.frame(data_odd_rows, data_even_rows)

colnames(practice_spike_df)[1] <- "Sequence_Information"
colnames(practice_spike_df)[2] <- "Mutations"

### Date ###

#Use a regular expression to extract the date
practice_spike_df$Sample_Date <- str_extract(practice_spike_df$Sequence_Information,
                                             "[0-9]{4}-[0-9]{2}-[0-9]{2}")

### Number of mutations ###
practice_spike_df$Number_of_Mutations <- str_count(practice_spike_df$Mutations, "\\|") + 1


##### Plotting the number of mutations per sequence #####
ggplot(practice_spike_df, aes(x = Sequence_Information, y = Number_of_Mutations)) +
  geom_col() +
  xlab("Sequences") +
  ylab("Number of Mutations") +
  ylim(0, 50) +
  theme_classic()






