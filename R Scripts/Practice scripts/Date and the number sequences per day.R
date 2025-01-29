################ Research Project #################



###### What does this R script contain ######

#Table of sequence information and mutations

#Counting the number of mutations per sequence

#Extracting the date using a regular expression

#Calculating the number of sequences per day

#Plotting the number of sequences per day

#Data used old_UK_seqs_msa_0522_spike_mutations

#Plotting the number of sequences per day (First 100 sequences)




### Information about the data ###
#This data is for mutations of the spike protein only


###### Set up ######

#Reading in the data
spike_mutation_data <- read.table("./data/old_UK_seqs_msa_0522_spike_mutations.txt")


###### Separating the data up ######
#Here I am trying to split the data up and create a data frame
#Data is split by sequence information, followed by the mutations on the next line
#spike_mutation_data2 <- strsplit(spike_mutation_data, "|", fixed = TRUE)

#Wanted to check the structure of the data
#lapply(spike_mutation_data2, paste, collapse = ",")

#Trying to create a data frame or matrix of the spike_mutation_data2
#unlist_spike <- unlist(spike_mutation_data2)


#Here I am creating two functions
#The 1st function will extract all the odd rows and add them to Sequence Information
#The 2nd function will extract all the even rows and add them to Mutations
#select_odd_rows <- function(x) {
#  x[seq(1, nrow(x), by = 2)]
# }

###### Odd rows ######

#Index the odd rows
row_odd <- seq_len(nrow(spike_mutation_data)) %% 2
#Identifying the odd rows, by creating a sequence and marking odd indices with 1
#%%2, the modulo operator, calculates the remainder of division by 2
#Modulo operator is used to calculate if there is a remainder or not
#Odd numbers remainder of 1 and even numbers have a remainder of 0
#Results in a repeating pattern of 1 and 0

#Going to extract the odd rows
data_row_odd <- spike_mutation_data[row_odd == 1, ]
#Created a logical vector, selecting only the odd rows
#The odd rows are all the sequence information


###### Even rows ######
row_even <- seq_len(nrow(spike_mutation_data)) %% 2
data_row_even <- spike_mutation_data[row_even == 0, ]
#The even rows are the mutations for each sequence


##### Number of mutations #####
spike_df$num_mutations <- str_count(spike_df$Mutations, "\\|") + 1
#\\| used to split the mutation data up
#The backslash is used as \ and | are special characters in regular expressions
#Want | to be taken literally



###### Table of sequence information and mutations ######

#Creating a data frame
spike_df <- data.frame(data_row_odd, data_row_even)

#Need to change the column names
colnames(spike_df)[1] <- "Sequence_Information"
colnames(spike_df)[2] <- "Mutations"


###### Extracting the date  ######

#Using the str_extract function from the stringr package to extract the sample date
#"[0-9]{4}-[0-9]{2}-[0-9]{2}", a regular expression, a sequence of characters that define
#a search pattern
#Regular expressions are used to match, search, extract or replace patterns in strings
spike_df$dates <- str_extract(spike_df$Sequence_Information, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
colnames(spike_df)[3] <- "Sample_Date"

###### Number of sequences per day ######

#Need to check that the sample date is in the correct format
spike_df$Sample_Date <- as.Date(spike_df$Sample_Date)

#Count the number of sequences per day
sequences_per_day2 <- spike_df %>%
  group_by(Sample_Date) %>%
  summarise(num_sequences = n_distinct(Sequence_Information))
  

### Plotting the number of sequences per day ###

ggplot(sequences_per_day2, aes(x = Sample_Date, y = num_sequences, colour = Sample_Date)) +
  geom_point() +
  xlab("Sample Date") +
  ylab("Number of Sequences") +
  theme_classic()

#Want to check a specific date
#2020-01-29 has 1 sequence
#result <- spike_df %>%
#  filter(Sample_Date == "2020-01-29")
#print(result)
#2020-01-29 only has 1 sequence on that day


###### Number of mutations per sequence ######

#Number of mutations per sequence was counted earlier
#Now need to make a plot of the number of mutations per sequence

# ggplot(spike_df, aes(x = Sequence_Information, y = num_mutations)) +
#    geom_point()



###### Number of sequences per day for the first 100 sequences ######

#Going to use the practice_spike_df data
view(practice_spike_df)

#Check the sample date is in the correct format
practice_spike_df$Sample_Date <- as.Date(practice_spike_df$Sample_Date)

#Count the number of sequences per day
sequences_per_day3 <- practice_spike_df %>%
  group_by(Sample_Date) %>%
  summarise(num_sequences = n_distinct(Sequence_Information)) %>%
  view()

#Plot the number of sequences per day
ggplot(sequences_per_day3, aes(x = Sample_Date, y = num_sequences, colour = Sample_Date)) +
  geom_point() +
  ylab("Number of sequences") +
  xlab("Sample date") +
  theme_classic()













