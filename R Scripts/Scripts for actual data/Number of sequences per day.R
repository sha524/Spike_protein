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
                                           pattern = )










