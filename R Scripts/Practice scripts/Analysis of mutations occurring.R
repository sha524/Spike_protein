################ Research Project #################

#Going to the use the practice data
#Only the first 100 sequences

###### What does this R script contain ######

#Counting the frequency of the different mutations

#Plotted the top 20 most common mutations

#Separating the non-synonymous mutations and the synonymous mutations




###### Data setup ###### 

#Selection of only the Mutation data
#Making the data a vector, so str_split can be used to split the data
list_mutations <- as.vector(practice_spike_df$Mutations)
#colnames(most_common_mutations)[1] <- "Mutations"



#Separating the data by the delimiter |
list_mutations <- str_split(list_mutations, pattern = "\\|",
                                   n = Inf, simplify = FALSE)


#Converted to a list_mutations to a matrix first
#Then converted to a data frame, as unnest needs a data frame to work
list_mutations <- as.matrix(list_mutations)
colnames(list_mutations)[1] <- "Mutations"
list_mutations <- data.frame(list_mutations)


###### Counting the frequency of the different mutations ###### 
most_common_mutations <- list_mutations %>%
  unnest(Mutations) %>%
  count(Mutations) %>%
  arrange(desc(n))

#Changing the column name n
colnames(most_common_mutations)[2] <- "Frequency_of_the_mutations"


#Making the histogram in base R
hist(most_common_mutations$Frequency_of_the_mutations, breaks = 40)

#ggplot histogram
ggplot(most_common_mutations, aes(x = Frequency_of_the_mutations)) +
  geom_histogram() +
  xlab("Frequency of the mutations") +
  ylab("Frequency Density") +
  theme_bw()


#Plot of the 20 most common mutations
twenty_most_common_mutations <- data.frame(most_common_mutations[1:20, 1:2])

ggplot(twenty_most_common_mutations, aes(x = Mutations, y = Frequency_of_the_mutations)) +
  geom_col() +
  xlab("Mutations") +
  ylab("Frequency of the mutations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))
  
#theme(axis.text.x = element_text(angle = 90,
#                                 vjust = 0.5,
#                                 hjust = 0.5))
#Avoiding overlapping labels on the x-axis
#Using the theme() function to customise the plot
#theme_bw etc... is a pre-made theme



###### Nonsynonymous vs Synonymous mutations ######

#Still looking at the first 100 sequences data
#Going to separate the nonsynonymous mutations into one tibble
#and the synonymous mutations into another tibble

#Creating a tibble containing two columns non vs syn
non_vs_syn <- most_common_mutations %>%
    mutate(non = str_extract(Mutations, pattern = "non.*")) %>%
    mutate(syn = str_extract(Mutations, pattern = "syn.*")) %>%
    select(non, syn)

#Create a tibble for each column, non and syn
#Going to remove all the missing values (NA)
non <- non_vs_syn %>%
    select(non) %>%
    filter(!is.na(non))
#filter(!is.na(non)) removes all missing values (NA)
#is.na() checks each element of the specified column for missing values
#! turns all TRUE to FALSE and all FALSE to TRUE
#filter only returns TRUE values


#Tibble for syn now
syn <- non_vs_syn %>%
  select(syn) %>%
  filter(!is.na(syn))











  