################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


##### What does this R script contain #####

#Clustering

#Elbow plots to determine the number of clusters to use





##### Setup and introduction to clustering #####

#K-means clustering is an unsupervised learning technique
#Clustering involves finding homogeneous subgroups within a larger group
#What groups exist within the data


#Will take a sample of wide_combined to ensure the algorithm works
sample_wide_combined <- wide_combined %>%
  select(-Sequence_Information) %>%
  sample_n(100000) %>%
  na.omit()

clustering_data <- wide_combined %>%
  select(-Sequence_Information) %>%
  na.omit()

##### Selecting the number of clusters #####

#Going to time how long the process takes
system.time({

#Initialise total within sum of squares error
#Will hold the wss values for 1 to 15 clusters
wss <- 0

#Looping through different numbers of clusters
#For 1 to 15 clusters
for(i in 1:15) {
  km.out <- kmeans(clustering_data, centers = i, nstart = 20)
  #Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss

}
})

#Plot total within sum of squares vs number of clusters
plot(1:15, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")






