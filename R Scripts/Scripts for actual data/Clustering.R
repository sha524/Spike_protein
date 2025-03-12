################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


###### What does this R script contain #####

#Clustering

#Elbow plots to determine the number of clusters to use





###### Setup and introduction to clustering #####

#K-means clustering is an unsupervised learning technique
#Clustering involves finding homogeneous subgroups within a larger group
#What groups exist within the data


#Will take a sample of wide_combined to ensure the algorithm works
sample_wide_combined <- wide_combined %>%
  sample_n(100) %>%
  na.omit()


###### Selecting the number of clusters #####

#Initialise total within sum of squares error
wss <- 0

#For 1 to 10 clusters
for(i in 1:15) {
  km.out <- kmeans(sample_wide_combined, centers = i, nstart = 20)
  #Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss

}
