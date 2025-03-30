################ Research Project #################

#Going to use the actual data
#UK_seqs_msa_0522_spike_mutations.txt


##### What does this R script contain #####

#Dimensional reduction
#Two methods used:
#Principle component analysis (PCA)
#t-distributed stochastic neighbor embedding (t-SNE)

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

##### Dimensional reduction #####

#### PCA ####
#Going to perform a dimensionality reduction on the clustering data
#1000 most common mutations

my_pca <- prcomp(clustering_data,
                scale = TRUE)
summary(my_pca)

#Scree plot
#Percentage from PCA
#Proportion of variance for a scree plot

#Variability of each principle component
pr.var <- my_pca$sdev^2

#Variance explained by each principle component
pve <- pr.var/sum(pr.var)

#Plot variance explained for each principle component
plot(pve, xlab = "Principle Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


#Plotting the cumulative proportion of variance explained
cumulative_pve <- cumsum(pve)
plot(cumulative_pve, xlab = "Principle Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Selecting the PC1 and PC2
my_pca_data <- data.frame(my_pca$x[ , 1:2])
head(my_pca_data)

#Selecting PC1 to PC9
#Looking the cumulative proportion of variance explained
#can see that PC1 to PC2 explain most of the variance
my_pca_data2 <- data.frame(my_pca$x[ , 1:9])
head(my_pca_data2)

#### t-SNE ####







##### Selecting the number of clusters #####

#Performing K-means clustering

#Going to time how long the process takes
system.time({

#Initialise total within sum of squares error
#Will hold the wss values for 1 to 15 clusters
wss <- 0

#Looping through different numbers of clusters
#For 1 to 10 clusters
for(i in 1:10) {
  km.out <- kmeans(my_pca_data2, centers = i, nstart = 10)
  #Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
})

#Plot total within sum of squares vs number of clusters
plot(1:10, log(wss), type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")


#K-means clustering
km <- kmeans(my_pca_data, centers = 3, nstart = 10)
summary(km)

# test_k$clusters
# size of the clusters

##### Visualisation #####

#PCA visualisation for PCA before K-means clustering
# autoplot(my_pca_data2,
#          data = clustering_data,
#          alpha = 0.7,
#          loadings = TRUE,
#          loadings.label = TRUE,
#          loadings.colour = "black",
#          loadings.label.colour = "black",
#          loadings.label.repel = TRUE)


#PCA clustering visualisation


