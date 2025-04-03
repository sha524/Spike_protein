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

#Setting a seed to make the algorithm reproducible
set.seed(1234)

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

my_pca <- prcomp(clustering_data)
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
#Used to help determine the number of principle components to retain
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

#Using the distinct() function to remove duplicate values
tsne_distinct <- clustering_data %>%
  distinct()
  
#Timing the t-SNE
system.time({
tsne <- Rtsne(tsne_distinct, perplexity = 30, n_iter = 500, n_components = 2)
})

#Component selection
#The two dimensional coordinates are stored in the Y object of t-SNE 
tsne_df <- tibble(tsne_x = tsne$Y[, 1], tsne_y = tsne$Y[, 2])


#### UMAP ####
system.time ({
umap <- umap(clustering_data)
})


##### Selecting the number of clusters #####

#Performing K-means clustering
#Following each dimensionality reduction method K-means clustering was performed

#Going to time how long the process takes
system.time({

#Initialise total within sum of squares 
#Will hold the wss values for 1 to 10 clusters
wss <- 0

#Looping through different numbers of clusters
#For 1 to 10 clusters
for(i in 1:10) {
  km.out <- kmeans(tsne_df, centers = i, nstart = 10)
  #Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
})

#Plot total within sum of squares vs number of clusters
plot(1:10, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within cluster sum of squares")


#K-means clustering
km <- kmeans(my_pca_data, centers = 7, nstart = 10)
summary(km)

km$size
# test_k$clusters
# size of the clusters

##### Visualisation #####

#Elbow plots visualisation
#PC1_PC2
#Because I forgot to set a random seed for the PCA results
#I am going to have to manually create a data frame
#containing the data points

PC1_PC2 <- tibble(Clusters = 1:10,
       log_wss = c(8.2, 7.75, 6.8, 6.7, 6.65, 6.6, 5.5, 5.4, 5.38, 5.37)) %>%
  ggplot(aes(x = Clusters, y = log_wss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("log Within cluster sum of squares") +
  ylim(5, 8.5) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 3.65, y = 7.6, colour = "black", label = "Elbow") +
  geom_segment(x = 3.5, y = 7.5, xend = 3.1, yend = 7, arrow = arrow(length = unit(0.7, "cm")),
                                                                                     colour = "black",
                                                                                     size = 1.5) +
  annotate("text", x = 8, y = 6.1, colour = "black", label = "Elbow") +
  geom_segment(x = 7.8, y = 6, xend = 7.2, yend = 5.6, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))

#PC1:PC9
PC1_PC9 <- tibble(Clusters = 1:10,
       log_wss = c(19.6, 19.3, 19, 18.95, 18.55, 18.25, 18.45, 17.75, 16.7, 17.3)) %>%
  ggplot(aes(x = Clusters, y = log_wss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("log Within cluster sum of squares") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 9, y = 17.5, colour = "black", label = "Elbow") +
  geom_segment(x = 9, y = 17.4, xend = 9, yend = 16.8, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))


#t-SNE elbow plot
tsne_elbow_plot <- tibble(Clusters = 1:10,
       wcss = wss) %>%
  ggplot(aes(x = Clusters, y = wcss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("Within cluster sum of squares") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 3.5, y = 3.1e+07, colour = "black", label = "Elbow") +
  geom_segment(x = 3.4, y = 3e+07, xend = 3.1, yend = 2e+07, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))
  

#Final plot
plot_grid(PC1_PC1, PC1_PC9, tsne_elbow_plot, labels = c("A", "B", "C"))




#PCA visualisation for PCA before K-means clustering
# autoplot(my_pca_data2,
#          data = clustering_data,
#          alpha = 0.7,
#          loadings = TRUE,
#          loadings.label = TRUE,
#          loadings.colour = "black",
#          loadings.label.colour = "black",
#          loadings.label.repel = TRUE)

# autoplot(my_pca, data = clustering_data)


#PCA clustering visualisation
# fvis_cluster(km, geom = "point", data = my_pca_data)
#or
#PC1:PC2
my_pca_data$cluster_id <- factor(km.out$cluster)
ggplot(my_pca_data, aes(x = PC1, y = PC2, colour = cluster_id)) +
  geom_point() +
  scale_colour_viridis_d()

#PC1:PC9
my_pca_data2$cluster_id <- factor(km.out$cluster)
ggplot(my_pca_data2)







