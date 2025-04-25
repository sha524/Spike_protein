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

#Having problems with UMAP
#Algorithm keeps crashing, most likely due memory problems
#Going to convert clustering_data into a sparse matrix
sparse_clustering <- as(as.matrix(clustering_data), "dgCMatrix")
sparse_clustering <- as_tibble(sparse_clustering)

#This still was not working so I took a random
#500000 sample
UMAP_sample <- clustering_data %>%
  sample_n(500000)

#Timing the umap
system.time ({
umap <- umap(UMAP_sample)
})

#Component selection


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
km <- kmeans(tsne_df, centers = 3, nstart = 10)
summary(km)

km$size
# test_k$clusters
# size of the clusters

##### Visualisation #####

#### Elbow plots visualisation ####
#PC1_PC2
#Because I forgot to set a random seed for the PCA results
#I am going to have to manually create a data frame
#containing the data points

part_a <- tibble(Clusters = 1:10,
       log_wss = c(8.2, 7.75, 6.8, 6.7, 6.65, 6.6, 5.5, 5.4, 5.38, 5.37)) %>%
  ggplot(aes(x = Clusters, y = log_wss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("log Within cluster sum of squares") +
  ylim(5, 8.5) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 3.65, y = 7.62, colour = "black", label = "Elbow") +
  geom_segment(x = 3.5, y = 7.5, xend = 3.1, yend = 7, arrow = arrow(length = unit(0.7, "cm")),
                                                                                     colour = "black",
                                                                                     size = 1.5) +
  annotate("text", x = 7.95, y = 6.12, colour = "black", label = "Elbow") +
  geom_segment(x = 7.8, y = 6, xend = 7.2, yend = 5.6, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))

#PC1:PC9
part_b <- tibble(Clusters = 1:10,
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
  annotate("text", x = 6, y = 19.4, colour = "black", label = "Elbow") +
  geom_segment(x = 6, y = 19.3, xend = 6, yend = 18.4, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))


#t-SNE elbow plot
part_c <- tibble(Clusters = 1:10,
       wcss = c(4.8e+07, 3e+07, 1.9e+07, 1.4e+07, 1.3e+07, 1.1e+07, 1e+07, 0.99e+07, 0.98e+07, 0.97e+07)) %>%
  ggplot(aes(x = Clusters, y = wcss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("Within cluster sum of squares") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 3.45, y = 3.15e+07, colour = "black", label = "Elbow") +
  geom_segment(x = 3.4, y = 3e+07, xend = 3.05, yend = 2e+07, arrow = arrow(length = unit(0.7, "cm")),
               colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))
  

#UMAP elbow plot
part_d <- tibble(Clusters = 1:10,
       wcss = wss) %>%
  ggplot(aes(x = Clusters, y = wcss)) +
  geom_point(size = 3, colour = "red") +
  geom_line(linewidth = 1) +
  xlab("Number of clusters") +
  ylab("Within cluster sum of squares") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  annotate("text", x = 3.7, y = 8e+07, colour = "black", label = "Elbow") +
  geom_segment(x = 3.4, y = 7e+07, xend = 3.1, yend = 5.4e+07, arrow = arrow(length = unit(0.7, "cm")),
                colour = "black", size = 1.5) +
  theme(panel.background = element_rect("white"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))
  


#Final plot
plot_grid(part_a, part_b, part_c, part_d, labels = c("A", "B", "C", "D"))


#### Dimensionality reduction visualisation ####

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
# fvis_cluster(km, geom = "point", data = my_pca_data)
#or
#PC1:PC2
# my_pca_data$cluster_id <- factor(km.out$cluster)
# ggplot(my_pca_data, aes(x = PC1, y = PC2, colour = cluster_id)) +
#   geom_point() +
#   scale_colour_viridis_d()
# 
#PC1:PC9
#Assigning each row to its respective cluster
my_pca_data$cluster_id <- factor(km.out$cluster)
PC1_PC2_plot <- ggplot(my_pca_data, aes(x = PC1, y = PC2, colour = cluster_id)) +
  geom_point() +
  scale_colour_viridis_d() +
  xlab("PC1") +
  ylab("PC2") +
  labs(colour = "Cluster") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
#Final plot for figure
plot_grid(PC1_PC2_plot, labels = "A")


#Graph of individuals
#was taking ages to plot
#so I am going to use ggplot
# pca_plot <- fviz_pca_ind(my_pca,
#                 axes = c(1, 2),
#                 label = "none")
# 
# ggsave("pca_plot", plot = pca_plot, width = 8, height = 6, dpi = 300)


#t-SNE
tsne_df$cluster_id <- factor(km.out$cluster)
tsne_plot <- ggplot(tsne_df, aes(x = tsne_x, y = tsne_y, colour = cluster_id)) +
  geom_point() +
  scale_colour_viridis_d() +
  xlab("t-SNE dimension 1") +
  ylab("t-SNE dimension 2") +
  labs(colour = "Cluster") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

#Final plot for figure
plot_grid(tsne_plot, labels = "B")


#Clustering plot
#Using 3 clusters
tsne_df$cluster_id <- factor(km$cluster)
tsne_3clusters <- ggplot(tsne_df, aes(x = tsne_x, y = tsne_y, colour = cluster_id)) +
  geom_point() +
  scale_colour_viridis_d() +
  xlab("t-SNE dimension 1") +
  ylab("t-SNE dimension 2") +
  labs(colour = "Cluster") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

#Final plot for figure
#Using 3 clusters
plot_grid(tsne_3clusters, labels = "A")
        


#umap
umap_df <- data.frame(umap)
umap_df$cluster_id <- factor(km.out$cluster)
umap_plot <- ggplot(umap_df, aes(x = X1, y = X2, colour = cluster_id)) +
  geom_point() +
  scale_colour_viridis_d() +
  xlab("UMAP dimension 1") +
  ylab("UMAP dimension 2") +
  labs(colour = "Cluster") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face = "bold", size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))



##### Clustering analysis #####

#Once the correct number of clusters had been identified, 3
#the clustering was analysed

#Cluster sizes
cluster_sizes <- km$size
cluster_sizes <- tibble(cluster_sizes) %>%
  mutate(cluster = 1:3) %>%
  relocate(cluster, .before = cluster_sizes)

cluster_sizes_plot <- cluster_sizes %>%
  ggplot(aes(x = cluster, y = cluster_sizes, fill = cluster)) +
  geom_col() +
  scale_fill_viridis_c() +
  xlab("Cluster") +
  ylab("Number of sequences") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 10),
        legend.position = "none")  

#Final plot for figure
plot_grid(cluster_size_plot, labels = "B")

#Kruskal-Wallis test for statistical significance
kruskal.test(cluster_sizes$cluster_sizes ~ cluster_sizes$cluster)


#Centroids
centroids <- km$centers
#Gives me the coordinates in t-SNE dimensional space
#Need to find the index of sequence that is the
#closet to the centroid of each cluster
#Going to calculate Euclidean distance between the
#centroid and all the individual points

#Storing the sequence index, same length as centroids
cluster_index <- nrow(centroids)  

#Using a nested for loop
#First for loop, loops through centroids
for (i in 1:nrow(centroids)) {
#dist is storing the Euclidean distances, same length as tsne_df
  dist <- nrow(tsne_df)
#Second for loop, loops through all tsne_df coordinates
  for (x in 1:nrow(tsne_df)) {
#Calculating the Euclidean distances between each centroid and and all the values of tsne_df    
    dist[x] <- sqrt((centroids[i, 1] - tsne_df[x, 1])^2 + (centroids[i, 2] - tsne_df[x, 2])^2)
  }
#Closet sequence to each of the 3 centroids  
  cluster_index[i] <- which.min(dist)    
}



#What mutations are present in the centroids?

#Cluster I
present_cluster_I <- colSums(clustering_data[36961, ]) == 1
which(present_cluster_I == TRUE)


#Cluster II
present_cluster_II <- colSums(clustering_data[26260, ]) == 1
which(present_cluster_II == TRUE)


#Cluster III
present_cluster_III <- colSums(clustering_data[41207, ]) == 1
which(present_cluster_III)






