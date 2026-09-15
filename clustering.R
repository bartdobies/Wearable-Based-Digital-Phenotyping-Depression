################ 
# Code for performing clustering
################

library(factoextra)
library(cluster)
library(clusterSim)

# Standardizing/scaling the data frame
df_clustering = scale(df_clustering_not_scaled)

#### Looking for optimal number of clusters #### 

# 1) Silhouette plot 
optimal_clusters = fviz_nbclust(df_clustering, 
                                kmeans, 
                                method = "silhouette",
                                k.max = 10) +  
  theme_bw(base_size = 14) +
  theme(text = element_text(face = "bold")) 

optimal_clusters

# 2) Cluster validation indices: WCSS, silhouette and Davies–Bouldin index

cluster_indices <- data.frame(
  k = 2:10,
  WCSS = NA_real_,
  silhouette = NA_real_,
  davies_bouldin = NA_real_
)

for (i in seq_along(cluster_indices$k)) {
  
  k <- cluster_indices$k[i]
  
  km <- kmeans(
    df_clustering,
    centers = k,
    nstart = 100
  )
  
  # WCSS
  cluster_indices$WCSS[i] <- km$tot.withinss
  
  # Silhouette
  sil <- silhouette(km$cluster, dist(df_clustering))
  cluster_indices$silhouette[i] <- mean(sil[, "sil_width"])
  
  # Davies-Bouldin
  cluster_indices$davies_bouldin[i] <- index.DB(
    df_clustering,
    km$cluster,
    centrotypes = "centroids"
  )$DB
}

cluster_indices

optimal_centers = 3 # based on the obtained results above; adapt to your situation

#### K-means clustering #### 

set.seed(1229122954)
kmeans_result = kmeans(df_clustering, centers = optimal_centers, nstart = 30)
kmeans_result

### Exploratory ###

# K-medoids clustering
kmedoids = cluster::pam(df_clustering, k = 3, metric = 'euclidean')
kmedoids

# K-medians clustering
kmedians = Kmedians::Kmedians(X = df_clustering, nclust = 3, ninit = 35, par=FALSE)
kmedians