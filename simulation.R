################ 
# Code for performing simulations aimed to find the optimal combination of metrics 
# that separate the clusters the most
################

library(dplyr)
library(purrr)
library(cluster)

# Candidate variables (ensure PHQ9 isn't included)
candidates <- setdiff(top_20_variables, 'PHQ9_score')
  # top_20_variables is a vector containing names of the 20 most correlated metrics with the outcome
  #phq9_var contains the name of the outcome

# Generate all combinations of pairs and triples from candidates
combinations <- list(
  pairs   = combn(candidates, 2, simplify = FALSE),
  triples = combn(candidates, 3, simplify = FALSE)
)

####### Correlation calculation

# Precompute absolute Spearman correlations with PHQ-9
phq9_abs_cor <- sapply(candidates, function(v) { 
  
  abs(cor(
    df[['PHQ9_score']], 
    df[[v]],
    method = "spearman",
    use = "complete.obs"
  ))
})

names(phq9_abs_cor) <- candidates

####### Function for simulation

# Prune highly correlated variables within each combination,
# keeping only the variable more strongly associated with PHQ-9

prune_high_correlation <- function(vars, threshold = 0.4) {
    
  repeat {
    
    if (length(vars) <= 1) break
    
    cor_matrix <- cor(
      df %>%
        dplyr::select(dplyr::all_of(vars)),
      method = "spearman",
      use = "complete.obs"
    )
    
    # Find highly correlated pairs (Spearman's rho > 0.4)
    high_corr <- which(
      abs(cor_matrix) > threshold & 
        lower.tri(cor_matrix),
      arr.ind = TRUE
    )
    
    if (nrow(high_corr) == 0) break
    
    # Take first highly correlated pair
    i <- high_corr[1, 1]
    j <- high_corr[1, 2]
    
    v1 <- vars[i]
    v2 <- vars[j]
    
    # Remove the variable less strongly associated with PHQ-9
    remove <- ifelse(
      phq9_abs_cor[v1] >= phq9_abs_cor[v2],
      v2,
      v1
    )
    
    vars <- setdiff(vars, remove)
  }
  
  return(vars)
}


####### Function for clustering

run_clustering_simulation <- function(vars, centers) {
  df_clustering_not_scaled <- df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::filter(if_all(everything(), ~ !is.na(.)))
  
  # Ensure enough rows for clustering
  if (nrow(df_clustering_not_scaled) < centers) {
    return(data.frame(
      variables_used = paste(vars, collapse = ", "),
      centers = centers,
      performance = NA_real_,
      silhouette = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  # Scale data
  df_clustering <- scale(df_clustering_not_scaled)
  
  # Run k-means
  kmeans_result <- kmeans(df_clustering, centers = centers, nstart = 30)
  
  # Metrics (performance indicates the cluster separation)
  performance <- kmeans_result$betweenss / kmeans_result$totss
  sil <- mean(silhouette(kmeans_result$cluster, dist(df_clustering))[, 3])
  
  data.frame(
    variables_used = paste(vars, collapse = ", "),
    centers = centers,
    performance = performance,
    silhouette = sil,
    stringsAsFactors = FALSE
  )
}

####### Generate results keeping clustering solutions with at least 2 metrics

results <- combinations %>%
  flatten() %>%
  map(prune_high_correlation) %>%
  keep(~ length(.x) >= 2) %>%
  map_dfr(function(vars) {
    map_dfr(2:3, ~ run_clustering_simulation(vars, .x))
  })
