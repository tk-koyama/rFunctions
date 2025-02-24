proximity_clustering <- function(y, clustering_distance) {
    if (all(is.na(y))) {
        return(data.frame(
            y = y, 
            original_position = seq_along(y), 
            cluster_id = 1, 
            cluster_size = length(y), 
            within_cluster_id = seq_along(y)
        ))
    }
    
    # Separate NA values
    na_indices <- which(is.na(y))
    non_na_indices <- which(!is.na(y))
    
    if (length(non_na_indices) == 0) {
        # If all values are NA, assign them as a single cluster
        return(data.frame(
            y = y, 
            original_position = seq_along(y), 
            cluster_id = 1, 
            cluster_size = length(y), 
            within_cluster_id = seq_along(y)
        ))
    }
    
    # Sort non-NA values while keeping track of original indices
    y_ordered <- order(y[non_na_indices])
    y_sorted <- data.frame(y = y[non_na_indices][y_ordered], original_position = non_na_indices[y_ordered])

    # Initialize each value as its own cluster
    clusters <- lapply(seq_len(nrow(y_sorted)), function(i) y_sorted[i, , drop = FALSE])

    while (length(clusters) > 1) {
        # Compute centroids efficiently
        cluster_centroids <- vapply(clusters, function(cl) mean(cl$y), numeric(1))

        # Compute distances between consecutive cluster centroids
        pairwise_distances <- diff(cluster_centroids)

        # Check if merging is possible
        if (length(pairwise_distances) == 0) break

        # Find the closest pair
        min_index <- which.min(pairwise_distances)
        min_distance <- pairwise_distances[min_index]

        # Stop if no valid merge is possible
        if (min_distance > clustering_distance) break  

        # Merge the closest clusters
        clusters[[min_index]] <- rbind(clusters[[min_index]], clusters[[min_index + 1]])

        # Remove the merged cluster
        clusters <- clusters[-(min_index + 1)]
    }

    # Convert clusters into final output
    out_list <- vector("list", length(clusters))
    for (i in seq_along(clusters)) {
        this_cluster <- clusters[[i]]
        n <- nrow(this_cluster)

        out_list[[i]] <- data.frame(
            y = this_cluster$y,
            original_position = this_cluster$original_position, 
            cluster_id = i,
            cluster_size = n,
            within_cluster_id = sample(n)  # Shuffle order if needed
        )
    }

    # Combine clustered data
    result <- do.call(rbind, out_list)

    # Handle NA values as a separate cluster with the largest cluster_id
    if (length(na_indices) > 0) {
        na_cluster <- data.frame(
            y = rep(NA, length(na_indices)),
            original_position = na_indices,
            cluster_id = max(result$cluster_id, na.rm = TRUE) + 1,
            cluster_size = length(na_indices),
            within_cluster_id = seq_along(na_indices)  # Assign within-cluster IDs sequentially
        )
        result <- rbind(result, na_cluster)
    }

    # Restore original order
    result <- result[order(result$original_position), ]
    
    return(result)
}

cluster_points <- proximity_clustering  ## alternative name (to be deleted later)
