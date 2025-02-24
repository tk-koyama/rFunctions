cluster_points <- function(y, clustering_distance) {
    if (length(y) == 1) {
        return(data.frame(
            y = y, 
            original_position = 1, 
            cluster_id = 1, 
            cluster_size = 1, 
            within_cluster_id = 1
        ))
    }

    # Store original positions
    y_order <- cbind(y, seq_along(y))  # First column: values, Second column: original indices
    y_sorted <- y_order[order(y_order[,1]), ]  # Sort by first column (y values)

    # Initialize clusters with individual elements
    clusters <- lapply(seq_len(nrow(y_sorted)), function(i) y_sorted[i, , drop = FALSE])

    while (length(clusters) > 1) {
        # Compute centroids of clusters efficiently
        cluster_centroids <- vapply(clusters, function(cl) mean(cl[,1]), numeric(1))

        # Compute distances between consecutive cluster centroids
        pairwise_distances <- diff(cluster_centroids)

        # Find the closest pair
        min_index <- which.min(pairwise_distances)
        min_distance <- pairwise_distances[min_index]

        if (!length(min_distance)>0 || min_distance > clustering_distance) break  # Stop if no valid merge is possible

        # Merge the closest clusters
        merged_cluster <- rbind(clusters[[min_index]], clusters[[min_index + 1]])

        # Remove merged clusters and insert the new merged cluster
        clusters <- append(clusters[-c(min_index, min_index + 1)], list(merged_cluster), after = min_index - 1)
    }

    # Preallocate a list and store clusters efficiently
    set.seed(620)
    out_list <- vector("list", length(clusters))
    for (i in seq_along(clusters)) {
        this_cluster <- clusters[[i]]
        n <- nrow(this_cluster)

        out_list[[i]] <- data.frame(
            y = this_cluster[,1],
            original_position = this_cluster[,2], 
            cluster_id = i,
            cluster_size = n,
            within_cluster_id = sample(seq_len(n), n, FALSE)  # Keep random as requested
        )
    }

    # Convert list to data frame and restore original order
    out <- do.call(rbind, out_list)
    row.names(out) <- NULL
    return(out)
}

