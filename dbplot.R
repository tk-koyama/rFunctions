dbplot <- function(y, g=NULL, data=NULL, clustering_distance=NULL, jitter_amount=0.1, output=FALSE, 
                   group_names=NULL, group_positions=NULL, 
                   NA_as_group=FALSE, NA_group='Missing', NA_position=NULL, 
                   indiv_col=NULL, indiv_pch=NULL, 
                   group_col=NULL, group_pch=NULL, 
                   col=1, pch=19, 
                   show_n=FALSE, 
                   ylim=NULL, xlim=NULL, ...) {
  
  # Handle formula input
  if (!is.null(data) && inherits(y, "formula")) {
    mf <- model.frame(y, data, na.action = na.pass)  # Keep NA values
    g <- mf[[2]]  # Extract grouping variable
    y <- mf[[1]]  # Extract numeric vector
  }
  
  if(length(y) != length(g) && !is.null(g)) stop('y and g must have the same length.\n')

  # If no grouping variable is provided, create a single group
  if (is.null(g)) {
    g <- factor(rep(1, length(y)))
  }
  g <- as.factor(g)  # Ensure g is a factor

  # If no group_names is provided, use levels of g.
  if(is.null(group_names)) group_names <- levels(g)
  if(length(group_names) != nlevels(g)) stop('Length of group_names must match the number of groups.\n')
  
  # If no group_positions is provided, use 1, 2, 3, ...
  if(is.null(group_positions)) group_positions <- seq_len(nlevels(g))
  if(length(unique(group_positions)) != nlevels(g)) stop('Length of unique group_positions must match the number of groups.\n')    
     
  # Handle NA in g only if there is missingness in g and NA_as_group is TRUE.
  if( all(!is.na(g)) ) NA_as_group <- FALSE
  if( NA_as_group & all(is.na(y[is.na(g)])) ){
      warning('y is all missing when g is missing. Nothing to plot in Missing group.\n')
      NA_as_group <- FALSE
  }

  # Suggest a good clustering_distance
  if(is.null(clustering_distance)) clustering_distance <- IQR(y, na.rm=TRUE) / 10

  # Handle missing values in g
  if(NA_as_group){
    levels(g) <- c(levels(g), NA_group)
    g[is.na(g)] <- NA_group
    group_names <- levels(g)
    NA_position <- ifelse(is.null(NA_position) || NA_position %in% group_positions, max(group_positions) + 1, NA_position)
    group_positions <- c(group_positions, NA_position)
  }

  # Colors
  COL <- if (!is.null(indiv_col)) {
    if (length(indiv_col) != length(y)) {
      warning("indiv_col length does not match data length. Ignoring indiv_col.")
      rep(col, length(y))
    } else {
      indiv_col
    }
  } else {
    rep_len(if (!is.null(group_col)) group_col else col, nlevels(g))[as.numeric(g)]
  }

  # pch
  PCH <- if (!is.null(indiv_pch)) {
    if (length(indiv_pch) != length(y)) {
      warning("indiv_pch length does not match data length. Ignoring indiv_pch.")
      rep(pch, length(y))
    } else {
      indiv_pch
    }
  } else {
    rep_len(if (!is.null(group_pch)) group_pch else pch, nlevels(g))[as.numeric(g)]
  }

  # Prepare data
  dat <- data.frame(y, g, col=COL, pch=PCH)
  grouped_data <- split(dat, dat$g) 

  # Compute jittered positions
  jitter_position <- function(cluster_size, within_cluster_id) {
    -0.5 - 0.5 * cluster_size + within_cluster_id
  }
      
  grouped_data <- lapply(grouped_data, function(subdat) {
      clustered_data <- proximity_clustering(subdat$y, clustering_distance=clustering_distance)
      clustered_data$jitter_pos <- jitter_position(clustered_data$cluster_size, clustered_data$within_cluster_id)
      clustered_data <- clustered_data[order(clustered_data$original_position), ]
      return(cbind(subdat, clustered_data[-1]))
  })
  
  ylim <- if (is.null(ylim)) range(y, na.rm=TRUE) else ylim
  xlim <- if (is.null(xlim)) range(group_positions) else xlim
  
  plot(0, type='n', ylim=ylim, xlim=xlim+c(-0.5, 0.5), xaxt='n', ...)
  
  axis(1, at=group_positions, labels=group_names, tick=FALSE, line=1)
  
  if (show_n) {
    group_sizes <- table(g[!is.na(y)])
    group_sizes_labels <- sprintf("N=%d", group_sizes)
    mtext(group_sizes_labels, side=3, at=group_positions, line=0.5)
  }
  
  for(i in seq_along(group_positions)) {
      this <- grouped_data[[i]] 
      points(group_positions[i] + this$jitter_pos * jitter_amount, 
             this$y, 
             col=this$col, 
             pch=this$pch,
           cex = list(...)$cex %||% 1)  # Extract cex or default to 1
  }

  if (output) return(grouped_data)
}
