dbplot <- function(y, g=NULL, data=NULL, clustering_distance=NULL, jitter_amount=0.1, output=FALSE, 
                   group_names=NULL, group_positions=NULL, 
                   NA_as_group=FALSE, NA_group='Missing', NA_position=NULL, 
                   indiv_col=NULL, indiv_pch=NULL, 
                   group_col=NULL, group_pch=NULL, 
                   col=1, pch=19, 
                   show.n=FALSE, 
                   ylim=NULL, xlim=NULL, ...) {
  

if(FALSE){

g=NULL
data=NULL
clustering_distance=0.1
jitter_amount=0.1
output=FALSE 
group_names=NULL; group_positions=NULL
NA_as_group=TRUE;  NA_group='Missing' ; NA_position=NULL
indiv_col=NULL; indiv_pch=NULL
group_col=NULL; group_pch=NULL
col=1
pch=19 
show.n=TRUE
ylim=NULL
xlim=NULL

y <- rnorm(50, 10, 5)
g <- sample(c('a','b','cat'), 50, T)
y[ c(4,6,7,8)] <- NA
g[ c(4,16,17,18)] <- NA

indiv_col <- sample( c(3,5,6,7), 50, T)


}










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
  # length(group_names) must be equal to nlevels(g)
  if( length(group_names) != nlevels(g) ) stop('Length of group_names must match the number of groups. \n')
  
  # If no group_positions is provided, use 1, 2, 3, ...
  if(is.null(group_positions)) group_positions <- seq_len(nlevels(g))
  # length(unique(group_positions)) must be equal to nlevels(g)
  if( length(unique(group_positions)) != nlevels(g) ) stop('Length of unique group_positions must match the number of groups. \n')    
     
  # Handle NA in g only if there is missingness in g and NA_as_group is TRUE.
  if( all(!is.na(g)) ) NA_as_group <- FALSE
  if( NA_as_group & all(is.na(y[is.na(g)])) ){
      warning('y is all missing when g is missing. Nothing to plot in Missing group. \n')
      NA_as_group <- FALSE
  }

    # Suggest a good clustering_distance
    if(is.null(clustering_distance)) clustering_distance <- IQR(y, na.rm=TRUE) / 10

  # Handle missing values in g
  if(NA_as_group){
    levels(g) <- c(levels(g), NA_group) # Add level for Missing g
    g[is.na(g)] <- NA_group
    group_names <- levels(g)
  # NA_position must be different from group_positions.
    if(is.null(NA_position)) NA_position <- max(group_positions) + 1
    if(NA_position %in% group_positions) NA_position <- max(group_positions) + 1
    group_positions <- c(group_positions, NA_position)
  }

# colors
if(!is.null(indiv_col)){  ## If indiv_col is given.
    if(length(indiv_col) != length(y)){
        warning("Length of indiv_col is not equal to the data length. Ignoring indiv_col.")
        indiv_col <- NULL
    } else{
        COL <- indiv_col
        group_col <- NULL
        col <- NULL
    }}

if(!is.null(group_col)){  ## if group_col is given and indiv_col is not being used.
    if(length(group_col) != num_groups){
        warning("Length of group_col is not equal to the number of groups. Check 'NA_as_group'. Recycling or truncaging values.")
    } 
    group_col <- rep_len(group_col, nlevels(g))
    COL <- group_col[as.numeric(g)]    
    col <- NULL
}

if(!is.null(col)){ ## neither indiv_col nor group_col is being used.
    COL <- rep(col, length(y))
}

# pch
if(!is.null(indiv_pch)){  ## If indiv_pch is given.
    if(length(indiv_pch) != length(y)){
        warning("Length of indiv_pch is not equal to the data length. Ignoring.")
        indiv_pch <- NULL
    } else{
        PCH <- indiv_pch
        group_pch <- NULL
        pch <- NULL
    }
}

if(!is.null(group_pch)){  ## if group_pch is given and indiv_pch is not being used.
    if(length(group_pch) != num_groups){
        warning("Length of group_pch is not equal to the number of groups. Check 'NA_as_group'. Recycling or truncaging values.")
    } 
    group_pch <- rep(group_pch, length.out=num_groups)
    PCH <- group_pch[as.numeric(g)]    
    pch <- NULL
}

if(!is.null(pch)){ ## neither indiv_pch nor group_pch is being used.
    PCH <- rep(pch, length(y))
}

  dat <- data.frame(y, g, col=COL, pch=PCH)
  grouped_data <- split(dat, dat$g) 

  # Compute jittered positions
  jitter_position <- function(cluster_size, within_cluster_id) {
    -0.5 - 0.5 * cluster_size + within_cluster_id
  }
      
  grouped_data <- lapply(grouped_data, function(subdat) {
      temp <- cluster_points(subdat$y, clustering_distance=clustering_distance)
      temp$jitter_pos <- jitter_position(temp$cluster_size, temp$within_cluster_id)
      temp <- temp[order(temp$original_position), ]
      
      temp <- data.frame(subdat, temp[-1])  # Keep original structure
      row.names(temp) <- NULL
      return(temp)
  })
  
  if (is.null(ylim)) ylim <- range(y, na.rm=TRUE)
  if (is.null(xlim)) xlim <- range(group_positions)
  
  plot(0, type='n', ylim=ylim, xlim=xlim+c(-0.5, 0.5), xaxt='n')#, ...)
  
  axis(1, at=group_positions, labels=group_names, tick=FALSE, line=1)  # Group names at bottom
  
  if (show.n) {
    group_sizes <- table(g[!is.na(y)])
    group_sizes_labels <- paste("N=", group_sizes, sep="")
    mtext(group_sizes_labels, side=3, at=group_positions, line=0.5)  # N values at top
  }
  
  for(i in seq_along(group_positions)) {
      this <- grouped_data[[i]] 
      points(group_positions[i] + this$jitter_pos * jitter_amount, this$y, col=this$col, pch=this$pch)
  }

if (output) return(grouped_data)

}



# out <- dbplot(y, g, indiv_col=indiv_col, col=1, pch=19, show.n=TRUE, jitter_amount=0.1, output=TRUE)


