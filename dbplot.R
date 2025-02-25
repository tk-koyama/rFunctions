
clustering_distance=NULL
jitter_amount=0.02
output=FALSE 

group_names=NULL
group_positions=NULL 

indiv_col=c(1,4)
group_col=1

indiv_pch=NULL
group_pch=21


NA_as_group=TRUE
NA_group_name='Missing'
NA_group_position=6 
NA_group_col=NULL 
NA_group_pch=NULL

box_col=NULL
NA_box_col=NULL


show_n=TRUE 
grid_x=NULL
grid_y=NULL

fig_type=c('d','b','db')
NA_fig_type='d'

data <- NULL

NNN <- 100
set.seed(30)
y <- round(rnorm(NNN, 100, 100))
    y[ sample(1:NNN, 20, TRUE)] <- NA
g <- sample(c('yui','moa','sue'), NNN, rep=TRUE)
    g[ sample(1:NNN, 20, TRUE)] <- NA
indiv_col <- sample(2:5, NNN, TRUE)
group_col 

y[1:5] <- c(472, 430, 450, 440, 430)


d <- data.frame(y,g, indiv_col)



dbplot <- function(y, g=NULL, data=NULL, clustering_distance=NULL, jitter_amount=NULL, output=FALSE, 
                   group_names=NULL, group_positions=NULL, 
                   indiv_col=NULL, group_col=NULL,
                   indiv_pch=NULL, group_pch=NULL,
                   NA_as_group=FALSE, 
                   NA_group_name=NULL, NA_group_position=NULL,
                   NA_group_col=NULL, NA_group_pch=NULL,
                   box_col=NULL, NA_box_col=NULL,
                   grid_x=NULL, grid_y=NULL,
                   show_n=TRUE, fig_type='d', NA_fig_type=NULL, grid_par=NULL, ...){
  
# Define valid arguments for each function
    valid_plot_args <- c("main", "sub", "xlab", "ylab", "xlim", "ylim", "axes", "frame.plot")
    valid_points_args <- c("cex", "bg")
    valid_boxplot_args <- c("range", "notch", "border", "boxwex")
    # valid_abline_args <- c("lwd", "lty", "col")
# Capture extra arguments
    extra_args <- list(...)
    plot_args <- extra_args[names(extra_args) %in% valid_plot_args]
    points_args <- extra_args[names(extra_args) %in% valid_points_args]
    boxplot_args <- extra_args[names(extra_args) %in% valid_boxplot_args]
    # abline_args <- list(...)[names(list(...)) %in% valid_abline_args]

    # grid_par is a list that includes col, lwd, lty, ... for grid line.
    # Defaults are col="grey80", lty=2, lwd=1

# Handle formula input
    if (inherits(y, "formula")) {
        if (is.null(data)) stop("A data frame must be provided when using a formula.")
    mf <- model.frame(y, data, na.action = na.pass)
    y <- as.numeric(mf[[1]])  # Ensure y is numeric
    g <- as.factor(mf[[2]])  # Ensure g is a factor
}
  
if(length(y) != length(g) && !is.null(g)) stop('y and g must have the same length.\n')

# If no grouping variable is provided, create a single group
if (is.null(g)) g <- factor(rep(1, length(y)))  
g <- as.factor(g)  # Ensure g is a factor

# Ensure NA_group_col and similar ones take the same value as group_col if group_col is a single value
if (length(group_col) == 1 && is.null(NA_group_col)) NA_group_col <- group_col
if (length(group_pch) == 1 && is.null(NA_group_pch)) NA_group_pch <- group_pch
if (length(box_col) == 1 && is.null(NA_box_col)) NA_box_col <- box_col
if (is.null(NA_fig_type)) NA_fig_type <- ifelse(length(fig_type)==1, fig_type, 'd')

# If no group_names is provided, use levels of g.
if(is.null(group_names)) group_names <- levels(g)
if(length(group_names) != nlevels(g)) stop('Length of group_names must match the number of groups.\n')
  
# If no group_positions is provided, use 1, 2, 3, ...
if(is.null(group_positions)) group_positions <- seq_len(nlevels(g))
if(length(unique(group_positions)) != nlevels(g)){
    warning('Length of unique group_positions does not match the number of groups. Using 1, 2, 3, ...\n')
    group_positions <- seq_len(nlevels(g))
}
  
# Replicate fig_type to cover all groups (except NA group)
fig_type <- rep(fig_type, length.out=nlevels(g))

# Assign default boxplot colors based on fig_type
box_col <- if (!is.null(box_col)) { 
    rep(box_col, length.out = nlevels(g)) 
} else { ifelse(fig_type == "db", "gray90", "gray85") }
# Assign default NA box color if necessary
if (is.null(NA_box_col)) NA_box_col <- ifelse(NA_fig_type == "db", "gray90", "gray85")
box_col <- c(box_col, NA_box_col)

# Handle NA in g only if there is missingness in g and NA_as_group is TRUE.
if( all(!is.na(g)) ) NA_as_group <- FALSE
if( NA_as_group & all(is.na(y[is.na(g)])) ){ 
    warning('y is all missing when g is missing. Nothing to plot in Missing group.\n') 
    NA_as_group <- FALSE 
}
  
# Suggest a good clustering_distance
if(is.null(clustering_distance)) clustering_distance <- IQR(y, na.rm=TRUE) / 10

# Handle missing values in g
if(is.null(NA_group_name)) NA_group_name <- 'Missing'
if(NA_as_group){ 
    levels(g) <- c(levels(g), NA_group_name)
    g[is.na(g)] <- NA_group_name
    group_names <- levels(g)
    NA_group_position <- ifelse(is.null(NA_group_position) || NA_group_position %in% group_positions, max(group_positions) + 1, NA_group_position)
    group_positions <- c(group_positions, NA_group_position) 
}
  
# Add NA_fig_type
fig_type <- c(fig_type, NA_fig_type)
if('bd' %in% fig_type) warning('\n fig_type = "bd" is not a good option. Consider "db" (dot in front of box) instead.\n')

# Colors 
COL <- if (!is.null(indiv_col)) { 
    if (length(indiv_col) != length(y)) { 
        warning("indiv_col length does not match data length. Ignoring indiv_col.") 
        rep(1, length(y))  # Default color for all if indiv_col is invalid 
    } else { indiv_col } 
} else { 
    # Assign default color to non-NA groups (1) and NA group (NA_group_col if specified) 
    col_vector <- rep_len(if (!is.null(group_col)) group_col else 1, nlevels(g))
    # If NA_group_col is specified, apply it to the NA group
    if (!is.null(NA_group_col)) col_vector[which(levels(g) == NA_group_name)] <- NA_group_col 
  col_vector[as.numeric(g)]
}

# pch 
PCH <- if (!is.null(indiv_pch)) { 
    if (length(indiv_pch) != length(y)) { 
        warning("indiv_pch length does not match data length. Ignoring indiv_pch.") 
        rep(19, length(y)) 
    } else { indiv_pch }
} else { 
    pch_vector <- rep_len(if (!is.null(group_pch)) group_pch else 19, nlevels(g)) 
    if (!is.null(NA_group_pch))  pch_vector[which(levels(g) == NA_group_name)] <- NA_group_pch 
    pch_vector[as.numeric(g)]
}

# Prepare data 
    dat <- data.frame(y, g, col=COL, pch=PCH) 
    grouped_data <- split(dat, dat$g) 
# Compute jittered positions
  jitter_position <- function(cluster_size, within_cluster_id) -0.5 - 0.5 * cluster_size + within_cluster_id

# Apply clustering algorithm #
grouped_data <- lapply(grouped_data, function(subdat) { 
        clustered_data <- proximity_clustering(subdat$y, clustering_distance=clustering_distance) 
        clustered_data$jitter_pos <- jitter_position(clustered_data$cluster_size, clustered_data$within_cluster_id) 
        clustered_data <- clustered_data[order(clustered_data$original_position), ] 
        return(cbind(subdat, clustered_data[-1])) 
                   }
)

# plot 
do.call(plot, c(list(0, type='n', 
                     ylim = if (!is.null(extra_args$ylim)) extra_args$ylim else range(y, na.rm=TRUE), 
                     xlim = if (!is.null(extra_args$xlim)) extra_args$xlim else range(group_positions) + c(-0.5, 0.5), 
                     xaxt='n'), 
                plot_args[!names(plot_args) %in% c("xlim", "ylim")]))

# grid 
if (!is.null(grid_y)) do.call(abline, c(list(h = grid_y), grid_par))
if (!is.null(grid_x)) do.call(abline, c(list(v = grid_x), grid_par))
# axis
axis(1, at=group_positions, labels=group_names, tick=FALSE, line=0)

# show_n
if (show_n) { 
    group_sizes <- table(g[!is.na(y)])
    group_sizes_labels <- sprintf("N=%d", group_sizes)
    mtext(group_sizes_labels, side=3, at=group_positions, line=0.5) 
}
  
# jitter_amount optimization
if(is.null(jitter_amount)){ 
    jps <- do.call('rbind', grouped_data) 
    max_jitter_pos <- max(jps$jitter_pos[!is.na(jps$y)]) 
    default_jitter_width <- ifelse(max_jitter_pos>1.6, 0.6, 0.2) 
    jitter_amount <- (default_jitter_width/2)/max_jitter_pos 
}

# plotting dots and/or boxplot
for(i in seq_along(group_positions)) { 
    this <- grouped_data[[i]] 
    if(fig_type[i] %in% c('b','db')){ 
            mdl <- ifelse(fig_type[i] == 'b', 3, 1)
        bpp <- do.call(boxplot, c(list(this$y, add=TRUE, at=group_positions[i], outline=FALSE, col=box_col[i], medlwd=mdl), boxplot_args))
        ol <- this[ this$y %in% bpp$out, ]
        do.call(points, c(list(group_positions[i] + ol$jitter_pos * jitter_amount, ol$y, col=ol$col, pch=ol$pch), points_args))
    } 
    if(fig_type[i] %in% c('d','db','bd')){
      do.call(points, c(list(group_positions[i] + this$jitter_pos * jitter_amount, this$y, col=this$col, pch=this$pch), points_args)) 
    }
    if(fig_type[i] %in% 'bd'){ 
        bdp <- do.call(boxplot, c(list(this$y, add=TRUE, at=group_positions[i], outline=FALSE, col=box_col[i]), boxplot_args))
        ol <- this[ this$y %in% bpp$out, ]
        do.call(points, c(list(group_positions[i] + ol$jitter_pos * jitter_amount, ol$y, col=ol$col, pch=ol$pch), points_args))
    } 
}

# Return output if requested
    default_out <- list(grouped_data, 
                        c('clustering_distance'=clustering_distance, 'jitter_amount'=jitter_amount))
if(output) return(default_out)

}



