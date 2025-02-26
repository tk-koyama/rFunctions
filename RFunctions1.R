## ------------- ##
## R Functions 1 ##
## ------------- ##

## -------------------------------------------------------------- ##
## dbplot, kmplot, jmplot, dsplot, show.colors, prop.plot (tplot) ##
## -------------------------------------------------------------- ##

############
## dbplot ##
############

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

if(FALSE){
set.seed(4)
y <- rnorm(100)
g <- sample(c('a','b','d'), 100, TRUE)

data=NULL
clustering_distance=0
jitter_amount=NULL
output=FALSE 
group_names=NULL
group_positions=NULL
indiv_col=NULL
group_col=NULL
indiv_pch=NULL
group_pch=NULL
NA_as_group=FALSE 
NA_group_name=NULL
NA_group_position=NULL
NA_group_col=NULL
NA_group_pch=NULL 
box_col=NULL
NA_box_col=NULL 
grid_x=NULL
grid_y=NULL 
show_n=TRUE
fig_type='d' 
NA_fig_type=NULL
grid_par=NULL


extra_args <- NULL
plot_args <- NULL
points_args <- NULL
boxplot_args <- NULL



}



















## Version 1.0 (2025-02-25) ##
    ## See https://tk-koyama.github.io/rhtml/RFunctions1.html
    ## This replaces tplot() that I've used since 2010.
    ## dotplot / boxplot combinations with lots of options.
    
## Requires proximity_clustering(). See below (around line 200).

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
g <- factor(g, levels=levels(g), labels=group_names)

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
    default_jitter_width <- ifelse(max_jitter_pos>1.6, 0.4, 0.2) 
    jitter_amount <- (default_jitter_width/2)/max_jitter_pos 
    if(max_jitter_pos==0)    jitter_amount <- 0
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

## -------------------- ##
## proximity_clustering ##
## -------------------- ##
proximity_clustering <- function(y, clustering_distance) {
    # ---------- #
    # 2025-02-25 #
    #            # ------------------------------ #
    # This function is used inside of dbplot().   #
    # It may be useful as a stand-along function. #
    # ------------------------------------------- #
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
    set.seed(620)
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

############
## kmplot ##
############
kmplot <- function(km, mark=3, simple=FALSE,
    xaxis.at=pretty(km$time), xaxis.lab=xaxis.at, 
    lty.surv=1, lwd.surv=1, col.surv=1,
    lty.ci=0, lwd.ci=1, col.ci=col.surv, #By default (lty.ci=0), confidence intervals are not plotted.
    group.names=NULL, group.order=seq(length(km$n)), extra.left.margin=4, 
    label.n.at.risk=FALSE, draw.lines=TRUE, cex.axis=1,
    xlab='', ylab='', main='', xlm=c(0,max(km$time)), ylm=c(0,1),
    grid=TRUE, lty.grid=1, lwd.grid=1, col.grid=grey(.9),
    legend=!is.null(km$strata), loc.legend='topright', add=FALSE,
    returnOutput=FALSE,
    ... # ... is passed to par()
    ){
        # Version 2.5.5: 2014/5/19
        # Version 2.6.1: 2017/3/29
            # x-axis limit (xlm) can be specified to truncate the plotting region. 
            # y-axis limit (ylm) can be specified to truncate the plotting region. (Not a very good idea...)
        # Version 2.6.3: 2017/5/19 ... Fixed the error. Now the lines extend to time=0.
        #                          ... Changed the coordinates of the lines by the group names.
        # Version 2.6.4: 2018/3/22 ... loc.legend (legend location) now accepts c(x-coordinate, y-coordinate).
        # Version 2.7.1: 2018/4/17 ... Fixed a pretty serious error regarding group order.

        # km is the output from survfit() function in survival package.
        # xaxis.at specifies where 'n at risk' will be computed and printed.
        # xaxis.lab specifies what will be printed at xaixs.at.  (see example)

        # If group names are long, add extra left margin by setting extra.left.margin to something greater than 0.

        # line specifications (lty.surv, lwd.surv, col.surv) will be recycled.
        # Set lty.ci to 1 if confidence intervals are needed.
        # group.names will overwrite whatever is specified in survfit() output.
        # group.order specifies the order of groups from top in 'n at risk'.  1 is at top, 2 next, and so on.

        # if add=TRUE, then par() is not refreshed.  allows multiple panels by
        # using, e.g., par(mfrow=c(2,2)).

        ng0 <- length( km$strata ) ; ng <- max(ng0,1) 
                # When only one group...
                if(ng0==0){ km$strata <- length(km$time) ; names(km$strata) <- 'All' ; legend <- draw.lines <- FALSE } 

        lty.surv <- rep(lty.surv, ng) ; lwd.surv <- rep(lwd.surv, ng) ; col.surv <- rep(col.surv, ng)
        lty.ci <- rep(lty.ci, ng) ;     lwd.ci <- rep(lwd.ci, ng)     ; col.ci <- rep(col.ci, ng)

        ## group names and error checking       
        gr <- c(km$strata)
        if( is.null(group.names) ){ group.names <- names(km$strata) }
        if( length(unique(group.names)) != ng ){ stop('\n','length(unique(group.names)) != number of groups.') }
        if( suppressWarnings(any( sort(group.order) != 1:ng)) )
                { stop('\n', 'Something wrong with group.order.','\n','sort(group.order) must equal 1:', ng, '.') }
        group.names <- gsub(' *$', '', group.names)  #to remove unwanted white spaces in group.names.
        if(ng==1 & (group.names[1]=='group.names') ){ group.names <- 'N at risk' ; label.n.at.risk = FALSE }

        ## graphic parameters
        if(!add){
        par(list(oma=c(1,1,1,1), mar=c(4+ng,4+extra.left.margin,4,2)+.1))
                if(simple) par( mar=c(3,4,2,1)+.1 )
        par( list(...) )
        }

        ## reformat survival estimates
        dat <- data.frame(time=km$time, n.risk=km$n.risk, n.event=km$n.event, 
                          survival=km$surv, std.err=km$std.err, lower=km$lower, upper=km$upper, 
                          group=factor(rep(names(gr), gr), levels=names(gr)) )
        dat.list <- split(dat, f=dat$group)
            for(i in 1:length(dat.list)){
                dat.list[[i]] <- rbind(dat.list[[i]][1,], dat.list[[i]])
                dat.list[[i]][1,c(1,5)] <- 0
                dat.list[[i]][1,c(4,6,7)] <- 1
                dat.list[[i]][1,3] <- 99
                row.names(dat.list[[i]]) <- NULL
                }

        ## plot (but not survival curves) 
        plot(0,type='n', xlim=xlm, ylim=ylm, xaxt='n', yaxt='n', xlab='', ylab='' )
                if(grid){
                        par('xpd'=FALSE)
                        abline(v=xaxis.at,       lty=lty.grid, lwd=lwd.grid, col=col.grid )
                        abline(h=pretty(c(0,1)), lty=lty.grid, lwd=lwd.grid, col=col.grid )
                }
        axis( side=2, at=pretty(c(0,1)), cex.axis=cex.axis )    
        axis( side=1, at=xaxis.at, label=xaxis.lab, line=-0.5, tick=FALSE, cex.axis=cex.axis )
        axis( side=1, at=xaxis.at, label=rep('',length(xaxis.at)), line=0, tick=TRUE )
        title(xlab=xlab, line=1.5, adj=.5, ...) ; title(ylab=ylab, ... )

        if(!simple){
        ## write group names
        group.name.pos <- par()$usr[1] - (par()$usr[2]-par()$usr[1]) / 8 ; padding <- group.name.pos / 8
                line.pos <- (1:ng)[order(group.order)] + 2
        mtext( group.names, side=1, line=line.pos, at=group.name.pos, adj=1, col=1, las=1, cex=cex.axis )

        ## draw matching lines for n at risk.
            if(draw.lines){
                    linePad <- (par()$usr[1] - group.name.pos) * 0.2
                    linePos1 <- group.name.pos + linePad
                    linePos2 <- par()$usr[1] - linePad
                par('xpd'=TRUE)
                for(i in 1:ng){
                axis(side=1, at=c(linePos1, linePos2), labels=FALSE, line=line.pos[i]+0.6, lwd.ticks=0,
                                         col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i] ) }
        }

        ## numbers at risk
        kms <- summary(km, times=xaxis.at) ; if(is.null(kms$strata)) kms$strata <- rep(1,length(kms$time) )
        d1 <- data.frame(time = kms$time, n.risk = kms$n.risk, strata = c(kms$strata))
            d1 <- d1[ d1$time >= xlm[1] & d1$time <= xlm[2], ]
        d2 <- split(d1, f=d1$strata)

        ## Right-justifying the numbers 
        ndigits <- lapply(d2, function(x) nchar(x[,2]) )
        max.len <- max( sapply(ndigits, length) )
        L <- do.call('rbind', lapply(ndigits, function(z){ length(z) <- max.len ; z} ) )
        nd <- apply( L, 2, max, na.rm=T )
        for( i in seq(ng) ){
                this <- d2[[i]] 
                w.adj <- strwidth('0', cex=cex.axis, font=par('font')) / 2 * nd[1:nrow(this)]
                mtext( side=1, at=this$time+w.adj, text=this$n.risk, line=line.pos[i], cex=cex.axis, adj=1, col=1, las=1)
                                   }
        if(label.n.at.risk) mtext( side=1, text='N at risk', at=group.name.pos, line=1.5, adj=1, col=1, las=1, cex=cex.axis )
        } ## End of if(!simple) 

        # Legend
        rlp <- group.order
        if(legend){
                bgc <- ifelse( par('bg')=='transparent', 'white', par('bg') )
        legend(x=loc.legend[1], y=loc.legend[2], legend=group.names[rlp], col=col.surv[rlp], lty=lty.surv[rlp], lwd=lwd.surv[rlp],
                bty='o', cex=cex.axis, bg=bgc, box.col='transparent', inset=.01 )
                        }

        ## draw confidence intervals and survival curves
        par(xpd=FALSE)
        for(i in 1:ng){
                this <- dat.list[[i]]
                        x <- this$time 
                            L <- this$lower
                            U <- this$upper
                            S <- this$survival
                            n <- this$n.event
                                naL <- which( is.na(L) ) ; L[naL] <- L[naL-1] ; U[naL] <- U[naL-1]
                lines( x, L, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )
                lines( x, U, type='s', col=col.ci[i], lty=lty.ci[i], lwd=lwd.ci[i] )

                lines( x, S, type='s', col=col.surv[i], lty=lty.surv[i], lwd=lwd.surv[i])
                    #lines(km, conf.int=FALSE, col=col.surv, lty=lty.surv, lwd=lwd.surv, mark=mark, xmax=xlm[2], ymin=ylm[1] )
                points( x[n==0], S[n==0], pch=mark, col=col.surv[i])
        }
        box(bty=par('bty'))
        # par(op) 
        if(returnOutput) return(dat.list)
}

#-----------#
#- example -#
#-----------#
if(!TRUE){
library(survival)
    kma <- survfit( Surv(time, status) ~ rx + adhere, data=colon )
    km <- survfit( Surv(time, status) ~ rx, data=colon )

kmplot(kma, mark='',
        xaxis.at=c(0,.5,1:9)*365,
        xaxis.lab=c(0,.5,1:9), # n.risk.at
        lty.surv=c(1,2),
        lwd.surv=1,
        col.surv=c(1,1,2,2,4,4), # survival.curves
        lty.ci=0,
        lwd.ci=1,
        col.ci=1, # confidence intervals not plotted 
        group.names=c('Obs ','Obs tumor adh','Lev','Lev tumor adh','Lev+5FU ','Lev+5FU tumor adh'),
        group.order=c(5,3,1,6,4,2), # order of appearance in the n.risk.at table and legend.
        extra.left.margin=6,
        label.n.at.risk=FALSE,
        draw.lines=TRUE,
        cex.axis=.8,
        xlab='Years',
        ylab='Survival Probability', # labels
        grid=TRUE,
        lty.grid=1,
        lwd.grid=1,
        col.grid=grey(.9),
        legend=TRUE, loc.legend='bottomleft',
        xlm=c(365*0, 365*7),
        ylm=c(0,1),
        add=FALSE,
        simple=FALSE,
        cex.lab=.8,
        xaxs='r',
        bty='L',
        las=1,
        tcl=-.2  # other parameters passed to plot()
) 
title(main='Chemotherapy for stage B/C colon cancer', adj=.5, font.main=1, line=0.5, cex.main=1)
}

############
## jmplot ##
############
jmplot <- function(x, y, levels, names=NULL, xlim=NULL, ylim=NULL, log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL, 
                   xratio=0.8, yratio=xratio, show.n=FALSE, cex.n=NULL, ann=par("ann"), axes=TRUE, frame.plot=axes,
                   panel.first=NULL, panel.last=NULL, asp=NA, ...) {
    ## Version 1.1  2012/3/8
    ## 
    ## Now that I am using dbplot() instead of tplot, I should rewrite this.
    ##
    localTplot <- function(..., type="b", horizontal=FALSE) tplot(..., type=type, axes=FALSE, horizontal=horizontal)
    eliminateTplot <- function(func, ..., type, dist, jit, names, group.col, boxcol, boxborder, group.pch, median.line, mean.line, median.pars, mean.pars, boxplot.pars, my.gray, axes, frame.plot, add, horizontal) func(...)

    localPlot <- function(xy, ..., lwd) eliminateTplot(plot.xy, xy, "p", ...)
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(axis, ...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(box, ...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(plot.window, ...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(title, ...)

    levels <- as.factor(levels)
    xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)), log)

    # Set up defaults
    if (is.null(names)) names <- levels(levels)
    if (is.null(xlab)) xlab <- xy$xlab
    if (is.null(ylab)) ylab <- xy$ylab

    # Save plotting parameters
    pars <- par(no.readonly=TRUE)
    mar <- pars$mar
    # Set the layout
    layout(matrix(c(1,3,0,2), 2), widths=c(xratio,1-xratio), heights=c(1-yratio,yratio))
    par(mar=c(0,0,0,0), oma=c(5,5,mar[3],mar[4])+pars$oma)

    # Calculate xlim, ylim
    lim <- function(z) {
        r <- range(z, na.rm=TRUE, finite=TRUE)
        #pm <- diff(r) / 20
        #r + pm * c(-1,1)
    }
    if (is.null(xlim)) xlim <- lim(xy$x)
    if (is.null(ylim)) ylim <- lim(xy$y)

    # plot X distribution on top
    par(mar=c(0,mar[2],0,0))
    localTplot(x~levels, ylim=xlim, horizontal=TRUE, show.n=FALSE, ...)
    if (axes) localAxis(side=2, at=1:nlevels(levels), labels=names, tick=FALSE, ...)

    # plot Y distribution on right
    par(mar=c(mar[1],0,0,0))
    localTplot(y~levels, ylim=ylim, show.n=show.n, cex.n=cex.n, ...)
    if (axes) localAxis(side=1, at=1:nlevels(levels), labels=names, tick=FALSE, ...)

    # plot X-Y points
    par(mar=c(mar[1],mar[2],0,0))
    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    panel.first
    localPlot(xy, xlim=xlim, ylim=ylim, ...)
    panel.last

    # add axes
    if (axes) {
        localAxis(side=1, ...)
        localAxis(side=2, ...)
    }
    if (frame.plot) localBox(...)
    # add titles
    if (ann) {
        localTitle(sub=sub, xlab=xlab, ylab=ylab, ...)
        localTitle(main=main, outer=TRUE, ...)
    }

    # Restore plotting parameters
    par(pars)
}
#-----------#
#- example -#
#-----------#
if(!TRUE){
  set.seed(1220)
  x <- rexp(100)
  y <- rexp(100)
  levels <- as.factor(sample(c("Male","Female"), 100, TRUE))
  jmplot(x, y, levels, col=levels)
}

                          
############
## dsplot ##
############
dsplot <- function(x, ...) UseMethod("dsplot")

dsplot.default <- function(x, y, bkgr=TRUE, col=1, pch=19, cex=0.8, ...) {
    # Scatter plot for discrete data
    # Only works with 'integer-valued' data.
    #if (any(x!=round(x), na.rm=TRUE) | any(y!=round(y), na.rm=TRUE)) { stop('This only works with integers.  Sorry.', '\n') }

    L <- length(x)
    cc <- complete.cases(x, y)

    if (length(pch) < L) { pch <- rep(pch, length.out=L) }
    if (length(col) < L) { col <- rep(col, length.out=L) }
    if (length(cex) < L) { cex <- rep(cex, length.out=L) }

    x <- x[cc]
    y <- y[cc]
    pch <- pch[cc]
    col <- col[cc]
    cex <- cex[cc]

    x.levels <- sort(unique(x))
        x.levels <- c(x.levels, max(x.levels)+1)
    y.levels <- sort(unique(y))
        y.levels <- c(y.levels, max(y.levels)+1)
    tab <- table(x, y)
    max.freq <- max(tab)
    box.size <- ceiling(sqrt(max.freq))
    X <- range(x) + c(0, 1)
    Y <- range(y) + c(0, 1)
    plot(X, Y, las=1, type='n', xaxs='i', yaxs='i', bty='n', xaxt='n', yaxt='n', ...)
    axis(1, at=pretty(x)+.5, labels=pretty(x), tick=FALSE, las=1)
    axis(2, at=pretty(y)+.5, labels=pretty(y), tick=FALSE, las=1)

    if (!bkgr) {
        for (i in y.levels) { segments(min(x), i, max(x)+1, i, col=grey(.9)) }
        for (i in x.levels) { segments(i, min(y), i, max(x)+1, col=grey(.9)) }
    }

    every.other.element.x <- function(n) {
        # to make 1,n,2,n,3,n, ..., n,n vector
        c(rbind(1:n, rep(n, n)))[-(2*n)]
    }
    every.other.element.y <- function(n) {
        c(rbind(rep(n, n), 1:n))[-(2*n)]
    }

    square.coordinates <- function(box.size) {
        x.c <- y.c <- 1
        for (i in 2:box.size) { x.c <- c(x.c, every.other.element.x(i)) }
        for (j in 2:box.size) { y.c <- c(y.c, every.other.element.y(j)) }
        data.frame(x.c, y.c)
    }

    sc <- square.coordinates(box.size)
    coord <- (1:box.size) / (box.size+1)
    off.set <- coord[1]/4

    grey.scale <- rev(seq(.65, .95, length=max.freq))

    dat <- data.frame(id=1:length(x), x, y)
    dat <- dat[order(dat$x, dat$y),]
    within <- c(t(tab))
    within <- within[within > 0]
    idx <- NULL
    hm <- NULL
    for (i in within) {
        idx <- c(idx, 1:i) # index within category
        hm <- c(hm, rep(i, i))
    }
    dat$idx <- idx
    dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2 # local offset
    dat$lx <- dat$ly + ((ceiling(sqrt(hm - 1)) ** 2 == hm - 1) & (hm > 1)) / (box.size + 1) / 2
    dat <- dat[order(dat$id),]
    dat$col <- col
    dat$pch <- pch

    if (bkgr) {
        for (i in x.levels) {
            for (j in y.levels) {
                n <- sum(x==i & y==j)
                if (n > 0) { rect(i+off.set, j+off.set, i+1-off.set, j+1-off.set, border=grey(grey.scale[n]), col=grey(grey.scale[n])) }
            }
        }
    }

    points(dat$x + coord[sc[dat$idx, 1]] + dat$lx, dat$y + coord[sc[dat$idx, 2]] + dat$ly, pch=dat$pch, col=dat$col, cex=cex)

    table(factor(y, levels=rev(min(y):max(y))), factor(x, levels=min(x):max(x)))
}

dsplot.formula <- function(formula, data=parent.frame(), ..., subset) {
    if (missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")

    enquote <- function(x) { as.call(list(as.name("quote"), x)) }

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)

    args <- lapply(m$..., eval, data, parent.frame())
    nmargs <- names(args)
    if ("main" %in% nmargs) args[["main"]] <- enquote(args[["main"]])
    if ("sub" %in% nmargs) args[["sub"]] <- enquote(args[["sub"]])
    if ("xlab" %in% nmargs) args[["xlab"]] <- enquote(args[["xlab"]])
    if ("ylab" %in% nmargs) args[["ylab"]] <- enquote(args[["ylab"]])

    m$... <- NULL
    #m$na.action <- na.pass
    subset.expr <- m$subset
    m$subset <- NULL
    require(stats, quietly=TRUE) || stop("package 'stats' is missing")
    m[[1]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")

    if (!missing(subset)) {
        s <- eval(subset.expr, data, parent.frame())
        n <- nrow(mf)
        dosub <- function(x) { if (length(x) == n) x[s] else x }
        args <- lapply(args, dosub)
        mf <- mf[s,]
    }
    do.call("dsplot", c(list(mf[[response]], mf[[-response]]), args))
}

#################
## show.colors ##
#################
show.colors <- function(){
   par(mfrow=c(1,1))
   #   par(mai=c(.4,.4,.4,.4), oma=c(.2,0,0,.2))
   x <- 22 ; y <- 30
   plot(c(-1,x),c(-1,y), xlab='', ylab='', type='n', xaxt='n', yaxt='n', bty='n')
   for(i in 1:x){for(j in 1:y){
         k <- y*(i-1)+j ; co <- colors()[k]
         rect(i-1, j-1, i, j, col=co, border=grey(.5) )}}

   text(rep(-.5,y),(1:y)-.5, 1:y, cex=1.2-.016*y)
   text((1:x)-.5, rep(-.5,x), y*(0:(x-1)), cex=1.2-.022*x)
   title('col=colors()[n]')
}
##############
## propPlot ##
##############
propPlot <- function(x, TrueFalse, howManyGroups=4, cutPoints=NULL, dig.x=1, ...){
    # This plot shows the proportion of 'TRUE' in bins with equal n (unless cutPoints are specified).

    # ... is passed to plot() function.

    ownCutPoints <- !is.null(cutPoints[1])

    d <- data.frame(x, TrueFalse)
    d <- d[ complete.cases(d), ]
        x <- d$x
        TrueFalse <- d$TrueFalse

    if(!ownCutPoints){
        cox <- quantile(x, seq(0,1, by=1/howManyGroups))
        co <- as.numeric(cox)
        co[1] <- co[1] - 1
        xc <- cut(x,co)
        co[1] <- co[1] + 1
    }
    if(ownCutPoints){
        co <- sort(cutPoints)
            if(min(x)<co[1]) co <- c(min(x),co)
            if(max(x)>rev(co)[1]) co <- c(co,max(x))
    }

    xc <- cut(x,co)
    tab <- table( TrueFalse , xc)

    plot(range(co), c(0,1), type='n', xaxt='n', yaxs='i', ...)
    abline(h=(1:4)*0.2, col=grey(0.95))
        cot <- formatC(co, format='f', digits=dig.x)
    axis(1, at=co, label=cot)
    la <- paste(round(100*seq(0,1, by=1/howManyGroups)), '%', sep='')
    if(!ownCutPoints) axis(1, at=co, label=la, line=1, tick=FALSE)

    midPoints <- colMeans(rbind( rev(rev(co)[-1]), co[-1]))
    width  <- abs(midPoints-co[-1])
        p <- as.numeric(prop.table(tab,2)[2,])
    for(i in 1:length(midPoints)){
        rect(midPoints[i]-width[i]/1.3,0,midPoints[i]+width[i]/1.3,p[i], col='royalblue', border='lightblue')
        text(midPoints[i], p[i], paste(tab[2,i], ' / ', colSums(tab)[i]), pos=3, cex=0.7 )
    }
    box(bty='L')
    tab
}


###########
## tplot ##
###########
tplot <- function(x, ...) UseMethod("tplot")

tplot.default <- function(x, ..., type="d", dist=NULL, jit=NULL, names, xlim=NULL, ylim=NULL, main=NULL,
                          sub=NULL, xlab=NULL, ylab=NULL, col=NULL, group.col=FALSE, boxcol=NULL,
                          boxborder=NULL, pch=par("pch"), group.pch=FALSE, median.line=FALSE,
                          mean.line=FALSE, median.pars=list(col=par("col")), mean.pars=median.pars,
                          boxplot.pars=NULL, show.n=FALSE, cex.n=NULL, my.gray=gray(0.75), ann=par("ann"),
                          axes=TRUE, frame.plot=axes, add=FALSE, at=NULL, horizontal=FALSE, panel.first=NULL,
                          panel.last=NULL) {

## ---------- ##
## 2025-02-25 ##
##            ------------------------------ ##
## This function is no long updated or used. ##
## Use dbplot() instead.                     ##
##                                           ##
.Deprecated('dbplot')                        ##
## ----------------------------------------- ##

        ## Version 3.2.1        6/17/14

    localPoints <- function(..., tick) points(...)
    localAxis <- function(..., bg, cex, lty, lwd) axis(...)
    localBox <- function(..., bg, cex, lty, lwd, tick) box(...)
    localWindow <- function(..., bg, cex, lty, lwd, tick) plot.window(...)
    localTitle <- function(..., bg, cex, lty, lwd, tick) title(...)
    localMtext <- function(..., bg, cex, lty, lwd, tick) mtext(..., cex=cex.n)

    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)){
        attributes(args)$names != ""} else {
            logical(length(args))
    }

    groups <- if (is.list(x)){
        x} else {
            args[!namedargs]
    }

    pars <- args[namedargs]

    if ((n <- length(groups)) == 0){ stop("invalid first argument") }
    if (length(class(groups))){ groups <- unclass(groups) }
    if (!missing(names)){ attr(groups, "names") <- names } else {
        if (is.null(attr(groups, "names")))
            attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }

    ng <- length(groups) # number of groups
    l <- sapply(groups, length) # size of each group
    g <- factor(rep(1:ng, l), levels=1:ng, labels=names(groups)) # groups as.numeric
    nv <- sum(l) # total count

    if (is.null(at)) at <- 1:ng
    if (length(at) != ng)
        stop("'at' must have same length as the number of groups")

    # set y scale
    if (is.null(ylim)) {
        r <- range(groups, na.rm=TRUE, finite=TRUE)
        pm <- diff(r) / 20
        ylim <- r + pm * c(-1,1)
    }
    # set x scale
    if (is.null(xlim)) {
        if (is.null(at)) xlim <- c(0.5, ng+0.5)
        else xlim <- c(0.5, max(at)+0.5)
    }

    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- ""
    if (is.null(main)) main <- ""
    if (is.null(sub)) sub <- ""

    type <- match.arg(type, choices=c("d","db","bd","b"), several.ok=TRUE)
    # type of plot for each group
    # if ((length(type) > 1) && (length(type) != ng))
    #    warning("length of 'type' does not match the number of groups")
    type <- rep(type, length.out=ng)
    #type[l > 1000] <- "b"

    # Handle default colors
    defcols <- c(my.gray, par("col"))
    # use 50% gray for box in back, otherwise default color
    if (is.null(boxborder)){ boxborder <- defcols[2-grepl(".b", type)] }
    boxborder <- rep(boxborder, length.out=ng)
    # use 50% gray for dots in back, otherwise default color
    if (is.null(col)) {
        col <- defcols[2-grepl(".d", type)]
        group.col <- TRUE
    }

    if(!is.null(boxcol)) boxcol <- rep(boxcol, length.out=ng)
    if( is.null(boxcol)) boxcol <- rep(0,      length.out=ng)

    # Use colors by group
    if (group.col) {
         # if (length(col) != ng)
          #  warning("length of 'col' does not match the number of groups")
        g.col <- rep(col, length.out=ng)
        col <- rep(g.col, l)
    # Use colors by individual or global
    } else {
       # if((length(col) > 1) && (length(col) != nv))
       #      warning("length of 'col' does not match the number of data points")
        col <- rep(col, length.out=nv)
        g.col <- rep(1, length.out=ng)
    }

    # Use plot characters by group
    if (group.pch) {
        # if (length(pch) != ng)
         #   warning("length of 'pch' does not match the number of groups")
        pch <- rep(rep(pch, length.out=ng), l)
    # Use plot characters by individual or global
    } else {
       # if((length(pch) > 1) && (length(pch) != nv))
       #     warning("length of 'pch' does not match the number of data points")
        pch <- rep(pch, length.out=nv)
    }

    # split colors and plot characters into groups
    col <- split(col, g)
    pch <- split(pch, g)
    # remove any NAs from the data and options
    nonas <- lapply(groups, function(x) !is.na(x))
    groups <- mapply("[", groups, nonas, SIMPLIFY=FALSE)
    l <- sapply(groups, length)
    col <- mapply("[", col, nonas, SIMPLIFY=FALSE)
    pch <- mapply("[", pch, nonas, SIMPLIFY=FALSE)

    # whether or not to display a mean and median line for each group
    mean.line <- rep(mean.line, length.out=ng)
    median.line <- rep(median.line, length.out=ng)

    # set defaults for dist and jit
    if (is.null(dist) || is.na(dist)) dist <- diff(range(ylim)) / 100
    if (is.null(jit) || is.na(jit)) jit <- 0.025 * ng

    # 1 2 3 1 3 2 1 1 4 2
    # -------------------
    # 1 1 1 2 2 2 3 4 1 3
    how.many.so.far <- function(g) {
        out <- NULL
        u <- unique(g)
        for (i in 1:length(u)) {
            j <- g == u[i]
            out[which(j)] <- 1:sum(j)
        }
        out
    }
    how.many.so.far2 <- function(g) {
        ## This is slower than how.many.so.far() above...
        rank(g, ties='first') - rank(g, ties='min') + 1
    }

    # turns the values in each group into their plotting points
    grouping <- function(v, dif) {
        vs <- sort(v)
        together <- c(FALSE, diff(vs) <= dif)
        g.id <- cumsum(!together)
        g.si <- rep(x<-as.vector(table(g.id)), x)
        vg <- cbind(vs, g.id, g.si)[rank(v),]
        if (length(v)==1) vg <- as.data.frame(t(vg))
        hmsf <- how.many.so.far(vg[,2])
        data.frame(vg, hmsf)
    }
    groups <- lapply(groups, grouping, dif=dist)

    # set up new plot unless adding to existing one
    if (!add) {
        plot.new()
        if (horizontal)
            do.call("localWindow", c(list(ylim, xlim), pars))
        else
            do.call("localWindow", c(list(xlim, ylim), pars))
    }
    panel.first

    # function to compute the jittering
    jit.f2 <- function(g.si, hm.sf) { hm.sf - (g.si + 1) / 2 }

    out <- list()

    Lme <- 0.2 * c(-1, 1)
    for (i in 1:ng) {
        to.plot <- groups[[i]]
        gs <- to.plot$g.si
        hms <- to.plot$hmsf
        x <- rep(at[i], nrow(to.plot)) + jit.f2(gs, hms) * jit
        y <- to.plot$vs

        if (type[i] == "bd") { # dots behind
            boxplotout <- do.call('boxplot', c(list(x=y, at=at[i], plot=FALSE, add=FALSE, axes=FALSE, col=boxcol[i], border=boxborder[i], outline=FALSE, horizontal=horizontal), boxplot.pars))
            notoplot <- (y <= boxplotout$stats[5,]) & (y >= boxplotout$stats[1,])
            if( sum(notoplot) > 0 ){ col[[i]][notoplot] <- '#BFBFBF' }
            if (horizontal)
                do.call("localPoints", c(list(x=y, y=x, pch=pch[[i]], col=col[[i]]), pars))
            else
                do.call("localPoints", c(list(x=x, y=y, pch=pch[[i]], col=col[[i]]), pars))
        }
        if (type[i] %in% c("bd", "b")) { # boxplot in front
            boxplotout <- do.call("boxplot", c(list(x=y, at=at[i], add=TRUE, axes=FALSE, col=boxcol[i], border=boxborder[i], outline=FALSE, horizontal=horizontal), boxplot.pars))
            toplot <- (y > boxplotout$stats[5,]) | (y < boxplotout$stats[1,])
            if( sum(toplot) > 0 ){
            if( col[[i]][toplot][1] == '#BFBFBF' ) col[[i]][toplot] <- 1 }
            if (horizontal)
                do.call("localPoints", c(list(x=y[toplot], y=x[toplot], pch=pch[[i]][toplot], col=col[[i]][toplot]), pars))
            else
                do.call("localPoints", c(list(x=x[toplot], y=y[toplot], pch=pch[[i]][toplot], col=col[[i]][toplot]), pars))
        }
        if (type[i] == "db") # boxplot behind
            do.call("boxplot", c(list(x=y, at=at[i], add=TRUE, axes=FALSE, col=boxcol[i], border=boxborder[i], outline=FALSE, horizontal=horizontal), boxplot.pars))
        if (type[i] %in% c("db", "d")) { # dots in front
            if (horizontal)
                do.call("localPoints", c(list(x=y, y=x, pch=pch[[i]], col=col[[i]]), pars))
            else
                do.call("localPoints", c(list(x=x, y=y, pch=pch[[i]], col=col[[i]]), pars))
        }
        if (mean.line[i]) { # mean line
            ## for(j in 1:length(mean.pars)) mean.pars[[j]] <- rep(mean.pars[[j]], ng)
            if (horizontal)
                do.call("lines", c(list(rep(mean(y), 2), at[i]+Lme), mean.pars))
            else
                do.call("lines", c(list(at[i]+Lme, rep(mean(y), 2)), mean.pars))
        }
        if (median.line[i]) { # median line
            if (horizontal)
                do.call("lines", c(list(rep(median(y), 2), at[i]+Lme), median.pars))
            else
                do.call("lines", c(list(at[i]+Lme, rep(median(y), 2)), median.pars))
        }

        out[[i]] <- data.frame(to.plot, col=col[[i]], pch=pch[[i]])
    }
    panel.last

    # add axes
    if (axes) {
        do.call("localAxis", c(list(side=1+horizontal, at=at, labels=names), pars))
        do.call("localAxis", c(list(side=2-horizontal), pars))
    }
    # optional sample sizes
    if (show.n){
        if(is.null(cex.n)) cex.n <- 1
        do.call("localMtext", c(list(paste("n=", l, sep=""), side=3+horizontal, at=at), pars, list(xaxt='s', yaxt='s')))
        }
    # add bounding box
    if (frame.plot)
        do.call("localBox", pars)
    # add titles
    if (ann) {
        if (horizontal)
           do.call("localTitle", c(list(main=main, sub=sub, xlab=ylab, ylab=xlab), pars))
        else
           do.call("localTitle", c(list(main=main, sub=sub, xlab=xlab, ylab=ylab), pars))
    }

    names(out) <- names(groups)
    invisible(out)
}

tplot.formula <- function(formula, data=parent.frame(), ..., subset) {
    if (missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")

    enquote <- function(x) { as.call(list(as.name("quote"), x)) }

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)

    args <- lapply(m$..., eval, data, parent.frame())
    nmargs <- names(args)
    if ("main" %in% nmargs) args[["main"]] <- enquote(args[["main"]])
    if ("sub" %in% nmargs) args[["sub"]] <- enquote(args[["sub"]])
    if ("xlab" %in% nmargs) args[["xlab"]] <- enquote(args[["xlab"]])
    if ("ylab" %in% nmargs) args[["ylab"]] <- enquote(args[["ylab"]])

    m$... <- NULL
    m$na.action <- na.pass
    subset.expr <- m$subset
    m$subset <- NULL
    require(stats, quietly=TRUE) || stop("package 'stats' is missing")
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")

    ## special handling of col and pch
    n <- nrow(mf)
    # pick out these options
    group.col <- if ("group.col" %in% names(args)) args$group.col else FALSE
    group.pch <- if ("group.pch" %in% names(args)) args$group.pch else FALSE
    # reorder if necessary
    if ("col" %in% names(args) && !group.col)
        args$col <- unlist(split(rep(args$col, length.out=n), mf[-response]))
    if ("pch" %in% names(args) && !group.pch)
        args$pch <- unlist(split(rep(args$pch, length.out=n), mf[-response]))

    if (!missing(subset)) {
        s <- eval(subset.expr, data, parent.frame())
        dosub <- function(x) { if (length(x) == n) x[s] else x }
        args <- lapply(args, dosub)
        mf <- mf[s,]
    }
    do.call("tplot", c(list(split(mf[[response]], mf[-response])), args))
}

