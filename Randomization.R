## -------------- ##
## Randomization  ##
## -------------- ##

## -------------------------------------------------------------------------- ##
## Block randomization (block_rand) and Stratified randomization (strat_rand) ##
## -------------------------------------------------------------------------- ##

################
## block_rand ##
################
block_rand <- function(n, Groups=c('A', 'B'), prob=c(0.2, 0.7, 0.1), allocation=NA, seed=NA){

## Verion 1.0 (2025-04-01) ##
    # Block Randomization with Custom Allocation Ratios
    # n: Total number of assignments needed
    # Groups: Treatment group names (e.g., c('Control', 'Treatment'))
    # prob: Probabilities for block sizes 1G, 2G, 3G where G is sum(allocation)
    # allocation: Allocation ratios (e.g., c(2,1,1) for 2:1:1 ratio; NA for equal allocation)
    # seed: Random seed for reproducibility (NA for random seed)
    
    # Input validation
    if (identical(allocation, NA)) allocation <- rep(1,length(Groups))
    if (length(Groups) != length(allocation)) stop("Groups and allocation vectors must have equal length")
    
    # Set random seed
    if (identical(seed, NA)) seed <- sample(1:10000, 1)
    set.seed(seed)
    
    # Calculate total allocation units
    G <- sum(allocation)
    block_sizes <- NULL
    while(sum(block_sizes) < n){
        block_sizes <- c(block_sizes, sample(G*(1:3), 1, prob=prob/sum(prob)))
    }
    
    # Generate assignments
    assignments <- character(0)
    for (i in block_sizes) {
        assignments <- c(assignments, sample(rep(Groups, allocation*(i/G))))
    }
    
    # Return results
    list(seed=seed, block_sizes=block_sizes, assignments=assignments)
}

################
## strat_rand ##
################
strat_rand <- function(stratum_size, Groups=c('A','B'), prob=c(0.2, 0.7, 0.1), allocation=NA, seed=NA, ...){

## Verion 1.0 (2025-04-01) ##
## Requires block_rand()   ##
   
  # Stratified Block Randomization
  # stratum_size: Sample size PER STRATUM (each subgroup gets this many subjects)
  # Groups: Treatment group names (e.g., c('Control','Treatment'))
  # prob: Probabilities for block sizes 1G, 2G, 3G (G = length(Group))
  # allocation: Allocation ratios (e.g., c(2,1,1) for 2:1:1 ratio; NA for equal allocation)
  # seed: Random seed for reproducibility (NA for random seed)
  # ---------------------------------------------------------------------------- #
  # ... Stratification variables as named vectors (e.g., Hospital=c('V','X','Y') #
  # ---------------------------------------------------------------------------- #
    if(identical(seed, NA)) seed <- sample(1e4, 1)
    
    strata <- if(...length()) list(...) else NULL  # if (length(list(...)) > 0)
        if(is.null(strata)) return(block_rand(stratum_size, Groups, prob, allocation=allocation, seed))
    
    grid <- expand.grid(strata, stringsAsFactors=FALSE)
        set.seed(seed)
        all_seeds <- sample(1e4, nrow(grid))
    # Initialize empty list to store results
    all_assignments <- stratum_info <- list()
    
    # Process each stratum separately 
    for (i in 1:nrow(grid)) { 
        # Create randomization for current stratum 
        wi_block <- block_rand(n=stratum_size, Groups=Groups, prob=prob, allocation=allocation, seed=all_seeds[i])
        # Store stratum metadata 
        stratum_info[[i]] <- list(seed=wi_block$seed, 
                                  block_sizes=wi_block$block_sizes, 
                                  stratum=grid[i,, drop=FALSE])
        # Create assignment data frame 
        all_assignments[[i]] <- data.frame(group=wi_block$assignments, grid[i,, drop=FALSE], row.names = NULL) 
    } 
    
    # output
    list(seed = seed, 
         assignments = do.call(rbind, all_assignments),
         strata = stratum_info)
}
