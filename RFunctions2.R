## ------------------ ##
##                    ##
## Summary statistics ##
##                    ##
## ------------------ ##
groupSum <- function(v, g=NULL, Combined=TRUE, select_stats=NULL) {
    # Summarizes a numeric vector, optionally by groups.  
    # Input: Numeric vector `v` and optional grouping variable `g` (factor or character).  
    # Output: Summary statistics for each group, including NA as a separate group.  
    if (length(g) > 0) {
        g <- addNA(g)  # Automatically includes NA as a factor level
    }
    vg <- if (length(g) > 0) split(v, f=g) else list(Overall=v)  # Split into groups
    
    summaryNA <- function(x) {
        x <- x[!is.na(x)]  # Ensure NAs in v are removed
        c(summary(x)[1:6], sum(x), length(x), sd(x), sd(x)/sqrt(length(x)))  # Summary stats
    }
    
    # Compute summaries
    sm <- as.data.frame(t(sapply(vg, summaryNA)))
    
    # Add combined row if needed
    comb <- if (Combined & length(g) > 0) summaryNA(v) else NULL
    if (!is.null(comb)) sm <- rbind(sm, Combined=comb)
    
    # Rename columns
    colnames(sm) <- c("Min", "Q1", "Med", "Mean", "Q3", "Max", "Total", "N", "SD", "SE")
    
    # Allow user-selected summary stats
    summary_stats <- c("N", "Min", "Q1", "Med", "Q3", "Max", "Mean", "SD", "SE")
    if (!is.null(select_stats)) summary_stats <- intersect(summary_stats, select_stats)
    sm <- sm[, summary_stats, drop = FALSE]
    
    return(sm)
}
## ----------------- ##
##                   ##
## Clean REDCap data ##
##                   ##
## ----------------- ##
editRedCapData <- function(d, changeNames=TRUE) {
    ## Requires library('Hmisc')

    # Identify columns ending in '.factor'
    factor_cols <- grep("\\.factor$", names(d), value = TRUE)
    if (length(factor_cols) == 0) return(d)  # Return unchanged if no factor columns exist

    # Extract base names by removing '.factor' suffix
    base_names <- gsub("\\.factor$", "", factor_cols)

    # Replace numeric columns with their factor counterparts while preserving labels
    for (base in base_names) {
        num_col <- match(base, names(d))  # Get numeric column index
        fac_col <- match(paste0(base, ".factor"), names(d))  # Get corresponding factor column index

        if (!is.na(num_col) & !is.na(fac_col)) {  # Use `is.na()` check
            d[[num_col]] <- structure(d[[fac_col]], label = label(d[[num_col]]))  # Replace & preserve label
        }
    }
    # Remove redundant '.factor' columns
    d <- d[, !names(d) %in% factor_cols]
    # Optionally rename columns
    if (changeNames){
        names(d) <- capitalize(gsub("_+", ".", names(d)))  # Replace underscores and capitalize names
    }
    return(d)
}
## --------------- ##
##                 ##
## Running related ##
##                 ##
## --------------- ##
multTime <- function(lap, multiplier, inSeconds=FALSE) {
    # Multiplies a time value by a given factor.  
    # Input: "HH:MM:SS" or "MM:SS" as a string, or MM.SS as a numeric.  
    # Output: Scaled time in "HH:MM:SS" or "MM:SS" format.

    if (is.character(lap)) {
        time_parts <- as.numeric(unlist(strsplit(lap, ":")))
            L <- length(time_parts)
        Sec <- as.numeric(substring(time_parts[L], 1,2)) # Extract first two digits
        Min <- as.numeric(time_parts[L-1]) # Extract first two digits
        Hr <- if (length(time_parts) == 3) time_parts[1] else 0
    } else {
        Hr <- 0  # Ensure Hr is always initialized
        Min <- floor(lap)  # Extract minutes
        Sec <- as.integer(round(100 * (lap - Min), 10))  # Prevent floating-point errors
    }
    out <- round((Hr * 3600 + Min * 60 + Sec) * multiplier)  # Scale & round total seconds
    if (!inSeconds) {
        Hr  <- out %/% 3600  # Extract hours
        Min <- (out %% 3600) %/% 60  # Extract minutes
        Sec <- out %% 60  # Extract seconds
        
        # Ensure two-digit seconds formatting
        Sec <- sprintf("%02d", Sec)
        
        # Zero-pad minutes only if hours exist
        out <- if (Hr > 0) paste(Hr, sprintf("%02d", Min), Sec, sep=":") else paste(as.integer(Min), Sec, sep=":")
    }
    return(out)
}

## -------- ##
##          ##
## -------- ##
addTime <- function(times) {
    # Adds multiple time values together.  
    # Input: Vector of times in "HH:MM:SS" or "MM:SS" as strings, or MM.SS as numerics.  
    # Output: Summed time in "HH:MM:SS" or "MM:SS" format.  
    totalSec <- sum(as.numeric(sapply(times, multTime, multiplier=1, inSeconds=TRUE)))  

    # Correctly extract minutes and remaining seconds
    M <- totalSec %/% 60
    S <- totalSec %% 60

    return(multTime(paste(M, S, sep=':'), 1)) # Convert back to formatted time
}
## -------- ##
##          ##
## -------- ##
addTime <- function(times) {
    # Adds multiple time values together.  
    # Input: Vector of times in "HH:MM:SS" or "MM:SS" as strings, or MM.SS as numerics.  
    # Output: Summed time in "HH:MM:SS" or "MM:SS" format.  
    totalSec <- 0  # Initialize total seconds

    for (time in times) {
        totalSec <- totalSec + as.numeric(multTime(time, multiplier=1, inSeconds=TRUE)) 
    }
    M <- totalSec %/% 60
    S <- totalSec - 60*M
    return(multTime(paste(M,S,sep=':'),1)) # Convert back to formatted time
}
## --------------------------- ##
##                             ##
## Super simple power analysis ##
##                             ##
## --------------------------- ##
BinPower <- function(n, p0, p1, alp=0.05, two.sided=FALSE) {
    # Computes Type I error and power for a one-sample Binomial test.  
    # Input: Sample size "n", null "p0", alternative "p1", significance level "alp", and "two.sided" option.  
    # Output: Critical values ("Xcr_low", "Xcr_high"), Type I error, and statistical power.  
    # "Xcr_low" and "Xcr_high" are in the rejection region.

    Xcr_low <- Xcr_high <- NA
    if(two.sided){
        # Find Xcr_high such that P(X >= Xcr_high) >= alp/2
        Xcr_high <- qbinom(1 - alp/2, n, p0) + 1
        # Find Xcr_low such that P(X <= Xcr_low) <= alp/2
        Xcr_low <- qbinom(alp/2, n, p0) - 1

        rejection_range <- c(0:Xcr_low, Xcr_high:n)
    } else {
        # One-sided test
        if(p0 <= p1) {
            # Upper-tail test
            Xcr_high <- qbinom(1 - alp, n, p0) + 1
            rejection_range <- Xcr_high:n
        } else {
            # Lower-tail test
            Xcr_low <- qbinom(alp, n, p0) - 1
            rejection_range <- 0:Xcr_low
        }
    }
    typeI <- sum(dbinom(rejection_range, n, p0))  # Type I error
    Power <- sum(dbinom(rejection_range, n, p1))  # Statistical power

    return(data.frame(n=n, p0=p0, p1=p1, alp=alp, two.sided=two.sided, 
                      Xcr_low=Xcr_low, Xcr_high=Xcr_high, typeI=typeI, Power=Power))
}
## ----------------- ##
##                   ##
## Beta distribution ##
##                   ##
## ----------------- ##
BetaMS <- function(a,b){
    ## Compute, mean and variance a Beta distribution.
        s <- a + b
    me <- a/s
    va <- (a*b) / (s^2*(s+1))
        ss <- sqrt(va)
    data.frame(a=a,b=b, mean=me, var=va, sd=ss)
}
## -------- ##
##          ##
## -------- ##
BetaParameters <- function(m,v, PLOT=FALSE){
    ## Solving for alpha and beta in BETA distribition given mean and variance.
    a <- m^2*(1-m)/v - m
    b <- (1-m)*(m*(1-m)/v-1)
        if(PLOT) plotBetaDis(a,b)
    c(a,b)
}
## -------- ##
##          ##
## -------- ##
plotBetaDis <- function(a,b, add=FALSE, ...){
  # Plot Beta(a,b) pdf.
  p <- seq(0,1, length=1000)
  q <- dbeta(p, a,b)
  if(!add) plot(p,q, type='l', ylab='Density', yaxt='n', ...)
  if( add) lines(p,q, ...)
}
## -------- ##
##          ##
## -------- ##
BetaCI <- function(a, b, x, n, p) {
  ## 100*p % credible interval and median from Beta(a,b) and data = x/n.
    if (a <= 0 | b <= 0) stop("Both 'a' and 'b' must be greater than 0.")
    if (x < 0 | x > n) stop("'x' must be between 0 and 'n'.")
    if (p <= 0 | p >= 1) stop("'p' must be between 0 and 1.")

    pp <- (1 - p) / 2
    lb <- qbeta(pp, a+x, b+n-x)
    ub <- qbeta(1-pp, a+x, b+n-x)
    med <- qbeta(0.5, a+x, b+n-x)

    data.frame(
        prior_a=a, prior_b=b, 
        successes=x, trials=n, 
        credibility_level=paste0(100 * p, "%"), 
        lower_bound=lb, median=med, upper_bound=ub)
}
## -------------------------- ##
##                            ##
## Draw Normal/t distribution ##
##                            ##
## -------------------------- ##
shadeDist <- function(df=NA, mean=NA, sd=NA, LEFT=NA, RIGHT=NA, BETWEEN=NA, co='royalblue', 
                      shadeDensity=60, add=FALSE, yl=NA, xlim=NA, ...){
    # Plot and shade regions under t or Normal distribution.
    # Determine whether to use t-distribution or normal distribution
    if (!is.na(df) && (is.na(mean) || is.na(sd))) {
        # Use t-distribution
        dist_type <- "t"
        density_fun <- function(x) dt(x, df=df)
        quantile_fun <- function(p) qt(p, df=df)
    } else if (!is.na(mean) && !is.na(sd) && is.na(df)) {
        # Use normal distribution
        dist_type <- "normal"
        density_fun <- function(x) dnorm(x, mean=mean, sd=sd)
        quantile_fun <- function(p) qnorm(p, mean=mean, sd=sd)
    } else {
        stop("Specify either df (for t-distribution) or mean and sd (for normal distribution), but not both.")
    }

    # Set dynamic x-axis limits
    if (is.na(xlim[1])) xlim <- quantile_fun(c(0.001, 0.999))
    X <- seq(xlim[1], xlim[2], length=500)
    Y <- density_fun(X)

    # Set y-axis limits if not provided
    if (is.na(yl[1])) yl <- c(0, max(Y) * 1.05)

    # Plot the distribution curve
    if (!add) {
        plot(X, Y, type='l', yaxs='i', ylim=yl, xlim=xlim, ...)
    } else {
        lines(X, Y, ...)
    }

    # Function to shade regions under the curve
    shade_region <- function(L, R, co) {
        if (L < R) {
            X_shade <- seq(L, R, length.out=shadeDensity)
            Y_shade <- density_fun(X_shade)
            polygon(c(X_shade, rev(X_shade)), c(Y_shade, rep(0, length(X_shade))), col=adjustcolor(co, alpha.f=0.5), border=NA)
        }
    }

    # Apply shading conditions
    if (!is.na(LEFT)) shade_region(L=xlim[1], R=LEFT, co)
    if (!is.na(RIGHT)) shade_region(L=RIGHT, R=xlim[2], co)
    if (!is.na(BETWEEN[1]) && length(BETWEEN) == 2) shade_region(L=BETWEEN[1], R=BETWEEN[2], co)

    # Redraw the curve on top for clarity
    lines(X, Y, ...)
}
## ------------------ ##
##                    ##
## surival parameters ##
##                    ##
## ------------------ ##
ps2hr <- function(Prop, Time){
    ## Conversion from proportion surviving (until T) to hazard rate ##
    ## Requires constant hazard assumption ##
    ## Prop = S(T)
    ## h = -ln(S(T))/T
    -log(Prop)/Time
}
## -------- ##
##          ##
## -------- ##
ms2hr <- function(MS){
    ## Conversion from median survival to hazard rate ##
    ## Requires constant hazard assumption ##
    ps2hr(0.50, MS)
}
## -------- ##
##          ##
## -------- ##
hr2ms <- function(hr){
    ## Conversion from hazard rate to median survival ##
    ## -log(0.50)/hr
    ## Requires constant hazard assumption ##
    log(2)/hr
}
## -------- ##
##          ##
## -------- ##
ps2ms <- function(Prop, Time){
    ## Conversion from proportion surviving (until Time) to median survival ##
    ## Requires constant hazard assumption ##
    hr2ms(ps2hr(Prop, Time))
}
## -------- ##
##          ##
## -------- ##
ms2ps <- function(MS, Time){
    ## Conversion from median survival to proportion surviving (until Time) ##
    ## Requires constant hazard assumption ##
    hr <- ms2hr(MS)
    1/exp(hr*Time)
}
## -------- ##
##          ##
## -------- ##
hr2ps <- function(hr, Time){
    ## Conversion from hazard rate to proportion surviving (until Time) ##
    ## Requires constant hazard assumption ##
    1/exp(hr*Time)
}
## ---------------- ##
##                  ##
## Have a nice day! ##
##                  ##
## ---------------- ##
myKindaDay <- function(dat){
    # Format date from 2025-02-25 to 2025/2/25.
    afd <- function(x, fmt) as.numeric(format(x, format=fmt))  # Convert to numeric to remove zero-padding
    y <- sapply(dat, afd, fmt='%Y')
    m <- sapply(dat, afd, fmt='%m')  # Converts "02" to 2, "07" to 7, etc.
    d <- sapply(dat, afd, fmt='%d')  # Converts "09" to 9, etc.
    paste(y, m, d, sep='/')
}

