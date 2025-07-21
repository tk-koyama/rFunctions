## ------------------ ##
##                    ##
## Summary statistics ##
##                    ##
## ------------------ ##
groupSum <- function(v, g=NULL, data=NULL, Combined=TRUE, select_stats=NULL, omit_all_NA=FALSE){
    # If data is provided, extract v and g correctly
    if (!is.null(data)) {
        v <- eval(substitute(v), data, parent.frame())
        if (!is.null(substitute(g))) g <- eval(substitute(g), data, parent.frame())
    }
    # Summarizes a numeric vector, optionally by groups.
    if (!is.null(g) && anyNA(g)) g <- addNA(g)  # Add NA level only if needed
    vg <- if (!is.null(g)) split(v, f = g) else list(Overall = v)

    summaryNA <- function(x) {
        n_miss <- sum(is.na(x))
        x <- na.omit(x)  
        n <- length(x)    
        if (n == 0) return(c(n, n_miss, rep(NA, 8)))  
        stats <- c(n, n_miss, summary(x)[c(1:3, 5, 6, 4)], sd(x), sd(x)/sqrt(n))
        if (n < 5) stats[c(4, 6)] <- NA  # Set Q1 & Q3 to NA if n < 5
        if (n < 2) stats[c(9, 10)] <- NA  # Set SD & SE to NA if n < 2
        return(stats)
    }
    sm <- as.data.frame(t(sapply(vg, summaryNA)))
    if (omit_all_NA) sm <- sm[sm[, 1] > 0, ]
    if (Combined & !is.null(g)) sm <- rbind(sm, Combined = summaryNA(v))
    
    colnames(sm) <- c("N", "Nmiss", "Min", "Q1", "Med", "Q3", "Max", 'Mean', "SD", "SE")

    if (!is.null(select_stats)){
        remove_mode <- any(grepl("^-", select_stats))  # Check if negative selection is used
        
        if (remove_mode){  
            select_stats <- sub("-", "", select_stats)  # Remove "-" prefix
            sm <- sm[, !colnames(sm) %in% select_stats, drop = FALSE]  # Remove specified stats
        } else{  
            sm <- sm[, select_stats, drop = FALSE]  # Keep only specified stats
        }
    }
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
    convert_one <- function(l) {
        if (is.character(l)) {
            time_parts <- as.numeric(unlist(strsplit(l, ":")))
            L <- length(time_parts)
            Sec <- as.numeric(substring(time_parts[L], 1, 2))
            Min <- as.numeric(time_parts[L - 1])
            Hr <- if (L == 3) time_parts[1] else 0
        } else {
            Hr <- 0
            Min <- floor(l)
            Sec <- as.integer(round(100 * (l - Min), 10))
        }
        total <- round((Hr * 3600 + Min * 60 + Sec) * multiplier)
        if (inSeconds) return(total)
        Hr <- total %/% 3600
        Min <- (total %% 3600) %/% 60
        Sec <- total %% 60
        Sec <- sprintf("%02d", Sec)
        if (Hr > 0) paste(Hr, sprintf("%02d", Min), Sec, sep=":") else paste(as.integer(Min), Sec, sep=":")
    }
    vapply(lap, convert_one, if (inSeconds) numeric(1) else character(1))
}
## -------- ##
##          ##
## -------- ##
addTime <- function(times) {
    # Adds multiple time values together.  
    # Input: Vector of times in "HH:MM:SS" or "MM:SS" as strings, or MM.SS as numerics.  
    # Output: Summed time in "HH:MM:SS" or "MM:SS" format.  
    totalSec <- sum(as.numeric(sapply(times, multTime, multiplier=1, inSeconds=TRUE)))  

    M <- totalSec %/% 60
    S <- totalSec %% 60

    return(multTime(paste(M, S, sep=':'), 1)) # Convert back to formatted time
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
        cred_level=paste0(100 * p, "%"), 
        lower=lb, median=med, upper=ub)
}
## ------------------ ##
##                    ##
## surival parameters ##
##                    ##
## ------------------ ##
ps2hr <- function(prop, Time){
    ## Conversion from proportion surviving (until T) to hazard rate ##
    ## Requires constant hazard assumption ##
    ## Prop = S(T)
    ## h = -ln(S(T))/T
    -log(prop)/Time
}
## -------- ##
##          ##
## -------- ##
ms2hr <- function(ms){
    ## Conversion from median survival to hazard rate ##
    ## Requires constant hazard assumption ##
    ps2hr(0.50, ms)
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
ps2ms <- function(prop, Time){
    ## Conversion from proportion surviving (until Time) to median survival ##
    ## Requires constant hazard assumption ##
    hr2ms(ps2hr(prop, Time))
}
## -------- ##
##          ##
## -------- ##
ms2ps <- function(ms, Time){
    ## Conversion from median survival to proportion surviving (until Time) ##
    ## Requires constant hazard assumption ##
    hr <- ms2hr(ms)
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
    afd <- function(x, fmt) as.numeric(format.Date(x, format=fmt))  # Convert to numeric to remove zero-padding
    y <- sapply(dat, afd, fmt='%Y')
    m <- sapply(dat, afd, fmt='%m')  # Converts "02" to 2, "07" to 7, etc.
    d <- sapply(dat, afd, fmt='%d')  # Converts "09" to 9, etc.
    paste(y, m, d, sep='/')
}

