## ----------------------------------------------- ##
## Proper Inference from Simon's Two-Stage Designs ##
## ----------------------------------------------- ##
## Please cite
## Koyama T and Chen H. ``Proper inference from Simon's two-stage designs''
## Statistics in Medicine 27(16), 2008. PMID: 17960777. PMCID: PMC6047527.
## --------------------- ##

################
## User input ##
################
# p0 ... P[Success under H0]
# p1 ... P[Success under H1]
# n1 ... Stage 1 sample size
# R1 ... Stage 1 critical value
# nT ... Final planned sample size
# RT ... Final planned critical value
# Convention is to use r1 (= R1 - 1) and rT (= RT - 1)

# x1 ... Stage 1 data
# m2 ... New stage 2 sample size (if changed)
# x2 ... Stage 2 data

#######################################
## Design and design characteristics ##
#######################################
TwoStageInf.prob <- function(p0, p1, n1, R1, nT, RT){
    ########################### 
    ########################### 
    ## Continue if X1 >= R1. ##
    ########################### 
    ########################### 
    ##  Reject if Xt >= RT.  ##
    ########################### 
    ########################### 
    ## Convention is to use r1 = R1-1 and rT = RT-1
    n2 <- nT-n1
    x1v <- 0:n1
    px0 <- dbinom(x1v, n1, p0)
    px1 <- dbinom(x1v, n1, p1)

    cont <- x1v >= R1
    R2 <- pmax(RT - x1v, 0)
    cp0 <- replace(1 - pbinom(R2 - 1, n2, p0), x1v < R1, 0)
    cp1 <- replace(1 - pbinom(R2 - 1, n2, p1), x1v < R1, 0)

    PROB <- data.frame(x1v, px0, px1, cp0, cp1)
        ## Stage 1 pmf and conditional power
    POWE <- c( sum(px0 * cp0), sum(px1 * cp1) )
        ## Unconditional type I erro rate and power
    PET  <- c( pbinom(R1-1, n1, p0), pbinom(R1-1, n1, p1) )
        ## Probability of early termination 
    EN   <- n1 + (nT-n1) * (1-PET)
        ## Expected sample size

    DESIGN <- data.frame(p0=p0, p1=p1, n1=n1, R1=R1, nT=nT, RT=RT)
    CHARA <- data.frame(POWE, PET, EN)
        row.names(CHARA) <- c('NULL','ALT')
    list(DESIGN, CHARA)
}

#######################################
## Inference from a two-stage design ##
#######################################

TwoStageInf <- function(p0, p1, n1, R1, nT, RT, x1, m2=nT-n1, x2, conf.level=0.9, two.sided=TRUE, ...){
                        ## Last update: 2021/8/5
## ----------------------------------------------- ##
## Proper Inference from Simon's Two-Stage Designs ##
## ----------------------------------------------- ##
## Please cite
## Koyama T and Chen H. ``Proper inference from Simon's two-stage designs''
## Statistics in Medicine 27(16), 2008. PMID: 17960777. PMCID: PMC6047527.
## --------------------- ##

################
## User input ##
################
# p0 ... P[Success under H0]
# p1 ... P[Success under H1]
# n1 ... Stage 1 sample size
# R1 ... Stage 1 critical value
# nT ... Final planned sample size
# RT ... Final planned critical value
# Convention is to use r1 (= R1 - 1) and rT (= RT - 1)

# x1 ... Stage 1 data
# m2 ... New stage 2 sample size (if changed)
# x2 ... Stage 2 data

#############
## p-value ##
#############
pval.st2 <- function(p, n1, R1, nT, RT, m2, x1, x2){
    ## p is p0 (because this is for p.value) unless computing confidence interval.
    ## m2 is the actual sample size for stage 2. If there is no change, use m2 = nT-n1.
    n2 <- nT - n1
    ## Continue if X1 >= R1.
    ## Convention is to use r1 = R1-1 and rT = RT-1
    x1c <- R1:n1
    x2c <- RT - x1c - 1
    R2 <- RT - x1
        d1m <- dbinom(x1c, n1, p)
    ## pi.star is such that A(x1,n2,pi.star) = conditional p.value
    cond.pval <- pbinom(x2-1, m2, p, lower.tail=FALSE)
        ## pi.star <- qbeta(cond.pval, R2, m2-R2+1) ## Gone back to original 6/16/2020
        ## pi.star <- qbeta(cond.pval, R2, n2-R2+1) ## Fixed 12/29/2019
        pi.star <- qbeta(cond.pval, max(R2,0), n2-R2+1) 
        ## p2m <- pbinom(x2c, m2, pi.star, lower.tail=FALSE)  ## Gone back to original 6/16/2020
        ## p2m <- pbinom(x2c, n2, pi.star, lower.tail=FALSE) ## Fixed 12/29/2019
        p2m <- pbinom(x2c, n2, pi.star, lower.tail=FALSE) ## Fixed 12/29/2019
    sum(d1m*p2m)
}

#########################
## Confidence interval ##
#########################
conf.int.st2 <- function(n1, R1, nT, RT, m2, x1, x2, conf.level=0.9, two.sided=TRUE){
    alp <- (1 - conf.level) / (1 + as.numeric(two.sided))
        f1 <- function(x,a,n2,R1,nT,RT,m2,x1,x2) pval.st2(x, n1, R1, nT, RT, m2, x1, x2) - a

    lower.bound <- uniroot(f1, interval=c(0,1), a=alp, n2=n2,R1=R1,nT=nT,RT=RT,m2=m2,x1=x1,x2=x2)$root
    median.estimate <- uniroot(f1, interval=c(0,1), a=0.5, n2=n2,R1=R1,nT=nT,RT=RT,m2=m2,x1=x1,x2=x2)$root
    upper.bound <- 1
    if (two.sided){
    upper.bound <- uniroot(f1, interval=c(0,1), a=1-alp, n2=n2,R1=R1,nT=nT,RT=RT,m2=m2,x1=x1,x2=x2)$root
    }

    out <- list()
    out$Conf.int <- c(lower.bound, upper.bound)
    out$median.est <- median.estimate
    out
}

##########################
## Putting all together ##
##########################
    result <- list()
        param <- TwoStageInf.prob(p0, p1, n1, R1, nT, RT)
    result$parameter <- param[[1]]
    result$character <- param[[2]]
    result$data <- data.frame(x1=x1, n2=nT-n1, m2=m2, x2=x2)
    if (x1 < R1)
    {
        result$phat <- x1/n1
        result$p.value <- 1-pbinom(x1-1, n1,p)
    }
    else
    {
        result$p.value <- pval.st2(p0,n1,R1,nT,RT,m2,x1,x2)
        conf <- conf.int.st2(n1,R1,nT,RT,m2,x1,x2, ...)
        result$conf <- conf[[1]]
        result$med.esti <- conf[[2]]
    }
    result
}

## --- ##
## END ##
## --- ##

## EXAMPLE in Koyama and Chen (2008)
# In Section 3.4 
# OPTIMAL design for p0=0.1, p1=0.3, alpha=0.05, beta=0.20
    # p0 <- 0.1 ; p1 <- 0.3
    # n1 <- 10 ; R1 <- 2 ; nT <- 29 ; RT <- 6
    # x1 <- 2 ; x2 <- 4
    # m2 <- nT-n1 ## No sample size change
TwoStageInf(p0=0.1, p1=0.3, n1=10, R1=2, nT=29, RT=6, x1=2, x2=4)

# Section 4.1 and Section 4.2
# MINIMAX design for p0=0.3, p1=0.5, alpha=0.05, beta=0.20
    # p0 <- 0.3 ; p1 <- 0.5
    # n1 <- 19 ; R1 <- 7 ; nT <- 39 ; RT <- 17
    # x1 <- 7 ; x2 <- 10
    # sample size change for stage 2
    # m2 <- 23
# With sample size change n2=20, m2=23
TwoStageInf(p0=0.3, p1=0.5, n1=19, R1=7, nT=39, RT=17, x1=7, m2=23, x2=10)

## Another example 
# p0 <- 0.05 ; p1 <- 0.25
# n1 <- 13 ; R1 <- 1 ; nT <- 20 ; RT <- 3
# x1 <- 5
# m2 <- nT - n1 ## No sample size change
# x2 <- 2
TwoStageInf(p0=0.05, p1=0.25, n1=13, R1=1, nT=20, RT=3, x1=5, m2=7, x2=2)
