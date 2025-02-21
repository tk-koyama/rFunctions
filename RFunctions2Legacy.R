## -------- ##
##          ##
## -------- ##
marginTab <- function(tab){
    colS <- colSums(tab)
    rowS <- rowSums(tab)
    gran <- sum(tab)
        Total <- c(rowS,gran)
        rbind(cbind(tab, colS), Total)
}
## -------- ##
##          ##
## -------- ##
TplusT <- function(p){
    pUp <- dbinom(0,3,p) + dbinom(1,3,p)*dbinom(0,3,p)
    pStay <- dbinom(1,3,p) * dbinom(1,3,p)
    pDown <- sum(dbinom(2:3,3,p)) + dbinom(1,3,p)*sum(dbinom(2:3,3,p))
    c(pUp, pStay, pDown)
}
## -------- ##
##          ##
## -------- ##
OneSampleNormalSs <- function(d0, d1, sig, alp, bet) (-qnorm(alp)-qnorm(bet))^2 * (sig^2) / ((d1-d0)^2)
## -------- ##
##          ##
## -------- ##
## Conversion of sd and quartiles assuming normal
qr2sd <- function(med, qr, LOG=FALSE){
    ## Given median and quartiles, find mean and sd.
    ## log transform if LOG=TRUE.
    mq <- as.numeric(c(qr[1],med,qr[2]))
    sd13 <- abs(mq[c(1,3)]-mq[2])/qnorm(0.75)
        # out <- c(sd1=sd13[1], sd3=sd13[2], sd=mean(sd13))
        out <- c(sd=mean(sd13))
    if(LOG){
        Lmq <- log(mq)
        sd13 <- abs(Lmq[c(1,3)]-Lmq[2])/qnorm(0.75)
        v <- mean(sd13^2)
        m <- Lmq[2]
        vL <- exp(2*m+v)*(exp(v)-1) ## Something is wrong. Check back
        out <- c(sd=sqrt(vL))
        out <- 'Something is wrong with my code. Fix it.'
    }
    out
}
## -------- ##
##          ##
## -------- ##
qr2sd.sim <- function(med, qr, B=1000){
    ## Fit an exponential distribution.
    lambda.med <- log(2)/med
    lambda.qua <- -log(1-c(0.25,0.75)) / qr
    lambda <- mean(c(lambda.med,lambda.qua))
    rexp(B, lambda)
}
## -------- ##
##          ##
## -------- ##
kn <- function(x){
    out <- knit(x)
    system(paste('pdflatex', out))
}
## -------- ##
##          ##
## -------- ##
logMain <- function(mn, mx, base){
    lg <- seq(floor(log(mn, base)), ceiling(log(mx, base)))
    or <- base^lg
    data.frame(lg,or,la=rep('',length(lg)))
}
## -------- ##
##          ##
## -------- ##
logBetween <- function(mn, mx, base){
    # starting point is 10^exponent
    lo <- c(floor(log(mn, base)), ceiling(log(mx, base)))
    sp <- seq(min(lo), max(lo), by=1)
        bs <- as.list(base^sp)
        or <- unlist(lapply(bs, function(x) cumsum(rep(x,9))[-1]))
        lg <- log(or, base)
        data.frame(lg,or,la=rep('',length(lg)))
}
## -------- ##
##          ##
## -------- ##
oneKlap <- function(laps, lapDist, runKeeper=TRUE){
    ## lapDist in meter. If 300m, put 300.
    ## Vandy's inside track is 300m lap.
    ## lapDist must be a multiple of 50.

    ## "laps" is in min.sec format. 
    ## if runKeeper, the output will be formated for it.

    int.part <- floor(laps)
        lapsInSec <- 60*int.part + 100*(laps-int.part)
    mul <- lapDist / 50
    every50 <- rep( lapsInSec/mul, each=mul)

        hmkm <- floor(length(every50) / 20) ## 20 * 50m = 1000m
        hm50 <- hmkm*1000/50
            remainDist <- 50*(length(every50) - hmkm*1000/50) / 1000
            remainT <- every50[(hm50+1):length(every50)]

        km <- matrix(every50[1:hm50], nrow=20, byrow=FALSE)
        kmsss <- round(colSums(km))

            sec2min <- function(s){
                MIN <- floor(s/60)
                SEC <- s-60*MIN
            MIN + SEC/100
            }

        everyKM <- sec2min(kmsss)
        remainTime <- sec2min(sum(remainT))

        out <- list(KM=everyKM, Remainkm=remainDist, RemainTime=remainTime)

        if(runKeeper){
            hmkm <- length(out$KM)
            rmkm <- out$Remainkm
            rmkmPerkm <- multTime(out$RemainTime, 1/rmkm, fmt=FALSE)
                rr <- !is.na(rmkmPerkm)
                    pc <- out$KM
                    if(rr) pc <- c(out$KM, rmkmPerkm)
                    pc <- multTime(pc,1)
            lap <- data.frame(PACE=pc)
                v <- 1:nrow(lap)
            rownames(lap) <- paste(formatC(v,format='f', digit=0, width=2), 'km ')
        out <- lap
        }
        out
}
## -------- ##
##          ##
## -------- ##
FormatSum <- function(sumReverse, round.proportion=0, round.numeric=1, saveCSV=FALSE, csvFileName=''){
    ## Version 1.1 5/4/2015
    ## Version 1.2 6/23/2017
            ## round.numeric is not implemented or not working...
    ## Version 1.3 12/1/2017
            ## Fixed round.numeric
    
    ## sumReverse is summary with method='reverse'.
    sum.cat <- function(outStats, round.proportion, round.numeric, var.name, pv, group.names){
        # Categorical
        # outStats is sumReverse$stats[[i]]
        o <- outStats
            nm <- dimnames(outStats)[[2]]
            l <- length(group.names)
        if( ncol(outStats) != l){
            o <- matrix(0, ncol=l, nrow=nrow(outStats))
            # row.names(o) <- gsub('-', '--', row.names(outStats))
            for(k in 1:ncol(outStats)){
                o[, which(group.names == nm[k]) ] <- outStats[,k]
                }
                }
            pt <- prop.table(o, margin=2)
                pp <- ifelse( any(pt>0.999, na.rm=TRUE), 3, 2)
                pp <- ifelse( round.proportion==0, pp, pp+1+round.proportion)
        P <- formatC( 100*c(pt), digits=round.proportion, width=pp, format='f')
            nn <- max(nchar(c(o)))
        N <- formatC( c(o), width=nn)
        o <- matrix(paste(N, ' (', P, '%)', sep=''), nrow=nrow(o))
            o[, !group.names %in% nm] <- rep('', nrow(o))
        o <- cbind(var.name, gsub('-', '--', row.names(outStats)), o, pv)
        if(nrow(o) == 2) o <- o[2,]
        if(!is.null(nrow(o))){
            o[,1] <- c( var.name, rep('', nrow(o)-1))
            o[,ncol(o)] <- c(pv, rep('', nrow(o)-1))
        }
        o
    }

    sum.num <- function(outStats, quant, round.numeric, var.name, pv, group.names){
        # Numerical
        # outStats is sumReverse$stats[[i]]
        # quant is sumReverse$quant
        o <- outStats
            l <- length(group.names)
        if( nrow(outStats) != l){
            o <- matrix(0, ncol=ncol(outStats), nrow=l)
                row.names(o) <- group.names
                for(k in 1:nrow(outStats)){
                    o[ which(row.names(o) == row.names(outStats)[k]),] <- outStats[k,]
                    }
            }
        med <- formatC( as.numeric( o[, quant==0.5 ]), digits=round.numeric, format='f')#, width=wdt)
        loq <- formatC( as.numeric( o[, quant==0.25]), digits=round.numeric, format='f')#, width=wdt)
        upq <- formatC( as.numeric( o[, quant==0.75]), digits=round.numeric, format='f')#, width=wdt)
        num <- paste(med, ' (', loq, ', ', upq, ')', sep='')
            num[! row.names(o) %in% row.names(outStats) ] <- ''
        c(var.name, '', num, pv)
    }

        args2 <- list(round.proportion, sumReverse$quant)
        num.dig <- c(round.proportion, round.numeric)
        group.names <- names(sumReverse$group.freq)

        if(!is.null(sumReverse$testresults)){
                pv <- sapply(sumReverse$testresults, function(x) x$P)
            pval <- paste('P=', formatC(pv, format='f', digits=3), sep='')
            pval[ pv<0.001 ] <- 'P<0.001'
            } else {
                pval <- rep(' ', length(sumReverse$stats) )
            }

    ta <- NULL

    for(i in 1:length(sumReverse$stats)){
        this.type <- sumReverse$type[i]
        this.fun <- list(sum.cat, sum.num)[[this.type]]
            ta0 <- this.fun(sumReverse$stats[[i]], args2[[this.type]], num.dig[this.type], sumReverse$labels[i], pval[i], group.names)
        ta <- rbind(ta, ta0)
    }

    ta <- rbind( c('', '', paste('(N=', as.numeric(sumReverse$group.freq), ')', sep=''), ''), ta)
    tad <- data.frame(ta, row.names=NULL)
    names(tad) <- c(' ', ' ', names(sumReverse$group.freq), 'P.value')
            
    if(saveCSV){
        csvFileName <- ifelse(csvFileName=='', 
            paste(paste(sample(LETTERS, 5, rep=TRUE), collapse=''), '.csv', sep=''), 
            csvFileName)
        write.csv(tad, csvFileName, row.names=FALSE)
        cat('File name:', csvFileName, '\n')
    }
    if(!saveCSV) return(tad)
}
