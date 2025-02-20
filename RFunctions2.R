## -------- ##
##          ##
## -------- ##
editRedCapData <- function(d, changeNames=TRUE){
    ## Do d <- data ; rm(data) first.
    ## Don't forget the .r file that redcap creates; it has rm(list=ls()) in it!

    fac <- unlist(strsplit(names(d)[grep('factor', names(d))], '.factor'))
        for(i in fac){
            nu <- which( names(d) == i)
            fa <- which( names(d) == paste(i,'factor',sep='.') )
            la <- label(d[,nu])
            d[,nu] <- d[,fa]
            label(d[,nu]) <- la
            names(d[,nu]) 
        }
    d <- d[, -grep('factor', names(d))]
        if(changeNames){ names(d) <- capitalize(gsub('_+', '.', names(d))) }
    d
}
## -------- ##
##          ##
## -------- ##
groupSum <- function(v,g=NULL, Combined=TRUE, Test=FALSE, np=FALSE, MissingAsGroup=FALSE, r=2){
    vg <- list(v)
    if(Test) Combined <- TRUE
    if(length(g) > 0) vg <- split(v, f=g)
        summaryNA <- function(x){
            x <- x[ !is.na(x) ]
            sx <- summary(x)[1:6]
            tot <- sum(x)
            L <- length(x)
            s <- sd(x)
            se <- s/sqrt(L)
            c(sx,tot,L,s,se)
        }
    sm <- data.frame(do.call('rbind', lapply(vg, summaryNA)))
        if(Combined & length(g)>0){
            comb <- summaryNA(v)
            sm <- rbind(sm,comb)
            row.names(sm)[nrow(sm)] <- 'Combined'
        }
    names(sm) <- c('Min','Q1','Med','Mean','Q3','Max','Total','N','SD','SE')
    sm <- subset(sm, select=c(N,Min,Q1,Med,Q3,Max,Mean,SD,SE))
    out <- round(data.frame(sm), r)
    pv <- NA
    if(Test){
        if(length(vg)==2){
            if(!np) pv <- c(NA,NA, t.test(vg[[1]], vg[[2]])$p.value)
            if( np) pv <- suppressWarnings( c(NA,NA, wilcox.test(vg[[1]], vg[[2]])$p.value) )
        out <- cbind(out, pv)
        } else {
        warning("I don't want to do a test with group size != 2.")
        }
    }
    round(data.frame(out), r)
}
## -------- ##
##          ##
## -------- ##
propPlot <- function(x, TrueFalse, howManyGroups=4, cutPoints=NULL){
    # x is continuous.
    # TrueFalse
    
    if(is.null(cutPoints[1])) usr <- FALSE

    TrueFalse <- TrueFalse[!is.na(x)]
    x <- x[!is.na(x)]

    if(is.null(cutPoints[1])){
        cox <- quantile(x, seq(0,1, by=1/howManyGroups))
        co <- as.numeric(cox)
        co[1] <- co[1] - 1
        xc <- cut(x,co)
        co[1] <- co[1] + 1
    }
    if(!is.null(cutPoints[1])) co <- cutPoints
        xc <- cut(x,co)

    tab <- table( TrueFalse , xc)

    plot(range(x), c(0,1), xlab='', ylab='', type='n', xaxt='n', yaxs='i')
        # segments( min(x),0,min(x),1, col=grey(.9))
        # segments( max(x),0,max(x),1, col=grey(.9))
        # for(i in 1:(length(co)-2)) segments(co[i],0,co[i],1, col=grey(.9))
    abline(h=(1:4)*0.2, col=grey(0.95))
        cot <- formatC(co, format='f', digits=2)
    axis(1, at=co, label=cot)
    if(is.null(cutPoints[1])) axis(1, at=co, label=names(cox), line=1, tick=FALSE)

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
## -------- ##
##          ##
## -------- ##
multTime <- function(lap, multiplier, fmt=TRUE){ ## xxx
    # lap is in 'Min.Sec' format. "8.47" means 8 minutes 47 seconds.
    Min <- floor(lap) ## interger portion.
    Sec <- (lap - Min) * 100
        m <- Min + Sec/60 ## in decimal MINUTES.
    MM <- m * multiplier
        MIN <- floor(round(MM*100)/100)
        SEC <- as.integer(round( 60*(MM - MIN)))
        out <- MIN+round(SEC/100,2)
        if(fmt) out <- paste(MIN, formatC(SEC, format='f', digit=0, width=2, flag=0), sep=':')
    out
}
## -------- ##
##          ##
## -------- ##
addTime <- function(laps, fmt=TRUE){
    # laps is in 'Min.Sec' format. "8.47" means 8 minutes 47 seconds.
    min <- floor(laps)
    sec <- (laps-min) * 100
        MIN <- sum(min)
        SEC <- sum(sec)
        secM <- SEC %/% 60
        secS <- round(SEC %% 60)
    multTime(MIN+secM+secS/100, 1, fmt)
}
## -------- ##
##          ##
## -------- ##
BinPower <- function(n, p0, p1, alp=0.05, r=TRUE){
    # one sample Binomial test.
    # one-sided test

    if(p0 < p1){
    Xcr <- qbinom(1-alp, n, p0)+1
    # Reject if X is Xcr or larger.
    typeI <- sum( dbinom(Xcr:n, n, p0) )
    Power <- sum( dbinom(Xcr:n, n, p1) )
    }
    if(p0 > p1){
    Xcr <- qbinom(alp, n, p0)-1
    # Reject if X is Xcr or smaller.
    typeI <- sum( dbinom(0:Xcr, n, p0) )
    Power <- sum( dbinom(0:Xcr, n, p1) )
    }
    if(r){
        typeI <- round(typeI, 4)
        Power <- round(Power, 4)
    }
    data.frame(n, p0, p1, alp, Xcr, typeI, Power)
    }
## -------- ##
##          ##
## -------- ##
BetaMV <- function(a,b){
    ## Compute, mean, variance, mode of a Beta distribution.
        s <- a + b
    me <- a/s
    va <- (a*b) / (s^2*(s+1))
        ss <- sqrt(va)
    mo <- (a-1) / (a-1+b-1)
        if( any(c(a,b) <= 1) ) mo <- NA
    data.frame(a=a,b=b, mean=me, var=va, sd=ss, mode=mo)
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
BetaCI <- function(a,b, x,n, p){
  ## 100*p % credible interval and median from Beta(a,b).
  ## and data = x/n.
  pp <- (1-p)/2
  lb <- qbeta(pp, a+x,b+n-x)
  ub <- qbeta(1-pp, a+x,b+n-x)
  med <- qbeta(0.5, a+x, b+n-x)
  data.frame(prior.a=a, prior.b=b, x=x, n=n, pr=100*p, lb, med, ub)
}
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
shadeNormalDis <- function(m,s, LEFT=NA, RIGHT=NA, BETWEEN=NA, co='red', shadeDensity=60, add=FALSE, yl=NA, ...){
    X <- seq(m-4*s, m+4*s, length=500)
    Y <- dnorm(X, mean=m, sd=s)
        if(is.na(yl[1])) yl <- c(0,max(Y)*1.05)
    if(!add){
    plot(X, Y, type='l', yaxs='i', ylim=yl, ...)
    } else {
        lines(X, Y, type='l', ...)
    }
        shade <- function(L,R, m,s, co){
            X <- seq(L,R, by=s*8/max(60,shadeDensity))
            Y <- dnorm(X, mean=m, sd=s)
            for(i in seq_along(X)) segments(X[i], 0, X[i], Y[i], col=co)
        }
    if(!is.na(LEFT)) shade(L=-m-4*s, R=LEFT, m, s, co)
    if(!is.na(RIGHT)) shade(L=RIGHT, R=m+4*s, m, s, co)
    if(!is.na(BETWEEN[1])) shade(L=BETWEEN[1], R=BETWEEN[2], m, s, co)
    lines(X, Y, ...)
}
## -------- ##
##          ##
## -------- ##
shadetDis <- function(df, LEFT=NA, RIGHT=NA, BETWEEN=NA, co='red', shadeDensity=60, add=FALSE, yl=NA, ...){
    X <- seq(-4, 4, length=500)
    Y <- dt(X, df=df)
        if(is.na(yl[1])) yl <- c(0,max(Y)*1.05)
    if(!add){
    plot(X, Y, type='l', yaxs='i', ylim=yl, ...)
    } else {
        lines(X, Y, type='l', ...)
    }
        shade <- function(L,R, df, co){
            X <- seq(L,R, by=8/max(60,shadeDensity))
            Y <- dt(X, df=df)
            for(i in seq_along(X)) segments(X[i], 0, X[i], Y[i], col=co)
        }
    if(!is.na(LEFT)) shade(L=-4, R=LEFT, df, co)
    if(!is.na(RIGHT)) shade(L=RIGHT, R=4, df, co)
    if(!is.na(BETWEEN[1])) shade(L=BETWEEN[1], R=BETWEEN[2], df, co)
    lines(X,Y, ...)
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
###################################
###################################
## Converting surival parameters ##
###################################
###################################
## -------- ##
##          ##
## -------- ##
ps2hr <- function(Prop, Time){
    ## Conversion from proportion surviving (until T) to hazard rate ##
    ## Prop = S(T)
    ## h = -ln(S(T))/T
    -log(Prop)/Time
}
ms2hr <- function(MS){
    ## Conversion from median survival to hazard rate ##
    ps2hr(0.50, MS)
}
hr2ms <- function(hr){
    ## Conversion from hazard rate to median survival ##
    ## -log(0.50)/hr
    log(2)/hr
}
ps2ms <- function(Prop, Time){
    ## Conversion from proportion surviving (until Time) to median survival ##
    hr2ms(ps2hr(Prop, Time))
}
ms2ps <- function(MS, Time){
    ## Conversion from median survival to proportion surviving (until Time) ##
    hr <- ms2hr(MS)
    1/exp(hr*Time)
}

hr2ps <- function(hr, Time){
    ## Conversion from hazard rate to proportion surviving (until Time) ##
    1/exp(hr*Time)
}
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
myKindaDay <- function(dat){
    # dat is a regular R date format 'YYYY-mm-dd'
    afd <- function(x,fmt) as.numeric(format.Date(x,format=fmt))
    y <- afd(dat, fmt='%Y')
    m <- afd(dat, fmt='%m')
    d <- afd(dat, fmt='%d')
    paste(y,m,d, sep='/')
}
## -------- ##
##          ##
## -------- ##
oneZpower<- function(n,d,sig){
    # Power of one sample z test
    z <- abs( d/sqrt(sig/n) )
    1-pnorm(z)
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
