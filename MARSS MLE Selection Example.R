#************************************************************************************************
#Project Name: ALASKAN SOCKEYE SIZE TRENDS DFA - Looping Through Process Numbers
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 4.16.17
#
#Purpose: Conduct a DFA on the size of Sockeye salmon throughout the state
#
#
#*************************************************************************************************
#VERSIONS:
#
#ASSUMPTIONS:
#
#
#NOTES:
#
#
#*************************************************************************************************
require(MARSS)
require(stringr)
require(xlsx)
require(reshape2)
require(ggplot2)
require(stringr)
require(RColorBrewer)
require(statss)
require(rstan)

wd <- '/Users/curryc2/Documents/2016/Lewis Size Eval'
setwd(wd)

source('R/get-DFA-fits.r')
source('R/rotate-trends-MARSS.r')
source('R/plot-trends-loadings-MARSS.r')

#==============================================
# CONTROL SECTION
fwa <- 2
oa <- 3

#Years
fit.years <- 1975:2015
n.fit.years <- length(fit.years)

#Boolean for covariates
do.covars <- FALSE
#==============================================

#Read in Data
# data <- read.csv('Data/Length at Age.csv', header=FALSE)
data <- read.xlsx2('Data/Length at Age.xlsx', sheetName='Reordered Length at Age.csv', startRow=1, header=FALSE, 
                   stringsAsFactors=FALSE)
n.groups <- ncol(data)-1
#Limit to only Mean Lengths
# head.titles <- as.vector(data[1,-1]) 
# head.stocks <- as.vector(data[2,-1])
# head.age <- data[3,-1]
# head.type <- data[4,-1]

head.location <- vector(length=n.groups)
head.region <- vector(length=n.groups)
head.source <- vector(length=n.groups)

head.titles <- vector(length=n.groups)
head.stocks <- vector(length=n.groups)
head.age <- vector(length=n.groups)
head.type <- vector(length=n.groups)

i <- 1
for(i in 1:n.groups) {
  head.location[i] <- data[1,i+1]
  head.region[i] <- data[2,i+1]
  head.source[i] <- data[3,i+1]
  head.titles[i] <- data[4,i+1]
  head.stocks[i] <- data[5,i+1]
  head.age[i] <- data[6,i+1]
  head.type[i] <- data[7,i+1]
}#next i

years <- as.numeric(data[-c(1:7),1])
n.years <- length(years)

#Extract values
# values <- read.xlsx('Data/Length at Age.xlsx', sheetName='Length at Age.csv', startRow=8, header=FALSE, stringsAsFactors=FALSE)
values <- as.data.frame(data[-c(1:7),-1])
values <- as.numeric(values)
dimnames(values)[[1]] <- years
names(values) <- head.titles

#Metadata
ages <- unique(head.age)
n.ages <- length(ages)

stocks <- unique(head.stocks)
n.stocks <- length(stocks)

types <- unique(head.type)
n.types <- length(types)

#########################################################################################
# SELECT DATA TO MODEL
temp.age <- paste(fwa, '.', oa, sep='')
temp.loc <- which(head.type=='mean' & head.age==temp.age)
temp.dat <- as.matrix(values[which(years %in% fit.years),temp.loc])
temp.dat.2 <- matrix(as.numeric(temp.dat), nrow=nrow(temp.dat), ncol=ncol(temp.dat))
temp.names <- head.stocks[temp.loc]
temp.locations <- head.location[temp.loc]
temp.sources <- head.source[temp.loc]
#########################################################################################
# PROCESS MODEL
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
#====================================================
##### COVARIATE SECTION: Influencing States #####
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
#====================================================
#COVARIANCE MATRIX for Processes
## 'QQ' is identity
QQ <- "identity"  # diag(mm)

#########################################################################################
# OBSERVATION MODEL

## number of obs time series - NUMBER OF POPULATIONS
nn <- ncol(temp.dat.2)#16 #Adjust me 



## 'aa' is the offset/scaling
aa <- "zero"
#====================================================
##### COVARIATE SECTION: Observation Process #####
dd <- "zero"
DD  <- "zero"
#=============================================
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal" #Separate sigma for each time-series
#########################################################################################
# FIT THE MARSS MODEL
trial.mm <- c(1:6)
n.trial.mm <- length(trial.mm)

Age <- rep(paste0(fwa,'.',oa), n.trial.mm)
AIC <- vector(length=n.trial.mm)
AICc <- vector(length=n.trial.mm)
conv <- vector(length=n.trial.mm)
fits.list <- vector('list', n.trial.mm)

dat <- t(scale(temp.dat.2))

t <- 1
for(t in 1:n.trial.mm) {
  mm <- trial.mm[t]
  
  ## 'ZZ' is loadings matrix
  ZZ <- matrix(list(0),nn,mm)
  
  #Updated
  i <- 1
  for(i in 1:nn) {
    j <- 1
    for(j in 1:mm) {
      if(i < j) {
        ZZ[i,j] <- 0
      }else {
        ZZ[i,j] <- paste(temp.names[i], j, sep='_')
      }
    }#cols = processes
  }#rows = time series
  
  mod_list <- list(B=BB, U=uu, C=CC, c=cc, Q=QQ, Z=ZZ, A=aa, D=DD, d=dd, R=RR)
  con_list <- list(maxit=5000, allow.degen=TRUE, safe=TRUE, conv.test.slope.tol=0.05)

  
  dfa <- MARSS(y=dat, model=mod_list, control=con_list)
  
  #Save Objects
  AIC[t] <- dfa$AIC
  AICc[t] <- dfa$AICc
  conv[t] <- dfa$convergence
  
  #Save Model
  fits.list[[t]] <- dfa
  
  #Create Figures
  if(mm==1) {
    Z_rot <- coef(dfa, type="matrix")$Z
    proc_rot <- dfa$states
    #No plotting
    unique.locs <- unique(temp.locations)
    n.unique.locs <- length(unique.locs)
    
    temp.pal <- brewer.pal(n.unique.locs, name='Set1')
    
    cols <- vector(length=nn) #c("brown","darkgreen","blue")
    i <- 1
    for(i in 1:nn) {
      cols[i] <- temp.pal[which(unique.locs %in% temp.locations[i])]
    }
    pdf(paste('Plots/MARSS Selection/', RR, ' ', fwa, '_', oa,' proc_', mm, '.pdf', sep=''), height=9, width=12)
    plot_trend_loadings_MARSS(MLEobj=dfa, proc_rot, Z_rot, fit.years, ts_names=temp.names, cols, nn)
    dev.off()
  }else {
    proc_rot <- rotate_trends_MARSS(dfa)$proc_rot
    Z_rot <- rotate_trends_MARSS(dfa)$Z_rot
  
  
  unique.locs <- unique(temp.locations)
  n.unique.locs <- length(unique.locs)
  
  temp.pal <- brewer.pal(n.unique.locs, name='Set1')
  
  cols <- vector(length=nn) #c("brown","darkgreen","blue")
  i <- 1
  for(i in 1:nn) {
    cols[i] <- temp.pal[which(unique.locs %in% temp.locations[i])]
  }
  pdf(paste('Plots/MARSS Selection/', RR, ' ', fwa, '_', oa,' proc_', mm, '.pdf', sep=''), height=9, width=12)
  plot_trend_loadings_MARSS(MLEobj=dfa, proc_rot, Z_rot, fit.years, ts_names=temp.names, cols, nn)
  dev.off()
  
  #Plot Fits
  mod_fit <- get_DFA_fits(dfa)
  pdf(paste('Plots/MARSS Selection/Fits_', RR, ' ', fwa, '_', oa,' proc_', mm, '.pdf', sep=''), height=9, width=12)
  par(mfrow=c(3,1), mai=c(0.6,0.7,0.1,0.1), omi=c(0,0,0,0), mar=c(2,4.25,0,0), oma=c(0,0,2.25,0))
  
  ts_names <- temp.names
  
  i <- 1
  for(i in 1:nn) {
    up <- mod_fit$up[i,]
    mn <- mod_fit$ex[i,]
    lo <- mod_fit$lo[i,]
    plot(fit.years,mn,xlab="",ylab=ts_names[i],xaxt="n",type="n", cex.lab=1.2,
         ylim=c(min(lo),max(up)))
    axis(1, at=fit.years, labels=fit.years, cex.axis=1)
    points(fit.years,dat[i,], pch=16, col=cols[i])
    lines(fit.years, up, col="darkgray")
    lines(fit.years, mn, col="black", lwd=2)
    lines(fit.years, lo, col="darkgray")
    if(i%%3==1) { mtext(paste('Age:',temp.age), side=3, outer=TRUE, line=0.5, font=1, cex=1.5) }
  }
  dev.off()
  }
}
#Output Data Frame
out.df <- data.frame(Age,trial.mm,AIC,AICc,conv)

write.csv(out.df, file=paste0('Output/',fwa,'.',oa,'_Models.csv'))



