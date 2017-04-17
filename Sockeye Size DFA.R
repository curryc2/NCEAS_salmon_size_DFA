#************************************************************************************************
#Project Name: ALASKAN SOCKEYE SIZE TRENDS - Dynamic Factor Analysis of Size-at-Age Trends
#Creator: Dr. Curry James Cunningham, College of Fisheries and Ocean Sciences, University of Alaska, Fairbanks
#Date: 4.1.16
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

source('R/get-DFA-fits.r')
source('R/rotate-trends-MARSS.r')
source('R/plot-trends-loadings-MARSS.r')

#==============================================
# CONTROL SECTION
fwa <- 1
oa <- 3

## number of processes
mm <- 1

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

#Intro Plots
# #Mean Length of 1.2's
# temp.data <- as.matrix(values[,which(head.type=='mean' & head.age=='1.2')])
# # dimnames(temp.data) <- list(years,head.stocks[which(head.type=='mean' & head.age=='1.2')])
# temp.list <- melt(temp.data, na.rm=TRUE)
# names(temp.list) <- c('year','stock','value')
# temp.list$value <- as.numeric(temp.list$value)
# temp.list$year <- as.numeric(temp.list$year)


# g <- ggplot(data=temp.list, aes(x=year, y=value, colour=stock)) + 
#        geom_line()
# plot(g)


#########################################################################################
# SELECT DATA TO MODEL
temp.age <- paste(fwa, '.', oa, sep='')
temp.loc <- which(head.type=='mean' & head.age==temp.age)
temp.dat <- as.matrix(values[which(years %in% fit.years),temp.loc])
temp.dat.2 <- matrix(as.numeric(temp.dat), nrow=nrow(temp.dat), ncol=ncol(temp.dat))
ts_names <- head.stocks[temp.loc]
temp.locations <- head.location[temp.loc]
temp.sources <- head.source[temp.loc]
#########################################################################################
# PROCESS MODEL

## number of processes
# mm <-2
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
# uu <- "unequal"

#====================================================
##### COVARIATE SECTION: Influencing States #####

# covar.dat <- read.xlsx('Data/Length at Age.xlsx', sheetName='Length at Age.csv', startRow=1, header=FALSE, 
                       # stringsAsFactors=FALSE)

## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)

#====================================================
#COVARIANCE MATRIX for Processes

## 'QQ' is identity
QQ <- "identity"  # diag(mm)
# QQ <- "equalvarcov"
# QQ <- "diagonal and unequal"

#########################################################################################
# OBSERVATION MODEL

## number of obs time series - NUMBER OF POPULATIONS
nn <- ncol(temp.dat.2)#16 #Adjust me 
## 'ZZ' is loadings matrix
ZZ <- matrix(list(0),nn,mm)
# ZZ[,1] <- paste("z",c("C","N","P"),1,sep="_") 
# ZZ[2:nn,2] <- paste("z",c("N","P"),2,sep="_")


#Updated

i <- 1
for(i in 1:nn) {
  j <- 1
  for(j in 1:mm) {
    if(i < j) {
      ZZ[i,j] <- 0
    }else {
      ZZ[i,j] <- paste(ts_names[i], j, sep='_')
    }
  }#cols = processes
}#rows = time series

ZZ


## 'aa' is the offset/scaling
aa <- "zero"

#====================================================
##### COVARIATE SECTION: Observation Process #####
## 'DD' and 'dd' are for covariates

if(do.covars==TRUE) {

  #Covariate input
  covar.dat <- read.xlsx('Data/Length at Age.xlsx', sheetName='Covariates', startRow=2, header=TRUE,
                           stringsAsFactors=FALSE)

  #Determine covariates to include
  covar.years <- fit.years-1

  AOI <- as.vector(scale(covar.dat$AOIa[which(covar.dat$Year %in% covar.years)]))
  AkSockeyeRel <- as.vector(scale(covar.dat$AkSockeyeRel[which(covar.dat$Year %in% covar.years)]))
  AkPinkRel <- as.vector(scale(covar.dat$AkPinkRel[which(covar.dat$Year %in% covar.years)]))


  covariates <- rbind(AkPinkRel)
  # covariates <- rbind(AOI, AkSockeyeRel, AkPinkRel)

  dd <- covariates
  DD <- "diagonal and unequal"
  # DD <- "unconstrained"  # matrix(0,mm,1)
}else {
  dd <- "zero"
  DD  <- "zero"
}



#This would be 
# DD <- matrix(0,mm,1)
# dd <- "zero"  # matrix(0,1,wk_last)

#=============================================
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal" #Separate sigma for each time-series
# RR <- 'diagonal and equal'

#Alternative Assume the error stdev is equal for each of two data types
# RR <- matrix(list(0), nrow=nn, ncol=nn)
# diag(RR) <- temp.sources
# RR <- "unconstrained"

#########################################################################################
# FIT THE MARSS MODEL

## list with specifications for model vectors/matrices
mod_list <- list(B=BB, U=uu, C=CC, c=cc, Q=QQ, Z=ZZ, A=aa, D=DD, d=dd, R=RR)
# mod_list <- list(B=BB, U=uu, C=CC, c=cc, Q=QQ, Z=ZZ, A=aa, R=RR)
## list with model inits
init_list <- list(x0=matrix(rep(0,mm),mm,1))
## list with model control parameters
con_list <- list(maxit=5000, allow.degen=TRUE, safe=TRUE, conv.test.slope.tol=0.05)

#DATA FILE, standardize to zero mena
# 
# #Add Names
# # names(dat) <- names(temp.dat)
# 
# # dat <- matrix(c(DOC,TDN,TDP), ncol=nn)

dat <- t(scale(temp.dat.2))


# 
# start <- date()
# ## fit MARSS
if(do.covars==TRUE) {
  # dfa <- MARSS(y=dat, model=mod_list, inits=init_list, control=con_list, covariates=covariates)
  
  
  mod_list.2 <- list(m=mm, R='diagonal and unequal')
  dfa <- MARSS(y=dat, model=mod_list.2, form='dfa', control=con_list, inits=init_list,
               covariates=covariates)
}else {
  # dfa <- MARSS(y=dat, model=mod_list, inits=init_list, control=con_list)
  
  mod_list.2 <- list(m=mm, R='diagonal and unequal')
  dfa <- MARSS(y=dat, model=mod_list.2, form='dfa', control=con_list, inits=init_list,
               covariates=NULL)
}


#########################################################################################
# ROTATING TRENDS AND LOADINGS

# ## get the estimated ZZ
# Z_est <- coef(dfa, type="matrix")$Z
# ## get the inverse of the rotation matrix
# H_inv <- varimax(Z_est)$rotmat
# 
# ## rotate factor loadings
# Z_rot = Z_est %*% H_inv   
# ## rotate processes
# proc_rot = solve(H_inv) %*% dfa$states

proc_rot <- rotate_trends_MARSS(dfa)$proc_rot
Z_rot <- rotate_trends_MARSS(dfa)$Z_rot

#########################################################################################
# Plotting the processes & loadings
pdf(paste('Plots/Sockeye Mean Size DFA ', RR, ' ', fwa, '_', oa,' proc_', mm, '.pdf', sep=''), height=9, width=12)

# ts_names <- ts_names 
# fit.years <- years 

unique.locs <- unique(temp.locations)
n.unique.locs <- length(unique.locs)

temp.pal <- brewer.pal(n.unique.locs, name='Set1')

cols <- vector(length=nn) #c("brown","darkgreen","blue")
i <- 1
for(i in 1:nn) {
  cols[i] <- temp.pal[which(unique.locs %in% temp.locations[i])]
}

plot_trend_loadings_MARSS(MLEobj=dfa, proc_rot, Z_rot, fit.years, ts_names=ts_names, cols, nn)

dev.off()

# 
# # years <- years# seq(from=wb_weekly[1,"Date"],by="weeks",length.out=wk_last)
# ylm <- c(-1,1)*max(abs(proc_rot))
# # layout(matrix(c(1,2,3,4),mm,mm),widths=c(2,1))
# layout(matrix(c(1:(2*mm)),mm,2),widths=c(1.75,1))
# ## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
# par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0), oma=c(0,0,2,0))
# ## plot the processes
# i <- 1
# for(i in 1:mm) {
#   ## set up plot area
#   plot(fit.years,proc_rot[i,], type="n", bty="L",
#        ylim=ylm, xlab="", ylab="")
#   ## draw zero-line
#   abline(h=0, col="gray")
#   ## plot trend line
#   lines(fit.years,proc_rot[i,], lwd=2)
# #   lines(years,proc_rot[i,], lwd=2)
#   ## add panel labels
#   mtext(paste("Process",i), side=3, line=0.5)
#   # axis(1, at=fit.years, labels=format(fit.years, "%b %y"), cex.axis=0.8)
#   if(i==1) { legend('topleft', legend=paste('AICc:', round(dfa$AICc,1))) }
#   #Add legend
#   if(i==1) { legend('bottom', title='Location', legend=unique.locs, fill=temp.pal, ncol=n.unique.locs) }
# }
# 
# ## plot the loadings
# minZ <- 0
# ylm <- c(-1,1)*max(abs(Z_rot))
# i <- 1
# for(i in 1:mm) {
#   plot(c(1:nn)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
#        lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,nn+0.5), col=cols)
#   for(j in 1:nn) {
#     if(Z_rot[j,i] > minZ) { text(j, -0.03, ts_names[j], srt=90, adj=1, cex=1.2, col=cols[j]) }
#     if(Z_rot[j,i] < -minZ) { text(j, 0.03, ts_names[j], srt=90, adj=0, cex=1.2, col=cols[j]) }
#     abline(h=0, lwd=1.5, col="gray")
#   } 
#   mtext(paste("Factor loadings on process",i),side=3,line=0.5)
# }
# mtext(paste('Age:',temp.age), side=3, outer=TRUE, line=0.5, font=1, cex=1.5)

# summary(dfa_1)

#########################################################################################
# PLOTTING MODEL FITS

#Function to return fitted values and CI
#Plot Model Fits
mod_fit <- get_DFA_fits(dfa)
## plot the fits
# ts_names <- c("DOC","TDN","TDP")
# fit.years <- seq(wb_weekly[1,"Date"],wb_weekly[n_wks,"Date"],by="6 months")
# cols <- c("brown","darkgreen","blue")
# years <- seq(from=wb_weekly[1,"Date"],by="weeks",length.out=wk_last)
par(mfrow=c(3,1), mai=c(0.6,0.7,0.1,0.1), omi=c(0,0,0,0), mar=c(2,4.25,0,0), oma=c(0,0,2.25,0))

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


#########################################################################################
# # PLOTTING COVARIATES
# est.covar <- MARSSparamCIs(dfa_1)
# 
# #Extract Covariates
# ts_names




dev.off()





###
# Printing Section
print(start)
print(end)
print(dfa_1$AICc)












