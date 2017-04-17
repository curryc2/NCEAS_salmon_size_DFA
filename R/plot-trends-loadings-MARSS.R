plot_trend_loadings_MARSS <- function(MLEobj, proc_rot, Z_rot, fit.years, ts_names, cols, nn) {
  
  # years <- years# seq(from=wb_weekly[1,"Date"],by="weeks",length.out=wk_last)
  ylm <- c(-1,1)*max(abs(proc_rot))
  # layout(matrix(c(1,2,3,4),mm,mm),widths=c(2,1))
  layout(matrix(c(1:(2*mm)),mm,2),widths=c(1.75,1))
  ## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
  par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0), oma=c(0,0,2,0))
  ## plot the processes
  i <- 1
  for(i in 1:mm) {
    ## set up plot area
    plot(fit.years,proc_rot[i,], type="n", bty="L",
         ylim=ylm, xlab="", ylab="")
    ## draw zero-line
    abline(h=0, col="gray")
    ## plot trend line
    lines(fit.years,proc_rot[i,], lwd=2)
    #   lines(years,proc_rot[i,], lwd=2)
    ## add panel labels
    mtext(paste("Process",i), side=3, line=0.5)
    # axis(1, at=xlbl, labels=format(xlbl, "%b %y"), cex.axis=0.8)
    if(i==1) { legend('topleft', legend=paste('AICc:', round(MLEobj$AICc,1))) }
    #Add legend
    if(i==1) { legend('bottom', title='Location', legend=unique.locs, fill=temp.pal, ncol=n.unique.locs) }
  }
  
  ## plot the loadings
  minZ <- 0
  ylm <- c(-1,1)*max(abs(Z_rot))
  i <- 1
  for(i in 1:mm) {
    plot(c(1:nn)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
         lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,nn+0.5), col=cols)
    for(j in 1:nn) {
      if(Z_rot[j,i] > minZ) { text(j, -0.03, ts_names[j], srt=90, adj=1, cex=1.2, col=cols[j]) }
      if(Z_rot[j,i] < -minZ) { text(j, 0.03, ts_names[j], srt=90, adj=0, cex=1.2, col=cols[j]) }
      abline(h=0, lwd=1.5, col="gray")
    } 
    mtext(paste("Factor loadings on process",i),side=3,line=0.5)
  }
  mtext(paste('Age:',temp.age), side=3, outer=TRUE, line=0.5, font=1, cex=1.5)
}