#Plot DFA model fits
plot_DFA_fit_MARSS <- function(MLEobj, mfrow=c(3,3), fit.years, ts_names, nn, dat, species) {
  mod_fit <- get_DFA_fits(MLEobj)
  # dat <- as.matrix(dat)
  
  # pdf(paste('Plots/MARSS Selection/Fits_', RR, ' ', fwa, '.', oa,' proc_', mm, '.pdf', sep=''), height=9, width=12)
  par(mfrow=mfrow, mai=c(0.6,0.7,0.1,0.1), omi=c(0,0,0,0), mar=c(2,4.25,0,0), oma=c(0,0,2.25,0))
  i <- 1
  for(i in 1:nn) {
    # print(i)
    up <- mod_fit$up[i,]
    mn <- mod_fit$ex[i,]
    lo <- mod_fit$lo[i,]
    plot(fit.years, mn , xlab="",ylab=ts_names[i], xaxt="n", type="n", cex.lab=1.2,
         ylim=c(min(lo),max(up)))
    
    axis(1, at=fit.years, labels=fit.years, cex.axis=1, col='gray')
    axis(1, at=pretty(fit.years)[-length(pretty(fit.years))], labels=FALSE)
    lines(fit.years, dat[i,], col=rgb(1,0,0, alpha=0.2))
    points(fit.years, dat[i,], pch=21, bg='red')#cols[i])
    polygon(x=c(fit.years,rev(fit.years)), y=c(up,rev(lo)), col=rgb(0,0,1, alpha=0.15), border=FALSE)
    lines(fit.years, mn, col=rgb(0,0,1, alpha=0.75), lwd=2)
    # lines(fit.years, up, col="darkgray")
    # lines(fit.years, mn, col="black", lwd=2)
    # lines(fit.years, lo, col="darkgray")
    if(i%%3==1) { mtext(species, side=3, outer=TRUE, line=0.5, font=1, cex=1.5) }
  }#next i
}#end function