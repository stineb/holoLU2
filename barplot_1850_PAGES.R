load( "data/dc_terr_bauska_lastmill_periods.Rdata" )
load( "data/dc_remainder_lastmill.Rdata" )
load( "data/pt_outarr_lastmill.Rdata" ) 
load( "data/fluc_per.Rdata" )

library(Hmisc)
library(plotrix)

ylab <- expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) )

par( las=1, mar=c(5.1, 4.1, 4.1, 2.1) )

ylim <- c(-70,70)
xlim <- c(0,5)

mybar1 <- barplot( 
                  -rev(t( cbind( dc_lastmill_outarr$mean, pt_outarr$mean_lhnfix, dc_remainder_outarr$mean ) )[,7]),
                  beside = TRUE,
                  ylim=ylim, xlim=xlim,
                  col=rev(c("springgreen3","dodgerblue3","darkgoldenrod2")),
                  space=c(0.2,1.5),
                  border=TRUE,
                  axes=FALSE,
                  xlab="period (yr CE)", ylab=ylab,
                  names.arg=rownames(dc_lastmill_outarr)[7]
                  )

axis( 2, lwd=1.5, at=seq(ylim[1], ylim[2], by=10 ));  axis( 2, at=seq(ylim[1], ylim[2], by=2 ), labels=F, tck=-0.01 )
axis( 4, lwd=1.5, labels=F, at=seq(ylim[1], ylim[2], by=10 ));  axis( 4, at=seq(ylim[1], ylim[2], by=2 ), labels=F, tck=-0.01 )

abline(0,0)

errbar(
       mybar1,
       -t( cbind( dc_lastmill_outarr$mean,   pt_outarr$mean_lhnfix,   dc_remainder_outarr$mean ) )[,7],
       -t( cbind( dc_lastmill_outarr$mean - dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix - pt_outarr$sd_lhnfix,   dc_remainder_outarr$mean - dc_remainder_outarr$sd ) )[,7],
       -t( cbind( dc_lastmill_outarr$mean + dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix + pt_outarr$sd_lhnfix,   dc_remainder_outarr$mean + dc_remainder_outarr$sd ) )[,7],
       add=TRUE, pch=NA
       )

## add bars for fluc
offl  <- -2
width <- 0.3
rect( mybar1[1]+offl+width,   rep(0,5), mybar1[1]+offl+2*width, fluc_per_lastmill$hyde32[7] ,  col="orchid"                 ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
rect( mybar1[1]+offl+2*width, rep(0,5), mybar1[1]+offl+3*width, fluc_per_lastmill$kk10[7] ,    col=add_alpha("turquoise3",1.0)      ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

# ## add points for simulated peat C balance
# par(new=TRUE)
# plotCI( mybar1[2], y=pt_outarr$lpx[7], uiw=0.0, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
# par(new=TRUE)
# plotCI( mybar1[1], y=dc_remainder_outarr$mean_lpx[7], uiw=dc_remainder_outarr$sd_lpx[7], col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

legend(
       "bottomleft",
       c("total terrestrial","peat","other processes"),
       fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
       bty="n",
       box.lwd=0
       )
legend(
       "bottom",
       c( "HYDE 3.2", "KK10" ),
       bty="n",
       fill=c("orchid","turquoise3"),
       box.lwd=0
       )
