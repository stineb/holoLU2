load( "data/dc_terr_bauska_lastmill_periods.Rdata" )
load( "data/dc_remainder_lastmill.Rdata" )
load( "data/pt_outarr_lastmill.Rdata" ) 
load( "data/fluc_per.Rdata" )

library(Hmisc)
library(plotrix)

even <- FALSE

if (even){
  period_margins <- read.csv( 'data/periods_lastmill_even.csv' )$period_margins
} else {
  period_margins <- read.csv( 'data/periods_lastmill.csv' )$period_margins
}

periodsName <- paste( 
  as.character( period_margins )[1:length(period_margins)-1],
  "-",
  as.character( period_margins )[2:length(period_margins)],
  sep=""
  )
nper <- length(period_margins)-1

add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

if (even){
  lper <- 1
  ylab <- "C balance change per period (PgC)"
} else {
  lper <- period_margins[2:length(period_margins)] - period_margins[1:(length(period_margins)-1)]
  ylab <- expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) )
}

if (even){
  pdf("fig/cbal_comparison_barplot_lastmill_even_holoLU2.pdf", width=10, height=6 )
} else {
  pdf("fig/cbal_comparison_barplot_lastmill_holoLU2.pdf", width=10, height=6 )
}

  if (even){
    par( las=1, mar=c(5.1, 4.1, 4.1, 5.1) )
    ylim <- c(-70,30)/lper    
  } else {
    par( las=1, mar=c(5.1, 4.1, 4.1, 2.1) )
    # ylim <- c(-100,40)
    ylim <- c(-0.8,0.3)
  }

  if (nper==5){
    xlim <- c(2.2,25)
  } else if (nper==6){
    # xlim <- c(2.2,35)
    xlim <- c(2.2,30)
  } else {
    xlim <- c(2.2,35)
  }

  mybar1 <- barplot( 
                    t( cbind( dc_lastmill_outarr$mean, pt_outarr$mean_lhnfix, dc_remainder_outarr$mean ) / lper ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE,
                    axes=FALSE,
                    xlab="period (yr CE)", ylab=ylab,
                    names.arg=rownames(dc_lastmill_outarr)
                    )

  if (even){
    axis( 2, lwd=1.5, at=seq(ylim[1], ylim[2], by=20));  axis( 2, at=seq(ylim[1], ylim[2], by=5), labels=F, tck=-0.01 )
    axis( 4, lwd=1.5, labels=format( seq(ylim[1], ylim[2], by=20)/(period_margins[2]-period_margins[1]), digits=1), at=seq(ylim[1], ylim[2], by=20));  axis( 4, at=seq(ylim[1], ylim[2], by=5), labels=F, tck=-0.01 )
    mtext( expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) ), side=4, line=3.5, las=3 )
  } else {
    axis( 2, lwd=1.5, at=seq(ylim[1], ylim[2], by=0.1 ));  axis( 2, at=seq(ylim[1], ylim[2], by=0.02 ), labels=F, tck=-0.01 )
    axis( 4, lwd=1.5, labels=F, at=seq(ylim[1], ylim[2], by=0.1 ));  axis( 4, at=seq(ylim[1], ylim[2], by=0.02 ), labels=F, tck=-0.01 )
  }

  abline(0,0)

  errbar(
         mybar1,
         t( cbind( dc_lastmill_outarr$mean,   pt_outarr$mean_lhnfix,   dc_remainder_outarr$mean ) / lper ) ,
         t( cbind( dc_lastmill_outarr$mean - dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix - pt_outarr$sd_lhnfix,   dc_remainder_outarr$mean - dc_remainder_outarr$sd ) / lper ) ,
         t( cbind( dc_lastmill_outarr$mean + dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix + pt_outarr$sd_lhnfix,   dc_remainder_outarr$mean + dc_remainder_outarr$sd ) / lper ) ,
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left <- mybar1 - 0.7
  right <- mybar1 + 0.6
  rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,3], ylim[1], left[1,4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  if (dim(mybar1)[2]>5){
    rect( left[1,5], ylim[1], left[1,6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    if (dim(mybar1)[2]>6){
      rect( left[1,7], ylim[1], right[3,7]+1.2, ylim[2], col=rgb(0,0,0,0.1), border=NA )
    }
  } else {
    rect( left[1,5], ylim[1], right[3,5]+1.2, ylim[2], col=rgb(0,0,0,0.1), border=NA ) 
  }

  ## add bars for fluc
  offl  <- 0.2
  width <- 0.3
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31u / lper , col=add_alpha("grey50", 0.5)     ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31 / lper ,  col="grey50"                     ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32u / lper , col=add_alpha("burlywood3",0.5)  ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32 / lper ,  col="burlywood3"                 ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_lastmill$kk10 / lper ,    col=add_alpha("khaki3",1.0)      ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  ## add points for simulated peat C balance
  par(new=TRUE)
  plotCI( left[2,]+0.7, y=pt_outarr$lpx / lper, uiw=0.0, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  par(new=TRUE)
  plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx / lper, uiw=dc_remainder_outarr$sd_lpx / lper, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

  legend(
         "bottomleft",
         c("total terrestrial","peat","other processes"),
         fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
         bty="n",
         box.lwd=0
         )
  legend(
         "bottom",
         c( "HYDE 3.1", "HYDE 3.2", "KK10" ),
         bty="n",
         fill=c("grey50","burlywood3","khaki3"),
         box.lwd=0
         )

dev.off()