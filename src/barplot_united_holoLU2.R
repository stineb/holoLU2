load("data/dc_remainder_holocene.Rdata")
load("data/pt_outarr.Rdata")
load("data/fluc_per.Rdata")
load("data/dc_elsig.Rdata")
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

library(Hmisc)
library(plotrix)

lper <- 2000
ylab <- expression( paste( "Mean C balance change per period (PgC yr"^{-1},")" ) )

## Plot bars for the periods
magn <- 0.8
ncols <- 2
nrows <- 1
widths <- magn * c(9,11)
heights <- magn * c(6)

pdf("fig/bal_comparison_barplot_united_holoLU2.pdf", width=sum(widths), height=sum(heights) )

  panel <- layout(
                  matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
  # layout.show(panel)

  ##############################
  ## HOLOCENE
  ##############################
  par( las=1, xaxs="i", mar=c(5.1, 4.1, 1, 3.5) )
  ylim <- c(-200,200)
  xlim <- c(1.2,26)

  mybar1 <- barplot( 
                    # t( cbind( dc_land_outarr$median, pt_outarr$median_yu, dc_remainder_outarr$median_yu ) ),
                    t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE, axes=FALSE,
                    xlab="period (ka BP)", ylab="land C balance per 2-ka period (PgC)",           
                    names.arg=rownames(dc_land_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=format( seq(ylim[1], ylim[2], by=50)/2000, digits=1),at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  # mtext( expression( paste( "Mean C balance change per period (PgC yr"^{-1},")" ) ), side=4, line=3.5, las=3 )
  abline(0,0)

  errbar(
         mybar1,
         # t( cbind( dc_land_outarr$median, pt_outarr$median_yu, dc_remainder_outarr$median_yu ) ),
         # t( cbind( dc_land_outarr$q10,    pt_outarr$q10_yu,    dc_remainder_outarr$q10_yu ) ),
         # t( cbind( dc_land_outarr$q90,    pt_outarr$q90_yu,    dc_remainder_outarr$q90_yu ) ),
         t( cbind( dc_land_outarr$mean,                        pt_outarr$mean_yu,                     dc_remainder_outarr$mean_yu ) ),
         t( cbind( dc_land_outarr$mean - dc_land_outarr$sd,    pt_outarr$mean_yu - pt_outarr$sd_yu,   dc_remainder_outarr$mean_yu - dc_remainder_outarr$sd_yu ) ),
         t( cbind( dc_land_outarr$mean + dc_land_outarr$sd,    pt_outarr$mean_yu + pt_outarr$sd_yu,   dc_remainder_outarr$mean_yu + dc_remainder_outarr$sd_yu ) ),
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left  <- mybar1 - 0.7
  right <- mybar1 + 0.6
  rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,3], ylim[1], left[1,4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,5], ylim[1], 26, ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## add bars for fluc
  offl  <- 0.2
  width <- 0.3
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31u, col=add_alpha("grey50", 0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31,  col="grey50" ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32u, col=add_alpha("burlywood3",0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32,  col="burlywood3" ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10,    col=add_alpha("khaki3",0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10d,   col="khaki3" ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  points( left[2,]+0.7, y=pt_outarr$lpx_peatc_global, pch=16 )
  plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx, uiw=dc_remainder_outarr$sd_lpx, col="darkgoldenrod4", pch=16, add=TRUE )

  legend(
         "bottomleft",
         c("total terrestrial","peat","other processes"),
         fill=c("springgreen3","dodgerblue3","darkgoldenrod2"),
         bty="n",
         box.lwd=0
         )
  legend(
         "bottom",
         c( "HYDE 3.1", "HYDE 3.2", "KK10D (KK10)" ),
         bty="n",
         fill=c("grey50","burlywood3","khaki3"),
         box.lwd=0
         )


  ##############################
  ## LAST MILLENNIUM
  ##############################
  rm( list=ls() )
  load( "data/dc_terr_bauska_lastmill_periods.Rdata" )
  load( "data/dc_remainder_lastmill.Rdata" )
  load( "data/pt_outarr_lastmill.Rdata" ) 
  load( "data/fluc_per.Rdata" )
  source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

  library(Hmisc)
  library(plotrix)

  ylim <- c(-0.8,0.3)
  xlim <- c(1.2,35.7)

  period_margins <- read.csv( 'data/periods_lastmill.csv' )$period_margins
  periodsName <- paste( 
    as.character( period_margins )[1:length(period_margins)-1],
    "-",
    as.character( period_margins )[2:length(period_margins)],
    sep=""
    )
  nper <- length(period_margins)-1
  lper <- period_margins[2:length(period_margins)] - period_margins[1:(length(period_margins)-1)]

  par( mar=c(5.1, 4.1, 1, 2) )
  mybar2 <- barplot( 
                    # t( cbind( dc_lastmill_outarr$median, pt_outarr$median_lhnfix, dc_remainder_outarr$median ) / lper ),
                    t( cbind( dc_lastmill_outarr$mean, pt_outarr$mean_lhnfix, dc_remainder_outarr$mean ) / lper ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE,
                    axes=FALSE,
                    xlab="period (yr CE)", ylab=expression( paste( "mean land C balance change per period (PgC yr"^{-1},")" ) ),
                    names.arg=rownames(dc_lastmill_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=0.1));  axis(2,at=seq(ylim[1],ylim[2],by=0.02),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=0.1));  axis(4,at=seq(ylim[1],ylim[2],by=0.02),labels=F,tck=-0.01)
  # box(lwd=1.75)

  abline(0,0)

  errbar(
         mybar2,
         # t( cbind( dc_lastmill_outarr$median, pt_outarr$median_lhnfix, dc_remainder_outarr$median ) / lper ),
         # t( cbind( dc_lastmill_outarr$q10,    pt_outarr$q10_lhnfix,    dc_remainder_outarr$q10 ) / lper ),
         # t( cbind( dc_lastmill_outarr$q90,    pt_outarr$q90_lhnfix,    dc_remainder_outarr$q90 ) / lper ),
         t( cbind( dc_lastmill_outarr$mean,                         pt_outarr$mean_lhnfix,                       dc_remainder_outarr$mean ) / lper ),
         t( cbind( dc_lastmill_outarr$mean - dc_lastmill_outarr$sd, pt_outarr$mean_lhnfix - pt_outarr$sd_lhnfix, dc_remainder_outarr$mean - dc_remainder_outarr$sd ) / lper ),
         t( cbind( dc_lastmill_outarr$mean + dc_lastmill_outarr$sd, pt_outarr$mean_lhnfix + pt_outarr$sd_lhnfix, dc_remainder_outarr$mean + dc_remainder_outarr$sd ) / lper ),
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left <- mybar2 - 0.7
  right <- mybar2 + 0.6
  rect( left[1,1], ylim[1], left[1,2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,3], ylim[1], left[1,4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,5], ylim[1], left[1,6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
  rect( left[1,7], ylim[1], right[3,7]+1.1, ylim[2], col=rgb(0,0,0,0.1), border=NA )

  ## add bars for fluc
  offl  <- 0.2
  width <- 0.3
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31u / lper, col=add_alpha("grey50", 0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_lastmill$hyde31 / lper,  col="grey50" ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32u / lper, col=add_alpha("burlywood3",0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_lastmill$hyde32 / lper,  col="burlywood3" ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_lastmill$kk10 / lper,    col=add_alpha("khaki3",1.0) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

  ## add points for simulated peat C balance
  par(new=TRUE)
  plotCI( left[2,]+0.7, y=pt_outarr$lpx / lper, uiw=0.0, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  par(new=TRUE)
  plotCI( left[3,]+0.7, y=dc_remainder_outarr$mean_lpx / lper, uiw=dc_remainder_outarr$sd_lpx / lper, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
  # plotCI( left[3,]+0.7, y=dc_remainder_outarr$median_lpx / lper, ui=dc_remainder_outarr$q90_lpx / lper, , li=dc_remainder_outarr$q10_lpx / lper, col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

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
