source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")

library(Hmisc)
library(plotrix)

## -----------------------------------------------------------------------------------------
## Select whether to use extended periods (up to 13 kyr BP)
## -----------------------------------------------------------------------------------------
ext <-  TRUE
## -----------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------
## Select delineation (30 or 45 deg N)
## -----------------------------------------------------------------------------------------
north_of_30 <-  TRUE
## -----------------------------------------------------------------------------------------

if (ext){
  if (north_of_30){
    load("data/pt_outarr_ext.Rdata")
    load("data/dc_peat_lpx_ext.Rdata")
  } else {
    load("data/pt_outarr_ext_45N.Rdata")
    load("data/dc_peat_lpx_ext_45N.Rdata")
  }
} else {
  if (north_of_30){
    load("data/pt_outarr.Rdata")
    load("data/dc_peat_lpx.Rdata")
  } else {
    load("data/pt_outarr_45N.Rdata")
    load("data/dc_peat_lpx_45N.Rdata")
  }
}

lper <- 2000
ylab <- expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) )

## Plot bars for the periods
pdf("fig/bal_peat_comparison_barplot_holocene_holoLU2.pdf", width=8, height=6 )

  par( las=1, mar=c(5.1, 4.1, 4.1, 5.1) )  # , mgp=c(3.8,1,0)
  ylim <- c(-50,200)

  mybar1 <- barplot( 
    # t( select( pt_outarr, median_yu, lpx_peatc_30N_90N, lpx_peatc_90S_30N )), 
    t( select( pt_outarr, mean_yu, lpx_peatc_30N_90N, lpx_peatc_90S_30N )), 
    beside=T, 
    width=c(2,1,1), 
    space=c(0,0.4),
    col=c("dodgerblue4", "dodgerblue1", "turquoise3"),
    border=TRUE, axes=FALSE,
    xlab="period (ka BP)", ylab="C balance per 2-ka period (PgC)",
    ylim=ylim    
    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=format( seq(ylim[1], ylim[2], by=50)/2000, digits=3),at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  mtext( expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) ), side=4, line=3.5, las=3 )
  
  abline(0,0)

  errbar(
         mybar1,
         # t( cbind( pt_outarr$median_yu, rep(NA, length(pt_outarr$median_yu)), rep(NA, length(pt_outarr$median_yu)) ) ),
         # t( cbind( pt_outarr$q10_yu,    rep(NA, length(pt_outarr$median_yu)), rep(NA, length(pt_outarr$median_yu)) ) ),
         # t( cbind( pt_outarr$q90_yu,    rep(NA, length(pt_outarr$median_yu)), rep(NA, length(pt_outarr$median_yu)) ) ),
         t( cbind( pt_outarr$mean_yu,                   rep(NA, length(pt_outarr$mean_yu)), rep(NA, length(pt_outarr$mean_yu)) ) ),
         t( cbind( pt_outarr$mean_yu - pt_outarr$sd_yu, rep(NA, length(pt_outarr$mean_yu)), rep(NA, length(pt_outarr$mean_yu)) ) ),
         t( cbind( pt_outarr$mean_yu + pt_outarr$sd_yu, rep(NA, length(pt_outarr$mean_yu)), rep(NA, length(pt_outarr$mean_yu)) ) ),
         add=TRUE, pch=NA
         )

  ## add grey rectangles 
  left  <- mybar1[1,] - 1.3
  right <- mybar1[3,] + 0.8
  rect( left, ylim[1], right, ylim[2], col=c(rgb(0,0,0,0.1),rgb(0,0,0,0)), border=NA )
  
  points( mybar1[2,], y=rev(dc_pt_yu10$north), pch=16 )
  points( mybar1[3,], y=rev(dc_pt_yu10$south+dc_pt_yu10$south), pch=16 )

  legend(
         "topright",
         c("YML","LPX North","LPX South + Tropics"),
         fill=c("dodgerblue4","dodgerblue1","turquoise3"),
         bty="n",
         box.lwd=0
         )
  legend(
         "topright",
         c(" Yu, 2011"),
         inset=c(0.14,0.18),
         pch=16,
         bty="n",
         box.lwd=0
         )

dev.off()

