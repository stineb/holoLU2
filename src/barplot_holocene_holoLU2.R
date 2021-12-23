load("../data/dc_remainder_holocene.Rdata")
load("../data/pt_outarr.Rdata")
load("../data/fluc_per.Rdata")
load("../data/dc_elsig.Rdata")

library(Hmisc)
library(plotrix)

lper <- 2000
ylab <- expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) )
peryear <- FALSE

## Plot bars for the periods
# pdf("../fig/bal_comparison_barplot_holocene_holoLU2.pdf", width=8, height=6 )
  if (peryear){
    par( las=1, xaxs="i", mar=c(5.1, 4.1, 4.1, 5.1) )  # , mgp=c(3.8,1,0)
  } else {
    par( las=1, xaxs="i", mar=c(5.1, 4.1, 4.1, 5.1) )
  }
  ylim <- c(-200,200)
  xlim <- c(1.2,26)

  mybar1 <- barplot( 
                    t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) ),
                    beside = TRUE,
                    ylim=ylim, xlim=xlim,
                    col=c("springgreen3","dodgerblue3","darkgoldenrod2"),
                    space=c(0.2,1.5),
                    border=TRUE, axes=FALSE,
                    xlab="period (ka BP)", ylab="C balance per 2-ka period (PgC)",           
                    names.arg=rownames(dc_land_outarr)
                    )

  axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  axis(4,lwd=1.5,labels=format( seq(ylim[1], ylim[2], by=50)/2000, digits=1),at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
  mtext( expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) ), side=4, line=3.5, las=3 )
  
  abline(0,0)

  errbar(
         mybar1,
         t( cbind( dc_land_outarr$mean,                     pt_outarr$mean_yu,                   dc_remainder_outarr$mean_yu ) ),
         t( cbind( dc_land_outarr$mean - dc_land_outarr$sd, pt_outarr$mean_yu - pt_outarr$sd_yu, dc_remainder_outarr$mean_yu - dc_remainder_outarr$sd_yu ) ),
         t( cbind( dc_land_outarr$mean + dc_land_outarr$sd, pt_outarr$mean_yu + pt_outarr$sd_yu, dc_remainder_outarr$mean_yu + dc_remainder_outarr$sd_yu ) ),
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
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31u, col=add_alpha("grey50", 0.5)    ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl,         rep(0,5), right[3,]+offl+width,  -fluc_per_holocene$hyde31,  col="grey50"                    ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32u, col=add_alpha("burlywood3",0.5) ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+width,   rep(0,5), right[3,]+offl+2*width,-fluc_per_holocene$hyde32,  col="burlywood3"                ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10,    col=add_alpha("khaki3",0.5)     ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  rect( right[3,]+offl+2*width, rep(0,5), right[3,]+offl+3*width,-fluc_per_holocene$kk10d,   col="khaki3"                    ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
  
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

# dev.off()


# pdf("fig/cbal_ocean_barplot_holocene_holoLU2.pdf", width=8, height=6 )
#   par( las=1 )
#   ylim <- c(-200,50)
# 
#   mybar1 <- barplot( 
#                     t(dc_ocean_outarr$mean),
#                     ylim=ylim,
#                     col=c("dodgerblue"),
#                     border=TRUE, axes=FALSE,
#                     xlab="period (ka BP)", ylab="C balance per 2-ka period (PgC)", 
#                     names.arg=rownames(dc_ocean_outarr)
#                     )
# 
#   axis(2,lwd=1.5,at=seq(ylim[1],ylim[2],by=50));           axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
#   axis(4,lwd=1.5,labels=F,at=seq(ylim[1],ylim[2],by=50));  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
#   abline(0,0)
# 
#   errbar(
#          mybar1,
#          t(dc_ocean_outarr$mean),
#          t(dc_ocean_outarr$mean-dc_ocean_outarr$sd),
#          t(dc_ocean_outarr$mean+dc_ocean_outarr$sd),
#          add=TRUE
#          )
# 
#   ## add grey rectangles 
#   left <- mybar1 - 0.6
#   right <- mybar1 + 0.6
#   rect( left[1], ylim[1], right[1], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#   rect( left[3], ylim[1], right[3], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#   rect( left[5], ylim[1], right[5], ylim[2], col=rgb(0,0,0,0.1), border=NA )
# 
# dev.off()

