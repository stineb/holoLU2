load("data/dc_remainder_holocene.Rdata")
load("data/pt_outarr.Rdata")
load("data/fluc_per.Rdata")
load("data/dc_elsig.Rdata")

library(Hmisc)
library(plotrix)

myarrow <- function( base, length, width, direction, ... ){

  if (direction=="right"){
    xedge <- base[1] + 0.9*length
    xtip <- base[1] + length
    yedge <- base[2]
    ytip <- base[2]
    xbase <- base[1]
    polygon( x = c( xbase, xedge, xtip, xedge, xbase ), y = c( yedge-width/2, yedge-width/2, yedge, yedge+width/2, yedge+width/2 ), ... )    
  } else if (direction=="left"){
    xedge <- base[1] - 0.9*length
    xtip <- base[1] - length
    yedge <- base[2]
    ytip <- base[2]
    xbase <- base[1]
    polygon( x = c( xbase, xedge, xtip, xedge, xbase ), y = c( yedge-width/2, yedge-width/2, yedge, yedge+width/2, yedge+width/2 ), ... )        
  }

}


lper <- 2000
ylab <- expression( paste( "Average C balance change per period (PgC yr"^{-1},")" ) )
peryear <- FALSE

## Plot bars for the periods
pdf("fig/fig2vB_PAGES.pdf", width=8, height=10 )

  par( las=1, xaxs="i", yaxs="i", mar=c(5.1, 4.1, 4.1, 5.1) )
  xlim <- c(-150,150)
  ylim <- c(-3.5,3.2)

  out <- rev( t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) )[,4])[2:3]
  
  mybar1 <- barplot( 
                    out,
                    beside = TRUE,
                    horiz = TRUE,
                    xlim=xlim,
                    ylim=ylim,
                    axes=FALSE,
                    col=c("dodgerblue3","springgreen3"), #,"darkgoldenrod2"
                    border=FALSE
                    # ylim=ylim, 
                    # space=c(0.2,1.5),
                    # xlab="period (ka BP)", ylab="C balance per 2-ka period (PgC)",           
                    # names.arg=rownames(dc_land_outarr)[4],
                    )
  
  ## define geometry/coordinate points
  right <- pt_outarr$mean_yu[4]
  # residual <- -dc_remainder_outarr$mean_yu[4]
  residual <- pt_outarr$mean_yu[4] - dc_land_outarr$mean[4]
  ycenter1 <- mybar1[1,1] - (mybar1[2,1] - mybar1[1,1]) - 0.1
  ycenter2 <- mybar1[1,1] - 0.5*(mybar1[2,1] - mybar1[1,1]) - 0.1 - 1.0
  ycenter3 <- mybar1[1,1] - 1.5*(mybar1[2,1] - mybar1[1,1]) - 0.1 - 1.0
  height <- 1

  ## background shading for lower coordinate system  
  # rect( xlim[1], mybar1[1,1]-height/2-0.1, xlim[2], ylim[2], border = NA, col = add_alpha("black",0.1))
  rect( xlim[1], mybar1[1,1]+height-0.4, xlim[2], ylim[2], border = NA, col = add_alpha("black",0.1))
  rect( xlim[1], ylim[1], xlim[2],1.28, border = NA, col = add_alpha("darkgoldenrod2",0.2))
  
  # ## top-down and bottom up text
  # text( -45, 1, "top-down", font=2, col = "grey60", cex = 1.5, srt=90 )
  # text( -43, -2.5, "bottom-up", font=2, col =add_alpha("darkgoldenrod4",0.4), cex = 1.5, srt=90 )
  
  ## box-arrows illustrating source and sink
  myarrow( base=c(0,2.8), length=40, width=0.5, direction="right", border=NA, col="grey80" )
  text( 5, 2.8, "sink", adj = c(0,0.5), font=1, col = "grey60", cex = 1.2 )

  myarrow( base=c(0,2.8), length=40, width=0.5, direction="left", border=NA, col="grey80" )
  text( -5, 2.8, "source", adj = c(1,0.5), font=1, col = "grey60", cex = 1.2 )

  ## axis on top
  axis( 3, lwd=1.5, at=seq(xlim[1],xlim[2],by=50) )
  axis( 3, at=seq( xlim[1],xlim[2], by=10 ), labels=F, tck=-0.01 )
  lines( c( 0, 0 ), c( ylim[2], mybar1[1,1]-height/2-0.1), lwd=1.5 )
  mtext( "C balance (PgC)", side = 3, line = 2.5, adj = c(0.5), cex=1.2 )
  
  ## uncertainty lines (whiskers)
  lines( c( out[1]-pt_outarr$sd_yu[4],   out[1]+pt_outarr$sd_yu[4] ),   c( mybar1[1,1], mybar1[1,1] ) )
  lines( c( out[2]-dc_land_outarr$sd[4], out[2]+dc_land_outarr$sd[4] ), c( mybar1[2,1], mybar1[2,1] ) )

  ## LUC emissions
  hyde <- fluc_per_holocene$hyde32[4]
  kk10 <- fluc_per_holocene$kk10[4]
  
  ## residual bar and whisker
  # rect( right-residual, ycenter1-height/2, right, ycenter1+height/2, border = NA, col="darkgoldenrod2" )
  # lines( c( right-residual-dc_remainder_outarr$sd_yu[4], right-residual+dc_remainder_outarr$sd_yu[4] ), c( ycenter1, ycenter1 ) )
  # myarrow( base=c(right,ycenter1), length=residual, width=height, direction="left", border=NA, col="grey80" )
  # text( 5, 2.8, "sink", adj = c(0,0.5), font=2, col = "grey60", cex = 1.5 )
  
  ## LUC bars
  rect( -hyde, ycenter2-height/2, 0, ycenter2+height/2, border = NA, col="orchid" )  ## HYDE 3.2
  rect( -kk10, ycenter3-height/2, 0, ycenter3+height/2, border = NA, col="turquoise3" )  ## KK10

  ## "natural sources" bars
  # rect( dc_land_outarr$mean[4], ycenter2-height/2, right-hyde, ycenter2+height/2, border = NA, col="grey80" )  ## HYDE 3.2
  # rect( dc_land_outarr$mean[4], ycenter3-height/2, right-kk10, ycenter3+height/2, border = NA, col="grey80" )  ## KK10
  myarrow( base=c(-hyde,ycenter2), length=residual-hyde, width=height, direction="left", border=NA, col="grey80" )
  myarrow( base=c(-kk10,ycenter3), length=residual-kk10, width=height, direction="left", border=NA, col="grey80" )
  
  ## line at 0 of LUC bars (and extension)
  lines( c( 0, 0 ), c( ycenter1+height/2+0.1,  mybar1[1,1]+height/2+0.1), lty=3 )
  lines( c( 0, 0 ), c( ycenter1+height/2+0.1, ylim[1] ), lwd=1.5 )
  
  # ## line at end point of total land and residual bar
  # lines( c( -residual, -residual ), c( ylim[1], mybar1[2,1]+height/2+0.1 ), lty=3 )

  ## labels  
  text( rep(30,3), c( mybar1[,1] ), c("peat", "total terrestrial"), adj=c(0,0.5), font = 1, cex = 1.2 ) #expression( paste( "ice core CO"[2], " and ", delta, ""^"13", "C"))
  text( rep(10,2), c( ycenter2, ycenter3 ), c("HYDE 3.2", "KK10"), adj=c(0,0.5), font = 1, cex = 1.2 )
  text( c(86-105,87-105), c( ycenter2, ycenter3 ), c("LULC", "LULC"), adj=c(0,0.5), font = 1 )
  text( c(-100,-100), c( ycenter2, ycenter3 ), c("non-peat natural sources", "non-peat natural sources"), adj=c(0,0.5), font = 1 )
  
  ## top-down and bottom up text
  text( -140, 1.6, "global C budget constraints", font=2, col = "grey60", cex = 1.2, srt=0, adj=c(0,0.5) )
  text( -140, -3.2, "process attribution", font=2, col =add_alpha("darkgoldenrod4",0.4), cex = 1.2, srt=0, adj=c(0,0.5) )
  
  # ## axis at the bottom
  # axis( 1, lwd=1.5, at=seq(right, right-150, by=-50 ), labels=as.character( seq( 0, 150, by=50 ) ) )
  # axis( 1, at=seq(right, right-150, by=-10 ), labels=F, tck=-0.01 )
  # mtext( expression( paste( "CO"[2], " emissions (PgC)")), side = 1, line = 2.5, adj = c(0.5), cex = 1.2 )
 
dev.off()