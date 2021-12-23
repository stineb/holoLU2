# load("data/dc_remainder_holocene.Rdata")
# load("data/pt_outarr.Rdata")
# load("data/fluc_per.Rdata")
# load("data/dc_elsig.Rdata")
# 
# library(Hmisc)
# library(plotrix)

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
pdf("fig/fig2vA_PAGES.pdf", width=8.5, height=7.1 )

  par( las=1, xaxs="i", yaxs="i", mar=c(5.1, 4.1, 4.1, 5.1) )
  xlim <- c(-50,150)
  ylim <- c(0.2,3.5)

  out <- rev( t( cbind( dc_land_outarr$mean, pt_outarr$mean_yu, dc_remainder_outarr$mean_yu ) )[,4])[2:3]
  
  dcland <- dc_land_outarr$mean[4]
  residual <- pt_outarr$mean_yu[4] - dc_land_outarr$mean[4]
  dcpeat <- pt_outarr$mean_yu[4]
  ycenter0 <- 3
  ycenter1 <- 2
  ycenter2 <- 1+0.2
  ycenter3 <- 0.4+0.2
  height <- 0.5
  
  plot( xlim, ylim, type="n", axes = FALSE, xlab="", ylab="" )
  
  ## background shading for lower coordinate system  
  rect( xlim[1], ycenter1+0.5, xlim[2], ylim[2], border = NA, col = add_alpha("black",0.1))
  rect( xlim[1], ylim[1], xlim[2], ycenter1+height/2+0.2, border = NA, col = add_alpha("darkgoldenrod2",0.2))
  
  rect( dcland, ycenter0-height/2, 0, ycenter0+height/2, border = NA, col="springgreen3" )  ## total terrestrial
  rect( 0, ycenter1-height/2, dcpeat, ycenter1+height/2, border = NA, col="dodgerblue3" )   ## peat
  
  # 
  # mybar1 <- barplot( 
  #                   out,
  #                   beside = TRUE,
  #                   horiz = TRUE,
  #                   xlim=xlim,
  #                   ylim=ylim,
  #                   axes=FALSE,
  #                   col=c("dodgerblue3","springgreen3"), #,"darkgoldenrod2"
  #                   border=FALSE
  #                   # ylim=ylim, 
  #                   # space=c(0.2,1.5),
  #                   # xlab="period (ka BP)", ylab="C balance per 2-ka period (PgC)",           
  #                   # names.arg=rownames(dc_land_outarr)[4],
  #                   )
  
  ## define geometry/coordinate points
  right <- dcpeat
  # ycenter2 <- mybar1[1,1] - 0.5*(mybar1[2,1] - mybar1[1,1]) - 0.1 - 1.0
  # ycenter3 <- mybar1[1,1] - 1.5*(mybar1[2,1] - mybar1[1,1]) - 0.1 - 1.0

  
  # ## top-down and bottom up text
  # text( -45, 1, "top-down", font=2, col = "grey60", cex = 1.5, srt=90 )
  # text( -43, -2.5, "bottom-up", font=2, col =add_alpha("darkgoldenrod4",0.4), cex = 1.5, srt=90 )
  
  # ## box-arrows illustrating source and sink
  # myarrow( base=c(0,2.8), length=40, width=0.5, direction="right", border=NA, col="grey80" )
  # text( 5, 2.8, "sink", adj = c(0,0.5), font=1, col = "grey60", cex = 1.2 )
  # 
  # myarrow( base=c(0,2.8), length=40, width=0.5, direction="left", border=NA, col="grey80" )
  # text( -5, 2.8, "source", adj = c(1,0.5), font=1, col = "grey60", cex = 1.2 )
  
  ## axis on top
  axis( 3, lwd=1.5, at=seq(xlim[1],xlim[2],by=50), cex.axis=1.2 )
  axis( 3, at=seq( xlim[1],xlim[2], by=10 ), labels=F, tck=-0.01 )
  lines( c( 0, 0 ), c( ylim[2], ycenter1-height/2-0.1), lwd=1.5 )
  mtext( "Terrestrial C balance (3-5 ka BP, PgC)", side = 3, line = 2.5, adj = c(0.2), cex=1.5 )
  
  ## uncertainty lines (whiskers)
  lines( c( dcpeat-pt_outarr$sd_yu[4],   dcpeat+pt_outarr$sd_yu[4] ),   c( ycenter1, ycenter1 ) )
  lines( c( dcland-dc_land_outarr$sd[4], dcland+dc_land_outarr$sd[4] ), c( ycenter0, ycenter0 ) )

  ## LUC emissions
  hyde <- fluc_per_holocene$hyde32[4]
  kk10 <- fluc_per_holocene$kk10[4]
  
  ## residual bar and whisker
  # rect( right-residual, ycenter1-height/2, right, ycenter1+height/2, border = NA, col="darkgoldenrod2" )
  # lines( c( right-residual-dc_remainder_outarr$sd_yu[4], right-residual+dc_remainder_outarr$sd_yu[4] ), c( ycenter1, ycenter1 ) )
  # myarrow( base=c(right,ycenter1), length=residual, width=height, direction="left", border=NA, col="grey80" )
  # text( 5, 2.8, "sink", adj = c(0,0.5), font=2, col = "grey60", cex = 1.5 )
  
  ## LUC bars
  rect( right-hyde,     ycenter2-height/2, right, ycenter2+height/2, border = NA, col="orchid" )  ## HYDE 3.2
  rect( right-kk10,     ycenter3-height/2, right, ycenter3+height/2, border = NA, col="turquoise3" )  ## KK10

  ## "natural sources" bars
  # rect( dc_land_outarr$mean[4], ycenter2-height/2, right-hyde, ycenter2+height/2, border = NA, col="grey80" )  ## HYDE 3.2
  # rect( dc_land_outarr$mean[4], ycenter3-height/2, right-kk10, ycenter3+height/2, border = NA, col="grey80" )  ## KK10
  myarrow( base=c(right-hyde,ycenter2), length=right-hyde-dc_land_outarr$mean[4], width=height, direction="left", border=NA, col="grey80" )
  myarrow( base=c(right-kk10,ycenter3), length=right-kk10-dc_land_outarr$mean[4], width=height, direction="left", border=NA, col="grey80" )
  
  ## line at 0 of LUC bars (and extension)
  lines( c( right, right ), c( ycenter2+height/2+0.1, ycenter1+height/2+0.1), lty=3 )
  lines( c( right, right ), c( ycenter2+height/2+0.1, ycenter3-height/2-0.1 ), lwd=1.5 )
  
  ## line at end point of total land and residual bar
  lines( c( dcland, dcland ), c( ycenter3-height/2-0.1, ycenter0+height/2+0.1 ), lty=3 )

  ## labels  
  text( rep(30,3), c( ycenter1, ycenter0 ), c("peat uptake", "total net change"), adj=c(0,0.5), font = 1, cex = 1.5 ) #expression( paste( "ice core CO"[2], " and ", delta, ""^"13", "C"))
  text( rep(110,2), c( ycenter2, ycenter3 ), c("HYDE 3.2", "KK10"), adj=c(0,0.5), font = 1, cex = 1.5 )
  text( c(86,86), c( ycenter2, ycenter3 ), c("LULC", "LULC"), adj=c(0,0.5), font = 1, cex = 1.2 )
  text( c(20,20), c( ycenter2, ycenter3 ), c("other natural sources", "other natural sources"), adj=c(0,0.5), font = 1, cex = 1.2 )
  
  ## top-down and bottom up text
  text( -45, ycenter0, "Top-down", font=2, col = "grey60", cex = 1.5, srt=90, adj=c(0.5,0.5) )
  text( -35, ycenter0, expression( paste( "from CO"[2], " / ", delta, ""^"13", "C")), font=1, col = "grey60", cex = 1.2, srt=90, adj=c(0.5,0.5) )
  text( -45, ycenter2-0.2, "Bottom-up", font=2, col =add_alpha("darkgoldenrod4",0.4), cex = 1.5, srt=90, adj=c(0,0.5) )
  text( -35, ycenter2-0.2, "process attribution", font=1, col =add_alpha("darkgoldenrod4",0.4), cex = 1.2, srt=90, adj=c(0.1,0.5) )
  
  # ## axis at the bottom
  # axis( 1, lwd=1.5, at=seq(right, right-150, by=-50 ), labels=as.character( seq( 0, 150, by=50 ) ) )
  # axis( 1, at=seq(right, right-150, by=-10 ), labels=F, tck=-0.01 )
  # mtext( expression( paste( "CO"[2], " emissions (PgC)")), side = 1, line = 2.5, adj = c(0.5), cex = 1.2 )
 
dev.off()