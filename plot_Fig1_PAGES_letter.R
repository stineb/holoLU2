##--------------------------------------------------------------------------
## example plot producing figure like Fig. 1 and S1 in Stocker et al., 2017
##--------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

source("~/.Rprofile")

load( "data/dc_terr_bauska_lastmill_periods.Rdata" )
load( "data/dc_remainder_lastmill.Rdata" )
load( "data/pt_outarr_lastmill.Rdata" ) 
load( "data/fluc_per.Rdata" )

library(Hmisc)
library(plotrix)

## load LUC emissions time series
fluc <- read_csv( "data/luc_co2_emissions_stocker17pnas.csv", comment="##", col_types="idddddddddddd" )

## load land use area time series
aluc_hyde31 <- read_table( "data/crop_hyde31_final_halfdeg_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "crop") ) %>%
               left_join( read_table( "data/past_hyde31_final_halfdeg_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "past") ), by="year" ) %>%
               mutate( tot = (crop + past) )

aluc_hyde32 <- read_table( "data/crop_hyde32_baseline_halfdeg_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "crop") ) %>%
               left_join( read_table( "data/past_hyde32_baseline_halfdeg_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "past") ), by="year" ) %>%
               mutate( tot = (crop + past) )

# aluc_kk10 <-  read_table( "data/crop_KK11_halfdeg_hydeslices_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "crop") ) %>%
#               left_join( read_table( "data/past_KK11_halfdeg_hydeslices_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "past") ), by="year" ) %>%
#               mutate( tot = (crop + past) )

aluc_kk10 <-  read_table( "data/test_kk10_hydeslices_GLOB.txt", col_types = "id", comment="#", col_names = c("year", "tot") )


print("plotting ... ")

## Aggregate to 'binwidth'-year periods (mean)
binwidth <- 5
bins <- seq( from=-10000, to=2000, by=binwidth )

# bincentres <- bins[1:(length(bins)-1)] + (bins[2]-bins[1])/2
 
fluc_agg <- fluc %>%  mutate( inbin  = cut( as.numeric(year), breaks = bins ) ) %>% 
                      group_by( inbin ) %>% 
                      summarise_all( mean ) %>%
                      filter( !is.na(inbin) )

      # ## add row: normalised VPD
      # fluc_agg <- fluc %>% group_by( inbin ) %>% 
      #                    summarise_all( mean ) # %>%
      #                    # complete( inbin, fill = list( vpd  = NA ) ) %>% 
      #                    # dplyr::select( vpd )
      # # tmp <- unlist( tmp )[1:(length(fvarbins)-1)]

## Get period delineations
period_margins_lastmill <- read.csv( 'data/periods_lastmill.csv' )$period_margins
periodsName <- paste( 
  as.character( period_margins_lastmill )[1:length(period_margins_lastmill)-1],
  "-",
  as.character( period_margins_lastmill )[2:length(period_margins_lastmill)],
  sep=""
  )

periodsBP <- read.csv( 'data/periods_holocene.csv' )$periodsBP
periodsAD <- periodsBP + 1950
periodsName <- paste( 
  as.character( -periodsBP*1e-3 )[1:length(periodsBP)-1],
  "-",
  as.character( -periodsBP*1e-3 )[2:length(periodsBP)],
  sep=""
  )
period_margins_holo <- periodsAD


##--------------------------------
## Total landuse area
##--------------------------------
  ## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
  magn <- 0.6
  ncols <- 4
  nrows <- 1
  widths <- c(7,4,0.6,3.2) 
  heights <- 5.1
  widths <- widths * 1.0
  heights <- heights * 1.0

  widths  <- widths  * magn
  heights <- heights * magn

  pdf( "fig/plot_aluc_PAGESletter.pdf", width=sum(widths),height=sum(heights) )

    panel <- layout(
                    matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
                    widths=widths,
                    heights=heights,
                    TRUE
                    )
    # layout.show(panel)

    ##--------------------------------
    ## HOLOCENE
    ##--------------------------------
      ylim <- c(-2,60)
      xlim <- c(-10000,1700)
      par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,4.3,2,0) )
      plot( c(-10000,2000), ylim, type="n", ylab=expression( paste( "total agricultural area (10"^6 ," km"^2, ")")), xlab="age (ka BP)", xlim=xlim, ylim=ylim, axes=FALSE )

      rect(xleft = xlim[1], xright = 1850, ybottom = ylim[1], ytop = ylim[2], col=rgb(0,0,0,0.1), border = NA )

      # axis( 1, lwd=1.25 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
      axis( 1, lwd=1.25, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 1, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
      # axis( 3, lwd=1.25, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
      axis( 2, lwd=1.25 );  axis( 2, at=seq(ylim[1],ylim[2],by=2), labels=F, tck=-0.01 )
      # axis( 4, lwd=1.25, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=2), labels=F, tck=-0.01 )
      box( lwd=1.25 )

      lines( aluc_hyde31$year, aluc_hyde31$tot * 1e-12, col="orchid", lwd=2.5  )
      lines( aluc_kk10$year, aluc_kk10$tot * 1e-12, col="turquoise3", lwd=2.5 )

      legend( "topleft", c( "HYDE 3.2", "KK10" ), col=c("orchid","turquoise3"), bty="n", lty=1, lwd=2 )

      mtext( "a", side = 3, adj = 0, font = 2, line = 0.5 )
      
    ##--------------------------------
    ## LAST MILLENNIUM
    ##--------------------------------
      xlim <- c(1700,2000)
      par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,2,1), xpd=TRUE )
      plot( range(fluc_agg$year), ylim, type="n", ylab="", xlab="year CE", xlim=xlim, ylim=ylim, axes=FALSE )

      rect(xleft = xlim[1], xright = 1850, ybottom = ylim[1], ytop = ylim[2], col=rgb(0,0,0,0.1), border = NA )
      rect(xleft = 1920, xright = xlim[2], ybottom = ylim[1], ytop = ylim[2], col=rgb(0,0,0,0.1), border = NA )

      axis( 1, lwd=1.25 );  axis( 1, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01 )
      # axis( 3, lwd=1.25, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01 )
      # axis( 2, lwd=1.25, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=2), labels=F, tck=-0.01 )
      axis( 4, lwd=1.25, labels=FALSE, ylab="cumulative C uptake (PgC)" );  axis( 4, at=seq(ylim[1],ylim[2],by=2), labels=F, tck=-0.01 )
      box( lwd=1.25 )

      lines( aluc_hyde31$year, aluc_hyde31$tot * 1e-12, col="orchid", lwd=2.5  )
      lines( aluc_kk10$year, aluc_kk10$tot * 1e-12, col="turquoise3", lwd=2.5 )

      lines( c(1850,2010), rep( aluc_hyde31$tot[ which( aluc_hyde31$year==1850 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )
      lines( c(1850,2010), rep( aluc_kk10$tot[ which( aluc_kk10$year==1850 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )

      lines( c(1920,2010), rep( aluc_hyde31$tot[ which( aluc_hyde31$year==1920 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )
      lines( c(1920,2010), rep( aluc_kk10$tot[ which( aluc_kk10$year==1920 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )
      
      # lines( aluc_kk10$year[ which( aluc_kk10$year <= 1850 )], aluc_kk10$tot[ which( aluc_kk10$year <= 1850 )] * 1e-12, col="turquoise3", lwd=2.5 )
      # lines( aluc_kk10$year, aluc_kk10$tot * 1e-12, col="turquoise3", lwd=2.5, lty=2 )

    ##--------------------------------
    ## arrows for expansion since 1850
    ##--------------------------------
      par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,2,0) )
      plot( c(0,0.9), ylim, axes=FALSE, type = "n", xlab = "", ylab = "" )
      
      arrows( 0.3, aluc_hyde31$tot[ which( aluc_hyde31$year==1850 ) ] * 1e-12, 0.3, aluc_hyde31$tot[ which( aluc_hyde31$year==1920 ) ] * 1e-12, col="orchid", lwd=2.5, length = 0.05 )
      arrows( 0.6, aluc_kk10$tot[ which( aluc_kk10$year==1850 ) ] * 1e-12, 0.6, aluc_kk10$tot[ which( aluc_kk10$year==1920 ) ] * 1e-12, col="turquoise3", lwd=2.5, length = 0.05 )

      lines( c(0,0.3), rep( aluc_hyde31$tot[ which( aluc_hyde31$year==1850 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )
      lines( c(0,0.6), rep( aluc_kk10$tot[ which( aluc_kk10$year==1850 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )

      lines( c(0,0.3), rep( aluc_hyde31$tot[ which( aluc_hyde31$year==1920 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )
      lines( c(0,0.6), rep( aluc_kk10$tot[ which( aluc_kk10$year==1920 ) ] * 1e-12, 2 ), lty=3, col=rgb(0,0,0,0.5) )

    ##--------------------------------
    ## Barplot
    ##--------------------------------
      par( mar=c(4.8,1,2,4), xpd=FALSE )

      ylim <- c(-10,60)
      xlim <- c(0,5)

      mybar1 <- barplot( 
                        -rev(t( cbind( dc_lastmill_outarr$mean, pt_outarr$mean_lhnfix ) )[,7]),
                        beside = TRUE,
                        ylim=ylim, xlim=xlim,
                        col=rev(c("springgreen3","dodgerblue3")),
                        space=c(0.2,2.5),
                        border=FALSE,
                        axes=FALSE,
                        xlab="", ylab = "",
                        names.arg=""
                        )

      # axis( 2, lwd=1.5, at=seq(ylim[1], ylim[2], by=10 ));  axis( 2, at=seq(ylim[1], ylim[2], by=2 ), labels=F, tck=-0.01 )
      axis( 4, lwd=1.5, labels=TRUE, at=seq(ylim[1], ylim[2], by=10 ));  axis( 4, at=seq(ylim[1], ylim[2], by=2 ), labels=F, tck=-0.01 )
      mtext( expression( paste( "C balance (PgC)" ) ), side=4, line=2.5, las=3, cex = 0.7 )
      
      errbar(
             rev(mybar1),
             -t( cbind( dc_lastmill_outarr$mean,   pt_outarr$mean_lhnfix ) )[,7],
             -t( cbind( dc_lastmill_outarr$mean - dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix - pt_outarr$sd_lhnfix ) )[,7],
             -t( cbind( dc_lastmill_outarr$mean + dc_lastmill_outarr$sd,   pt_outarr$mean_lhnfix + pt_outarr$sd_lhnfix ) )[,7],
             add=TRUE, pch=NA, cap=0
             )

      ## add bars for fluc
      offl  <- -3
      width <- 0.4
      rect( mybar1[1]+offl+width,   rep(0,5), mybar1[1]+offl+2*width, fluc_per_lastmill$hyde32[7], border = NA,  col="orchid"                 )         #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )
      rect( mybar1[1]+offl+2*width+0.05, rep(0,5), mybar1[1]+offl+3*width+0.05, fluc_per_lastmill$kk10[7]  , border = NA,    col=add_alpha("turquoise3",1.0)      ) #, pch=8, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0 )

      abline(h=0, lwd=1.5)
      
      # ## add points for simulated peat C balance
      # par(new=TRUE)
      # plotCI( mybar1[2], y=pt_outarr$lpx[7], uiw=0.0, pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )
      # par(new=TRUE)
      # plotCI( mybar1[1], y=dc_remainder_outarr$mean_lpx[7], uiw=dc_remainder_outarr$sd_lpx[7], col="darkgoldenrod4", pch=16, ylim=ylim, xlim=xlim, axes=FALSE, ylab="", xlab="", cex=1.0, lwd=1 )

      legend(
             "topleft",
             c("total terrestrial","peat"),
             fill=c("springgreen3","dodgerblue3"),
             bty="n",
             box.lwd=0, 
             border = NA
             )
      par( xpd=TRUE )
      mtext( "LULC emissions", side = 1, line = 1.0, adj = 0.2, cex = 0.7 )
      legend(
             "bottomleft",
             c( "HYDE 3.2", "KK10" ),
             bty="n",
             fill=c("orchid","turquoise3"),
             box.lwd=0,
             inset = c(0,-0.28),
             border = NA
             )

      mtext( "b", side = 3, adj = 0, font = 2, line = 0.5 )
      
  dev.off()





##--------------------------------
## Cumulative LUC emissions (NOT USED)
##--------------------------------
  ## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
  magn <- 0.8
  ncols <- 2
  nrows <- 1
  widths <- c(7,4) 
  heights <- 5
  widths <- widths * 1.0
  heights <- heights * 1.0

  widths  <- widths  * magn
  heights <- heights * magn

  pdf( "fig/plot_fluc_PAGESletter.pdf", width=sum(widths),height=sum(heights) )

    panel <- layout(
                    matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
                    widths=widths,
                    heights=heights,
                    TRUE
                    )
    # layout.show(panel)

    ##--------------------------------
    ## HOLOCENE
    ##--------------------------------
      ylim <- c(-100,400)
      xlim <- c(-10000,1700)
      par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,4.3,1,0) )
      plot( c(-10000,2000), ylim, type="n", ylab="cumulative LULC emissions (PgC)", xlab="age (ka BP)", xlim=xlim, ylim=ylim, axes=FALSE )

      # axis( 1, lwd=1.25 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
      axis( 1, lwd=1.25, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 1, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
      # axis( 3, lwd=1.25, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
      axis( 2, lwd=1.25 );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
      # axis( 4, lwd=1.25, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
      box( lwd=1.25 )

      lines( fluc_agg$year, fluc_agg$hyde32_cum, col="orchid", lwd=2.5  )
      # polygon( c(fluc_agg$year, rev(fluc_agg$year)), c(fluc_agg$hyde32_cum, rev(fluc_agg$hyde32u_cum)), col=add_alpha("orchid", 0.5), border=NA )

      lines( fluc_agg$year, fluc_agg$kk10_cum, col="turquoise3", lwd=2.5 )

      legend( "topleft", c( "HYDE 3.2", "KK10" ), col=c("orchid","turquoise3"), bty="n", lty=1, lwd=2 )


    ##--------------------------------
    ## LAST MILLENNIUM
    ##--------------------------------
      xlim <- c(1700,2000)
      par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,1,1) )
      plot( range(fluc_agg$year), ylim, type="n", ylab="", xlab="year CE", xlim=xlim, ylim=ylim, axes=FALSE )

      axis( 1, lwd=1.25 );  axis( 1, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01 )
      # axis( 3, lwd=1.25, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01 )
      # axis( 2, lwd=1.25, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
      axis( 4, lwd=1.25, labels=TRUE, ylab="cumulative C uptake (PgC)" );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
      box( lwd=1.25 )

      lines( fluc_agg$year, fluc_agg$hyde32_cum, col="orchid", lwd=2.5  )
      # polygon( c(fluc_agg$year, rev(fluc_agg$year)), c(fluc_agg$hyde32_cum, rev(fluc_agg$hyde32u_cum)), col=add_alpha("orchid", 0.5), border=NA )

      lines( fluc_agg$year, fluc_agg$kk10_cum, col="turquoise3", lwd=2.5 )

  dev.off()



##--------------------------------
## Summary numbers
##--------------------------------
print( paste( "Terrestrial C source in 1850-1920:", format( dc_lastmill_outarr$mean[7], digits=2 ) ) )
print( paste( "Average annual C loss in 1850-1920:", format( dc_lastmill_outarr$mean[7] / 70, digits=2 ) ) )
print( paste( "Unexplained source after KK10:", format( -dc_remainder_outarr$mean[7] - fluc_per_lastmill$kk10[7], digits=2 ) ))
print( paste( "difference in cumulative "))

