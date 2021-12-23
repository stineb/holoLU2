library(plotrix)

## Yu Holocene data
load( "data/pt_yu_holocene.Rdata" )
load( "data/pt_yu_lastmill.Rdata" )
load( 'data/dc_terr_elsig_holocene.Rdata')
load( "data/dc_remainder_holocene.Rdata")
load( "data/fluc_holoLU2.Rdata" )
load( "data/dc_merged.Rdata")
source("/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/add_alpha.R")
source('~/.Rprofile')

add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

dstep <- 10

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


## LPX data
trace129 <- list()
filn <- "data/peatglobnep_trace21_129.dat"
col.names <- c( "year", "peatglobnep_global", "peatglobnep_maskedby_yu", "peatglobnep_maskedby_yu_north" )  ## this is used for time series
trace129$globnep <- read.table( filn, col.names=col.names )

filn <- "data/peatc_trace21_129.dat"
col.names <- c( "year", "peatc_global", "peatc_90S_30S", "peatc_30S_30N", "peatc_30N_90N", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat", "peatc_maskedby_lpxpres" )
trace129$c <- read.table( filn, col.names=col.names )

## offset relative to first year in elsig data (df_cum_land_uptake$year[1]=-8995)
firstyear <- df_cum_land_uptake$year[1]
off <- df_cum_pt_mean$median[ which(df_pt_mean$year==firstyear)]
off_lpx <- trace129$c$peatc_global[ which(trace129$c$year==firstyear)]*1e-15

## cut all data to common time range: first year determined by elsig data (-8995 AD)
startyr <- df_cum_land_uptake$year[1]
# trace129$globnep <- trace129$globnep[ which(trace129$globnep$year==startyr):dim(trace129$globnep)[1],]
df_cum_land_uptake   <- df_cum_land_uptake[ which(df_cum_land_uptake$year==startyr):dim(df_cum_land_uptake)[1],]

## CO2 data
epica <- read.table('/alphadata01/bstocker/data/co2/cCO2_epica_dome_c_monnin04.txt', skip=92, header = TRUE )
epica$year <- 1950 - epica$Age
wais <- read.table('/alphadata01/bstocker/data/co2/cCO2_wais_divide_ahn12.txt', skip=100, header = TRUE )
law <- read.table('/alphadata01/bstocker/data/co2/cCO2_law_dome_etheridge10.txt', skip=163, header = TRUE )

## CO2 increase between 7 and 5 ka
out <- smooth.spline( epica$year, epica$CO2, spar=0.5 )
co2_7ka <- out$y[ which.min(abs(out$x+5050)) ]
co2_5ka <- out$y[ which.min(abs(out$x+3050)) ]
print(paste("CO2 increase between 7 and 5 ka", co2_5ka - co2_7ka))

## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
magn <- 0.8
ncols <- 2
nrows <- 2
widths <- c(7,5) 
heights <- c(3.4,3.9)
widths <- widths * 1.0
heights <- heights * 1.0

widths  <- widths  * magn
heights <- heights * magn

pdf( "fig/tseries_holocene.pdf", width=sum(widths),height=sum(heights) )

  panel <- layout(
                  matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
  # layout.show(panel)

  ##--------------------------------
  ## FIRST PLOT: CUMULATIVE CHANGES
  ##--------------------------------
  ## HOLOCENE
  ##--------------------------------
    ylim <- c(0,650)
    xlim <- c(-10000,periodsAD[6])
    par( las=1 , xaxs="i", yaxs="i", mar=c(1,4.3,1,1) )
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )

    axis( 1, lwd=1.75, labels=FALSE );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75 );                axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    ## add period margins to plot
    # abline( v=period_margins_holo )
    # print( period_margins_holo )
    rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

    dc_terr_startyr <- df_cum_land_uptake$year[1]

    ## CO2
    par(new=TRUE)
    plotCI( epica$year, epica$CO2, uiw=epica$sigma, add=FALSE, pch=20, xlim=xlim, ylim=c(240,360), axes=FALSE, ylab="", col=add_alpha("grey50",0.5), sfrac=0.0 )
    lines( smooth.spline( epica$year, epica$CO2, spar=0.5 ), col="grey50" )


    # # PEAT C
    # # plot this time series of land c uptake into the previously opened plot
    # for (i in 1:nruns){
    #   lines( df_cum_pt_mean$year, (df_cum_pt_mean[[ colsvec[i] ]]), col=add_alpha( "dodgerblue4", 0.01 ) )
    # }

    ## add +/- 1-sigma range to plot
    par( new=TRUE )
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )
    polygon( 
            c( df_cum_pt_mean$year, rev(df_cum_pt_mean$year) ), 
            # c( (df_cum_pt_mean$mean - df_cum_pt_mean$sd), rev( (df_cum_pt_mean$mean + df_cum_pt_mean$sd) ) ), 
            c( ( df_cum_pt_mean$q10 - off ), rev( ( df_cum_pt_mean$q90 - off ) ) ), 
            col=add_alpha( "dodgerblue4", 0.3 ), border=NA
            )
    # ## Add mean to plot
    # lines( df_cum_pt_mean$year, df_cum_pt_mean$mean, col="red", lwd=2 )

    # ## Add median to plot
    lines( df_cum_pt_mean$year, df_cum_pt_mean$median - off, col="dodgerblue4", lwd=2, lty=1 )
    # lines( df_cum_pt_mean$year, cumsum( df_pt_mean$median ), col="red", lwd=3, lty=1  )

    ## Add Charly's median to plot
    # lines( df_charly_pt_mean$year, df_charly_pt_mean$cumsum_median, col="blue", lwd=1 )

    # ## Add model cumulative NEP to plot
    # lines( trace129$globnep$year[ which.min( abs( trace129$globnep$year - df_cum_pt_mean$year[1] ) ):length(trace129$globnep$peatglobnep_global) ], 
    #   cumsum( trace129$globnep$peatglobnep_global[ which.min( abs( trace129$globnep$year - df_cum_pt_mean$year[1] ) ):length(trace129$globnep$peatglobnep_global) ])/1e14, 
    #   lwd=2, col="dodgerblue2"
    #   )

    # ## Add model total peat C (masked by Yu-map)
    # lines( trace129$c$year, trace129$c$peatc_maskedby_yu*1e-15, lwd=2, col="dodgerblue2" )
    ## (total)
    lines( trace129$c$year, trace129$c$peatc_global*1e-15 - off_lpx, lwd=1, col=add_alpha("dodgerblue2",0.5))
    lines( smooth.spline( trace129$c$year, trace129$c$peatc_global*1e-15-off_lpx, spar=0.7 ), lwd=2, col="dodgerblue2")

    # lines( smooth.spline( trace129$c$year, trace129$c$netpeat*1e-15-off_lpx, spar=0.7 ), lwd=4, col="dodgerblue2")


    # # TOTAL TERRESTRIAL C
    # # plot this time series of land c uptake into the previously opened plot
    # for (i in 1:nruns){
    #   numstring <- sprintf( "%05d", i-1)
    #   colstring <- paste( "r", numstring, sep="" )
    #   lines( df_cum_land_uptake$year, df_cum_land_uptake[[ colstring ]], col=add_alpha("springgreen4",0.02) )
    # }

    ## add mean to plot
    lines( df_cum_land_uptake$year, df_cum_land_uptake$median , col="springgreen4", lwd=2 )
    polygon( 
            c( df_cum_land_uptake$year, rev(df_cum_land_uptake$year) ), 
            c( (df_cum_land_uptake$q10 ), rev( (df_cum_land_uptake$q90 ) ) ), 
            col=add_alpha("springgreen4",0.3), border=NA 
            )

    legend( "topleft", c(expression(paste("total terrestrial (",Delta ,"C"[tot],")")), expression(paste("peat, YML (",Delta ,"C"[peat],")")), expression(paste("peat, LPX (",Delta ,"C"[peat],")"))), lty=1, lwd=2, col=c("springgreen4","dodgerblue4","dodgerblue2"), bty="n" )
    legend( "bottomright", c(expression(paste("CO"[2], " EPICA"))), pch=20, col=c(add_alpha("grey50",0.5)), bty="n" )
    # legend( "left", "", lty=1, col="grey50", bty="n" )



  ##--------------------------------
  ## SECOND PLOT: CUMULATIVE CHANGES
  ##--------------------------------
  ## LAST MILLENNIUM
  ##--------------------------------
    ylim <- c(0,650)
    # xlim <- c(firstyear,periodsAD[6])
    xlim <- c(750,2000)
    # par( las=1 , xaxs="i", yaxs="i", mar=c(5,10,1,1) )
    par( las=1 , xaxs="i", yaxs="i", mar=c(1,0,1,7.5) )
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )

    axis( 1, lwd=1.75, labels=FALSE );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75, labels=TRUE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    ## add period margins to plot
    # abline( v=period_margins_holo )
    # print( period_margins_holo )
    rect( period_margins_lastmill[1], ylim[1], period_margins_lastmill[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[3], ylim[1], period_margins_lastmill[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[5], ylim[1], period_margins_lastmill[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[7], ylim[1], period_margins_lastmill[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )


    ## CO2
    par(new=TRUE)
    plotCI( wais$year, wais$co2, uiw=wais$sigma, add=FALSE, pch=20, xlim=xlim, ylim=c(240,360), axes=FALSE, ylab="", col=add_alpha("burlywood3", 0.5), sfrac=0.0 )
    lines( smooth.spline( wais$year, wais$co2, spar=0.5 ), col="burlywood3" )

    points( law$CO2gasAge, law$CO2, pch=20, col=add_alpha("grey50", 0.5) )
    lines( smooth.spline( law$CO2gasAge, law$CO2, spar=0.5 ), col="grey50" )

    axis( 4, lwd=1.75, labels=TRUE, line=2.8, ylim=c(240,360), col="grey50", col.axis="grey50" );  axis( 4, at=seq(240,360,by=5), labels=F, tck=-0.01, col="grey50", line=2.8 )
    # mtext( side=4, text=expression(paste("atmospheric CO"[2], " (ppm)")), line=3.4, las=1 )
    text(2500, 300, labels = expression(paste("atmospheric CO"[2], " (ppm)")), xpd = NA, srt = -90, col="grey50")

    ## add +/- 1-sigma range to plot
    par(new=TRUE)
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )
    polygon( 
            c( df_cum_pt_mean_lhnfix$year, rev(df_cum_pt_mean_lhnfix$year) ), 
            # c( (df_cum_pt_mean_lhnfix$mean - df_cum_pt_mean_lhnfix$sd), rev( (df_cum_pt_mean_lhnfix$mean + df_cum_pt_mean_lhnfix$sd) ) ), 
            c( ( df_cum_pt_mean_lhnfix$q10 - off ), rev( ( df_cum_pt_mean_lhnfix$q90 - off ) ) ), 
            col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
            )
    # ## Add mean to plot
    # lines( df_cum_pt_mean_lhnfix$year, df_cum_pt_mean_lhnfix$mean, col="red", lwd=2 )

    # ## Add median to plot
    lines( df_cum_pt_mean_lhnfix$year, df_cum_pt_mean_lhnfix$median - off, col="dodgerblue4", lwd=2, lty=1 )
    # lines( df_cum_pt_mean_lhnfix$year, cumsum( df_pt_mean$median ), col="red", lwd=3, lty=1  )


    # ## Add model total peat C (masked by Yu-map)
    # lines( trace129$c$year, trace129$c$peatc_maskedby_yu*1e-15, lwd=2, col="dodgerblue2" )
    ## (total)
    lines( trace129$c$year, trace129$c$peatc_global*1e-15 - off_lpx, lwd=1, col=add_alpha("dodgerblue2",0.5))
    lines( smooth.spline( trace129$c$year, trace129$c$peatc_global*1e-15-off_lpx, spar=0.7 ), lwd=2, col="dodgerblue2")

    # lines( smooth.spline( trace129$c$year, trace129$c$netpeat*1e-15-off_lpx, spar=0.7 ), lwd=4, col="dodgerblue2")


    # TOTAL TERRESTRIAL C
    # plot this time series of land c uptake into the previously opened plot
    ## add mean to plot
    lines( dc_cum_merged_sum$year, dc_cum_merged_sum$median , col="springgreen4", lwd=2 )
    polygon( 
            c( dc_cum_merged_sum$year, rev(dc_cum_merged_sum$year) ), 
            c( (dc_cum_merged_sum$q10 ), rev( (dc_cum_merged_sum$q90 ) ) ), 
            col=add_alpha("springgreen4",0.3), border=NA 
            )

    legend( "bottomleft", c( expression(paste("CO"[2], " Law Dome")), expression(paste("CO"[2], " WAIS")) ), pch=20, col=c( add_alpha("grey50",0.5), add_alpha("burlywood3",0.5) ), bty="n" )


  ##--------------------------------
  ## THIRD PLOT: REMAINDER AND ELUC
  ##--------------------------------
  ## HOLOCENE
  ##--------------------------------
    ylim <- c(-400,100)
    xlim <- c(-10000,periodsAD[6])
    par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,4.3,0,1) )
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="age (ka BP)", xlim=xlim, ylim=ylim, axes=FALSE )

    # axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
    axis( 1, lwd=1.75, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 1, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75 );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    ## remainder
    rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

    lines( df_cum_remainder$year, df_cum_remainder$median , col="darkgoldenrod2", lwd=2 )
    polygon( 
            c( df_cum_remainder$year, rev(df_cum_remainder$year) ), 
            c( (df_cum_remainder$q10 ), rev( (df_cum_remainder$q90 ) ) ), 
            col=add_alpha("darkgoldenrod2",0.3), border=NA 
            )

    ## eLUC
    rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

    lines( fluc_rdc$hyde31$year, -fluc_rdc$hyde31$cumfluc, col="tomato", lwd=1.5  )
    polygon( c(fluc_rdc$hyde31$year, rev(fluc_rdc$hyde31$year)), c(-fluc_rdc$hyde31$cumfluc, rev(-fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

    lines( fluc_rdc$hyde32$year, -fluc_rdc$hyde32$cumfluc, col="orchid", lwd=1.5  )
    polygon( c(fluc_rdc$hyde32$year, rev(fluc_rdc$hyde32$year)), c(-fluc_rdc$hyde32$cumfluc, rev(-fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

    lines( fluc_rdc$kk10$year, -fluc_rdc$kk10$cumfluc, col="turquoise3", lwd=1.5, lty=2 )
    lines( fluc_rdc$kk10$year, -fluc_rdc$kk10d$cumfluc, col="turquoise3", lwd=1.5 )

    legend( "bottomleft", c(expression(paste("budget residual (",delta ,")")), "HYDE 3.1", "HYDE 3.2", "KK10", "KK10D"), col=c("darkgoldenrod2","tomato","orchid","turquoise3","turquoise3"), bty="n", lty=c(1,1,1,2,1), lwd=2 )


  ##--------------------------------
  ## FOURTH PLOT: REMAINDER AND ELUC
  ##--------------------------------
  ## HOLOCENE
  ##--------------------------------
    ylim <- c(-400,100)
    xlim <- c(750,2000)
    par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,0,7.5) )
    plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="", xlab="year CE", xlim=xlim, ylim=ylim, axes=FALSE )

    axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75, labels=TRUE, ylab="cumulative C uptake (PgC)" );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    ## remainder
    rect( period_margins_lastmill[1], ylim[1], period_margins_lastmill[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[3], ylim[1], period_margins_lastmill[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[5], ylim[1], period_margins_lastmill[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_lastmill[7], ylim[1], period_margins_lastmill[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )

    lines( dc_cum_remainder_merged_sum$year, dc_cum_remainder_merged_sum$median , col="darkgoldenrod2", lwd=2 )
    polygon( 
            c( dc_cum_remainder_merged_sum$year, rev(dc_cum_remainder_merged_sum$year) ), 
            c( (dc_cum_remainder_merged_sum$q10 ), rev( (dc_cum_remainder_merged_sum$q90 ) ) ), 
            col=add_alpha("darkgoldenrod2",0.3), border=NA 
            )


    ## eLUC
    rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

    lines( fluc_rdc$hyde31$year, -fluc_rdc$hyde31$cumfluc, col="tomato", lwd=1.5  )
    polygon( c(fluc_rdc$hyde31$year, rev(fluc_rdc$hyde31$year)), c(-fluc_rdc$hyde31$cumfluc, rev(-fluc_rdc$hyde31u$cumfluc)), col=add_alpha("tomato", 0.5), border=NA )

    lines( fluc_rdc$hyde32$year, -fluc_rdc$hyde32$cumfluc, col="orchid", lwd=1.5  )
    polygon( c(fluc_rdc$hyde32$year, rev(fluc_rdc$hyde32$year)), c(-fluc_rdc$hyde32$cumfluc, rev(-fluc_rdc$hyde32u$cumfluc)), col=add_alpha("orchid", 0.5), border=NA )

    lines( fluc_rdc$kk10$year, -fluc_rdc$kk10$cumfluc, col="turquoise3", lwd=1.5, lty=2 )
    lines( fluc_rdc$kk10$year, -fluc_rdc$kk10d$cumfluc, col="turquoise3", lwd=1.5 )

dev.off()


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

pdf( "fig/tseries_peat_holocene.pdf", width=sum(widths),height=sum(heights) )

  panel <- layout(
                  matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
  # layout.show(panel)

  ##--------------------------------
  ## FIRST PLOT: PEAT NCB
  ##--------------------------------
  ## HOLOCENE
  ##--------------------------------
    par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,4.3,1,1) )
    ylim <- c(-0.01,0.120)
    xlim <- c(-10000,periodsAD[6])
    plot( range(df_pt_mean$year), ylim, type="n", ylab=expression( "annual peat C balance (PgC" ~ yr^{-1} ~ ")" ), xlab="age (ka BP)", xlim=xlim, axes=FALSE )
    
    axis( 1, lwd=1.75, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 1, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75 );  axis( 2, at=seq(xlim[1],xlim[2],by=0.005), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    ## add period margins to plot
    # abline( v=period_margins_holo )
    # print( period_margins_holo )
    rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

    text( periodsAD+1000, -0.005, paste( periodsName, "ka BP" ), cex=0.7, adj=c(0.5,0) )

    # ## plot this time series of land c uptake into the previously opened plot
    # for (i in 1:100){
    #   lines( df_pt_mean$year, df_pt_mean[[ colsvec[i] ]] / dstep, col=add_alpha( "dodgerblue4", 0.01 ) )
    # }

    ## add +/- 1-sigma range to plot
    polygon( 
            c( df_pt_mean$year, rev(df_pt_mean$year) ), 
            c( (df_pt_mean$q10), rev( (df_pt_mean$q90) ) ) / dstep, 
            col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
            )


    # ## Add mean to plot
    # lines( df_pt_mean$year, df_pt_mean$mean / dstep, col="red", lwd=2 )

    ## PEAT Add median to plot
    lines( df_pt_mean$year, df_pt_mean$median / dstep, col="dodgerblue4", lwd=2, lty=1 )

    # ## Add Charly's median to plot
    # lines( df_charly_pt_mean$year, df_charly_pt_mean$median / dstep, col="blue", lwd=1 )

    ## Add model NEP to plot
    lines( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e15, lwd=1, col=add_alpha( "dodgerblue2", 0.3 ) )
    lines( smooth.spline( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e15, spar=0.2 ), lwd=2, col="dodgerblue2" )

    legend( "topleft", c("peat, YML", "peat, LPX"), lty=1, lwd=2, col=c("dodgerblue4","dodgerblue2"), bty="n" )


  ##--------------------------------
  ## SECOND PLOT: PEAT NCB
  ##--------------------------------
  ## LAST MILLENNIUM
  ##--------------------------------
    par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,1,4.0) )
    xlim <- c(750,2000)
    plot( range(df_pt_mean$year), ylim, type="n", ylab="", xlab="year CE", xlim=xlim, axes=FALSE )

    axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
    axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=0.005), labels=F, tck=-0.01 )
    axis( 4, lwd=1.75 );  axis( 4, at=seq(ylim[1],ylim[2],by=0.005), labels=F, tck=-0.01 )
    box( lwd=1.75 )

    # rect( period_margins_lastmill[1], ylim[1], period_margins_lastmill[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    # rect( period_margins_lastmill[3], ylim[1], period_margins_lastmill[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    # rect( period_margins_lastmill[5], ylim[1], period_margins_lastmill[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
    # rect( period_margins_lastmill[7], ylim[1], period_margins_lastmill[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )

    ## original
    polygon( 
            c( df_pt_mean_lehigh$year, rev(df_pt_mean_lehigh$year) ), 
            c( (df_pt_mean_lehigh$q10), rev( (df_pt_mean_lehigh$q90) ) ) / dstep, 
            col=rgb(0,0,0,0.1), border=NA 
            )
    # lines( df_pt_mean_lehigh$year, df_pt_mean_lehigh$median / dstep, col="black", lwd=2, lty=1 )
    # lines( df_pt_mean_lehigh$year, df_pt_mean_lehigh$median / dstep, col="grey50", lwd=1, lty=1 )

    ## fixed after 1200
    polygon( 
            c( df_pt_mean_lhnfix$year, rev(df_pt_mean_lhnfix$year) ), 
            c( (df_pt_mean_lhnfix$q10), rev( (df_pt_mean_lhnfix$q90) ) ) / dstep, 
            col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
            )
    lines( df_pt_mean_lhnfix$year, df_pt_mean_lhnfix$median / dstep, col="dodgerblue4", lwd=2, lty=1 )

    ## MODEL DATA
    lines( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e15, lwd=1, col=add_alpha( "dodgerblue2", 1.0 ) )
    lines( trace129$globnep$year, smooth.spline( trace129$globnep$year, trace129$globnep$peatglobnep_global/1e15, spar=0.05 )$y, lwd=2, col=add_alpha( "dodgerblue2", 1.0 ) )

    # legend( "topleft", c("YML, fixed after 1200 CE", "LPX"), lty=1, lwd=2, col=c("dodgerblue4","dodgerblue2"), bty="n" )

dev.off()
