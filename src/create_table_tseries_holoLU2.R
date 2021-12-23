library(plyr)
library(dplyr)

load("data/pt_yu_holocene.Rdata" ) 
load("data/pt_yu_lastmill.Rdata" ) ## contains df_pt_mean_lehigh and df_pt_mean_lhnfix
load('data/dc_terr_elsig_holocene.Rdata')
load("data/dc_remainder_holocene.Rdata")
load("data/fluc_holoLU2.Rdata" )
load("data/dc_merged.Rdata")

##//////////////////////////////////////////////////
## For plotting until only <<<<<<<<<<<<
##--------------------------------------------------

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

##<<<<<<<<<<<<<<<<<<<<<<<

##//////////////////////////////////////////////////
## total terrestrial C balance
##--------------------------------------------------
dc_terr_cum <- select( df_cum_land_uptake, year, mean, median, sd, q10, q90 )


##//////////////////////////////////////////////////
## peat YML data
##--------------------------------------------------
dstep <- 10 # yr

## offset relative to first year in elsig data (df_cum_land_uptake$year[1]=-8995)
firstyear <- dc_terr_cum$year[1]
print( paste( "first year in dc_terr time series", firstyear ) )
off <- df_cum_pt_mean$median[ which(df_pt_mean$year==firstyear) ]  # 'off' is 24.654 PgC

## cut all data to common time range: first year determined by elsig data (-8995 AD)
dc_terr_cum   <- dc_terr_cum[ which(dc_terr_cum$year==firstyear):dim(dc_terr_cum)[1],]

## peat YML: df_pt_mean, 10 yr resolution
peat_yml            <- select( df_pt_mean, age, year, mean, median, sd, q10, q90 )
peat_yml_cum        <- select( df_cum_pt_mean, age, year, mean, median, sd, q10, q90 )

peat_yml_lhnfix     <- select( df_pt_mean_lhnfix, age, year, mean, mean, median, sd, q10, q90 )
peat_yml_lhnfix_cum <- select( df_cum_pt_mean_lhnfix, age, year, mean, mean, median, sd, q10, q90 )

peat_yml[,3:7]          <- peat_yml[,3:7] / dstep
peat_yml_cum[,3:7]      <- peat_yml_cum[,3:7] / dstep
peat_yml_lhnfix[,3:7]   <- peat_yml_lhnfix[,3:7] / dstep


##//////////////////////////////////////////////////
## peat LPX data
##--------------------------------------------------
print("getting LPX peat data ...")
filn <- "data/peatglobnep_trace21_129.dat"
col.names <- c( "year", "peatglobnep_global", "peatglobnep_maskedby_yu", "peatglobnep_maskedby_yu_north" )  ## this is used for time series
peat_lpx <- read.table( filn, col.names=col.names )

peat_lpx     <- select( peat_lpx, year, peatglobnep_global )
peat_lpx     <- rename( peat_lpx, c("peatglobnep_global"="nep") )
peat_lpx$nep <- peat_lpx$nep * 1e-15 ## conversion from g to Pg

filn <- "data/peatc_trace21_129.dat"
# col.names <- c( "year", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
col.names <- c( "year", "peatc_global", "peatc_90S_30S", "peatc_30S_30N", "peatc_30N_90N", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat", "peatc_maskedby_lpxpres" )

tmp <- read.table( filn, col.names=col.names )

tmp     <- select( tmp, year, peatc_global, peatc_maskedby_lpxpres )
peat_lpx$totc <- tmp$peatc_global * 1e-15 ## conversion from g to Pg
peat_lpx$totc_maskedby_lpxpres <- tmp$peatc_maskedby_lpxpres * 1e-15 ## conversion from g to Pg

off_lpx        <- peat_lpx$totc[                  which(peat_lpx$year==firstyear)] # 'off_lpx' is 285.1803 GtC
off_lpx_masked <- peat_lpx$totc_maskedby_lpxpres[ which(peat_lpx$year==firstyear)] # 'off_lpx' is 285.1803 GtC


##//////////////////////////////////////////////////
## LUC emissions
##--------------------------------------------------
print("getting LUC emissions ...")
fluc_rdc <- data.frame( 
  year    = fluc_rdc$hyde31$year, 

  hyde31  = fluc_rdc$hyde31$fluc,
  hyde32  = fluc_rdc$hyde32$fluc,
  hyde31u = fluc_rdc$hyde31u$fluc,
  hyde32u = fluc_rdc$hyde32u$fluc,
  kk10    = fluc_rdc$kk10$fluc,
  kk10d   = fluc_rdc$kk10d$fluc,

  hyde31_cum  = fluc_rdc$hyde31$cumfluc,
  hyde32_cum  = fluc_rdc$hyde32$cumfluc,
  hyde31u_cum = fluc_rdc$hyde31u$cumfluc,
  hyde32u_cum = fluc_rdc$hyde32u$cumfluc,
  kk10_cum    = fluc_rdc$kk10$cumfluc,
  kk10d_cum   = fluc_rdc$kk10d$cumfluc

 )

fluc <- data.frame( 
  year    = fluc$hyde31$year, 

  hyde31  = fluc$hyde31$fluc,
  hyde32  = fluc$hyde32$fluc,
  hyde31u = fluc$hyde31u$fluc,
  hyde32u = fluc$hyde32u$fluc,
  kk10    = fluc$kk10$fluc,
  kk10d   = fluc$kk10d$fluc,

  hyde31_cum  = fluc$hyde31$cumfluc,
  hyde32_cum  = fluc$hyde32$cumfluc,
  hyde31u_cum = fluc$hyde31u$cumfluc,
  hyde32u_cum = fluc$hyde32u$cumfluc,
  kk10_cum    = fluc$kk10$cumfluc,
  kk10d_cum   = fluc$kk10d$cumfluc

 )

##//////////////////////////////////////////////////
## Write data to files (the rest is testing)
##--------------------------------------------------

## required data:
print("writing to CSV files ...")
write.csv( peat_yml_lhnfix, file="peat_yml_crude.csv", row.names=FALSE )
write.csv( peat_yml_lhnfix_cum, file="peat_yml_cum_crude.csv", row.names=FALSE )
write.csv( peat_lpx[ which(peat_lpx$year>-11000), ], file="peat_lpx_crude.csv", row.names=FALSE )
write.csv( fluc_rdc, file="fluc_rdc_crude.csv", row.names=FALSE )
write.csv( fluc, file="fluc_crude.csv", row.names=FALSE )


# ## Plot: time series of CUMULATIVE NCB, MEAN METHOD, HOLOCENE
# magn <- 0.8
# ncols <- 2
# nrows <- 3
# widths <- c(6,4.8) 
# heights <- c(3.4,3.4,3.9)
# widths <- widths * 1.0
# heights <- heights * 1.0

# widths  <- widths  * magn
# heights <- heights * magn

# ##--------------------------------
# ## NICE PLOT
# ##--------------------------------
#   pdf( "totc_peat_holocene_TEST.pdf", width=sum(widths),height=sum(heights) )

#     panel <- layout(
#                     matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE),
#                     widths=widths,
#                     heights=heights,
#                     TRUE
#                     )

#     ##--------------------------------
#     ## FIRST PLOT: PEAT NCB
#     ##--------------------------------
#     ## HOLOCENE
#     ##--------------------------------
#       par( las=1 , xaxs="i", yaxs="i", mar=c(1,4.3,1,1) )
#       ylim <- c(-0.01,0.110)
#       xlim <- c(-10000,periodsAD[6])
#       plot( range(peat_yml$year), ylim, type="n", ylab=expression( "annual peat C balance (PgC" ~ yr^{-1} ~ ")" ), xlab="", xlim=xlim, axes=FALSE )
#       axis( 1, lwd=1.75, labels=F );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#       axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#       # axis( 3, lwd=1.75, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 3, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
#       # mtext( "ka BP", side=3, line=2.5, cex=0.7 )
#       axis( 2, lwd=1.75 );  axis( 2, at=seq(xlim[1],xlim[2],by=0.005), labels=F, tck=-0.01 )
#       axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(xlim[1],xlim[2],by=0.005), labels=F, tck=-0.01 )
#       box( lwd=1.75 )

#       ## add period margins to plot
#       rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#       rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#       rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

#       text( periodsAD+1000, -0.005, paste( periodsName, "ka BP" ), cex=0.7, adj=c(0.5,0) )

#       ## add +/- 1-sigma range to plot
#       polygon( 
#               c( peat_yml$year, rev(peat_yml$year) ), 
#               c( (peat_yml$q10), rev( (peat_yml$q90) ) ),
#               col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
#               )

#       ## PEAT Add median to plot
#       lines( peat_yml$year, peat_yml$median, col="dodgerblue4", lwd=2, lty=1 )

#       ## Add model NEP to plot
#       boxl <- 1
#       lines( peat_lpx$year, filter( peat_lpx$nep, rep(1/boxl,boxl), sides=2 ), lwd=1, col=add_alpha( "dodgerblue2", 0.3 ) )
#       boxl <- 30
#       lines( peat_lpx$year, filter( peat_lpx$nep, rep(1/boxl,boxl), sides=2 ), lwd=2, col="dodgerblue2" )

#       legend( "topleft", c("peat, YML", "peat, LPX"), lty=1, lwd=2, col=c("dodgerblue4","dodgerblue2"), bty="n" )


#     ##--------------------------------
#     ## SECOND PLOT: PEAT NCB
#     ##--------------------------------
#     ## LAST MILLENNIUM
#     ##--------------------------------
#       par( las=1 , xaxs="i", yaxs="i", mar=c(1,0,1,7.5) )
#       ylim <- c(-0.01,0.110)
#       xlim <- c(750,2000)
#       plot( range(peat_yml$year), ylim, type="n", ylab="", xlab="", xlim=xlim, axes=FALSE )
#       axis( 1, lwd=1.75, labels=FALSE );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#       axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#       axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=0.005), labels=F, tck=-0.01 )
#       axis( 4, lwd=1.75, labels=TRUE, ylab=expression( "annual peat C balance (PgC" ~ yr^{-1} ~ ")" ) );  axis( 4, at=seq(ylim[1],ylim[2],by=0.005), labels=F, tck=-0.01 )
#       box( lwd=1.75 )

#       ## original
#       polygon( 
#               c( peat_yml$year, rev(peat_yml$year) ), 
#               c( (peat_yml$q10), rev( (peat_yml$q90) ) ), 
#               col=rgb(0,0,0,0.1), border=NA 
#               )

#       ## fixed after 1200
#       polygon( 
#               c( peat_yml_lhnfix$year, rev(peat_yml_lhnfix$year) ), 
#               c( (peat_yml_lhnfix$q10), rev( (peat_yml_lhnfix$q90) ) ), 
#               col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
#               )
#       lines( df_pt_mean_lhnfix$year, df_pt_mean_lhnfix$median, col="dodgerblue4", lwd=2, lty=1 )

#       ## MODEL DATA
#       boxl <- 1
#       lines( peat_lpx$year, filter( peat_lpx$nep, rep(1/boxl,boxl), sides=2 ), lwd=1, col=add_alpha( "dodgerblue2", 1.0 ) )
#       lines( peat_lpx$year, smooth.spline( peat_lpx$year, peat_lpx$nep, spar=0.05 )$y, lwd=3, col=add_alpha( "dodgerblue2", 1.0 ) )


#       ##--------------------------------
#       ## THIRD PLOT: CUMULATIVE CHANGES
#       ##--------------------------------
#       ## HOLOCENE
#       ##--------------------------------
#         ylim <- c(0,650)
#         # xlim <- c(firstyear,periodsAD[6])
#         xlim <- c(-10000,periodsAD[6])
#         # par( las=1 , xaxs="i", yaxs="i", mar=c(5,10,1,1) )
#         par( las=1 , xaxs="i", yaxs="i", mar=c(1,4.3,0,1) )
#         plot( range(peat_yml$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )

#         axis( 1, lwd=1.75, labels=FALSE );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#         axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#         axis( 2, lwd=1.75 );                axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         box( lwd=1.75 )

#         ## add period margins to plot
#         rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

#         ## add +/- 1-sigma range to plot
#         par( new=TRUE )
#         plot( range(peat_yml$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )
#         polygon( 
#                 c( peat_yml$year, rev(peat_yml$year) ), 
#                 # c( ( cumsum(peat_yml$q10) - off )*dstep, rev( ( cumsum(peat_yml$q90) - off ) )*dstep ), 
#                 c( ( peat_yml_lhnfix_cum$q10 - off ), rev( ( peat_yml_lhnfix_cum$q90 - off ) ) ), ## 'off' is 24.654 GtC
#                 col=add_alpha( "dodgerblue4", 0.3 ), border=NA
#                 )

#         # ## Add median to plot
#         lines( peat_yml_lhnfix_cum$year, peat_yml_lhnfix_cum$median - off, col="dodgerblue4", lwd=2, lty=1 )

#         # ## Add model total peat C (masked by Yu-map)
#         lines( peat_lpx$year,peat_lpx$totc - off_lpx, lwd=1, col=add_alpha("dodgerblue2",0.5))
#         lines( smooth.spline( peat_lpx$year,peat_lpx$totc - off_lpx, spar=0.7 ), lwd=2, col="dodgerblue2")

#         ## add mean to plot
#         lines( dc_terr_cum$year, dc_terr_cum$median , col="springgreen4", lwd=2 )
#         polygon( 
#                 c( dc_terr_cum$year, rev(dc_terr_cum$year) ), 
#                 c( (dc_terr_cum$q10 ), rev( (dc_terr_cum$q90 ) ) ), 
#                 col=add_alpha("springgreen4",0.3), border=NA 
#                 )

#         legend( "topleft", c(expression(paste("total terrestrial (",Delta ,"C"[tot],")")), expression(paste("peat, YML (",Delta ,"C"[peat],")")), expression(paste("peat, LPX (",Delta ,"C"[peat],")"))), lty=1, lwd=2, col=c("springgreen4","dodgerblue4","dodgerblue2"), bty="n" )
#         # legend( "bottomright", c(expression(paste("CO"[2], " EPICA"))), pch=20, col=c(add_alpha("grey50",0.5)), bty="n" )


#       ##--------------------------------
#       ## FOURTH PLOT: CUMULATIVE CHANGES
#       ##--------------------------------
#       ## LAST MILLENNIUM
#       ##--------------------------------
#         ylim <- c(0,650)
#         # xlim <- c(firstyear,periodsAD[6])
#         xlim <- c(750,2000)
#         par( las=1 , xaxs="i", yaxs="i", mar=c(1,0,0,7.5) )
#         plot( range(peat_yml_cum$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )

#         axis( 1, lwd=1.75, labels=FALSE );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#         axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#         axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         axis( 4, lwd=1.75, labels=TRUE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         box( lwd=1.75 )

#         ## add period margins to plot
#         rect( period_margins_lastmill[1], ylim[1], period_margins_lastmill[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[3], ylim[1], period_margins_lastmill[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[5], ylim[1], period_margins_lastmill[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[7], ylim[1], period_margins_lastmill[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )

#         ## add +/- 1-sigma range to plot
#         par(new=TRUE)
#         plot( range(peat_yml_cum$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )
#         polygon( 
#                 c( peat_yml_lhnfix_cum$year, rev(peat_yml_lhnfix_cum$year) ), 
#                 c( ( peat_yml_lhnfix_cum$q10 - off ), rev( ( peat_yml_lhnfix_cum$q90 - off ) ) ), 
#                 col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
#                 )

#         # ## Add median to plot
#         lines( peat_yml_lhnfix_cum$year, peat_yml_lhnfix_cum$median - off, col="dodgerblue4", lwd=2, lty=1 )

#         # ## Add model total peat C
#         lines( peat_lpx$year,peat_lpx$totc - off_lpx, lwd=1, col=add_alpha("dodgerblue2",0.5))
#         lines( smooth.spline( peat_lpx$year,peat_lpx$totc - off_lpx, spar=0.7 ), lwd=2, col="dodgerblue2")


#         # TOTAL TERRESTRIAL C
#         # plot this time series of land c uptake into the previously opened plot
#         ## add mean to plot
#         lines( dc_cum_merged_sum$year, dc_cum_merged_sum$median , col="springgreen4", lwd=2 )
#         polygon( 
#                 c( dc_cum_merged_sum$year, rev(dc_cum_merged_sum$year) ), 
#                 c( (dc_cum_merged_sum$q10 ), rev( (dc_cum_merged_sum$q90 ) ) ), 
#                 col=add_alpha("springgreen4",0.3), border=NA 
#                 )


#       ##--------------------------------
#       ## FIFTH PLOT: REMAINDER AND ELUC
#       ##--------------------------------
#       ## HOLOCENE
#       ##--------------------------------
#         ylim <- c(-400,100)
#         xlim <- c(-10000,periodsAD[6])
#         par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,4.3,0,1) )
#         plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="ka BP", xlim=xlim, ylim=ylim, axes=FALSE )

#         # axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#         axis( 1, lwd=1.75, at=periodsAD, labels=as.graphicsAnnot(periodsBP*-1e-3) );  axis( 1, at=seq(from=range(periodsAD)[1],to=range(periodsAD)[2],by=500), labels=F, tck=-0.01 )
#         axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=500), labels=F, tck=-0.01 )
#         axis( 2, lwd=1.75 );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         axis( 4, lwd=1.75, labels=FALSE );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         box( lwd=1.75 )

#         ## remainder
#         rect( period_margins_holo[1], ylim[1], period_margins_holo[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_holo[3], ylim[1], period_margins_holo[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_holo[5], ylim[1], period_margins_holo[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )

#         lines( df_cum_remainder$year, df_cum_remainder$median , col="darkgoldenrod2", lwd=2 )
#         polygon( 
#                 c( df_cum_remainder$year, rev(df_cum_remainder$year) ), 
#                 c( (df_cum_remainder$q10 ), rev( (df_cum_remainder$q90 ) ) ), 
#                 col=add_alpha("darkgoldenrod2",0.3), border=NA 
#                 )

#         ## eLUC
#         rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$hyde31_cum, col="tomato", lwd=1.5  )
#         polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde31_cum, rev(-fluc_rdc$hyde31u_cum)), col=add_alpha("tomato", 0.5), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$hyde32_cum, col="orchid", lwd=1.5  )
#         polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde32_cum, rev(-fluc_rdc$hyde32u_cum)), col=add_alpha("orchid", 0.5), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$kk10_cum, col="turquoise3", lwd=1.5, lty=2 )
#         lines( fluc_rdc$year, -fluc_rdc$kk10d_cum, col="turquoise3", lwd=1.5 )

#         legend( "bottomleft", c(expression(paste("budget residual (",delta ,")")), "HYDE 3.1", "HYDE 3.2", "KK10", "KK10D"), col=c("darkgoldenrod2","tomato","orchid","turquoise3","turquoise3"), bty="n", lty=c(1,1,1,2,1), lwd=2 )


#       ##--------------------------------
#       ## SIXTH PLOT: REMAINDER AND ELUC
#       ##--------------------------------
#       ## HOLOCENE
#       ##--------------------------------
#         ylim <- c(-400,100)
#         xlim <- c(750,2000)
#         par( las=1 , xaxs="i", yaxs="i", mar=c(4.8,0,0,7.5) )
#         plot( range(df_cum_pt_mean$year), ylim, type="n", ylab="", xlab="year CE", xlim=xlim, ylim=ylim, axes=FALSE )

#         axis( 1, lwd=1.75 );  axis( 1, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#         axis( 3, lwd=1.75, labels=FALSE );  axis( 3, at=seq(xlim[1],xlim[2],by=50), labels=F, tck=-0.01 )
#         axis( 2, lwd=1.75, labels=FALSE );  axis( 2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         axis( 4, lwd=1.75, labels=TRUE, ylab="cumulative C uptake (PgC)" );  axis( 4, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01 )
#         box( lwd=1.75 )

#         ## remainder
#         rect( period_margins_lastmill[1], ylim[1], period_margins_lastmill[2], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[3], ylim[1], period_margins_lastmill[4], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[5], ylim[1], period_margins_lastmill[6], ylim[2], col=rgb(0,0,0,0.1), border=NA )
#         rect( period_margins_lastmill[7], ylim[1], period_margins_lastmill[8], ylim[2], col=rgb(0,0,0,0.1), border=NA )

#         lines( dc_cum_remainder_merged_sum$year, dc_cum_remainder_merged_sum$median , col="darkgoldenrod2", lwd=2 )
#         polygon( 
#                 c( dc_cum_remainder_merged_sum$year, rev(dc_cum_remainder_merged_sum$year) ), 
#                 c( (dc_cum_remainder_merged_sum$q10 ), rev( (dc_cum_remainder_merged_sum$q90 ) ) ), 
#                 col=add_alpha("darkgoldenrod2",0.3), border=NA 
#                 )


#         ## eLUC
#         rect( xlim[1], mintotc[1], xlim[2], maxtotc, col=rgb(0,0,0,0.2), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$hyde31_cum, col="tomato", lwd=1.5  )
#         polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde31_cum, rev(-fluc_rdc$hyde31u_cum)), col=add_alpha("tomato", 0.5), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$hyde32_cum, col="orchid", lwd=1.5  )
#         polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde32_cum, rev(-fluc_rdc$hyde32u_cum)), col=add_alpha("orchid", 0.5), border=NA )

#         lines( fluc_rdc$year, -fluc_rdc$kk10_cum, col="turquoise3", lwd=1.5, lty=2 )
#         lines( fluc_rdc$year, -fluc_rdc$kk10d_cum, col="turquoise3", lwd=1.5 )

#   dev.off()


##--------------------------------
## CRUDE PLOT
##--------------------------------
rm( list=ls() )

peat_yml_lhnfix  <- read.csv( file="peat_yml_crude.csv" )
peat_yml_lhnfix_cum  <- read.csv( file="peat_yml_cum_crude.csv" )
peat_lpx  <- read.csv( file="peat_lpx_crude.csv" )
fluc_rdc  <- read.csv( file="fluc_rdc_crude.csv" )
fluc  <- read.csv( file="fluc_crude.csv" )

add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}

print("plotting ... ")
off     <- 24.654
off_lpx <- 285.1803
off_lpx_masked <- 236.4059

  ##--------------------------------
  ## FIRST PLOT: PEAT NCB
  ##--------------------------------
  ## HOLOCENE
  ##--------------------------------
    xlim <- c(-10000,2000)
    plot( range(peat_yml_lhnfix$year), range(peat_yml_lhnfix$median), type="n", ylab=expression( "annual peat C balance (PgC" ~ yr^{-1} ~ ")" ), xlab="", xlim=xlim )
    
    ## add +/- 1-sigma range to plot
    lines( peat_yml_lhnfix$year, peat_yml_lhnfix$q10 )
    lines( peat_yml_lhnfix$year, peat_yml_lhnfix$q90 )

    ## PEAT Add median to plot
    lines( peat_yml_lhnfix$year, peat_yml_lhnfix$median, col="dodgerblue4", lwd=2, lty=1 )

    ## Add model NEP to plot
    boxl <- 1
    lines( peat_lpx$year, peat_lpx$nep, lwd=1, col="dodgerblue2" )
    boxl <- 30
    lines( peat_lpx$year, peat_lpx$nep, lwd=2, col="dodgerblue2" )

    ##--------------------------------
    ## THIRD PLOT: CUMULATIVE CHANGES
    ##--------------------------------
    ## HOLOCENE
    ##--------------------------------
      pdf( "fig/dcpeat_masked.pdf", width=8, height=6 )
      par(las=1, xaxs="i")
      ylim <- c(0,650)
      plot( range(peat_yml_lhnfix$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="year CE", xlim=xlim, ylim=ylim )

      ## add +/- 1-sigma range to plot
      par( new=TRUE )
      plot( range(peat_yml_lhnfix$year), ylim, type="n", ylab="", xlab="", xlim=xlim, ylim=ylim, axes=FALSE )
      polygon( 
        c( peat_yml_lhnfix$year, rev(peat_yml_lhnfix$year) ), 
        c( peat_yml_lhnfix_cum$q10 - off, rev(peat_yml_lhnfix_cum$q90 - off) ),
        col=add_alpha( "dodgerblue4", 0.3 ), border=NA 
      )

      # ## Add median to plot
      lines( peat_yml_lhnfix_cum$year, peat_yml_lhnfix_cum$median - off, col="dodgerblue4", lwd=2, lty=1 )

      # ## Add model total peat C
      lines( peat_lpx$year, peat_lpx$totc - off_lpx, lwd=1, col="dodgerblue2" )
      lines( smooth.spline( peat_lpx$year,peat_lpx$totc - off_lpx, spar=0.7 ), lwd=2, col="dodgerblue2")

      ## model total peat C masked by today's peatlands
      lines( smooth.spline( peat_lpx$year, peat_lpx$totc_maskedby_lpxpres - off_lpx_masked, spar=0.7 ), lwd=2, lty=2, col="dodgerblue2")

      # off <- cumsum( peat_lpx$nep )[which( peat_lpx$year==-8995 )] * 10
      # lines( peat_lpx$year, cumsum( peat_lpx$nep ) * 10 - off, lty=2, lwd=2, col="dodgerblue2" )

      legend( "topleft", c( 
        expression(paste("peat, YML (",Delta ,"C"[peat],")")), 
        expression(paste("peat, LPX (",Delta ,"C"[peat],")")),
        expression(paste("peat, LPX (",Delta ,"C"[peat]," on today's peatland gridcells)"))
        ), lty=c(1,1,2), lwd=2, col=c("dodgerblue4","dodgerblue2","dodgerblue2"), bty="n" )
      dev.off()

    ##--------------------------------
    ## FIFTH PLOT: REMAINDER AND ELUC
    ##--------------------------------
    ## HOLOCENE
    ##--------------------------------
      ylim <- c(-400,100)
      plot( range(fluc_rdc$year), ylim, type="n", ylab="cumulative C uptake (PgC)", xlab="ka BP", xlim=xlim, ylim=ylim )

      lines( fluc_rdc$year, -fluc_rdc$hyde31_cum, col="tomato", lwd=1.5  )
      polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde31_cum, rev(-fluc_rdc$hyde31u_cum)), col="tomato", border=NA )

      lines( fluc_rdc$year, -fluc_rdc$hyde32_cum, col="orchid", lwd=1.5  )
      polygon( c(fluc_rdc$year, rev(fluc_rdc$year)), c(-fluc_rdc$hyde32_cum, rev(-fluc_rdc$hyde32u_cum)), col="orchid", border=NA )

      lines( fluc_rdc$year, -fluc_rdc$kk10_cum, col="turquoise3", lwd=1.5, lty=2 )
      lines( fluc_rdc$year, -fluc_rdc$kk10d_cum, col="turquoise3", lwd=1.5 )

      lines( fluc$year, -fluc$hyde31_cum )
      lines( fluc$year, -fluc$hyde32_cum )
      lines( fluc$year, -fluc$kk10_cum)
      lines( fluc$year, -fluc$kk10d_cum )

      lines( fluc$year, -cumsum(fluc$kk10d) )

