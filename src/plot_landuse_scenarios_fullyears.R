library(ncdf)
library(fields)
library(sp)
library(maptools)
library(rgeos)
library(abind)

source('~/.Rprofile')

inextlower <- function( vec, val ){
  i <- 1
  while (val>vec[i]) {
    i <- i + 1
  }
  return( i-1 )
}

inextupper <- function( vec, val ){
  i <- 1
  while (val>vec[i]) {
    i <- i + 1
  }
  return( i )
}

interpol_field <- function( time0, time1, field0, field1, outtime ){
  outfield <- field0 + (field1-field0)/(time1-time0) * (outtime - time0)
  return(outfield)
}

do_read <- FALSE

################
## LUC in periods: HYDE vs. KK11
################
  fils <- c( 
            "../data/landuse_hyde31_final_halfdeg.cdf", 
            "../data/landuse_hyde32_baseline_halfdeg.cdf", 
            "../data/landuse_KK11_halfdeg_hydeslices.nc",
            "../data/landuse_KK11delayed_halfdeg_hydeslices.nc"
            )
  name_scen <- c(
                  "HYDE 3.1",
                  "HYDE 3.2",
                  "KK10",
                  "KK10D"
                  )

  ## read files
  if (do_read){
    nc <- open.ncdf( fils[1] )
    crop_hyde31 <- get.var.ncdf(nc, "crop")
    past_hyde31 <- get.var.ncdf(nc, "past")
    lon <- get.var.ncdf(nc, "LONGITUDE")
    lat <- get.var.ncdf(nc, "LATITUDE")
    time <- get.var.ncdf(nc, "TIME")
    close.ncdf(nc)

    nc <- open.ncdf( fils[2] )
    crop_hyde32 <- get.var.ncdf(nc, "crop")
    past_hyde32 <- get.var.ncdf(nc, "past")
    time <- get.var.ncdf(nc, "TIME")
    close.ncdf(nc)

    nc <- open.ncdf( fils[3] )
    crop_kk11 <- get.var.ncdf(nc, "crop")
    past_kk11 <- get.var.ncdf(nc, "past")
    close.ncdf(nc)

    nc <- open.ncdf( fils[4] )
    crop_kk11d <- get.var.ncdf(nc, "crop")
    past_kk11d <- get.var.ncdf(nc, "past")
    close.ncdf(nc)
  }

  # ## HOLOCENE PERIODS
  # ## Alternativ: fixe Margins
  # # periodsBP <- read.csv( '../data/periods_holocene.csv' )$periodsBP
  # periodsBP <- seq( -11000, -1000, by=1000 )
  # periodsAD <- periodsBP + 1950
  # yearBPName <- as.character( -periodsBP*1e-3 )
  # periodsName <- paste( 
  #   as.character( -periodsBP*1e-3 )[1:length(periodsBP)-1],
  #   "-",
  #   as.character( -periodsBP*1e-3 )[2:length(periodsBP)],
  #   sep=""
  #   )
  # period_margins <- periodsAD
  # nper <- length(period_margins)-1


  # magn <- 3.5
  # ncols <- 3
  # nrows <- 2
  # widths <- rep(1.6*magn,ncols)
  # widths[1] <- 0.15*widths[2]
  # heights <- rep(0.9*magn,nrows)
  # order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)
  
  # ylim <- c(-60,85)
  # lat.labels <- seq(-90, 90, 30)
  # lat.short  <- seq(-90, 90, 10)
  # lon.labels <- seq(-180, 180, 60)
  # lon.short  <- seq(-180, 180, 10)

  # a <- sapply( lat.labels, function(x) bquote(.(x)*degree ~ N) )
  # b <- sapply( lon.labels, function(x) bquote(.(x)*degree ~ E) )

  # lev <- c(0, 0.01, 0.1, 0.15, 0.2, 0.4, 0.7, 1)
  # color <- c("wheat3","green4","yellow","orange","red")

  # for (i in 1:(length(periodsBP)-1)){

  #   filn <- paste( "../fig/fig_LUC_maps/LUC_map_holocene_", yearBPName[i], "kaBP.pdf", sep="" )
  #   pdf( filn, width=sum(widths), height=sum(heights) )

  #     panel <- layout(
  #               order,
  #               widths=widths,
  #               heights=heights,
  #               TRUE
  #               )
  #     # layout.show(panel)

  #     ## interpolate cropland area to period margin
  #     myinextlower <- inextlower( time, periodsAD[i])
  #     myinextupper <- inextupper( time, periodsAD[i])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_hyde31[,,myinextlower] #+ past_hyde31[,,myinextlower]
  #     field1 <- crop_hyde31[,,myinextupper] #+ past_hyde31[,,myinextupper]
  #     field_hyde31_0 <- interpol_field( time0, time1, field0, field1, periodsAD[i] )

  #     myinextlower <- inextlower( time, periodsAD[i+1])
  #     myinextupper <- inextupper( time, periodsAD[i+1])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_hyde31[,,myinextlower] #+ past_hyde31[,,myinextlower]
  #     field1 <- crop_hyde31[,,myinextupper] #+ past_hyde31[,,myinextupper]
  #     field_hyde31_1 <- interpol_field( time0, time1, field0, field1, periodsAD[i+1] )

  #     myinextlower <- inextlower( time, periodsAD[i])
  #     myinextupper <- inextupper( time, periodsAD[i])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_hyde32[,,myinextlower] #+ past_hyde32[,,myinextlower]
  #     field1 <- crop_hyde32[,,myinextupper] #+ past_hyde32[,,myinextupper]
  #     field_hyde32_0 <- interpol_field( time0, time1, field0, field1, periodsAD[i] )

  #     myinextlower <- inextlower( time, periodsAD[i+1])
  #     myinextupper <- inextupper( time, periodsAD[i+1])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_hyde32[,,myinextlower] #+ past_hyde32[,,myinextlower]
  #     field1 <- crop_hyde32[,,myinextupper] #+ past_hyde32[,,myinextupper]
  #     field_hyde32_1 <- interpol_field( time0, time1, field0, field1, periodsAD[i+1] )

  #     myinextlower <- inextlower( time, periodsAD[i])
  #     myinextupper <- inextupper( time, periodsAD[i])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
  #     field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
  #     field_kk11_0 <- interpol_field( time0, time1, field0, field1, periodsAD[i] )
  #     print(paste("for first field, using interpolation between", time0, "and", time1, "to", periodsAD[i]))

  #     myinextlower <- inextlower( time, periodsAD[i+1])
  #     myinextupper <- inextupper( time, periodsAD[i+1])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
  #     field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
  #     field_kk11_1 <- interpol_field( time0, time1, field0, field1, periodsAD[i+1] )
  #     print(paste("for second field, using interpolation between", time0, "and", time1, "to", periodsAD[i+1]))

  #     myinextlower <- inextlower( time, periodsAD[i])
  #     myinextupper <- inextupper( time, periodsAD[i])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
  #     field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
  #     field_kk11d_0 <- interpol_field( time0, time1, field0, field1, periodsAD[i] )
  #     print(paste("for first field, using interpolation between", time0, "and", time1, "to", periodsAD[i]))

  #     myinextlower <- inextlower( time, periodsAD[i+1])
  #     myinextupper <- inextupper( time, periodsAD[i+1])
  #     time0 <- time[myinextlower]
  #     time1 <- time[myinextupper]
  #     field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
  #     field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
  #     field_kk11d_1 <- interpol_field( time0, time1, field0, field1, periodsAD[i+1] )
  #     print(paste("for second field, using interpolation between", time0, "and", time1, "to", periodsAD[i+1]))

  #     # lev <- c(-1, -0.5, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.5, 1)
  #     # color <- c("springgreen4","springgreen","wheat3","wheat3","orange","red")
  #     # filn <- paste( "../fig/fig_dLUC_maps/dLUC_map_holocene_p",i,".pdf", sep="" )
      
  #     # pdf( filn, width=sum(widths), height=sum(heights) )

  #     #   panel <- layout(
  #     #             order,
  #     #             widths=widths,
  #     #             heights=heights,
  #     #             TRUE
  #     #             )
  #     #   # layout.show(panel)

  #     #   ## Color key
  #     #   par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
  #     #   out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

  #     #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #     #   image(
  #     #         lon, lat,
  #     #         field_hyde31_1 - field_hyde31_0,
  #     #         ylim=ylim, 
  #     #         zlim=range(lev), yaxt="n", xaxt="n",
  #     #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #     #         )
  #     #   text(0.03,0.08,paste("HYDE 3.1,",periodsName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     #   axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     #   axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     #   axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #     #   image(
  #     #         lon, lat,
  #     #         field_hyde32_1 - field_hyde32_0,
  #     #         ylim=ylim, 
  #     #         zlim=range(lev), yaxt="n", xaxt="n",
  #     #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #     #         )
  #     #   text(0.03,0.08,paste("HYDE 3.2,",periodsName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     #   axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     #   axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     #   axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
  #     #   out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

  #     #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #     #   image( 
  #     #         lon, lat,
  #     #         field_kk11_1 - field_kk11_0, 
  #     #         ylim=ylim, 
  #     #         zlim=range(lev), yaxt="n", xaxt="n",
  #     #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #     #         )
  #     #   text(0.03,0.08,paste("KK10,",periodsName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     #   axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     #   axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     #   axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
  #     #   image( 
  #     #         lon, lat,
  #     #         field_kk11d_1 - field_kk11d_0, 
  #     #         ylim=ylim, 
  #     #         zlim=range(lev), yaxt="n", xaxt="n",
  #     #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #     #         )
  #     #   text(0.03,0.08,paste("KK10D,",periodsName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     #   axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     #   axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
  #     #   axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     #   axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     #   axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     # dev.off()

  #     ## Color key
  #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1 )
  #     out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

  #     ## period i
  #     image(
  #           lon, lat,
  #           field_hyde31_0,
  #           ylim=ylim, 
  #           zlim=range(lev), yaxt="n", xaxt="n",
  #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #           )
  #     text(-173,-56,paste("HYDE 3.1,",yearBPName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
  #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
  #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     image(
  #           lon, lat,
  #           field_hyde32_0,
  #           ylim=ylim, 
  #           zlim=range(lev), yaxt="n", xaxt="n",
  #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #           )
  #     text(-173,-56,paste("HYDE 3.2,",yearBPName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
  #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
  #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
  #     out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

  #     image( 
  #           lon, lat,
  #           field_kk11_0, 
  #           ylim=ylim, 
  #           zlim=range(lev), yaxt="n", xaxt="n",
  #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #           )
  #     text(-173,-56,paste("KK10,",yearBPName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
  #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
  #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     image( 
  #           lon, lat,
  #           field_kk11d_0, 
  #           ylim=ylim, 
  #           zlim=range(lev), yaxt="n", xaxt="n",
  #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
  #           )
  #     text(-173,-56,paste("KK10D,",yearBPName[i],"ka BP"),cex=1.5,adj=c(0,0))
  #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
  #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
  #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
  #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
  #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  #   dev.off()
    
  # }


    ## LAST MILLENNIUM PERIODS
    ## Alternativ: fixe Margins
    # period_margins <- read.csv( '../data/periods_lastmill.csv' )$period_margins
    period_margins <- c( seq( 1000, 1500, by=100 ), seq( 1550, 2000, by=50 ) )
    periodsName <- paste( 
      as.character( period_margins )[1:length(period_margins)-1],
      "-",
      as.character( period_margins )[2:length(period_margins)],
      sep=""
      )
    nper <- length(period_margins)-1

    for (i in 1:nper){

      ## interpolate cropland area to period margin
      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde31[,,myinextlower] #+ past_hyde31[,,myinextlower]
      field1 <- crop_hyde31[,,myinextupper] #+ past_hyde31[,,myinextupper]
      field_hyde31_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde31[,,myinextlower] #+ past_hyde31[,,myinextlower]
      field1 <- crop_hyde31[,,myinextupper] #+ past_hyde31[,,myinextupper]
      field_hyde31_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde32[,,myinextlower] #+ past_hyde32[,,myinextlower]
      field1 <- crop_hyde32[,,myinextupper] #+ past_hyde32[,,myinextupper]
      field_hyde32_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_hyde32[,,myinextlower] #+ past_hyde32[,,myinextlower]
      field1 <- crop_hyde32[,,myinextupper] #+ past_hyde32[,,myinextupper]
      field_hyde32_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )
      print(paste("for first field, using interpolation between", time0, "and", time1, "to", period_margins[i]))

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )
      print(paste("for second field, using interpolation between", time0, "and", time1, "to", period_margins[i+1]))

      myinextlower <- inextlower( time, period_margins[i])
      myinextupper <- inextupper( time, period_margins[i])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11d_0 <- interpol_field( time0, time1, field0, field1, period_margins[i] )
      print(paste("for first field, using interpolation between", time0, "and", time1, "to", period_margins[i]))

      myinextlower <- inextlower( time, period_margins[i+1])
      myinextupper <- inextupper( time, period_margins[i+1])
      time0 <- time[myinextlower]
      time1 <- time[myinextupper]
      field0 <- crop_kk11d[,,myinextlower] #+ past_kk11[,,myinextlower]
      field1 <- crop_kk11d[,,myinextupper] #+ past_kk11[,,myinextupper]
      field_kk11d_1 <- interpol_field( time0, time1, field0, field1, period_margins[i+1] )
      print(paste("for second field, using interpolation between", time0, "and", time1, "to", period_margins[i+1]))


      # lev <- c(-1, -0.5, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.5, 1)
      # color <- c("springgreen4","springgreen","wheat3","wheat3","orange","red")
      # filn <- paste( "../fig/fig_dLUC_maps/dLUC_map_lastmill_p",i,".pdf", sep="" )
      # pdf( filn, width=sum(widths), height=sum(heights) )

      #   panel <- layout(
      #             order,
      #             widths=widths,
      #             heights=heights,
      #             TRUE
      #             )
      #   # layout.show(panel)

      #   ## Color key
      #   par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
      #   out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

      #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
      #   image(
      #         field_hyde31_1 - field_hyde31_0,
      #         ylim=ylim, 
      #         zlim=range(lev), yaxt="n", xaxt="n",
      #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #         )
      #   text(0.03,0.08,paste("HYDE 3.1,",periodsName[i],"CE"),cex=1.5,adj=c(0,0))

      #   par(mar=c(1,1,1,1),xaxs="i", yaxs="i",las=1)
      #   image(
      #         field_hyde32_1 - field_hyde32_0,
      #         ylim=ylim, 
      #         zlim=range(lev), yaxt="n", xaxt="n",
      #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #         )
      #   text(0.03,0.08,paste("HYDE 3.2,",periodsName[i],"CE"),cex=1.5,adj=c(0,0))

      #   image( 
      #         field_kk11_1 - field_kk11_0, 
      #         ylim=ylim, 
      #         zlim=range(lev), yaxt="n", xaxt="n",
      #         col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #         )
      #   text(0.03,0.08,paste("KK10,",periodsName[i],"CE"),cex=1.5,adj=c(0,0))

      # dev.off()


      magn <- 3.5
      ncols <- 3
      nrows <- 2
      widths <- rep(1.6*magn,ncols)
      widths[1] <- 0.15*widths[2]
      heights <- rep(0.9*magn,nrows)
      order <- matrix(c(1:(nrows*ncols)),nrows,ncols,byrow=TRUE)
      
      ylim <- c(-60,85)
      lat.labels <- seq(-90, 90, 30)
      lat.short  <- seq(-90, 90, 10)
      lon.labels <- seq(-180, 180, 60)
      lon.short  <- seq(-180, 180, 10)

      a <- sapply( lat.labels, function(x) bquote(.(x)*degree ~ N) )
      b <- sapply( lon.labels, function(x) bquote(.(x)*degree ~ E) )

      lev <- c(0, 0.01, 0.1, 0.15, 0.2, 0.4, 0.7, 1)
      color <- c("wheat3","green4","yellow","orange","red")

      color <- c("wheat3","green4","yellow","orange","red")
      filn <- paste( "../fig/fig_LUC_maps/LUC_map_lastmill_p",as.character(period_margins[i]),".pdf", sep="" )
      pdf( filn, width=sum(widths), height=sum(heights) )

        panel <- layout(
                  order,
                  widths=widths,
                  heights=heights,
                  TRUE
                  )
        # layout.show(panel)

        ## Color key
        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )
        
        ## period i
        image(
              lon, lat,
              field_hyde31_0,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(-173,-56,paste("HYDE 3.1,",as.character(period_margins[i]),"CE"),cex=1.5,adj=c(0,0))
        axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
        axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 4, at=lat.labels, lab=F, lwd=1.5 )
        axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
        axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        axis( 3, at=lon.labels, lab=F, lwd=1.5 )
        axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        image(
              lon, lat,
              field_hyde32_0,
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(-173,-56,paste("HYDE 3.2,",as.character(period_margins[i]),"CE"),cex=1.5,adj=c(0,0))
        axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
        axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 4, at=lat.labels, lab=F, lwd=1.5 )
        axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
        axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        axis( 3, at=lon.labels, lab=F, lwd=1.5 )
        axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
        out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

        image( 
              lon, lat,
              field_kk11_0, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(-173,-56,paste("KK10,",as.character(period_margins[i]),"CE"),cex=1.5,adj=c(0,0))
        axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
        axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 4, at=lat.labels, lab=F, lwd=1.5 )
        axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
        axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        axis( 3, at=lon.labels, lab=F, lwd=1.5 )
        axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        image( 
              lon, lat,
              field_kk11d_0, 
              ylim=ylim, 
              zlim=range(lev), yaxt="n", xaxt="n",
              col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
              )
        text(-173,-56,paste("KK10D,",as.character(period_margins[i]),"CE"),cex=1.5,adj=c(0,0))
        axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
        axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 4, at=lat.labels, lab=F, lwd=1.5 )
        axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

        axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
        axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        axis( 3, at=lon.labels, lab=F, lwd=1.5 )
        axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

        dev.off()

      # if (i==1){

      #   filn <- paste( "../fig/fig_LUC_maps/LUC_map_lastmill_p",as.character(period_margins[i]),".pdf", sep="" )
      #   pdf( filn, width=sum(widths), height=sum(heights) )

      #     panel <- layout(
      #               order,
      #               widths=widths,
      #               heights=heights,
      #               TRUE
      #               )
      #     # layout.show(panel)

      #     ## Color key
      #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
      #     out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

      #     ## period i
      #     image(
      #           lon, lat,
      #           field_hyde31_1,
      #           ylim=ylim, 
      #           zlim=range(lev), yaxt="n", xaxt="n",
      #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #           )
      #     text(-173,-56,paste("HYDE 3.1,",as.character(-periodsBP[i+1]/1000),"ka BP"),cex=1.5,adj=c(0,0))
      #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
      #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
      #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
      #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
      #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     image(
      #           lon, lat,
      #           field_hyde32_1,
      #           ylim=ylim, 
      #           zlim=range(lev), yaxt="n", xaxt="n",
      #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #           )
      #     text(-173,-56,paste("HYDE 3.2,",as.character(-periodsBP[i+1]/1000),"ka BP"),cex=1.5,adj=c(0,0))
      #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
      #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
      #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.8, lwd=1.5, mgp=c(1,0.5,0) )
      #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
      #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     par(mar=c(1,3,1,1),xaxs="i", yaxs="i",las=1)
      #     out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE )

      #     image( 
      #           lon, lat,
      #           field_kk11_1, 
      #           ylim=ylim, 
      #           zlim=range(lev), yaxt="n", xaxt="n",
      #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #           )
      #     text(-173,-56,paste("KK10,",as.character(-periodsBP[i+1]/1000),"ka BP"),cex=1.5,adj=c(0,0))
      #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
      #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
      #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
      #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
      #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     image( 
      #           lon, lat,
      #           field_kk11d_1, 
      #           ylim=ylim, 
      #           zlim=range(lev), yaxt="n", xaxt="n",
      #           col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
      #           )
      #     text(-173,-56,paste("KK10D,",as.character(-periodsBP[i+1]/1000),"ka BP"),cex=1.5,adj=c(0,0))
      #     axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.8, lwd=1.5 )
      #     axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 4, at=lat.labels, lab=F, lwd=1.5 )
      #     axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 1, at=lon.labels, lab=F, cex.axis=0.8, lwd=1.5 )
      #     axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #     axis( 3, at=lon.labels, lab=F, lwd=1.5 )
      #     axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

      #   dev.off()

      # }

    }
