library(ncdf)
library(fields)
library(sp)
library(maptools)
library(rgeos)

source('~/.Rprofile')
source('/alphadata01/bstocker/utilities/cdf.write.R')

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

# image.plot( lon, lat,
#             nbp.ffluc.nat,
#             col=out.mycolorbar$colors, plot="n"
#             )
# data(wrld_simpl)
# plot(wrld_simpl, add = TRUE)

################
## SAGE
# ################
  ## Read land suitability field (SAGE)
  nc <- open.ncdf( "../data/peat_lpx_trace21_r129_holoLU2.nc" )
  fpeat <- get.var.ncdf(nc, "FPEAT")
  cpeat <- get.var.ncdf(nc, "CPEAT")
  lon <- get.var.ncdf(nc, "LONGITUDE")
  lat <- get.var.ncdf(nc, "LATITUDE")
  time <- get.var.ncdf(nc, "TIME")
  close.ncdf(nc)

  print(dim(fpeat))

  out <- fpeat[,,which.min(abs(time-2000))] - fpeat[,,which.min(abs(time-(1950-13000)))]

  magn <- 4
  ncols <- 2
  nrows <- 1
  widths <- rep(1.6*magn,ncols)
  widths[1] <- widths[2]*0.18
  heights <- rep(magn,nrows)
  order <- matrix( c(1:(nrows*ncols)), nrows, ncols, byrow=TRUE )

  lev_pos <- c(0.02,0.05,0.1,0.2,0.5,1)
  lev <- c(rev(-lev_pos),0,lev_pos)

  lat.labels <- seq(-90, 90, 30)
  lat.short  <- seq(-90, 90, 10)
  lon.labels <- seq(-180, 180, 60)
  lon.short  <- seq(-180, 180, 10)

  a <- sapply( lat.labels, function(x) bquote(.(x)*degree ~ N) )
  b <- sapply( lon.labels, function(x) bquote(.(x)*degree ~ E) )

  color <- rev(c("springgreen4","springgreen","wheat3","wheat3","orange","red"))

  ylim <- c(0.05,1)
  pdf( "../fig/fpeat.pdf", width=sum(widths), height=sum(heights) )

    panel <- layout(
              order,
              widths=widths,
              heights=heights,
              TRUE
              )
    # layout.show(panel)

    par( mar=c(3,3.5,1,1), xaxs="i", yaxs="i",las=1 )
    out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE, cex.axis=0.8 )
    par( mar=c(3,2,1,1), xaxs="i", yaxs="i",las=1 )
    image(
          lon, lat,
          out,
          zlim=range(lev), xlim=range(lon.labels), ylim=c(-60,85),
          yaxt="n", xaxt="n",
          col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
          )
    map( add=TRUE, interior=FALSE )
    # text( -165, -50, expression( Delta ~ italic("f")[peat] ),cex=1.5,adj=c(0,0))

    axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
    axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

    axis( 4, at=lat.labels, lab=F, lwd=1.5 )
    axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

    axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
    axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

    axis( 3, at=lon.labels, lab=F, lwd=1.5 )
    axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

  dev.off()

  ## Write peatland area change to NetCDF file
  idxs     <- which.min(abs(time-(1950-13000))):which.min(abs(time-2000))
  outfpeat <- fpeat[,,idxs]
  outcpeat <- cpeat[,,idxs]
  out      <- fpeat[,,idxs]
  time_out <- time[idxs]

  cdf.write( 
            outfpeat, "fpeat",
            lon, lat,
            "../data/peatland_area_carbon_13kaBP_2000CE_stocker17pnas.nc",
            z_dim          = NA,
            time           = time_out,
            make.zdim      = FALSE,
            make.tdim      = TRUE,
            nvars          = 2,
            var2           = outcpeat, varnam2 = "cpeat",
            vartype        = "NC_FLOAT",
            verbose        = TRUE,
            units_var1     = "gridcell area fraction",
            units_var2     = "gC m-2",
            units_time     = "year CE",
            long_name_var1 = "peatland area fraction",
            long_name_var2 = "peat soil C",
            glob_title     = "Peatland area fraction and soil carbon stocks from LPX (Stocker et al., 2017, PNAS), http://dx.doi.org/10.3334/ORNLDAAC/1382"
            )


