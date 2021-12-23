## /////////////////////////////////////////////////////////////////////////////////////////
## Reads simulated LPX peat C and creates array to be plotted.
## -----------------------------------------------------------------------------------------
trace129 <- list()

## peat variables averaged across sites
filn <- "data/peatvars_avg_across_sites_trace21_129.dat"
col.names <- c( "time", "peatc_dens_avg_site", "peatnep_dens_avg_site", "peatnpp_dens_avg_site", "peatrh_dens_avg_site" )
trace129$vars_site <- read.table( filn, col.names=col.names )

## peatland area
filn <- "data/peatarea_trace21_129.dat"
col.names <- c( "time", "peatarea_global", "peatarea_maskedby_yu", "peatarea_maskedby_yu_north" )
trace129$area <- read.table( filn, col.names=col.names )

## add scaled area 
scale <- 3.7e12 / mean(trace129$area$peatarea_maskedby_yu_north[2190:2204])
trace129$area$peatarea_maskedby_yu_north_scaled <- trace129$area$peatarea_maskedby_yu_north * scale

## peat C
filn <- "data/peatc_trace21_129.dat"
col.names <- c( "time", "peatc_global", "peatc_maskedby_yu", "peatc_maskedby_yu_north", "netpeat" )
trace129$c <- read.table( filn, col.names=col.names )
trace129$c$peatc_at_sites <- trace129$vars_site$peatc_dens_avg_site * trace129$area$peatarea_maskedby_yu_north_scaled

## FOR EVALUATION OF PERIODS: HOLOCENE
  ## Alternativ: fixe Margins
  periodsBP <- read.csv( 'data/periods_holocene.csv' )$periodsBP
  periodsAD <- periodsBP + 1950
  periodsName <- paste( 
    as.character( -periodsBP*1e-3 )[1:length(periodsBP)-1],
    "-",
    as.character( -periodsBP*1e-3 )[2:length(periodsBP)],
    sep=""
    )
  period_margins <- periodsAD
  nper <- length(period_margins)-1

  ## period margins (start and end)
  dc_pt_per_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_pt_per_lpx$iper_start <- rep( NA, nper )
  dc_pt_per_lpx$iper_end <- rep( NA, nper )

  ## period margin's corresponding index in full (annual) data frame
  for (i in 1:nper){

    dc_pt_per_lpx$iper_start[i] <- which.min( abs( dc_pt_per_lpx$per_start[i] - trace129$area$time ) )
    dc_pt_per_lpx$iper_end[i]   <- which.min( abs( dc_pt_per_lpx$per_end[i] - trace129$area$time ) )

    dc_pt_per_lpx$netpeat[i]                 <- (trace129$c$netpeat[dc_pt_per_lpx$iper_end[i]]                 - trace129$c$netpeat[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_global[i]            <- (trace129$c$peatc_global[dc_pt_per_lpx$iper_end[i]]            - trace129$c$peatc_global[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_maskedby_yu[i]       <- (trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_end[i]]       - trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_maskedby_yu_north[i] <- (trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_end[i]] - trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_at_sites[i]          <- (trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_end[i]]          - trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_start[i]])/1e15

  }

  pt_lpx_outarr <- subset( dc_pt_per_lpx, select=c( netpeat, peatc_global, peatc_maskedby_yu, peatc_maskedby_yu_north, peatc_at_sites ) )
  row.names( pt_lpx_outarr ) <- periodsName

  save( pt_lpx_outarr, file="data/dc_peat_lpx.Rdata" )

## FOR EVALUATION OF PERIODS: LAST MILLENNIUM
  period_margins <- read.csv( 'data/periods_lastmill.csv' )$period_margins
  periodsName <- paste( 
    as.character( period_margins )[1:length(period_margins)-1],
    "-",
    as.character( period_margins )[2:length(period_margins)],
    sep=""
    )
  nper <- length(period_margins)-1

  ## period margins (start and end)
  dc_pt_per_lpx <- data.frame( per_start=period_margins[1:(length(period_margins)-1)], per_end=period_margins[2:length(period_margins)] )
  dc_pt_per_lpx$iper_start <- rep( NA, nper )
  dc_pt_per_lpx$iper_end <- rep( NA, nper )

  ## period margin's corresponding index in full (annual) data frame
  for (i in 1:nper){

    dc_pt_per_lpx$iper_start[i] <- which.min( abs( dc_pt_per_lpx$per_start[i] - trace129$area$time ) )
    dc_pt_per_lpx$iper_end[i]   <- which.min( abs( dc_pt_per_lpx$per_end[i] - trace129$area$time ) )

    dc_pt_per_lpx$netpeat[i]                 <- (trace129$c$netpeat[dc_pt_per_lpx$iper_end[i]]                 - trace129$c$netpeat[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_global[i]            <- (trace129$c$peatc_global[dc_pt_per_lpx$iper_end[i]]            - trace129$c$peatc_global[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_maskedby_yu[i]       <- (trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_end[i]]       - trace129$c$peatc_maskedby_yu[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_maskedby_yu_north[i] <- (trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_end[i]] - trace129$c$peatc_maskedby_yu_north[dc_pt_per_lpx$iper_start[i]])/1e15
    dc_pt_per_lpx$peatc_at_sites[i]          <- (trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_end[i]]          - trace129$c$peatc_at_sites[dc_pt_per_lpx$iper_start[i]])/1e15

  }

  pt_lpx_lastmill_outarr <- subset( dc_pt_per_lpx, select=c( netpeat, peatc_global, peatc_maskedby_yu, peatc_maskedby_yu_north, peatc_at_sites ) )
  row.names( pt_lpx_lastmill_outarr ) <- paste( as.character( period_margins[1:nper] ), "-", as.character( period_margins[2:(nper+1)] ), sep="" )

  save( pt_lpx_lastmill_outarr, file="data/dc_peat_lpx_lastmill.Rdata" )




