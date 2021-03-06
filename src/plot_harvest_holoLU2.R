hyde31 <- read.table("data/aharv_holoLU2_hyde31.dat", col.names=c("year","aharv"))
hyde32 <- read.table("data/aharv_holoLU2_hyde32.dat", col.names=c("year","aharv"))
kk10   <- read.table("data/aharv_holoLU2_kk10.dat", col.names=c("year","aharv"))
kk10D  <- read.table("data/aharv_holoLU2_kk10D.dat", col.names=c("year","aharv"))
hurtt  <- read.table("data/aharv_hurtt.dat", col.names=c("year","aharv"))

pdf("fig/aharv.pdf", width=6, height=4 )
par( las=1, mar=c(4.5,4.5,1,1), xaxs="i", yaxs="i" )
plot( hyde31$year, hyde31$aharv*1e-9, type="l", xlab="year CE", ylab=expression(paste("global harvested area (10"^{9},"m"^{2},")")), ylim=c(0,75), xlim=c(-1000,2000), lwd=2, col="red" )
lines( hurtt$year, hurtt$aharv*1e-9, lwd=2 )
lines( hyde32$year, hyde32$aharv*1e-9, col="tomato", lwd=2 )
lines( kk10$year, kk10$aharv*1e-9, col="blue", lwd=2 )
lines( kk10D$year, kk10D$aharv*1e-9, col="cyan", lwd=2 )

legend("topleft",c("Hurtt et al., 2006","HYDE 3.1","HYDE 3.2","KK10","KK10D"), col=c("black", "red", "tomato", "blue", "cyan"), lty=1, bty="n", lwd=2 )
dev.off()
