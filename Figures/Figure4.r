source('func.R')
library(maps)

library(R.matlab)
amazon=readMat('/home/orchidee01/yyao/rebuild/amazon_papa.mat')
amazon_lon=amazon$amazon.lon
amazon_lat=amazon$amazon.lat

tiff('Figure 4 Zscore annual 8 droughts 3Dec.tiff',width=1000,height=500)
recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)
elnino=c('1916','1926*','1963','1983*','1998*','2005','2010','2016*')
cols=brewer.pal(6,'Reds')[6:1]
par(mfrow=c(2,4))
aa=0
for (year in recent) {
	grid_lat=pixel.data$latitude
	grid_lon=pixel.data$longitude
	grid_pixel.recent=rep(NA,180*360)
	dim(grid_pixel.recent)=c(180,360)
	for (i in 1:532) {
		lon.loc=floor((180+grid_lon[i])/1)+1
		lat.loc=floor((90-grid_lat[i])/1)+1
		grid_pixel.recent[lat.loc,lon.loc]=zscore.feld.118[i,year-1902+1]
	}
	grid_pixel.recent=t(grid_pixel.recent)
	grid_pixel.recent[grid_pixel.recent>0]=NA
	#grid_pixel.recent[grid_pixel.recent<(-1.6)]=1
	grid_pixel.recent[grid_pixel.recent<(-6)]=-5.9
	#grid_pixel.recent[grid_pixel.recent<(-10)]=-10
	#grid_pixel.recent[grid_pixel.recent>10]=10
	
	par(mar=c(3,3,3,3))
	aa=aa+1
	image(seq(-179.5,179.5,by=1),seq(-89.5,89.5,by=1),grid_pixel.recent[,180:1],
	ylim=c(-23,15),xlim=c(-80,-45),main=elnino[aa],cex.main=2,zlim=c(-6,0),
	breaks=seq(-6,0,by=1),lab.breaks=c('',seq(-5,0,by=1)),col=cols,xlab='',ylab='',
	axis.args=list(cex.axis=2),xaxt='n',yaxt='n',legend.shrink=1)
	lines(amazon_lon,amazon_lat,lwd=2)
	map(database = "world",ylim=c(-23,15), xlim=c(-80,-45),
		 mar=c(1,1,1,1),col='gray', panel.first = grid(),add=TRUE)
	map.axes(cex.axis=2, las=1)
}
dev.off()


