
source('func.R')
library(R.matlab)
amazon=readMat('/home/orchidee01/yyao/rebuild/amazon_papa.mat')
amazon_lon=amazon$amazon.lon
amazon_lat=amazon$amazon.lat
#lines(amazon_lon,amazon_lat,lwd=2)

load('amazon_cover.Rdata')
load('istree.Rdata')

load('pixel_data_532.Rdata')
library(maps)
recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)
elnino=c('1916','1926*','1963','1983*','1998*','2005','2010','2016*')
tiff('Figure 6 eight drought events 20 century net AGB feld o8May.tiff',width=1000,height=500) 
recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)
cols=brewer.pal(10,'RdYlGn')
par(mfrow=c(2,4))
xx=0
for (year in recent) {
	grid_lat=pixel.data$latitude
	grid_lon=pixel.data$longitude
	grid_pixel.recent=rep(NA,180*360)
	dim(grid_pixel.recent)=c(180,360)
	for (i in 1:532) {
		lon.loc=floor((180+grid_lon[i])/1)+1
		lat.loc=floor((90-grid_lat[i])/1)+1
		if (istree[i]) {
			grid_pixel.recent[lat.loc,lon.loc]=crujraS2biomassnet.feld[year-1902+1,i]
		}
	}
	grid_pixel.recent=t(grid_pixel.recent)
	
	grid_pixel.recent[grid_pixel.recent<(-8)]=-7.9
	grid_pixel.recent[grid_pixel.recent>8]=7.9
	cols=brewer.pal(8,'RdYlGn')
	par(mar=c(3,3,2,2))
	xx=xx+1
	image(seq(-179.5,179.5,by=1),seq(-89.5,89.5,by=1),grid_pixel.recent[,180:1],
	ylim=c(-23,15),xlim=c(-80,-45),zlim=c(-8,8),main=elnino[xx],cex.main=2,xlab='',ylab='',
	xaxt='n',yaxt='n',col=cols,cex.axis=2)

	map(database = "world",ylim=c(-23,15), xlim=c(-80,-45),
		 mar=c(1,1,1,1),col='gray', panel.first = grid(),add=TRUE,lwd=2)
	map.axes(cex.axis=2,lwd=2, las=1)
	lines(amazon_lon,amazon_lat,lwd=2)
	#mtext(expression(MgC~ha^-1~yr^-1),side=3,cex=1.2,line=0.5,at=-43)
}

dev.off()
