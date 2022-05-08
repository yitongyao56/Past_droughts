source('func.R')

load(file='combine3_crujraS2gain_feld0823.Rdata')
load(file='combine3_crujraS2loss_feld0823.Rdata')
load(file='combine3_crujraS2biomassnet_feld0823.Rdata')

net_decade_drought=rep(NA,532*6)
dim(net_decade_drought)=c(532,6)
for (ss in 1:532) {
	for (decade in 1:6) {   # 
		peri=((decade-1)*20+1):(decade*20)
		if (decade==6) {
			peri=101:118
		}
		net_decade_drought[ss,decade]=mean(crujraS2loss.feld[peri,ss])
	}
}	

tiff('Figure 9 separated AGB loss bar overlap o8May.tiff',width=800,height=600)
load('/home/orchidee01/yyao/rebuild/pixel_data_532.Rdata')
regionname=c('1AMA_GuSh','3AMA_W','2AMA_EC','4AMA_BrSh')
regionlabel=c('Guiana Shield','Western Amazon','East-central Amazon','Brazilian Shield')
decade_text=c('1901-1920','1921-1940','1941-1960',
'1961-1980','1981-2000','2001-2019')
cols=brewer.pal(7,'Set2')
cols=cols[-6]
cols[6]='#880808'
par(mfrow=c(2,2))
for (region in 1:4) {
	rows=which(pixel.data$region==regionname[region])
	for (peri in 1:6) {
		A=net_decade_drought[rows,peri]  # 1961-1980
		#B=net_decade_drought[rows,5]  # 1981-2000
		#C=net_decade_drought[rows,6]  # 2001-2019
		A[A>8]=7.9
		ax=seq(0,8,by=1)
		hgA <- hist(A, breaks = ax, plot = FALSE)
	
	
		if (peri==1) {
			par(mar=c(5,5,3,3))
	plot(hgA$mids, hgA$counts,col = 'white',bg='white',
	main=regionlabel[region],xaxs='i',yaxs='i',
	xlab=expression(Carbon~losses~(MgC~ha^-1~yr^-1)),
	ylab='# pixels',cex.axis=2,cex.lab=2,cex.main=2,ylim=c(0,100))
		}
		lines(hgA$mids,hgA$counts,col=cols[peri],lwd=2)
		if (peri==6) {
			lines(hgA$mids,hgA$counts,col=cols[peri],lwd=4)
		}
	#plot(hgB, col = c2, add = TRUE)
	#plot(hgC, col = c3, add = TRUE)
	}
	if (region==1) {
		legend('topright',legend=decade_text,text.col=cols,bty='n',cex=2)
	}
}

dev.off()
