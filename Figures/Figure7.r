source('func.R')

load(file='combine3_crujraS2gain_feld0823.Rdata')
load(file='combine3_crujraS2loss_feld0823.Rdata')
load(file='combine3_crujraS2biomassnet_feld0823.Rdata')

deltaAGB.degree=rep(NA,8*8)
dim(deltaAGB.degree)=c(8,8)
ax=seq(-8,8,by=2)

for (yr in 1:8) {
	year=recent[yr]	
	agbnet=crujraS2biomassnet.feld[year-1902+1,]
	agbnet[agbnet>8]=7.9
	agbnet[agbnet<(-8)]=-7.9
	for (rr in 1:8) {
		rows=which(agbnet>ax[rr] & agbnet<ax[rr+1])
		deltaAGB.degree[yr,rr]=length(rows)
	}
}

tiff('Figure 7 deltaAGB whole o8May.tiff',width=600,height=300)
cols=brewer.pal(8,'RdYlGn')
par(mar=c(5,5,3,3))
tt=t(deltaAGB.degree)
barplot(tt,col=cols,names.arg=recent,cex.names=1.5,
cex.lab=2,cex.axis=2,ylab='# pixels',width=0.1,space=1)
abline(h=0,lwd=2)
dev.off()