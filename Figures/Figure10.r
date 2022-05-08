source('func.R')

recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)

load('/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_annual_532.Rdata')

mcwd_feld.mean=apply(plot_mcwd_annual_mod[,-(recent-1902+1)],1,mean)
mcwd_feld.sd=apply(plot_mcwd_annual_mod[,-(recent-1902+1)],1,sd)


zscore.119=rep(NA,532*118)
dim(zscore.119)=c(532,118)
for (year in 1:118) {
	zscore.119[,year]=(plot_mcwd_annual_mod[,year]-mcwd_feld.mean)/mcwd_feld.sd
}


load(file='combine3_crujraS1gain_feld0823.Rdata')
load(file='combine3_crujraS1loss_feld0823.Rdata')
load(file='combine3_crujraS1biomassnet_feld0823.Rdata')


load(file='combine3_crujraS2gain_feld0823.Rdata')
load(file='combine3_crujraS2loss_feld0823.Rdata')
load(file='combine3_crujraS2biomassnet_feld0823.Rdata')



load(file='combine3_crujraS3gain_feld0823.Rdata')
load(file='combine3_crujraS3loss_feld0823.Rdata')
load(file='combine3_crujraS3biomassnet_feld0823.Rdata')


elnino=c('1916','1926*','1963','1983*','1998*','2005','2010','2016*')
tiff('Figure 10 drought 3 scenarios o8May.tiff',width=1000,height=500)
par(mfrow=c(2,4))
aa=0
for (year in recent) {
	aa=aa+1
	mcwd.anomaly=zscore.119[,year-1902+1]
	rows=which(!is.na(mcwd.anomaly) & mcwd.anomaly<(-1.645))
	net.S1=mean(crujraS1biomassnet.feld[year-1902+1,rows])
	net.S2=mean(crujraS2biomassnet.feld[year-1902+1,rows])
	net.S3=mean(crujraS3biomassnet.feld[year-1902+1,rows])
	
	gain.S1=mean(crujraS1gain.feld[year-1902+1,rows])
	gain.S2=mean(crujraS2gain.feld[year-1902+1,rows])
	gain.S3=mean(crujraS3gain.feld[year-1902+1,rows])
	
	loss.S1=mean(crujraS1loss.feld[year-1902+1,rows])
	loss.S2=mean(crujraS2loss.feld[year-1902+1,rows])
	loss.S3=mean(crujraS3loss.feld[year-1902+1,rows])
	
	yy=rep(NA,3*3)
	dim(yy)=c(3,3)
	yy[1,1]=net.S1
	yy[2,1]=net.S2
	yy[3,1]=net.S3
	
	yy[1,2]=gain.S1
	yy[2,2]=gain.S2
	yy[3,2]=gain.S3
	
	yy[1,3]=loss.S1
	yy[2,3]=loss.S2
	yy[3,3]=loss.S3
	
	
	par.data=rep(NA,length(rows)*3*3)
	dim(par.data)=c(length(rows),3,3)
	for (ss in 1:3) {
		ev_text=paste0("net.feld=crujraS",ss,"biomassnet.feld")
		eval(parse(text=ev_text))
		ev_text=paste0("gain.feld=crujraS",ss,"gain.feld")
		eval(parse(text=ev_text))
		ev_text=paste0("loss.feld=crujraS",ss,"loss.feld")
		eval(parse(text=ev_text))
		
		par.data[,ss,1]=net.feld[year-1902+1,rows]
		par.data[,ss,2]=gain.feld[year-1902+1,rows]
		par.data[,ss,3]=-loss.feld[year-1902+1,rows]		
	}
	vv_name=c('Net','Gain','Loss')
	ss_name=c('S1','S2','S3')
	scenarios=c()
	indicators=c()
	donnee=c()
	for (ss in 1:3) {
		for (vv in 1:3) {
			scenarios=c(scenarios,rep(ss_name[ss],length(rows)))
			indicators=c(indicators,rep(vv_name[vv],length(rows)))
			donnee=c(donnee,par.data[,ss,vv])
		}
	}
	
	plot.data=data.frame(scenario=scenarios,variables=indicators,donnee=donnee)
	#barplot(yy,beside=TRUE,main=year,cex.main=2)	
	cols=c('#7fc97f','#beaed4','#fdc086')
	par(mar=c(5,5,3,3))
	boxplot(donnee~scenario+variables,data=plot.data,main=elnino[aa],cex.main=2,
	col=cols,ylim=c(-25,10),ylab=expression(unit~':'~MgC~ha^-1~yr^-1),
	cex.lab=2,cex.axis=2,at=c(1:3,5:7,9:11),names=c('','Gain','','','Loss','','','Net',''),
	las=2, xlab='')
	abline(h=0,lty=2,lwd=2)
	if (year==1916) {
		legend('bottomright',legend=c('S1','S2','S3'),bty='n',
		col=cols,text.col=cols,cex=2)
	}
	abline(v=4,lty=2)
	abline(v=8,lty=2)
	#text(2, -30, 'Gain', cex=2, xpd=TRUE)
	#text(6, -30, 'Loss', cex=2, xpd=TRUE)
	#text(10, -30, 'Net', cex=2, xpd=TRUE)
}
dev.off()
