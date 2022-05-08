
source('func.R')
load(file='/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_month_532.Rdata')
recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)
plot.mcwd.month.mean=apply(plot_mcwd_month_mod[,-(recent-1902+1),],c(1,3),mean)
plot.mcwd.month.sd=apply(plot_mcwd_month_mod[,-(recent-1902+1),],c(1,3),sd)

plot.mcwd.drought=rep(NA,532*8*12)
dim(plot.mcwd.drought)=c(532,8,12)
aa=0
for (year in recent) {
	aa=aa+1
	plot.mcwd.drought[,aa,]=plot_mcwd_month_mod[,year-1902+1,]
}

plot.mcwd.month.zscore=rep(NA,532*8*12)
dim(plot.mcwd.month.zscore)=c(532,8,12)
for (year in 1:8) {
	plot.mcwd.month.zscore[,year,1:12]=(plot.mcwd.drought[,year,1:12]-plot.mcwd.month.mean[,1:12])/plot.mcwd.month.sd[,1:12]
}

plot.mcwd.month.zscore[plot.mcwd.month.zscore<(-10)]=-9.9
plot.mcwd.month.zscore[is.na(plot.mcwd.month.zscore)]=0

plot.month.zscore.stat=rep(NA,8*12*5)
dim(plot.month.zscore.stat)=c(8,12,5)

for (year in 1:8) {
	for (month in 1:12) {
		plot.month.zscore.stat[year,month,1]=sum(plot.mcwd.month.zscore[,year,month]>=0)
		plot.month.zscore.stat[year,month,2]=sum(plot.mcwd.month.zscore[,year,month]<0 & plot.mcwd.month.zscore[,year,month]>(-1.645))
		plot.month.zscore.stat[year,month,3]=sum(plot.mcwd.month.zscore[,year,month]<(-1.645) & plot.mcwd.month.zscore[,year,month]>(-1.96))
		plot.month.zscore.stat[year,month,4]=sum(plot.mcwd.month.zscore[,year,month]<(-1.96) & plot.mcwd.month.zscore[,year,month]>(-2.576))
		plot.month.zscore.stat[year,month,5]=sum(plot.mcwd.month.zscore[,year,month]<(-2.576))
	}
}


#############################
tiff('Figure 5 month mcwd zscore o8May.tiff',width=900,height=400)
cols=c('white',brewer.pal(4,'Reds'))
par(mfrow=c(2,4))

for (year in 1:8) {
	aa=plot.month.zscore.stat[year,,]
	aa=t(aa)
	par(mar=c(5,5,3,3))
	barplot(aa,col=cols,names.arg=c(10:12,1:9),main=elnino[year],cex.main=2,
	cex.axis=2,cex.lab=2,ylab='# pixels',cex.names=2,xlab='Month')	
}
dev.off()
