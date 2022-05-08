source('func.R')

load(file='combine3_crujraS2gain_feld0823.Rdata')
load(file='combine3_crujraS2loss_feld0823.Rdata')
load(file='combine3_crujraS2biomassnet_feld0823.Rdata')

nonan=sum(!is.na(crujraS2biomassnet.feld[1,]))

tiff('Figure 2 crujraS2 hubau o8May.tiff',height=500,width=400)
par(mfrow=c(3,1))
par(mar=c(5,5,3,3))
net.mean=apply(crujraS2biomassnet.feld,1,mean, na.rm=TRUE)
net.sd=apply(crujraS2biomassnet.feld,1,sd, na.rm=TRUE)
net.se=net.sd/sqrt(nonan)
low.pred=net.mean-1.96*net.se
high.pred=net.mean+1.96*net.se
plot(1980:2019,net.mean[79:118],
ylab=expression(MgC~ha^-1~yr^-1),cex.lab=2,cex.axis=2,xlab='',
col='white',main='(a) Net carbon sink',cex.main=2)
newx=1902:2019
polygon(c(rev(newx), newx), c(rev(low.pred), high.pred), col = 'grey80', border = NA)
lines(1980:2019,net.mean[79:118],lwd=2)
yy=net.mean[79:118]
xx=1980:2019
lmm=lm(yy~xx)
slope=round(lmm$coefficients[2],3)
pval=0.21
legend('bottom',legend=paste0('slope=',slope,' p=',pval),cex=2,bty='n')

add_interval(xx,yy)
#lines(Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("AGBnetchange.ha.yr")]~Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("year")],lwd=2,lty=mean_type  ,col=col_amazon,type='l')


par(mar=c(5,5,3,3))
net.mean=apply(crujraS2gain.feld,1,mean,  na.rm=TRUE)
net.sd=apply(crujraS2gain.feld,1,sd,  na.rm=TRUE)
net.se=net.sd/sqrt(nonan)
low.pred=net.mean-1.96*net.se
high.pred=net.mean+1.96*net.se
plot(1980:2019,net.mean[79:118],xlab='',
ylab=expression(MgC~ha^-1~yr^-1),cex.lab=2,cex.axis=2,main='(b) Carbon gains',cex.main=2,
col='white')
polygon(c(rev(newx), newx), c(rev(low.pred), high.pred), col = 'grey80', border = NA)
lines(1980:2019,net.mean[79:118],lwd=2)
yy=net.mean[79:118]
xx=1980:2019
lmm=lm(yy~xx)
slope=round(lmm$coefficients[2],3)
pval=0.001
legend('bottom',legend=paste0('slope=',slope,' p<',pval),cex=2,bty='n')
add_interval(xx,yy)
#lines(Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("AGBGain_tot.ha.yr")]~Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("year")],
#lwd=2.0,lty=mean_type,col=col_amazon,type='l')


par(mar=c(5,5,3,3))
net.mean=apply(crujraS2loss.feld,1,mean,  na.rm=TRUE)
net.sd=apply(crujraS2loss.feld,1,sd,  na.rm=TRUE)
net.se=net.sd/sqrt(nonan)
low.pred=net.mean-1.96*net.se
high.pred=net.mean+1.96*net.se
plot(1980:2019,net.mean[79:118],
ylab=expression(MgC~ha^-1~yr^-1),cex.lab=2,cex.axis=2,xlab='',main='(c) Carbon loss',cex.main=2,
col='white')
polygon(c(rev(newx), newx), c(rev(low.pred), high.pred), col = 'grey80', border = NA)
lines(1980:2019,net.mean[79:118],lwd=2)
yy=net.mean[79:118]
xx=1980:2019
lmm=lm(yy~xx)
slope=round(lmm$coefficients[2],3)
pval=0.001
legend('top',legend=paste0('slope=',slope,' p<',pval),cex=2,bty='n')
add_interval(xx,yy)
#lines(Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("AGBMor_tot.ha.yr")]~Basin_long_pl_wss_amazon[1:length(Basin_long_pl_wss_amazon$year),
#c("year")],lwd=2,lty=mean_type ,col=col_amazon, type='l')

dev.off()

