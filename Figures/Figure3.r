source('func.R')

load(file='combine3_crujraS2gain_feld0823.Rdata')
load(file='combine3_crujraS2loss_feld0823.Rdata')
load(file='combine3_crujraS2biomassnet_feld0823.Rdata')

recent=c(1916, 1926, 1963, 1983, 1998,2005,2010,2016)


load('/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_annual_532.Rdata')
plot_mcwd_119.mean=apply(plot_mcwd_annual_mod[,-(recent-1902+1)],1,mean)
plot_mcwd_119.sd=apply(plot_mcwd_annual_mod[,-(recent-1902+1)],1,sd)


zscore.feld.118=rep(NA,532*118)
dim(zscore.feld.118)=c(532,118)
for (year in 1:118) {
	zscore.feld.118[,year]=(plot_mcwd_annual_mod[,year]-plot_mcwd_119.mean)/plot_mcwd_119.sd
}


diff.net.feld.2005=diff_crujra.feld(crujraS2biomassnet.feld,2005)
diff.gain.feld.2005=diff_crujra.feld(crujraS2gain.feld,2005)
diff.loss.feld.2005=diff_crujra.feld(crujraS2loss.feld,2005)

diff.net.feld.2016=diff_crujra_2016.feld(crujraS2biomassnet.feld,2016)
diff.gain.feld.2016=diff_crujra_2016.feld(crujraS2gain.feld,2016)
diff.loss.feld.2016=diff_crujra_2016.feld(crujraS2loss.feld,2016)

diff.net.feld.2010=diff_crujra_2010.feld(crujraS2biomassnet.feld,2010)
diff.gain.feld.2010=diff_crujra_2010.feld(crujraS2gain.feld,2010)
diff.loss.feld.2010=diff_crujra_2010.feld(crujraS2loss.feld,2010)

load('/home/orchidee01/yyao/rebuild/pixel_data_532.Rdata')
newprepare=pixel.data


tiff('Figure 3 crujra S2 figure 2 p-100mm o8May.tiff',width=900,height=700)

par(mfrow=c(3,3))
par(mar=c(5,5,3,3))
mcwd.diff=diff.net.feld.2005[,1]
biomass.diff=diff.net.feld.2005[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-30,10),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Net carbon sink',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
cols=c('#4daf4a','#e41a1c','#a65628','#377eb8')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)
mtext("2005",cex=2,side=3,line=0.5,at=-100)

par(mar=c(5,5,3,3))
mcwd.diff=diff.gain.feld.2005[,1]
biomass.diff=diff.gain.feld.2005[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-3,3),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon gains',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}	
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)


par(mar=c(5,5,3,3))
mcwd.diff=diff.loss.feld.2005[,1]
biomass.diff=diff.loss.feld.2005[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-10,30),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon losses',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)


#par(mfrow=c(1,3))
par(mar=c(5,5,3,3))
mcwd.diff=diff.net.feld.2010[,1]
biomass.diff=diff.net.feld.2010[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-30,10),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Net carbon sink',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
cols=c('#4daf4a','#e41a1c','#a65628','#377eb8')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)
mtext("2010",cex=2,side=3,line=0.5,at=-75)

par(mar=c(5,5,3,3))
mcwd.diff=diff.gain.feld.2010[,1]
biomass.diff=diff.gain.feld.2010[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-3,3),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon gains',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}	
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)


par(mar=c(5,5,3,3))
mcwd.diff=diff.loss.feld.2010[,1]
biomass.diff=diff.loss.feld.2010[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-10,30),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon losses',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)



par(mar=c(5,5,3,3))
mcwd.diff=diff.net.feld.2016[,1]
biomass.diff=diff.net.feld.2016[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-30,10),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Net carbon sink',cex.main=2)

reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
cols=c('#4daf4a','#e41a1c','#a65628','#377eb8')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)
mtext("2016",cex=2,side=3,line=0.5,at=-190)

par(mar=c(5,5,3,3))
mcwd.diff=diff.gain.feld.2016[,1]
biomass.diff=diff.gain.feld.2016[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-3,3),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon gains',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}	
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)


par(mar=c(5,5,3,3))
mcwd.diff=diff.loss.feld.2016[,1]
biomass.diff=diff.loss.feld.2016[,2]
plot(mcwd.diff,biomass.diff,pch=19,col='white',ylim=c(-10,30),
xlab='MCWD anomaly (mm)',ylab=expression(unit:~MgC~ha^-1~yr^-1),
cex.axis=2,cex.lab=2,main='Carbon losses',cex.main=2)
reg=c('1AMA_GuSh','2AMA_EC','3AMA_W','4AMA_BrSh')
#cols=c('#ACC6A9','#F9ABAB','#F3CFAB','#ACBDD9')
for (pp in 1:4) {
	rows=which(newprepare$region==reg[pp])
	points(mcwd.diff[rows],biomass.diff[rows],col=cols[pp],pch=19)
}
xx=mcwd.diff
yy=biomass.diff
add_interval(xx,yy)
abline(v=0,lty=2)
abline(h=0,lty=2)
lmm=lm(yy~xx)
coefs=lmm$coefficients[2]*100
legend('topright',legend=paste0('slope=',round(coefs,2),'/100mm'),bty='n',cex=2)

dev.off()






