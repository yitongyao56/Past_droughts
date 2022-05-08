
library(pracma)
library(shape)
library(ncdf4)
library(fields)
library(RColorBrewer)

toyear=function(data) {
	final=c()
	for (year in 1:(length(data)/365)) {
		final=c(final,mean(data[((year-1)*365+1):(year*365)]))
	}
	return(final)
}

monthdays=c(31,28,31,30,31,30,31,31,30,31,30,31)
cum_monthdays=cumsum(monthdays)
# lai gpp biomass Re
myfunction=function(stom_var) {
   month_var=c()
   for (year in 1:(length(stom_var)/365)) {
        year_series=stom_var[((year-1)*365+1):(year*365)]
        for (month in 1:12) {
           month_series= year_series[(cum_monthdays[month]-monthdays[month]+1):cum_monthdays[month]]
           month_var=c(month_var,mean(month_series))
        }
   }
   return(month_var)
}

continuous=function(data,threshold) {
	days=0
	suc=0
	for (i in 1:365) {
		if (data[i]>50) {
			suc=suc+1
			if (suc>=threshold) {
				days=days+1
			}
		} else {
			suc=0
		}
	}
	return(days)
}

add_interval=function(xx,yy) {
	lmm=lm(yy~xx)
	newx=xx
	new.speeds <- data.frame(
	  mcwd = xx
	)
	preds=predict(lmm, newdata = new.speeds, interval = "confidence")
	order.newx=order(newx)
	sort.lwr=preds[order.newx,2]
	sort.upr=preds[order.newx,3]
	sort.fit=preds[order.newx,1]
	lines(newx[order.newx],sort.fit,lty=1,col='black')
	lines(newx[order.newx],sort.lwr,lty=2,col='black')
	lines(newx[order.newx],sort.upr,lty=2,col='black')
}


diff_crujra.feld=function(crujra.data,drought) {
	#load(paste0('/home/orchidee01/yyao/rebuild/plot_mcwd506_yr119_feld.Rdata'))
	load('/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_annual_532.Rdata')
	#load(file='/home/orchidee01/yyao/rebuild/plot_mcwd506_yr119_evap.Rdata')
	# 21 1999-2019 
	# 20 2000-2019 
	#rows=which(!is.na(study_biomass_s2_diff[1,]))
	## 1902 begin 2000
	plot_mcwd=plot_mcwd_annual_mod[,99:118]
	rows=1:532
	mcwd.pre2005=apply(plot_mcwd[rows,c(1:5)],1,mean)
	mcwd.2005=plot_mcwd[rows,drought-2000+1]
	
	study_biomass_s2=crujra.data[99:118,] # 2000-1902+1 # restore to 99:118
	biomass.pre2005=apply(study_biomass_s2[c(1:5),rows],2,mean)
	biomass.2005=study_biomass_s2[drought-2000+1,rows]
	mcwd.diff=mcwd.pre2005-mcwd.2005
	biomass.diff=biomass.2005-biomass.pre2005
	yyt=cbind(mcwd.diff,biomass.diff)
	return(yyt)
}

diff_crujra_2016.feld=function(crujra.data,drought) {
	#load(paste0('/home/orchidee01/yyao/rebuild/plot_mcwd506_yr119_feld.Rdata'))
	load('/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_annual_532.Rdata')
	# 21 1999-2019 
	# 20 2000-2019 
	#rows=which(!is.na(study_biomass_s2_diff[1,]))
	## 1902 begin 2000
	plot_mcwd=plot_mcwd_annual_mod[,99:118]
	rows=1:532
	mcwd.pre2005=apply(plot_mcwd[rows,c(1:5,7:10,12:16)],1,mean)
	mcwd.2005=plot_mcwd[rows,drought-2000+1]
	
	study_biomass_s2=crujra.data[99:118,] # 2000-1902+1
	biomass.pre2005=apply(study_biomass_s2[c(1:5,7:10,12:16),rows],2,mean)
	biomass.2005=study_biomass_s2[drought-2000+1,rows]
	mcwd.diff=mcwd.pre2005-mcwd.2005
	biomass.diff=biomass.2005-biomass.pre2005
	yyt=cbind(mcwd.diff,biomass.diff)
	return(yyt)
}



diff_crujra_2010.feld=function(crujra.data,drought) {
	#load(paste0('/home/orchidee01/yyao/rebuild/plot_mcwd506_yr119_feld.Rdata'))
	load('/home/orchidee01/yyao/rebuild/feld_plot_maximum_cumulative_wd_annual_532.Rdata')
	# 21 1999-2019 
	# 20 2000-2019 
	#rows=which(!is.na(study_biomass_s2_diff[1,]))
	## 1902 begin 2000
	plot_mcwd=plot_mcwd_annual_mod[,99:118]
	rows=1:532
	mcwd.pre2005=apply(plot_mcwd[rows,c(1:5,7:10)],1,mean)
	mcwd.2005=plot_mcwd[rows,drought-2000+1]
	
	study_biomass_s2=crujra.data[99:118,] # 2000-1902+1
	biomass.pre2005=apply(study_biomass_s2[c(1:5,7:10),rows],2,mean)
	biomass.2005=study_biomass_s2[drought-2000+1,rows]
	mcwd.diff=mcwd.pre2005-mcwd.2005
	biomass.diff=biomass.2005-biomass.pre2005
	yyt=cbind(mcwd.diff,biomass.diff)
	return(yyt)
}



