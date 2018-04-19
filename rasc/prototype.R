rm(list=ls(all=TRUE))
setwd('U:\\natercia\\student_perf_tool')
dat=read.csv('performance data.csv',as.is=T)

#set up colors
cores=hsv(h=c(0,0.175,0.35),1,1,alpha=0.5)
nweek=13
nomes=paste('week',1:nweek,sep='')
dat1=t(apply(dat[,nomes],1,cumsum))

#choose a specific student
cond=!is.na(dat$SIS.User.ID) & dat$SIS.User.ID==17831385 & 
     !is.na(dat$SIS.Login.ID) & dat$SIS.Login.ID=='nandreacchio@ufl.edu'
ind=which(cond)

#------------------------------------------------------------------
#show trend through time
quant=rep(NA,nweek)
for (i in 1:nweek){
  quant[i]=mean(dat[ind,nomes[i]] >= dat[,nomes[i]],na.rm=T)
}
plot(NA,NA,xlim=c(1,nweek),ylim=c(0,1),main='Weekly performance',
     ylab='Proportion that scored the same or lower',
     xlab='Week')
polygon(x=c(-10,nweek,nweek,-10),y=c(-10,-10,1/3,1/3),col=cores[1])
polygon(x=c(-10,nweek,nweek,-10),y=c(1/3,1/3,2/3,2/3),col=cores[2])
polygon(x=c(-10,nweek,nweek,-10),y=c(2/3,2/3,10,10),col=cores[3])

#show trend through time
lines(1:nweek,quant)
points(1:nweek,quant,pch=19)

#------------------------------------------------------------------
#show cumulative trend through time
quant=rep(NA,nweek)
for (i in 1:nweek){
  quant[i]=mean(dat1[ind,nomes[i]] >= dat1[,nomes[i]],na.rm=T)
}

plot(NA,NA,xlim=c(1,nweek),ylim=c(0,1),main='Cumulative performance',
     ylab='Proportion that scored the same or lower',
     xlab='Week')
polygon(x=c(-10,nweek,nweek,-10),y=c(-10,-10,1/3,1/3),col=cores[1])
polygon(x=c(-10,nweek,nweek,-10),y=c(1/3,1/3,2/3,2/3),col=cores[2])
polygon(x=c(-10,nweek,nweek,-10),y=c(2/3,2/3,10,10),col=cores[3])
lines(1:nweek,quant)
points(1:nweek,quant,pch=19)
