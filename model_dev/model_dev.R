library(dplyr)
library(pool)
library(lubridate)
library(purrr)
library(tidyr)
# library(geoR)
library(mgcv)
library(caret)
source("Auth.R")

# tbl(connection,"weather")%>%
#   mutate(sunDuration=sunsetTime-sunriseTime,
#          tempMax=temperatureMax)%>%
#   select(date,sunDuration,tempMax,cloudCover,pressure,dewPoint)%>%
#   group_by(date)%>%
#   summarize_if(is.numeric,funs(sd(.)/mean(.)))%>%
#   ungroup%>%
#   summarize_if(is.numeric,mean)
# 
# cc=tbl(connection,"weather")%>%
#   select(date,postcode,cloudCover)%>%
#   filter(day(date)==1&!is.na(cloudCover))%>%
#   collect
# 
# coords=readRDS("data/coords.rds")
# 
# vg=cc%>%
#   inner_join(coords)%>%
#   nest(-date)%>%
#   mutate(vg=map(data,function(x)x%>%as.geodata(coords.col=3:4,data.col=2)%>%variog(max.dist=1)))
# 
# par(mfrow=c(4,4))
# for(i in 1:nrow(vg)){
#   plot(vg$vg[[i]],xlim=c(0,1))
# }
# par(mfrow=c(1,1))


X=weather%>%
  mutate(day=lubridate::yday(date),
         sunDuration=as.numeric(as.POSIXct(sunsetTime)-as.POSIXct(sunriseTime)),
         pressure=as.numeric(pressure),
         cloudCover=as.numeric(cloudCover))%>%
  select(postcode,date,day,sunDuration,tempMax=temperatureMax,cloudCover,pressure,dewPoint)%>%
  inner_join(output%>%
  mutate(eff=output/size)%>%
  group_by(postcode,date)%>%
  summarize(eff=mean(eff))%>%
  ungroup)%>%
  inner_join(postcode)



ind.train=with(X,which((day+1)%%5!=0&day%%5!=0))  #1-3
ind.valid=with(X,which((day+1)%%5==0))  #4
ind.test=with(X,which(day%%5==0))  #5
## train
bam=bam(eff~s(day,bs="cc"),data=X[ind.train,],method="fREML")
sqrt(mean((predict(bam,newdata=X[ind.valid,])-X[ind.valid,"eff"])^2))
plot(bam)

bam2=bam(eff~s(day,bs="cc")+s(sunDuration,bs="cr")+s(tempMax,bs="cr")+s(cloudCover,bs="cr")+s(pressure,bs="cr")+s(dewPoint,bs="cr"),data=X[ind.train,],method="fREML")
sqrt(mean((predict(bam2,newdata=X[ind.valid,])-X[ind.valid,"eff"])^2,na.rm=T))
plot(bam2)


bam3=bam(eff~s(day,bs="cc")+s(tempMax,bs="cr")+s(cloudCover,bs="cr")+s(pressure,bs="cr")+s(dewPoint,bs="cr"),data=X[ind.train,],method="fREML")
sqrt(mean((predict(bam3,newdata=X[ind.valid,])-X[ind.valid,"eff"])^2,na.rm=T))
plot(bam3)

bam4=bam(eff~te(lat,lon,bs="cr")+s(day,bs="cc")+s(tempMax,bs="cr")+s(cloudCover,bs="cr")+s(pressure,bs="cr")+s(dewPoint,bs="cr"),data=X[ind.train,],method="fREML")
sqrt(mean((predict(bam4,newdata=X[ind.valid,])-X[ind.valid,"eff"])^2,na.rm=T))
plot(bam4)


trim=function(m){
  print(object.size(m),unit="auto")
  m$prior.weights=NULL
  m$offset=NULL
  m$weights=NULL
  m$fitted.values=NULL
  m$linear.predictors=NULL
  m$y=NULL
  m$residuals=NULL
  m$G=NULL
  m$na.action=NULL
  m$model=NULL
  m$qrx=NULL
  m$family=NULL
  print(object.size(m),unit="auto")
  m
}

bam5=trim(bam4)
  
all.equal(predict(bam5,newdata=X[1:2,],se.fit=T),predict(bam4,newdata=X[1:2,],se.fit=T))

saveRDS(bam5,file="bam.rds")
