library(plm)
library(readxl)
library(spdep)
library(lmtest)
library(car)
library(spgwr)
library(GWmodel)

datagwpr<-read.csv('E:/Semester 6/Analisis Data Spasial/Data Tugas Kelompok.csv', header=T, sep=',')
datagwpr

#Koefisisen korelasi antar variabel
cor.test(datagwpr$Y,datagwpr$X1)
cor.test(datagwpr$Y,datagwpr$X2)
cor.test(datagwpr$Y,datagwpr$X3)
cor.test(datagwpr$X1,datagwpr$X2)
cor.test(datagwpr$X1,datagwpr$X3)
cor.test(datagwpr$X2,datagwpr$X3)

#Uji Multikolinieritas
multikol=lm(Y~X1+X2+X3,data=datagwpr)
car::vif(multikol)


#Model Awal Data Panel
#common effect model
ce<-plm(Y~X1+X2+X3,data=datagwpr,model="pooling",index=c("kabupaten.kota","Tahun"))
summary(ce)

#fixed effect model
fe<-plm(Y~X1+X2+X3,data=datagwpr,model="within",index=c("kabupaten.kota","Tahun"))
summary(fe)

#random effect model
re<-plm(Y~X1+X2+X3,data=datagwpr,model="random",index=c("kabupaten.kota","Tahun"))
summary(re)

#Pemilihan model regresi data panel
#Uji Chow
pFtest(fe,ce)

#Uji Hausman
phtest(fe,re)

#pendugaan parameter
model1<-plm(Y~X1+X2+X3,data=datagwpr,model="within",index=c("kabupaten.kota","Tahun"))
summary(model1)

#Uji Asumsi Kenormalan
library(stats)
error=resid(model1)
shapiro.test(error)

#transformasi variabel respon
powerTransform(datagwpr$Y)
p<-powerTransform(datagwpr$Y)
y<-bcPower(datagwpr$Y, p$lambda)
y
qqnorm(y)
qqline(y, col="red")

#penduga parameter setelah transformasi
datagwpr.trans<-read.csv('E:/Semester 6/Analisis Data Spasial/Data Tugas Kelompok Trans.csv', header=T, sep=',')
datagwpr.trans
model2<-plm(Y~X1+X2+X3,data=datagwpr.trans,model="within",index=c("kabupaten.kota","Tahun"))
summary(model2)

library(stats)
error=resid(model2)
shapiro.test(error)

#Uji Keragaman Spasial
library(lmtest)
bptest(model2)

#Penentuan bandwith optimum
data.sp.GWPR=datagwpr.trans
coordinates(data.sp.GWPR)=4:5
class(data.sp.GWPR)

bwd.GWPR=bw.gwr(Y~X1+X2+X3, data=data.sp.GWPR,
                approach="CV",kernel="bisquare",adaptive=F)
hasil.GWPR=gwr.basic(Y~X1+X2+X3,
                     data=data.sp.GWPR,                  
                     bw=bwd.GWPR,kernel="bisquare",adaptive=F)

hasil.GWPR
#Menampilkan bandwidth tiap lokasi
Longlat=cbind(data.sp.GWPR$Long[1:23],data.sp.GWPR$Lat[1:23])
bwd.lokasi=gw.adapt(dp=Longlat,fp=Longlat,
                    quant=hasil.GWPR$GW.arguments$bw/23)
bwd.lokasi=as.data.frame(bwd.lokasi)


#Uji asumsi GWPR Model
#Uji asumsi normalitas
library(stats)
error=resid(model2)
shapiro.test(error)
#Uji Homokedastisitas
library(lmtest)
bptest(model2)
#Uji Autokorelasi
dwtest(datagwpr.trans$Y~datagwpr.trans$X1+datagwpr.trans$X2+datagwpr.trans$X3)

#Estimasi parameter
parameter.GWPR=as.data.frame(hasil.GWPR$SDF[,2:5])[,-6]
View(parameter.GWPR)

#P-value
p.value.GWPR=as.data.frame(gwr.t.adjust(hasil.GWPR)$result$p)
View(p.value.GWPR)
