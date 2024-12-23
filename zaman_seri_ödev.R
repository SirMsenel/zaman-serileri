rm(list = ls())

install.packages("fpp")
install.packages("forecast")

library(fpp)
library(forecast)
library(lmtest)

library(readxl)
zaman_serisi_odevi <- read_excel("C:/Users/pc/Desktop/zaman_serisi_odevi/zaman_serisi_odevi.xlsx")
View(zaman_serisi_odevi)

# Tarih sütununu Date formatýna dönüþtürün
zaman_serisi_odevi$observation_date <- as.Date(zaman_serisi_odevi$observation_date, format="%Y-%m-%d")


# Zaman serisini oluþturun (yýllýk frekans, 12 ayda bir döngü)
zaman_serisi_odevi_ts <- ts(zaman_serisi_odevi$MRTSSM4481USN, start=c(1992, 01), frequency=12)
zaman_serisi_odevi_ts

#zaman serisi grafigi cizelim.
ts.plot(zaman_serisi_odevi_ts,gpars=list(xlab="Zaman", ylab="zaman_serisi_odevi"))


#ACF ve PACF grafiklerini cizdirelim:

library(forecast)

# ACF grafiði ile baþlýk ekleme
acf(zaman_serisi_odevi_ts, lag.max = 42, ylim = c(-1, 1), lwd = 3, main = "Otomatik Korelasyon Fonksiyonu (ACF)")

# PACF grafiði ile baþlýk ekleme
pacf(zaman_serisi_odevi_ts, lag.max = 42, ylim = c(-1, 1), lwd = 3, main = "Kýsmi Otomatik Korelasyon Fonksiyonu (PACF)")


#trent mevcut. Tuketim serisinin trent bileseni regresyon analizi yardimi ile olusturulur.
zaman_serisi_odevi_trent<-tslm(zaman_serisi_odevi_ts~trend)
zaman_serisi_odevi_trent

#fitted values=orijinal serinin trent bileseni
periyot<- zaman_serisi_odevi_ts-zaman_serisi_odevi_trent[["fitted.values"]]  #serinin periyoduna sahip mevsimsel bilesen serisi
periyot

#veya
#periyot<- Tuketim_ts-Tuketim_trent$fitted.values

#serinin periyoduna sahip mevsimsel bilesen serisinin ACF grafiginden periyodu bulalim:
Acf(periyot,lag.max = 42,  ylim=c(-1,1), lwd=3)


#Merkezsel Hareketli Ortalama hesabi
zaman_serisi_odevi_1_ts<- ma(zaman_serisi_odevi_ts, order = 12, centre = TRUE)  #germe sayisi=12
zaman_serisi_odevi_1_ts

#Mevsimsel bilesenin bulunusu (hata terimi de mevcut)
Mevsim<- zaman_serisi_odevi_ts-zaman_serisi_odevi_1_ts
Mevsim


#Mevsim serisinin ortalamalari
donemort<-t(matrix(data=Mevsim, nrow = 12, ncol=32))

colMeans(donemort, na.rm = T)
sum(colMeans(donemort, na.rm = T))
mean(colMeans(donemort, na.rm = T))



#mevsimsel endeks degerlerinin bulunusu
endeks<- colMeans(donemort, na.rm = T)-mean(colMeans(donemort, na.rm = T))
endeks

#endeks degerlerini seri boyunca yazdirma islemi
indeks<-  matrix(data = endeks, nrow = 384)


#extra olarak indeks degerlerini seri boyunca yazdirmak istersek,
#indeks_alternatif<- decompose(Tuketim_ts, "additive") kodu kullanilir.


#trent bileseni bulalÄ±m (hata terimi de mevcut)
trenthata <- zaman_serisi_odevi_ts - indeks
trenthata


# Zaman serisinin trend deðerlerini oluþturun
trend <- time(zaman_serisi_odevi_ts)


# Trend'i zaman serisi formatýna dönüþtürme
trend_ts <- ts(trend, start = start(trenthata), frequency = frequency(trenthata))
trend_ts


# Doðrusal regresyon modelini kurma (tslm yerine lm kullanarak)
trent <- lm(trenthata ~ trend_ts)

#tahmin serisini bulalim: (mevsimsel endeks+saf trent serisi)
tahmin<- indeks+trent[["fitted.values"]]
tahmin

#hata serisini bulalim:
hata<- zaman_serisi_odevi_ts-indeks-trent[["fitted.values"]]
hata


######Modelin Guvenilirligi######


#Toplamsal modelin ele alinan seri uzerinde gecerli bir model olup olmadigini kontrol edelim
#(yani tahminleri guvenilir mi???)


# Zaman serisi matrisini vektöre dönüþtür
zaman_serisi_v <- as.vector(t(zaman_serisi_odevi_ts))  # Sütunlarý birleþtirerek vektör haline getir

# Baþlangýç ve frekans bilgisiyle zaman serisine dönüþtür
start_year <- as.numeric(colnames(zaman_serisi_odevi_ts)[1])  # Ýlk sütundaki yýl
zaman_serisi_ts <- ts(zaman_serisi_v, start=c(start_year, 1), frequency=12)  # Aylýk frekans (12)

# Tahmin serisini zaman serisi haline getir
tahmin_ts <- ts(tahmin, start=start(zaman_serisi_ts), frequency=frequency(zaman_serisi_ts))
tahmin_ts

# Orijinal zaman serisi ve tahmini çiz
plot(zaman_serisi_ts, xlab="Zaman", ylab="", lty=1, col=4, lwd=2,
     ylim=range(c(zaman_serisi_ts, tahmin_ts), na.rm=TRUE))  # Y eksenini iki seriye göre ayarla
lines(tahmin_ts, lty=3, col=2, lwd=3)  # Tahmin serisini ekle

# Legend ekle
legend("topleft", c("Zaman Serisi Ödevi", "Tahmin"), lwd=c(2, 2), lty=c(1, 3), cex=0.8, col=c(4, 2))




# ACF ve PACF Grafikleri
par(mfrow=c(2,1))  # Grafik düzeni: 2 satýr, 1 sütun

# ACF Grafiði
acf_plot <- Acf(hata, main="ACF: Hata", lag.max=42, ylim=c(-1,1), lwd=3)
abline(h=c(-1.96/sqrt(length(hata)), 1.96/sqrt(length(hata))), col="red", lty=2)  # Güven sýnýrlarý

# PACF Grafiði
pacf_plot <- Pacf(hata, main="PACF: Hata", lag.max=42, ylim=c(-1,1), lwd=3)
abline(h=c(-1.96/sqrt(length(hata)), 1.96/sqrt(length(hata))), col="red", lty=2)  # Güven sýnýrlarý





#####Carpimsal Ayristirma Yontemi

#mevsimsel bileseni bulunmasi (Zt/MHO) (hata terimi de mevcut)
Mevsim1 <- zaman_serisi_odevi_ts/zaman_serisi_odevi_1_ts
Mevsim1


#her bir periyot icin ortalama degerlerinin hesabi
donemort1<-t(matrix(data=Mevsim1, nrow = 12, ncol=32))
donemort1


colMeans(donemort1, na.rm = T)

#toplam
sum(colMeans(donemort1, na.rm = T))

#ortalamalarin ortalamasi
mean(colMeans(donemort1, na.rm = T))


#mevsimsel endeks degerlerinin bulunusu
endeks1<- colMeans(donemort1, na.rm = T)/mean(colMeans(donemort1, na.rm = T))
#mean(endeks1)=1?
endeks1


#endeks degerlerini seri boyunca yazdirma islemi
indeks1<-  matrix(data = endeks1, nrow = 384)
indeks1


#trent serisi (hata da mevcut) (orijinal seri/mevsimsel endeks serisi)
trenthata1<- zaman_serisi_odevi_ts/indeks1
trenthata1

#hatadan arindirma islemi
trent1<- lm(trenthata1 ~ trend_ts)

tahmin1<- indeks1*trent1[["fitted.values"]] #tahmin=endeks*orijinal serinin trent bileseni
tahmin1

# Tahmin serisini zaman serisi haline getir
tahmin1_ts <- ts(tahmin1, start=start(zaman_serisi_ts), frequency=frequency(zaman_serisi_ts))

# Orijinal seri ile tahmin serisinin uyumunu çiziyoruz
plot(window(zaman_serisi_ts), 
     xlab="Zaman", ylab="", lty=1, col=4, lwd=2, 
     ylim=range(c(window(zaman_serisi_ts), window(tahmin1_ts)), na.rm=TRUE))
lines(window(tahmin1_ts), lty=3, col=2, lwd=3)
legend("topleft", c(expression(paste(zaman_serisi_odevi_ts)), 
                    expression(paste(Tahmin))),
       lwd=c(2, 2), lty=c(1, 3), cex=0.8, col=c(4, 2))




#hata serisi
hata1<- zaman_serisi_odevi_ts-tahmin1
hata1


# HATALAR AK GÜRÜLTÜ MÜ ?

# ACF ve PACF Grafikleri
par(mfrow=c(2,1))  # Grafik düzeni: 2 satýr, 1 sütun

# ACF Grafiði
acf_plot <- Acf(hata1, main="ACF: Hata", lag.max=42, ylim=c(-1,1), lwd=3)
abline(h=c(-1.96/sqrt(length(hata1)), 1.96/sqrt(length(hata1))), col="red", lty=2)  # Güven sýnýrlarý

# PACF Grafiði
pacf_plot <- Pacf(hata1, main="PACF: Hata", lag.max=42, ylim=c(-1,1), lwd=3)
abline(h=c(-1.96/sqrt(length(hata1)), 1.96/sqrt(length(hata1))), col="red", lty=2)  # Güven sýnýrlarý





#uygulama 5 
####################################
###Toplamsal model



t<-1: 1: 384   #t terimini olusturalim

sin1<-sin(2*3.1416*t/12)  
cos1<-cos(2*3.1416*t/12)

veriseti<-as.data.frame(cbind(zaman_serisi_odevi$MRTSSM4481USN, t, sin1, cos1))

names(veriseti)<- c("y", "t", "sin1", "cos1")
attach(veriseti)

regresyon.model1<-lm(y~t+sin1+cos1)
summary(regresyon.model1)



########â™¥

sin2<-t*sin(2*3.1416*2*t/12)
cos2<-t*cos(2*3.1416*2*t/12)


veriseti2<-as.data.frame(cbind(zaman_serisi_odevi$MRTSSM4481USN, t, sin1, cos1, sin2, cos2))

names(veriseti2)<- c("y", "t", "s1", "c1", "s2", "c2")
attach(veriseti2)

regresyon.model2<-lm(y~t+sin1+cos1+sin2+cos2)
summary(regresyon.model2)

#sin2 ve cos2 anlamsÄ±z, modelde sadece t, cos1, sin1 yer alir. Tekrar 1.modele donuyoruz.

##durbin-watson testi
dwtest(y~t+sin1+cos1)

#1. model icin tahmin serisi, hata serisi ve tahminin alt ve Ã¼st sÄ±nÄ±rlarÄ±na ait seriler
tahmin1<-predict(regresyon.model1)
sinir1<-predict(regresyon.model1, interval = 'confidence' ,level = .95)
tahmin1
sinir1
hata1<-resid(regresyon.model1)
hata1


plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir1[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir1[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(zaman_serisi_odevi)),
                   expression(paste(Altsinir)),
                   expression(paste(Ustsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata1,lag.max = 42,  ylim=c(-1,1), lwd=3)

#Box-Ljung
Box.test(hata1, lag = 24, type = "Ljung")


############################################
###Ã‡arpÄ±msal model

s1<-t*sin(2*3.1416*t/12)
c1<-t*cos(2*3.1416*t/12)


veriseti3<-as.data.frame(cbind(zaman_serisi_odevi$MRTSSM4481USN, t, s1, c1))

names(veriseti3)<- c("y", "t", "s1", "c1")
attach(veriseti3)

regresyon.model3<-lm(y~t+s1+c1)
summary(regresyon.model3)

########â™¥

s2<-t*sin(2*3.1416*2*t/12)
c2<-t*cos(2*3.1416*2*t/12)


veriseti4<-as.data.frame(cbind(zaman_serisi_odevi$MRTSSM4481USN, t, s1, c1, s2, c2))

names(veriseti4)<- c("y", "t", "s1", "c1", "s2", "c2")
attach(veriseti4)

regresyon.model4<-lm(y~t+s1+c1+s2+c2)
summary(regresyon.model4)

#s2 ve c2 anlamsÄ±z, modelde sadece t, c1, s1 yer alir. Tekrar 3.modele donuyoruz.

##durbin-watson testi
dwtest(y~t+s1+c1+s2+c2)

#3.model icin tahmin serisi, hata serisi ve tahminin alt ve Ã¼st sÄ±nÄ±rlarÄ±na ait seriler
tahmin3<-predict(regresyon.model4)
sinir3<-predict(regresyon.model4, interval = 'confidence' ,level = .95)
hata3<-resid(regresyon.model4)


plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir3[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir3[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(zaman_serisi_odevi)),
                   expression(paste(Altsinir)),
                   expression(paste(Ustsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata3,lag.max = 42,  ylim=c(-1,1), lwd=3)

#Box-Ljung
Box.test(hata3, lag = 24, type = "Ljung")










#uygulama 6




ts.plot(zaman_serisi_odevi_ts,gpars=list(xlab="Zaman", ylab="Seri1"), lwd=2)

Acf(zaman_serisi_odevi_ts,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(zaman_serisi_odevi_ts,lag.max = 42, ylim=c(-1,1), lwd=3)

Acf(diff(zaman_serisi_odevi_ts),lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(diff(zaman_serisi_odevi_ts),lag.max = 42, ylim=c(-1,1), lwd=3)

zaman_serisi_odevi_ts2_ts <- ts(zaman_serisi_odevi, frequency = 12)


Acf(diff(diff(zaman_serisi_odevi_ts),12),lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(diff(diff(zaman_serisi_odevi_ts),12),lag.max = 42, ylim=c(-1,1), lwd=3)


###############################
## Toplamsal Winters Yontemi ##
###############################

Winters1<- ets(zaman_serisi_odevi_ts, model = "AAA")

summary(Winters1)

#l=ortalama duzeyin baslangic degeri= l = 7334.4038
#b=egimin bas.degeri  b = 0.4059
#mevsimsel terimin baslangÄ±c degerleri s

##############################
## Carpimsal Winters Yontemi ##
##############################

Winters2<- ets(abs(zaman_serisi_odevi_ts), model = "MAM")

summary(Winters2)


###################################

tahmin1<- Winters1[["fitted"]]
tahmin1

plot( window(zaman_serisi_odevi_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Orjinalseri)),
                   expression(paste(Winters1Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))


hata123<- Winters1[["residuals"]]

Box.test (hata1, lag = 42, type = "Ljung")

Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

checkresiduals(Winters1, lag = 42)

ongoru <- forecast(Winters1,h=5)

ongoru[["mean"]]





zaman_serisi_odevi_2_ts <- ts(zaman_serisi_odevi[,2], start = 1992,  frequency =12)
zaman_serisi_odevi_2_ts


ts.plot(zaman_serisi_odevi_2_ts,main="Satis", xlab="Zaman", ylab="", lwd=2)

Acf(zaman_serisi_odevi_2_ts,lag.max = 100,  ylim=c(-1,1), lwd=3)
Pacf(zaman_serisi_odevi_2_ts,lag.max = 100,  ylim=c(-1,1), lwd=3)


Acf(diff(zaman_serisi_odevi_2_ts),lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(diff(zaman_serisi_odevi_2_ts),lag.max = 42, ylim=c(-1,1), lwd=3)

Acf(diff(diff(zaman_serisi_odevi_2_ts,12)),lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(diff(diff(zaman_serisi_odevi_2_ts,12)),lag.max = 42, ylim=c(-1,1), lwd=3)


zaman_serisi_odevi_2_arima1 <- Arima(zaman_serisi_odevi_2_ts,order = c(1,1,2), seasonal= c(1,1,1), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima1)
coeftest(zaman_serisi_odevi_2_arima1)


zaman_serisi_odevi_2_arima2 <- Arima(zaman_serisi_odevi_2_ts,order = c(1,1,2), seasonal= c(0,1,1), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima2)
coeftest(zaman_serisi_odevi_2_arima2)

zaman_serisi_odevi_2_arima3 <- Arima(zaman_serisi_odevi_2_ts,order = c(2,1,3), seasonal= c(1,1,2), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima3)
coeftest(zaman_serisi_odevi_2_arima3)


zaman_serisi_odevi_2_arima4 <- Arima(zaman_serisi_odevi_2_ts,order = c(2,1,3), seasonal= c(0,1,0), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima4)
coeftest(zaman_serisi_odevi_2_arima4)

zaman_serisi_odevi_2_arima5 <- Arima(zaman_serisi_odevi_2_ts,order = c(2,1,3), seasonal= c(1,1,1), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima5)
coeftest(zaman_serisi_odevi_2_arima5)

zaman_serisi_odevi_2_arima6 <- Arima(zaman_serisi_odevi_2_ts,order = c(2,1,3), seasonal= c(0,1,1), include.constant=TRUE)
summary(zaman_serisi_odevi_2_arima6)
coeftest(zaman_serisi_odevi_2_arima6)




dnm<- auto.arima(zaman_serisi_odevi_2_ts, trace=TRUE)
summary(dnm)
coeftest(dnm)


tahmin_b<- zaman_serisi_odevi_2_arima6[["fitted"]]
tahmin_b
hata_b<- zaman_serisi_odevi_2_arima6[["residuals"]]
hata_b

plot( window(zaman_serisi_odevi_2_ts), 
      xlab="Zaman (Y?l)", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_a) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(zaman_serisi_odevi)),
                   expression(paste(Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))




ongoru<- forecast(auto.arima(zaman_serisi_odevi_2_ts) , h=7)
ongoru

Box.test (hata_b, lag = 42, type = "Ljung")

checkresiduals(zaman_serisi_odevi_2_arima6)

Acf(hata_b,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata_b,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

LjungBoxTest (hata_b,k=2,StartLag=1, lag=42 )











