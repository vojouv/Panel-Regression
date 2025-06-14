install.packages("plm")
install.packages("ggplot2")
install.packages("kableExtra")
install.packages("lmtest")
install.packages("dplyr")
install.packages("car")
install.packages("glmnet")
install.packages("MASS")
install.packages("tseries")
library(tseries)
library(glmnet)
library(MASS)
library(plm)
library(lmtest)
library(car)
library(ggplot2)
library(kableExtra) #untuk tampilan tabel
library(dplyr)
library(readr)
library(readxl)
df <- read_excel("C:/Users/Lenovo/OneDrive - UNIVERSITAS INDONESIA/KULIAH/OFFICIAL STAT/PanelNEW.xlsx", col_types = c("text", 
                                                                                                                      "numeric", "numeric", "numeric", "numeric", 
                                                                                                                      "numeric", "numeric"))

head(df, n=10)
str(df)
names(df)

summary(df)

#Replace col names
colnames(df)[colnames(df) == "Tingkat Kemiskinan"] <- "Kemiskinan"
colnames(df)[colnames(df) == "Kabupaten/Kota"] <- "region"

#Model Data
model1 <-lm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD, df%>%filter(Tahun==2018))
model2 <-lm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD , df%>%filter(Tahun==2019))
model3 <-lm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD , df%>%filter(Tahun==2020))
model4 <-lm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD , df%>%filter(Tahun==2021))
model5 <-lm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD , df%>%filter(Tahun==2022))


str(df)
sapply(df[, 3:7], class)

#Uji Multikolinearitas
Multiko <- rbind(as.vector(vif(model1)),as.vector(vif(model2)),as.vector(vif(model3)),as.vector(vif(model4)),as.vector(vif(model5)))

dim(Multiko)
rownames(Multiko) <- paste0("Tahun ", 2018:(2017 + nrow(Multiko)))
rownames(Multiko) <- c("Tahun 2018", "Tahun 2019", "Tahun 2020", "Tahun 2021", "Tahun 2022")
colnames (Multiko) <- c("PDRB" , "IPM" , "Tingkat_Pengangguran" , "PPD")
Multiko

#Scalling data
df[,3:7] <- scale(df[,3:7])
head(df)

## POOLED REGRESSION##

#MODEL CEM
cem <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD , data=df, model = "pooling")
summary(cem)
res.cem <- residuals(cem)
(normal <- jarque.bera.test(res.cem))
(homos <- bptest(cem))
(autokol <- pbgtest(cem))


#MODEL FEM
fem.twoway <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD, data=df, model = "within", 
                  effect= "twoways", index = c("region", "Tahun"))
summary(fem.twoway)

## FEM efek individu 
fem.ind <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD, data=df, model = "within", 
               effect= "individual", index = c("region", "Tahun"))
summary(fem.ind)

res.fem1 <- residuals(fem.ind)
(normal <- jarque.bera.test(res.fem1))
(homos <- bptest(fem.ind))
(autokol <- pbgtest(fem.ind))

##FEM Efek Waktu
fem.time <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran + PPD, data=df, model = "within", 
                effect= "time", index = c("region", "Tahun"))
summary(fem.time)

res.fem2 <- residuals(fem.time)
(normal <- jarque.bera.test(res.fem2))
(homos <- bptest(fem.time))
(autokol <- pbgtest(fem.time))


## UJI CHOW ##
pooltest(cem,fem.twoway)
pooltest(cem,fem.ind)
pooltest(cem,fem.time)


# Tolak H0 artinya FEM lebih baik dibanding CEM


#MODEL REM
rem.twoway <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran, data=df, model = "random", 
                  effect= "twoways", index = c("region", "Tahun"))
summary(rem.twoway)

#rem individu
rem.ind <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran, data=df, model = "random", 
               effect= "individual", index = c("region", "Tahun"))
summary(rem.ind)
res.rem1 <- residuals(rem.ind)
(normal <- jarque.bera.test(res.rem1))
(homos <- bptest(rem.ind))
(autokol <- pbgtest(rem.ind))

#rem efek waktu
rem.time <- plm(Kemiskinan ~ PDRB + IPM + Tingkat_Pengangguran, data=df, model = "random", 
                effect= "time", index = c("region", "Tahun"))
summary(rem.time)
res.rem2 <- residuals(rem.time)
(normal <- jarque.bera.test(res.rem2))
(homos <- bptest(rem.time))
(autokol <- pbgtest(rem.time))


## UJI HAUSMAN ##
phtest(fem.twoway, rem.twoway)
phtest(fem.ind, rem.ind)
phtest(fem.time, rem.time)


#Tolak H0 artinya model FEM lebih baik dibanding REM

#Karena model FEM lebih baik, perlu dilakukan uji asumsi lagi


# FE Model

#Pengecekan pengaruh wilayah dan tahun
plmtest(fem.twoway,type = "bp", effect="twoways" )

#Pengaruh Wilayah
plmtest(fem.twoway,type = "bp", effect="individual" )

#Pengaruh Waktu
plmtest(fem.twoway,type = "bp", effect="time" )

# Berdasarkan hasil di atas model memiliki efek pengaruh waktu dan individu, 
# yang paling tepat digunakan adalah model FEM Two Ways

#Uji Asumsi FEM
res.fem.twoway <- residuals(fem.twoway)

#Uji Normalitas
(normal <- jarque.bera.test(res.fem.twoway))
ggplot(as.data.frame(res.fem.twoway), aes(x = res.fem.twoway)) +
  geom_histogram(aes(y = after_stat(density)), color = "white", fill = "steelblue") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()
## terpenuhi asumsi normalnya

#Uji Durbin-Watson
pdwtest(fem.twoway)
##DW mendekati 2 artinya terpenuhi


#Breusch-Pagan Test
bptest(fem.twoway)
##gak terpenuhi karena hetero (?)



# Penghapusan variabel tidak signifikan
#MODEL FEM
fem.twoway1 <- plm(Kemiskinan ~  IPM + Tingkat_Pengangguran, data=df, model = "within", 
                   effect= "twoways", index = c("region", "Tahun"))
summary(fem.twoway1)

#Uji Asumsi 
res.fem.twoway1 <- residuals(fem.twoway1)
#Uji Normalitas
(normal <- jarque.bera.test(res.fem.twoway1))
ggplot(as.data.frame(res.fem.twoway1), aes(x = res.fem.twoway1)) +
  geom_histogram(aes(y = after_stat(density)), color = "white", fill = "steelblue") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()

#Uji Durbin-Watson
pdwtest(fem.twoway1)

#Breusch-Pagan Test
bptest(fem.twoway1)
## setelah di hapus var x tdk significant , asumsi homo terpenuhi
