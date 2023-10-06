#inputing original dataset
data_panel <- read.csv(file.choose())
data_panel <- data_panel[,c(1:4,6)]

library(Amelia)
library(plm)
library(tseries)

nimp <- 5
data.mi <- amelia(data_panel,idvars=2, ts=3, cs=1, polytime = 1, intercs = F, m=nimp)
midata <- data.mi$imputations;midata

#turning imputations into datasets, which then can be manually added into one full dataset
for (i in 1:nimp) {
  write.csv(midata[[i]], paste("dfamelia",i,".csv",sep=""), row.names = T)
  
}

##inputing full dataset with imputed values
panel_diff <- na.omit(readxl::read_excel(file.choose()))[,-1]
panel_diff


panel_diff12<- pdata.frame(panel_diff[,-2], index = c("Provinsi", "Tahun"))

#testing for stationarity using augmented dickey fuller. (can be looped)
adf.test(panel_diff12$Listrik1, k=2)
adf.test(panel_diff12$Listrik2, k=2)
adf.test(panel_diff12$Listrik3, k=2)
adf.test(panel_diff12$Listrik4, k=2)
adf.test(panel_diff12$Listrik5, k=2)
adf.test(panel_diff12$Rata21,k=2)
adf.test(panel_diff12$Rata22,k=2)
adf.test(panel_diff12$Rata23,k=2)
adf.test(panel_diff12$Rata24,k=2)
adf.test(panel_diff12$Rata25,k=2)

#Durbin wu hausman test for fixed vs random. (can be looped)
fixed1 <- plm(Listrik1 ~ Rata21, index = c("Provinsi", "Tahun"), model = "within", data = panel_diff12)
random1 <- plm(Listrik1 ~ Rata21, index = c("Provinsi", "Tahun"), model = "random", data = panel_diff12)
phtest(fixed1, random1)

fixed2 <- plm(Listrik2 ~ Rata22, index = c("Provinsi", "Tahun"), model = "within", data = panel_diff12)
random2 <- plm(Listrik2 ~ Rata22, index = c("Provinsi", "Tahun"), model = "random", data = panel_diff12)
phtest(fixed2, random2)

fixed3 <- plm(Listrik3 ~ Rata23, index = c("Provinsi", "Tahun"), model = "within", data = panel_diff12)
random3 <- plm(Listrik3 ~ Rata23, index = c("Provinsi", "Tahun"), model = "random", data = panel_diff12)
phtest(fixed3, random3)

fixed4 <- plm(Listrik4 ~ Rata24, index = c("Provinsi", "Tahun"), model = "within", data = panel_diff12)
random4 <- plm(Listrik4 ~ Rata24, index = c("Provinsi", "Tahun"), model = "random", data = panel_diff12)
phtest(fixed4, random4)

fixed5 <- plm(Listrik5 ~ Rata25, index = c("Provinsi", "Tahun"), model = "within", data = panel_diff12)
random5 <- plm(Listrik5 ~ Rata25, index = c("Provinsi", "Tahun"), model = "random", data = panel_diff12)
phtest(fixed5, random5)

#Lagrange multiplier Breusch Pagan for common vs random effect model (can be looped)
pool1 <- plm(Listrik1 ~ Rata21, index = c("Provinsi", "Tahun"), model = "pooling", data = panel_diff12)
plmtest(pool1, type = "bp")

pool2 <- plm(Listrik2 ~ Rata22, index = c("Provinsi", "Tahun"), model = "pooling", data = panel_diff12)
plmtest(pool2, type = "bp")

pool3 <- plm(Listrik3 ~ Rata23, index = c("Provinsi", "Tahun"), model = "pooling", data = panel_diff12)
plmtest(pool3, type = "bp")

pool4 <- plm(Listrik4 ~ Rata24, index = c("Provinsi", "Tahun"), model = "pooling", data = panel_diff12)
plmtest(pool4, type = "bp")

pool5 <- plm(Listrik5 ~ Rata25, index = c("Provinsi", "Tahun"), model = "pooling", data = panel_diff12)
plmtest(pool5, type = "bp")

#final test for heteroskedasticity using breusch pagan
library(lmtest)
bptest(Listrik1 ~ Rata21 + factor(Provinsi), data = panel_diff, studentize = F)
bptest(Listrik2 ~ Rata22 + factor(Provinsi), data = panel_diff, studentize = F)
bptest(Listrik3 ~ Rata23 + factor(Provinsi), data = panel_diff, studentize = F)
bptest(Listrik4 ~ Rata24 + factor(Provinsi), data = panel_diff, studentize = F)
bptest(Listrik5 ~ Rata25 + factor(Provinsi), data = panel_diff, studentize = F)

#if heteroskedasticity assumption isn't fulfilled, use robust covariance matrix
#type depends on models needed

library(sandwich)
library(lmtest)
coeftest(random1, vcovHC(random1, type = "HC0"))
coeftest(random1, vcovHC(random1, type = "HC1")) 
coeftest(random1, vcovHC(random1, type = "HC2")) 
coeftest(random1, vcovHC(random1, type = "HC3")) 
coeftest(random1, vcovHC(random1, type = "HC4"))
coeftest(random1, vcovHC(random1, method = "white2")) 

coeftest(random2, vcovHC(random2, type = "HC0"))
coeftest(random2, vcovHC(random2, type = "HC1")) 
coeftest(random2, vcovHC(random2, type = "HC2")) 
coeftest(random2, vcovHC(random2, type = "HC3")) 
coeftest(random2, vcovHC(random2, type = "HC4"))
coeftest(random2, vcovHC(random2, method = "white2")) 


coeftest(random3, vcovHC(random3, type = "HC0"))
coeftest(random3, vcovHC(random3, type = "HC1")) 
coeftest(random3, vcovHC(random3, type = "HC2")) 
coeftest(random3, vcovHC(random3, type = "HC3")) 
coeftest(random3, vcovHC(random3, type = "HC4")) 
coeftest(random3, vcovHC(random3, method = "white2")) 


coeftest(random4, vcovHC(random4, type = "HC0"))
coeftest(random4, vcovHC(random4, type = "HC1")) 
coeftest(random4, vcovHC(random4, type = "HC2")) 
coeftest(random4, vcovHC(random4, type = "HC3")) 
coeftest(random4, vcovHC(random4, type = "HC4")) 
coeftest(random4, vcovHC(random4, method = "white2")) 


pool5alt <- lm(formula = Listrik5 ~ Rata25, data = panel_diff12)
coeftest(pool5alt,vcovHC(pool5alt, type = "const"))
