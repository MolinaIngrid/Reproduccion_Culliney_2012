# primera parte culliney alala carry behavior
Alala.carry <- as.data.frame(read.delim(file="Data.Alala.carry.txt", header=TRUE, sep="\t"))
names(Alala.carry)
unique(Alala.carry$Plant)
CHETR <- subset(Alala.carry, Plant== "CHETR")

#Regresión; 
CHETR.glm <- glm(CHETR$carries~ CHETR$Sex + CHETR$Age,weights=CHETR$given, na.action = na.fail)
#Resumen de estadísticas
summary(CHETR.glm)

CHETR.AIC <- dredge(CHETR.glm)
#Valores del AIC
CHETR.AIC

#estimados promediados del modelo 
average.CHETR <- model.avg(get.models(CHETR.AIC, seq(nrow(CHETR.AIC))))
CHETR.Age <- CHETR$Age
CHETR.Sex <- CHETR$Sex

# modelos promediadios con los errores estandar
newdata.CHETR <- as.data.frame(cbind(CHETR.Age,CHETR.Sex))
pred.avg.CHETR <- predict(average.CHETR, newdata.CHETR,se.fit=TRUE, type="response")
results.CHETR <- unique(cbind(newdata.CHETR,pred.avg.CHETR$fit,pred.avg.CHETR$se.fit))

#archivo de texto 
write.table(results.CHETR,"CARRY.results.CHETR.txt",sep="S", col.names=TRUE)

#OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU

#subsetting for olapa; create a new file to work with
CLEHA <- subset(Alala.carry, Plant== "CLEHA")

#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
CLEHA.glm <- glm(CLEHA$carries~ CLEHA$Sex + CLEHA$Age,weights=CLEHA$given,na.action = na.fail)
#this gives you your typical stats
summary(CLEHA.glm)
#getting AIC tables and coefficients
CLEHA.AIC <- dredge(CLEHA.glm)
#please show me those AIC values
CLEHA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(CLEHA.AIC,"CARRY.CLEHA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.CLEHA <- model.avg(get.models(CLEHA.AIC, seq(nrow(CLEHA.AIC))))
CLEHA.Age <- CLEHA$Age
CLEHA.Sex <- CLEHA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.CLEHA <- as.data.frame(cbind(CLEHA.Age,CLEHA.Sex))
pred.avg.CLEHA <- predict(average.CLEHA, newdata.CLEHA,se.fit=TRUE, type="response")
results.CLEHA <- unique(cbind(newdata.CLEHA,pred.avg.CLEHA$fit,pred.avg.CLEHA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.CLEHA,"CARRY.results.CLEHA.txt",sep="S", col.names=TRUE)

#PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO

#subsetting for just pilo; create a new file to work with
COPRH <- subset(Alala.carry, Plant== "COPRH")

summary(COPRH$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
COPRH.glm <- glm(COPRH$carries~ COPRH$Sex + COPRH$Age,weights=COPRH$given, na.action = na.fail)

#this gives you your typical stats
summary(COPRH.glm)

#calculates AIC tables and coefficients
COPRH.AIC <- dredge(COPRH.glm)
#please show me those AIC values
COPRH.AIC
#writes a text file from the AIC table which can be read into excel
write.table(COPRH.AIC,"CARRY.COPRH.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.COPRH <- model.avg(get.models(COPRH.AIC, seq(nrow(COPRH.AIC))))
COPRH.Age <- COPRH$Age
COPRH.Sex <- COPRH$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.COPRH <- as.data.frame(cbind(COPRH.Age,COPRH.Sex))
pred.avg.COPRH <- predict(average.COPRH, newdata.COPRH,se.fit=TRUE, type="response")
results.COPRH <- unique(cbind(newdata.COPRH,pred.avg.COPRH$fit,pred.avg.COPRH$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.COPRH,"CARRY.results.COPRH.txt",sep="S", col.names=TRUE)

#KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU KAWAU

#subsetting for just kawau; create a new file to work with
ILEAN <- subset(Alala.carry, Plant== "ILEAN")

summary(ILEAN$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
ILEAN.glm <- glm(ILEAN$carries~ ILEAN$Sex + ILEAN$Age,weights=ILEAN$given, na.action = na.fail)

#this gives you your typical stats
summary(ILEAN.glm)

#getting AIC tables and coefficients
ILEAN.AIC <- dredge(ILEAN.glm)
#please show me those AIC values
ILEAN.AIC
#writes a text file from the AIC table which can be read into excel
write.table(ILEAN.AIC,"CARRY.ILEAN.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.ILEAN <- model.avg(get.models(ILEAN.AIC, seq(nrow(ILEAN.AIC))))
ILEAN.Age <- ILEAN$Age
ILEAN.Sex <- ILEAN$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.ILEAN <- as.data.frame(cbind(ILEAN.Age,ILEAN.Sex))
pred.avg.ILEAN <- predict(average.ILEAN, newdata.ILEAN,se.fit=TRUE, type="response")
results.ILEAN <- unique(cbind(newdata.ILEAN,pred.avg.ILEAN$fit,pred.avg.ILEAN$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.ILEAN,"CARRY.results.ILEAN.txt",sep="S", col.names=TRUE)

#NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO NAIO

#subsetting for just naio; create a new file to work with
MYOSA <- subset(Alala.carry, Plant== "MYOSA")

summary(MYOSA$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
MYOSA.glm <- glm(MYOSA$carries~ MYOSA$Sex + MYOSA$Age,weights=MYOSA$given, na.action = na.fail)

#this gives you your typical stats
summary(MYOSA.glm)

#getting AIC tables and coefficients
MYOSA.AIC <- dredge(MYOSA.glm)
#please show me those AIC values
MYOSA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(MYOSA.AIC,"CARRY.MYOSA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.MYOSA <- model.avg(get.models(MYOSA.AIC, seq(nrow(MYOSA.AIC))))
MYOSA.Age <- MYOSA$Age
MYOSA.Sex <- MYOSA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.MYOSA <- as.data.frame(cbind(MYOSA.Age,MYOSA.Sex))
pred.avg.MYOSA <- predict(average.MYOSA, newdata.MYOSA,se.fit=TRUE, type="response")
results.MYOSA <- unique(cbind(newdata.MYOSA,pred.avg.MYOSA$fit,pred.avg.MYOSA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.MYOSA,"CARRY.results.MYOSA.txt",sep="S", col.names=TRUE)

#KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA KOLEA

#subsetting for just kolea; create a new file to work with
MYRLA <- subset(Alala.carry, Plant== "MYRLA")

summary(MYRLA$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
MYRLA.glm <- glm(MYRLA$carries~ MYRLA$Sex + MYRLA$Age,weights=MYRLA$given, na.action = na.fail)

#this gives you your typical stats
summary(MYRLA.glm)

#getting AIC tables and coefficients
MYRLA.AIC <- dredge(MYRLA.glm)
#please show me those AIC values
MYRLA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(MYRLA.AIC,"CARRY.MYRLA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.MYRLA <- model.avg(get.models(MYRLA.AIC, seq(nrow(MYRLA.AIC))))
MYRLA.Age <- MYRLA$Age
MYRLA.Sex <- MYRLA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.MYRLA <- as.data.frame(cbind(MYRLA.Age, MYRLA.Sex))
pred.avg.MYRLA <- predict(average.MYRLA, newdata.MYRLA,se.fit=TRUE, type="response")
results.MYRLA <- unique(cbind(newdata.MYRLA,pred.avg.MYRLA$fit,pred.avg.MYRLA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.MYRLA,"CARRY.results.MYRLA.txt",sep="S", col.names=TRUE)

#MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI

#subsetting for just mamaki; create a new file to work with
PIPAL <- subset(Alala.carry, Plant== "PIPAL")

summary(PIPAL$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
PIPAL.glm <- glm(PIPAL$carries~ PIPAL$Sex + PIPAL$Age,weights=PIPAL$given, na.action = na.fail)

#this gives you your typical stats
summary(PIPAL.glm)

#getting AIC tables and coefficients
PIPAL.AIC <- dredge(PIPAL.glm)
#please show me those AIC values
PIPAL.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PIPAL.AIC,"CARRY.PIPAL.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.PIPAL <- model.avg(get.models(PIPAL.AIC, seq(nrow(PIPAL.AIC))))
PIPAL.Age <- PIPAL$Age
PIPAL.Sex <- PIPAL$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.PIPAL <- as.data.frame(cbind(PIPAL.Age, PIPAL.Sex))
pred.avg.PIPAL <- predict(average.PIPAL, newdata.PIPAL,se.fit=TRUE, type="response")
results.PIPAL <- unique(cbind(newdata.PIPAL,pred.avg.PIPAL$fit,pred.avg.PIPAL$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PIPAL,"CARRY.results.PIPAL.txt",sep="S", col.names=TRUE)

#HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA

#subsetting for just hoawa; create a new file to work with
PITHO <- subset(Alala.carry, Plant== "PITHO")

summary(PITHO$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
PITHO.glm <- glm(PITHO$carries~ PITHO$Sex + PITHO$Age,weights=PITHO$given, na.action = na.fail)

#this gives you your typical stats
summary(PITHO.glm)

#getting AIC tables and coefficients
PITHO.AIC <- dredge(PITHO.glm)
#please show me those AIC values
PITHO.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PITHO.AIC,"CARRY.PITHO.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.PITHO <- model.avg(get.models(PITHO.AIC, seq(nrow(PITHO.AIC))))
PITHO.Age <- PITHO$Age
PITHO.Sex <- PITHO$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.PITHO <- as.data.frame(cbind(PITHO.Age,PITHO.Sex))
pred.avg.PITHO <- predict(average.PITHO, newdata.PITHO,se.fit=TRUE, type="response")
results.PITHO <- unique(cbind(newdata.PITHO,pred.avg.PITHO$fit,pred.avg.PITHO$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PITHO,"CARRY.results.PITHO.txt",sep="S", col.names=TRUE)

#HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE HALAPEPE

#subsetting for just halapepe; create a new file to work with
PLEHA <- subset(Alala.carry, Plant== "PLEHA")

summary(PLEHA$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
PLEHA.glm <- glm(PLEHA$carries~ PLEHA$Sex + PLEHA$Age,weights=PLEHA$given, na.action = na.fail)

#this gives you your typical stats
summary(PLEHA.glm)

#getting AIC tables and coefficients
PLEHA.AIC <- dredge(PLEHA.glm)
#please show me those AIC values
PLEHA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PLEHA.AIC,"CARRY.PLEHA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.PLEHA <- model.avg(get.models(PLEHA.AIC, seq(nrow(PLEHA.AIC))))
PLEHA.Age <- PLEHA$Age
PLEHA.Sex <- PLEHA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.PLEHA <- as.data.frame(cbind(PLEHA.Age,PLEHA.Sex))
pred.avg.PLEHA <- predict(average.PLEHA, newdata.PLEHA,se.fit=TRUE, type="response")
results.PLEHA <- unique(cbind(newdata.PLEHA,pred.avg.PLEHA$fit,pred.avg.PLEHA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PLEHA,"CARRY.results.PLEHA.txt",sep="S", col.names=TRUE)

#RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU RIPELOULU

#subsetting for just ripeloulu; create a new file to work with
PRISCb <- subset(Alala.carry, Plant== "PRISCb")

summary(PRISCb$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
PRISCb.glm <- glm(PRISCb$carries~ PRISCb$Sex + PRISCb$Age,weights=PRISCb$given, na.action = na.fail)

#this gives you your typical stats
summary(PRISCb.glm)

#getting AIC tables and coefficients
PRISCb.AIC <- dredge(PRISCb.glm)
#please show me those AIC values
PRISCb.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PRISCb.AIC,"CARRY.PRISCb.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.PRISCb <- model.avg(get.models(PRISCb.AIC, seq(nrow(PRISCb.AIC))))
PRISCb.Age <- PRISCb$Age
PRISCb.Sex <- PRISCb$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.PRISCb <- as.data.frame(cbind(PRISCb.Age,PRISCb.Sex))
pred.avg.PRISCb <- predict(average.PRISCb, newdata.PRISCb,se.fit=TRUE, type="response")
results.PRISCb <- unique(cbind(newdata.PRISCb,pred.avg.PRISCb$fit,pred.avg.PRISCb$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PRISCb,"CARRY.results.PRISCb.txt",sep="S", col.names=TRUE)

#GREENLOULU GREENLOULU GREENLOULU GREENLOULU GREENLOULU GREENLOULU GREENLOULU GREENLOULU GREENLOULU

#subsetting for just greenloulu; create a new file to work with
PRISCg <- subset(Alala.carry, Plant== "PRISCg")

summary(PRISCg$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
PRISCg.glm <- glm(PRISCg$carries~ PRISCg$Sex + PRISCg$Age,weights=PRISCg$given, na.action = na.fail)

#this gives you your typical stats
summary(PRISCg.glm)

#getting AIC tables and coefficients
PRISCg.AIC <- dredge(PRISCg.glm)
#please show me those AIC values
PRISCg.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PRISCg.AIC,"CARRY.PRISCg.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.PRISCg <- model.avg(get.models(PRISCg.AIC, seq(nrow(PRISCg.AIC))))
PRISCg.Age <- PRISCg$Age
PRISCg.Sex <- PRISCg$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.PRISCg <- as.data.frame(cbind(PRISCg.Age,PRISCg.Sex))
pred.avg.PRISCg <- predict(average.PRISCg, newdata.PRISCg,se.fit=TRUE, type="response")
results.PRISCg <- unique(cbind(newdata.PRISCg,pred.avg.PRISCg$fit,pred.avg.PRISCg$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PRISCg,"CARRIES.results.PRISCg.txt",sep="S", col.names=TRUE)

#PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE PUKIAWE

#subsetting for pukiawe; create a new file to work with
STYTA <- subset(Alala.carry, Plant== "STYTA")

#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
STYTA.glm <- glm(STYTA$carries~ STYTA$Sex + STYTA$Age,weights=STYTA$given, na.action = na.fail)
#this gives you your typical stats
summary(STYTA.glm)

#getting AIC tables and coefficients
STYTA.AIC <- dredge(STYTA.glm)
#please show me those AIC values
STYTA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(STYTA.AIC,"CARRY.STYTA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.STYTA <- model.avg(get.models(STYTA.AIC, seq(nrow(STYTA.AIC))))
STYTA.Age <- STYTA$Age
STYTA.Sex <- STYTA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.STYTA <- as.data.frame(cbind(STYTA.Age,STYTA.Sex))
pred.avg.STYTA <- predict(average.STYTA, newdata.STYTA,se.fit=TRUE, type="response")
results.STYTA <- unique(cbind(newdata.STYTA,pred.avg.STYTA$fit,pred.avg.STYTA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.STYTA,"CARRY.results.STYTA.txt",sep="S", col.names=TRUE)

#OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA OHEMAUKA

#subsetting for just ohemauka; create a new file to work with
TETHA <- subset(Alala.carry, Plant== "TETHA")

summary(TETHA$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
TETHA.glm <- glm(TETHA$carries~ TETHA$Sex + TETHA$Age,weights=TETHA$given, na.action = na.fail)

#this gives you your typical stats
summary(TETHA.glm)

#getting AIC tables and coefficients
TETHA.AIC <- dredge(TETHA.glm)
#please show me those AIC values
TETHA.AIC
#writes a text file from the AIC table which can be read into excel
write.table(TETHA.AIC,"CARRY.TETHA.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.TETHA <- model.avg(get.models(TETHA.AIC, seq(nrow(TETHA.AIC))))
TETHA.Age <- TETHA$Age
TETHA.Sex <- TETHA$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.TETHA <- as.data.frame(cbind(TETHA.Age,TETHA.Sex))
pred.avg.TETHA <- predict(average.TETHA, newdata.TETHA,se.fit=TRUE, type="response")
results.TETHA <- unique(cbind(newdata.TETHA,pred.avg.TETHA$fit,pred.avg.TETHA$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.TETHA,"CARRY.results.TETHA.txt",sep="S", col.names=TRUE)

#OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO

#subsetting for just ohelo; create a new file to work with
VACRE <- subset(Alala.carry, Plant== "VACRE")

summary(VACRE$carries)
#this is the actual regression; the order that this is in, is the order of the variables in the AIC table
VACRE.glm <- glm(VACRE$carries~ VACRE$Sex + VACRE$Age,weights=VACRE$given, na.action = na.fail)

#this gives you your typical stats
summary(VACRE.glm)

#getting AIC tables and coefficients
VACRE.AIC <- dredge(VACRE.glm)
#please show me those AIC values
VACRE.AIC
#writes a text file from the AIC table which can be read into excel
write.table(VACRE.AIC,"CARRY.VACRE.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- remember, if the confidence interval bounds zero it does not matter!  Use these estimates for talking about the magnitude of age & sex effects
average.VACRE <- model.avg(get.models(VACRE.AIC, seq(nrow(VACRE.AIC))))
VACRE.Age <- VACRE$Age
VACRE.Sex <- VACRE$Sex

#this is to get those averaged model estimates with the standard errors, you can use a formula to estimate the confidence interval using the SE
newdata.VACRE <- as.data.frame(cbind(VACRE.Age,VACRE.Sex))
pred.avg.VACRE <- predict(average.VACRE, newdata.VACRE,se.fit=TRUE, type="response")
results.VACRE <- unique(cbind(newdata.VACRE,pred.avg.VACRE$fit,pred.avg.VACRE$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.VACRE,"CARRY.results.VACRE.txt",sep="S", col.names=TRUE)
# THE END
