Plant.germ <- as.data.frame(read.delim(file="Data.Plant.germ.txt", header=TRUE, sep="\t"));
#this tells you the names of the colums in your data base
names(Plant.germ)

#lists all of the names of the plants
unique(Plant.germ$Plant)

#get a separate data set for only Olapa
CHETR <- subset(Plant.germ, Plant== "CHETR")

#get a separate data set for only Oha kepau
CLEHA <- subset(Plant.germ, Plant== "CLEHA")

#get a separate data set for only Pilo
COPRH <- subset(Plant.germ, Plant== "COPRH")

#get a separate data set for only Mamaki
PIPAL <- subset(Plant.germ, Plant== "PIPAL")

#get a separate data set for only Hoawa
PITHO <- subset(Plant.germ, Plant== "PITHO")

#get a separate data set for only Ohelo
VACRE <- subset(Plant.germ, Plant== "VACRE")

#OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA OLAPA

#regression with variables in the order that they'll appear in the AIC table.
CHETRgerm.glm <- glm(CHETR$germsucc~ CHETR$trt,weights=CHETR$planted, na.action = na.fail)
#stats for the regression
summary(CHETRgerm.glm)

#getting AIC tables and coefficients
CHETRgerm.AIC <- dredge(CHETRgerm.glm)
#please show me those AIC values
CHETRgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(CHETRgerm.AIC,"GERM.CHETR.AIC.txt",sep="S", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients-
average.CHETRgerm <- model.avg(get.models(CHETRgerm.AIC, seq(nrow(CHETRgerm.AIC))))
CHETRgerm.trt <- CHETR$trt

#this is to get those averaged model estimates with the standard errors
newdata.CHETRgerm <- as.data.frame(cbind(CHETRgerm.trt))
pred.avg.CHETRgerm <- predict(average.CHETRgerm, newdata.CHETRgerm,se.fit=TRUE, type="response")
results.CHETRgerm <- unique(cbind(newdata.CHETRgerm,pred.avg.CHETRgerm$fit,pred.avg.CHETRgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.CHETRgerm,"GERM.results.CHETR.txt",sep=" ", col.names=TRUE)


#OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU OHAKEPAU

#regression with variables in the order that they'll appear in the AIC table.
CLEHAgerm.glm <- glm(CLEHA$germsucc~ CLEHA$trt,weights=CLEHA$planted, na.action = na.fail)
#stats for the regression
summary(CLEHAgerm.glm)

#getting AIC tables and coefficients
CLEHAgerm.AIC <- dredge(CLEHAgerm.glm)
#please show me those AIC values
CLEHAgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(CLEHAgerm.AIC,"GERM.CLEHA.AIC.txt",sep=" ", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients-
average.CLEHAgerm <- model.avg(get.models(CLEHAgerm.AIC, seq(nrow(CLEHAgerm.AIC))))
CLEHAgerm.trt <- CLEHA$trt

#this is to get those averaged model estimates with the standard errors
newdata.CLEHAgerm <- as.data.frame(cbind(CLEHAgerm.trt))
pred.avg.CLEHAgerm <- predict(average.CLEHAgerm, newdata.CLEHAgerm,se.fit=TRUE, type="response")
results.CLEHAgerm <- unique(cbind(newdata.CLEHAgerm,pred.avg.CLEHAgerm$fit,pred.avg.CLEHAgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.CLEHAgerm,"GERM.results.CLEHA.txt",sep=" ", col.names=TRUE)

#PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO PILO

#regression with variables in the order that they'll appear in the AIC table.
COPRHgerm.glm <- glm(COPRH$germsucc~ COPRH$trt,weights=COPRH$planted, na.action = na.fail)
#stats for the regression
summary(COPRHgerm.glm)

#getting AIC tables and coefficients
COPRHgerm.AIC <- dredge(COPRHgerm.glm)
#please show me those AIC values
COPRHgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(COPRHgerm.AIC,"GERM.COPRH.AIC.txt",sep=" ", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients-
average.COPRHgerm <- model.avg(get.models(COPRHgerm.AIC, seq(nrow(COPRHgerm.AIC))))
COPRHgerm.trt <- COPRH$trt

#this is to get those averaged model estimates with the standard errors
newdata.COPRHgerm <- as.data.frame(cbind(COPRHgerm.trt))
pred.avg.COPRHgerm <- predict(average.COPRHgerm, newdata.COPRHgerm,se.fit=TRUE, type="response")
results.COPRHgerm <- unique(cbind(newdata.COPRHgerm,pred.avg.COPRHgerm$fit,pred.avg.COPRHgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.COPRHgerm,"GERM.results.COPRH.txt",sep=" ", col.names=TRUE)

#MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI MAMAKI

#regression with variables in the order that they'll appear in the AIC table.
PIPALgerm.glm <- glm(PIPAL$germsucc~ PIPAL$trt,weights=PIPAL$planted, na.action = na.fail)
#stats for the regression
summary(PIPALgerm.glm)

#getting AIC tables and coefficients
PIPALgerm.AIC <- dredge(PIPALgerm.glm)
#please show me those AIC values
PIPALgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PIPALgerm.AIC,"GERM.PIPAL.AIC.txt",sep=" ", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients-
average.PIPALgerm <- model.avg(get.models(PIPALgerm.AIC, seq(nrow(PIPALgerm.AIC))))
PIPALgerm.trt <- PIPAL$trt

#this is to get those averaged model estimates with the standard errors
newdata.PIPALgerm <- as.data.frame(cbind(PIPALgerm.trt))
pred.avg.PIPALgerm <- predict(average.PIPALgerm, newdata.PIPALgerm,se.fit=TRUE, type="response")
results.PIPALgerm <- unique(cbind(newdata.PIPALgerm,pred.avg.PIPALgerm$fit,pred.avg.PIPALgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PIPALgerm,"GERM.results.PIPAL.txt",sep=" ", col.names=TRUE)

#HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA HOAWA

#regression with variables in the order that they'll appear in the AIC table.
PITHOgerm.glm <- glm(PITHO$germsucc~ PITHO$trt,weights=PITHO$planted, na.action = na.fail)
#stats for the regression
summary(PITHOgerm.glm)

#getting AIC tables and coefficients
PITHOgerm.AIC <- dredge(PITHOgerm.glm)
#please show me those AIC values
PITHOgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(PITHOgerm.AIC,"GERM.PITHO.AIC.txt",sep=" ", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients- 
average.PITHOgerm <- model.avg(get.models(PITHOgerm.AIC, seq(nrow(PITHOgerm.AIC))))
PITHOgerm.trt <- PITHO$trt

#this is to get those averaged model estimates with the standard errors
newdata.PITHOgerm <- as.data.frame(cbind(PITHOgerm.trt))
pred.avg.PITHOgerm <- predict(average.PITHOgerm, newdata.PITHOgerm,se.fit=TRUE, type="response")
results.PITHOgerm <- unique(cbind(newdata.PITHOgerm,pred.avg.PITHOgerm$fit,pred.avg.PITHOgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.PITHOgerm,"GERM.results.PITHO.txt",sep=" ", col.names=TRUE)

#OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO OHELO

#regression with variables in the order that they'll appear in the AIC table.
VACREgerm.glm <- glm(VACRE$germsucc~ VACRE$trt,weights=VACRE$planted, na.action = na.fail)
#stats for the regression
summary(VACREgerm.glm)

#getting AIC tables and coefficients
VACREgerm.AIC <- dredge(VACREgerm.glm)
#please show me those AIC values
VACREgerm.AIC
#writes a text file from the AIC table which can be read into excel
write.table(VACREgerm.AIC,"GERM.VACRE.AIC.txt",sep=" ", col.names=TRUE)

#this is to do model averaging and get model averaged estimates of each of the coefficients-
average.VACREgerm <- model.avg(get.models(VACREgerm.AIC, seq(nrow(VACREgerm.AIC))))
VACREgerm.trt <- VACRE$trt

#this is to get those averaged model estimates with the standard errors
newdata.VACREgerm <- as.data.frame(cbind(VACREgerm.trt))
pred.avg.VACREgerm <- predict(average.VACREgerm, newdata.VACREgerm,se.fit=TRUE, type="response")
results.VACREgerm <- unique(cbind(newdata.VACREgerm,pred.avg.VACREgerm$fit,pred.avg.VACREgerm$se.fit))

#This writes a text file with the averaged estimates results that you can read into excel
write.table(results.VACREgerm,"GERM.results.VACRE.txt",sep=" ", col.names=TRUE)
# FINAL FELIZ
