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
