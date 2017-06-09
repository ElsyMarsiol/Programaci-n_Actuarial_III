setwd("~/GitHub/Programaci-n_Actuarial_III/Dataset")
xtra<-read.table("./train/X_train.txt")
xtes<-read.table("./test/X_test.txt")
total <- rbind(xtra, xtes)

rm(xtra,xtes)

Etiqueta<-read.table("features.txt")
colnames(total)<-Etiqueta[,2]
igual <- c("mean", "std")
nototal<-subset(total, select = grepl(paste(igual, collapse = "|"), 
                                       names(total)))
rm(Etiqueta,igual,total)

colnames(nototal) <- sub("-", "", colnames(nototal))
colnames(nototal) <- sub("^", "MeanOf", colnames(nototal))
colnames(nototal) <- sub("BodyBody", "Body", colnames(nototal))
colnames(nototal) <- sub("f", "Frequency", colnames(nototal))
colnames(nototal) <- sub("t", "Time", colnames(nototal))
colnames(nototal) <- sub("std", "Std", colnames(nototal))
colnames(nototal) <- sub("mean", "Mean", colnames(nototal))

atrain<-read.table("./train/y_train.txt")
acttest<-read.table("./test/y_test.txt")
actividad<-rbind(atrain,acttest)

rm(atrain,acttest)

Actividad<-factor(actividad$V1,levels = c(1:6))
levels(Actividad)<-c("Walking","Walking Upstairs","Walking Downstairs",
                     "Sitting","Standing","Laying")



PersonasTrain<-read.table("./train/subject_train.txt")
PersonasTest<-read.table("./test/subject_test.txt")
Persona<-rbind(PersonasTrain,PersonasTest)[,1]
nototal2<-cbind(Actividad,nototal)
nototal3<-cbind(Persona,nototal2)

library(dplyr)
Tabla <- nototal3 %>% group_by(Persona, Actividad) %>% summarise_each(funs(mean))
write.table(Tabla, row.names= F,file = "ordenada.txt") 
View(Tabla)