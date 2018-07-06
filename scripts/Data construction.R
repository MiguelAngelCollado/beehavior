library(dplyr)
Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")

#How many individuals do we have?----
nrow(Beeh.data)
summary(Beeh.data$Genus)
summary(Beeh.data$Species)
n.of.species<-as.data.frame(summary(Beeh.data$Species))
colnames(n.of.species)<-"individuals captured"
#We export the list for the article
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/figures")
write.csv(n.of.species, "list of species.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")

nrow(Beeh.data)
length(summary(Beeh.data$Species))

which(summary(Beeh.data$Species)>2)
which(summary(Beeh.data$Species)==2)
which(summary(Beeh.data$Species)==1)
#How many species have less than 3 individuals?
length(which(summary(Beeh.data$Species)<3))

#Data treatment and construction-----
#PER variables must be numerical (pasalos a as.character primero)
str(Beeh.data)
Beeh.data$PER.sugar1<-as.numeric(as.character(Beeh.data$PER.sugar1))
Beeh.data$PER.sugar2<-as.numeric(as.character(Beeh.data$PER.sugar2))
Beeh.data$PER.sugar3<-as.numeric(as.character(Beeh.data$PER.sugar3))
Beeh.data$PER.sugar4<-as.numeric(as.character(Beeh.data$PER.sugar4))
Beeh.data$PER.sugar5<-as.numeric(as.character(Beeh.data$PER.sugar5))
Beeh.data$PER.sugar6<-as.numeric(as.character(Beeh.data$PER.sugar6))
Beeh.data$PER.sugar7<-as.numeric(as.character(Beeh.data$PER.sugar7))
Beeh.data$PER.sugar.test<-as.numeric(as.character(Beeh.data$PER.sugar.test))
Beeh.data$PER.water1<-as.numeric(as.character(Beeh.data$PER.water1))
Beeh.data$PER.water2<-as.numeric(as.character(Beeh.data$PER.water2))
Beeh.data$PER.water3<-as.numeric(as.character(Beeh.data$PER.water3))
Beeh.data$PER.water4<-as.numeric(as.character(Beeh.data$PER.water4))
Beeh.data$PER.water5<-as.numeric(as.character(Beeh.data$PER.water5))
Beeh.data$PER.water6<-as.numeric(as.character(Beeh.data$PER.water6))
Beeh.data$PER.water7<-as.numeric(as.character(Beeh.data$PER.water7))
Beeh.data$PER.water.test<-as.numeric(as.character(Beeh.data$PER.water.test))
str(Beeh.data)


#PER.sugar data.frame
Beeh.PER.sugar<-data.frame(Beeh.data$ID,
                           Beeh.data$Species,
                           Beeh.data$PER.sugar1,
                           Beeh.data$PER.sugar2,
                           Beeh.data$PER.sugar3,
                           Beeh.data$PER.sugar4,
                           Beeh.data$PER.sugar5,
                           Beeh.data$PER.sugar6,
                           Beeh.data$PER.sugar7,
                           Beeh.data$PER.sugar.test)

Beeh.PER.sugar<-rename(Beeh.PER.sugar, 
       ID = Beeh.data.ID, 
       Species = Beeh.data.Species,
       PER.sugar1 = Beeh.data.PER.sugar1,
       PER.sugar2 = Beeh.data.PER.sugar2,
       PER.sugar3 = Beeh.data.PER.sugar3,
       PER.sugar4 = Beeh.data.PER.sugar4,
       PER.sugar5 = Beeh.data.PER.sugar5,
       PER.sugar6 = Beeh.data.PER.sugar6,
       PER.sugar7 = Beeh.data.PER.sugar7,
       PER.sugar.test = Beeh.data.PER.sugar.test)


#Number of individuals that reacted to the test#

#Useless bees and useful bees----

useless<-which(is.na(Beeh.PER.sugar$PER.sugar1) & 
                 is.na(Beeh.PER.sugar$PER.sugar2) & 
                 is.na(Beeh.PER.sugar$PER.sugar3) & 
                 is.na(Beeh.PER.sugar$PER.sugar4) & 
                 is.na(Beeh.PER.sugar$PER.sugar5) & 
                 is.na(Beeh.PER.sugar$PER.sugar6) & 
                 is.na(Beeh.PER.sugar$PER.sugar7) & 
                 is.na(Beeh.PER.sugar$PER.sugar.test))
useless.bees<-Beeh.data[useless,]
functional.bees<-Beeh.data[-useless,]

#Data frame of bees that worked
functional.bees
#Data frame of bees that did not work
useless.bees

#Let's identify which species did not work at all
useless.bees
useless.species.temp<-unique(useless.bees$Species)
useless.species.temp
usefulness<-rep("useless", length(useless.species.temp))

useless.species.temp2<-data.frame(useless.species.temp, usefulness)


functional.bees
useful.species.temp<-unique(functional.bees$Species)
useful.species.temp
usefulness<-rep("useful", length(useful.species.temp))

useful.species.temp2<-data.frame(useful.species.temp, usefulness)

colnames(useless.species.temp2)<-c("Species","Usefulness")
colnames(useful.species.temp2)<-c("Species","Usefulness")
usefulness.data<-rbind(useful.species.temp2,useless.species.temp2)

#We create a list of species that did not react at all to the experiment
useful.temp<-usefulness.data[!duplicated(usefulness.data[,"Species"]),]
useless.species.list<-subset(useful.temp, subset = (useful.temp$Usefulness == "useless"))
useless.species.list<-useless.species.list$Species
useless.species.list

useless.species<-NULL

for (n in 1:length(useless.species.list)) {
temp<-subset(Beeh.data, subset = (Beeh.data$Species == (useless.species.list[n])))
useless.species<-rbind(useless.species,temp)
  }

#Here we have it
useless.species<-droplevels.data.frame(useless.species)
useless.species
summary(useless.species$Species)
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/figures")
write.csv(summary(useless.species$Species), "list of useless species.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")



summary(Beeh.data$Species)
#Graphs of every individual for PER sugar
#Plot test
plot(t(Beeh.PER.sugar[16,(3:10)]), xlab="Trial number", ylab = "Time")
lines(t(Beeh.PER.sugar[16,(3:10)]))

is.na(Beeh.PER.sugar)
par(mfrow = c(3,3))
colnames(Beeh.PER.sugar)
par(mfrow = c(3,4))
for (n in 1:(nrow(Beeh.PER.sugar))) {
  if(is.na(Beeh.PER.sugar$PER.sugar1[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar2[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar3[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar4[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar5[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar6[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar7[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar.test[n])){
    print("This bee does not work")}else{                                                                                                                                                                                                                       
  print(plot(t(Beeh.PER.sugar[n,(3:10)]), xlab="Trial number", ylab = "Time", main = (Beeh.PER.sugar$Species[n])))
  lines(t(Beeh.PER.sugar[n,(3:10)]))
}}
par(mfrow = c(1,1))


#Individual graphs
par(mfrow = c(1,1))
plot(t(Beeh.PER.sugar[1,(3:10)]), xlab="Trial number", ylab = "Time", main=(Beeh.PER.sugar$Beeh.data.Species))
lines(t(Beeh.PER.sugar[1,(3:10)]))


#Some bees, spent too much time between tasting one cue and the other, let's explore
#that

max.xlim<-max(na.omit(c(Beeh.data$PER.sugar1,Beeh.data$PER.sugar2,Beeh.data$PER.sugar3,Beeh.data$PER.sugar4,Beeh.data$PER.sugar5,Beeh.data$PER.sugar6,Beeh.data$PER.sugar7,Beeh.data$PER.sugar.test,Beeh.data$PER.water1,Beeh.data$PER.water2,Beeh.data$PER.water3,Beeh.data$PER.water4,Beeh.data$PER.water5,Beeh.data$PER.water6,Beeh.data$PER.water7,Beeh.data$PER.water.test)))
par(mfrow=c(2,2))
plot(abs(Beeh.data$PER.sugar1 - Beeh.data$PER.water1), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 1", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar2 - Beeh.data$PER.water2), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 2", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar3 - Beeh.data$PER.water3), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 3", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar4 - Beeh.data$PER.water4), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 4", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar5 - Beeh.data$PER.water5), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 5", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar6 - Beeh.data$PER.water6), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 6", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar7 - Beeh.data$PER.water7), row.names(Beeh.data), xlab="Time difference between sugar PER and water PER", ylab="Individual", main = "Trial 7", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
plot(abs(Beeh.data$PER.sugar.test - Beeh.data$PER.water.test), row.names(Beeh.data), ylab="Individual", xlab="Time difference between sugar PER and water PER", main = "Test", ylim = c(0,nrow(Beeh.data)), xlim = c(0,max.xlim))
abline(v = 120)
par(mfrow=c(1,1))

par(mfrow=c(1,2))

boxplot(Beeh.data$PER.sugar1,Beeh.data$PER.sugar2,Beeh.data$PER.sugar3,Beeh.data$PER.sugar4,Beeh.data$PER.sugar5,Beeh.data$PER.sugar6,Beeh.data$PER.sugar7,Beeh.data$PER.sugar.test, main= "PER sugar dispersion along trials", names = c("Trial 1","Trial 2","Trial 3","Trial 4","Trial 5","Trial 6","Trial 7","Test"), ylab="Time", ylim=c(0,max.xlim))
boxplot(Beeh.data$PER.water1 ,Beeh.data$PER.water2,Beeh.data$PER.water3,Beeh.data$PER.water4,Beeh.data$PER.water5,Beeh.data$PER.water6,Beeh.data$PER.water7,Beeh.data$PER.water.test, main= "PER water dispersion along trials", names = c("Trial 1","Trial 2","Trial 3","Trial 4","Trial 5","Trial 6","Trial 7","Test"), ylab="Time", ylim=c(0,max.xlim))
