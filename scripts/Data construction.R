library(dplyr)
Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")

#How many individuals do we have?----
summary(Beeh.data$Genus)
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

#Let's identify which species did not work
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










summary(Beeh.data$Species)
#Graphs of every individual for PER sugar
#Plot test
plot(t(Beeh.PER.sugar[16,(3:10)]), xlab="Trial number", ylab = "Time")
lines(t(Beeh.PER.sugar[16,(3:10)]))

is.na(Beeh.PER.sugar)
par(mfrow = c(3,3))
n=1


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



