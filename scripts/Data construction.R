getwd()
library(dplyr)
Beeh.data<-read.csv("data/Behavior comparison.csv")
nrow(Beeh.data)
#If we don't have the correct cue, we can't use that bee
Beeh.data<-Beeh.data[-which(Beeh.data$Correct.cue == ""),]
#If we don't have the genus, we can't use that bee
Beeh.data<-Beeh.data[-which(is.na(Beeh.data$Genus)),]
#Check
which(Beeh.data$Correct.cue == "")
which(is.na(Beeh.data$Genus))
#The number of individuals identified is
nrow(Beeh.data)



#Explore and correct data----
#We transform data as numeric
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
Beeh.data$Brain.weight<-as.numeric(as.character(Beeh.data$Brain.weight))
Beeh.data$Experimental.tube<-as.factor(Beeh.data$Experimental.tube)
str(Beeh.data)
summary(Beeh.data$Place)
summary(Beeh.data$Genus)
summary(Beeh.data$Species)
summary(Beeh.data$Sex)
summary(Beeh.data$Age.signals)


#How many individuals do we have?----
nrow(Beeh.data)
Beeh.data$Species<-droplevels(Beeh.data$Species)
n.of.species<-data.frame(summary(Beeh.data$Species))
colnames(n.of.species)<-"individuals captured"
#We export the list for the article
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/figures")
#write.csv(n.of.species, "list of species.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")

summary(Beeh.data$Species)
length(summary(Beeh.data$Species))


which(summary(Beeh.data$Species)>2)
which(summary(Beeh.data$Species)==2)
which(summary(Beeh.data$Species)==1)

#Data treatment and construction-----
#PER variables must be numerical


#Check brain weight

par(cex.axis=0.4)
boxplot(Beeh.data$Brain.weight~Beeh.data$Species, las = 2)
boxplot(Beeh.data$Brain.weight~Beeh.data$Genus, las = 2)
par(cex.axis=1)

#D41 Is within the boxplot at genus level, but not at indefined sp. level


#Check IT's
par(cex.axis=0.4)
boxplot(Beeh.data$IT..mm.~Beeh.data$Species, las = 2)
boxplot(Beeh.data$IT..mm.~Beeh.data$Genus, las = 2)
par(cex.axis=1)


#Constructing data frames----
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

Beeh.PER.water<-data.frame(Beeh.data$ID,
                           Beeh.data$Species,
                           Beeh.data$PER.water1,
                           Beeh.data$PER.water2,
                           Beeh.data$PER.water3,
                           Beeh.data$PER.water4,
                           Beeh.data$PER.water5,
                           Beeh.data$PER.water6,
                           Beeh.data$PER.water7,
                           Beeh.data$PER.water.test)

Beeh.PER.water<-rename(Beeh.PER.water, 
       ID = Beeh.data.ID, 
       Species = Beeh.data.Species,
       PER.water1 = Beeh.data.PER.water1,
       PER.water2 = Beeh.data.PER.water2,
       PER.water3 = Beeh.data.PER.water3,
       PER.water4 = Beeh.data.PER.water4,
       PER.water5 = Beeh.data.PER.water5,
       PER.water6 = Beeh.data.PER.water6,
       PER.water7 = Beeh.data.PER.water7,
       PER.water.test = Beeh.data.PER.water.test)

Beeh.success<-data.frame(Beeh.data$ID,
                           Beeh.data$Species,
                           Beeh.data$Success1,
                           Beeh.data$Success2,
                           Beeh.data$Success3,
                           Beeh.data$Success4,
                           Beeh.data$Success5,
                           Beeh.data$Success6,
                           Beeh.data$Success7,
                           Beeh.data$Success.test)

Beeh.water.exploring<-data.frame(Beeh.data$ID,
                                 Beeh.data$Species,
                                 Beeh.data$Water.exploring1,
                                 Beeh.data$Water.exploring2,
                                 Beeh.data$Water.exploring3,
                                 Beeh.data$Water.exploring4,
                                 Beeh.data$Water.exploring5,
                                 Beeh.data$Water.exploring6,
                                 Beeh.data$Water.exploring7,
                                 Beeh.data$Water.exploring.test)




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
sequedaronporelcamino<-which(functional.bees$Success.test == "")
last.test.done.bees<-functional.bees[-sequedaronporelcamino,]

#Data frame of bees that worked
functional.bees
#Data frame of bees that did not work
useless.bees
#Data frame of bees that did the last test
last.test.done.bees

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
useless.species
#Here we have it
useless.species<-droplevels.data.frame(useless.species)
useless.species
summary(useless.species$Species)
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/figures")
#write.csv(summary(useless.species$Species), "list of useless species.csv")
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





plot(c(mean(na.omit(Beeh.PER.sugar$PER.sugar1)),
           mean(na.omit(Beeh.PER.sugar$PER.sugar2)),
           mean(na.omit(Beeh.PER.sugar$PER.sugar3)),
           mean(na.omit(Beeh.PER.sugar$PER.sugar4)), 
           mean(na.omit(Beeh.PER.sugar$PER.sugar5)), 
           mean(na.omit(Beeh.PER.sugar$PER.sugar6)), 
           mean(na.omit(Beeh.PER.sugar$PER.sugar7)), 
           mean(na.omit(Beeh.PER.sugar$PER.sugar.test))), xlab="Time until proboscis extension", ylab = "Trial number", main="PER Sugar time tests") 


#Number of success of each individual-----
nrow(last.test.done.bees)
corrected.data1<-last.test.done.bees

#QUESTION: we count NA's as NO
#Create a data.frame just to count successess
Success8trials<-data.frame(last.test.done.bees$ID,
                           last.test.done.bees$Species,
                           last.test.done.bees$Success1,
                           last.test.done.bees$Success2,
                           last.test.done.bees$Success3,
                           last.test.done.bees$Success4,
                           last.test.done.bees$Success5,
                           last.test.done.bees$Success6,
                           last.test.done.bees$Success7,
                           last.test.done.bees$Success.test)

colnames(Success8trials)
Success8trials<-rename(Success8trials, 
                       ID = last.test.done.bees.ID, 
                       Species = last.test.done.bees.Species,
                       Success1 = last.test.done.bees.Success1,
                       Success2 = last.test.done.bees.Success2,
                       Success3 = last.test.done.bees.Success3,
                       Success4 = last.test.done.bees.Success4,
                       Success5 = last.test.done.bees.Success5,
                       Success6 = last.test.done.bees.Success6,
                       Success7 = last.test.done.bees.Success7,
                       Success.test = last.test.done.bees.Success.test)



#We replace NAs with NO
Success8trials$Success1[is.na(Success8trials$Success1)] <- "No"
Success8trials$Success2[is.na(Success8trials$Success2)] <- "No"
Success8trials$Success3[is.na(Success8trials$Success3)] <- "No"
Success8trials$Success4[is.na(Success8trials$Success4)] <- "No"
Success8trials$Success5[is.na(Success8trials$Success5)] <- "No"
Success8trials$Success6[is.na(Success8trials$Success6)] <- "No"
Success8trials$Success7[is.na(Success8trials$Success7)] <- "No"
Success8trials$Success.test[is.na(Success8trials$Success.test)] <- "No"



#We replace No with 0
Success8trials$Success1<- replace(as.character(Success8trials$Success1), as.character(Success8trials$Success1)=="No", 0)
Success8trials$Success2<- replace(as.character(Success8trials$Success2), as.character(Success8trials$Success2)=="No", 0)
Success8trials$Success3<- replace(as.character(Success8trials$Success3), as.character(Success8trials$Success3)=="No", 0)
Success8trials$Success4<- replace(as.character(Success8trials$Success4), as.character(Success8trials$Success4)=="No", 0)
Success8trials$Success5<- replace(as.character(Success8trials$Success5), as.character(Success8trials$Success5)=="No", 0)
Success8trials$Success6<- replace(as.character(Success8trials$Success6), as.character(Success8trials$Success6)=="No", 0)
Success8trials$Success7<- replace(as.character(Success8trials$Success7), as.character(Success8trials$Success7)=="No", 0)
Success8trials$Success.test<- replace(as.character(Success8trials$Success.test), as.character(Success8trials$Success.test)=="No", 0)


#We replace Yes with 1
Success8trials$Success1<- replace(as.character(Success8trials$Success1), as.character(Success8trials$Success1)=="Yes", 1)
Success8trials$Success2<- replace(as.character(Success8trials$Success2), as.character(Success8trials$Success2)=="Yes", 1)
Success8trials$Success3<- replace(as.character(Success8trials$Success3), as.character(Success8trials$Success3)=="Yes", 1)
Success8trials$Success4<- replace(as.character(Success8trials$Success4), as.character(Success8trials$Success4)=="Yes", 1)
Success8trials$Success5<- replace(as.character(Success8trials$Success5), as.character(Success8trials$Success5)=="Yes", 1)
Success8trials$Success6<- replace(as.character(Success8trials$Success6), as.character(Success8trials$Success6)=="Yes", 1)
Success8trials$Success7<- replace(as.character(Success8trials$Success7), as.character(Success8trials$Success7)=="Yes", 1)
Success8trials$Success.test<- replace(as.character(Success8trials$Success.test), as.character(Success8trials$Success.test)=="Yes", 1)



#We transform to numeric
Success8trials$Success1<-as.numeric(Success8trials$Success1)
Success8trials$Success2<-as.numeric(Success8trials$Success2)
Success8trials$Success3<-as.numeric(Success8trials$Success3)
Success8trials$Success4<-as.numeric(Success8trials$Success4)
Success8trials$Success5<-as.numeric(Success8trials$Success5)
Success8trials$Success6<-as.numeric(Success8trials$Success6)
Success8trials$Success7<-as.numeric(Success8trials$Success7)
Success8trials$Success.test<-as.numeric(Success8trials$Success.test)

str(Success8trials)


Success8trials
n.of.success<-rowSums(Success8trials[3:10])



Success8trials<-cbind(Success8trials,n.of.success)

Success8trials


############Let's add mean time of each trial

PERsugar8trials<-data.frame(last.test.done.bees$ID,
                           last.test.done.bees$Species,
                           last.test.done.bees$PER.sugar1,
                           last.test.done.bees$PER.sugar2,
                           last.test.done.bees$PER.sugar3,
                           last.test.done.bees$PER.sugar4,
                           last.test.done.bees$PER.sugar5,
                           last.test.done.bees$PER.sugar6,
                           last.test.done.bees$PER.sugar7,
                           last.test.done.bees$PER.sugar.test)

PERsugar8trials<-rename(PERsugar8trials, 
                       ID = last.test.done.bees.ID, 
                       Species = last.test.done.bees.Species,
                       PER.sugar1 = last.test.done.bees.PER.sugar1,
                       PER.sugar2 = last.test.done.bees.PER.sugar2,
                       PER.sugar3 = last.test.done.bees.PER.sugar3,
                       PER.sugar4 = last.test.done.bees.PER.sugar4,
                       PER.sugar5 = last.test.done.bees.PER.sugar5,
                       PER.sugar6 = last.test.done.bees.PER.sugar6,
                       PER.sugar7 = last.test.done.bees.PER.sugar7,
                       PER.sugar.test = last.test.done.bees.PER.sugar.test)


PERsugar8trials

aggregate(PER.sugar1 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar2 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar3 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar4 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar5 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar6 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar7 ~ Species, FUN = mean, data = PERsugar8trials)
aggregate(PER.sugar.test ~ Species, FUN = mean, data = PERsugar8trials)


success.mean<-as.data.frame(aggregate(n.of.success ~ Species, FUN = mean, data = Success8trials))
n<-as.data.frame(summary(Success8trials$Species))
Species<-row.names(n)
n<-cbind(Species,n)
success.mean<-merge(success.mean,n)
colnames(success.mean)<-c("Species","mean.of.success","n.of.individuals")
subset(success.mean, subset = (n.of.individuals > 2))


PER.sugar1.mean<-as.data.frame(aggregate(PER.sugar1 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar2.mean<-as.data.frame(aggregate(PER.sugar2 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar3.mean<-as.data.frame(aggregate(PER.sugar3 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar4.mean<-as.data.frame(aggregate(PER.sugar4 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar5.mean<-as.data.frame(aggregate(PER.sugar5 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar6.mean<-as.data.frame(aggregate(PER.sugar6 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar7.mean<-as.data.frame(aggregate(PER.sugar7 ~ Species, FUN = mean, data = PERsugar8trials))
PER.sugar.test.mean<-as.data.frame(aggregate(PER.sugar.test ~ Species, FUN = mean, data = PERsugar8trials))

PERsugar.success.mean<-merge(merge(merge(merge(merge(merge(merge(merge(success.mean, PER.sugar1.mean, all.x = TRUE), PER.sugar2.mean, all.x = TRUE), PER.sugar3.mean, all.x = TRUE), PER.sugar4.mean, all.x = TRUE), PER.sugar5.mean, all.x = TRUE), PER.sugar6.mean, all.x = TRUE), PER.sugar7.mean, all.x = TRUE), PER.sugar.test.mean, all.x = TRUE)
View(PERsugar.success.mean)

#Boxplots PERsugar per species for every trial
par(cex.axis=0.4)

boxplot(PERsugar8trials$PER.sugar1 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 1", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar2 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 2", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar3 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 3", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar4 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 4", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar5 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 5", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar6 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 6", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar7 ~ PERsugar8trials$Species, las = 2, main= "PER sugar trial 7", ylab= "Time", ylim = c(0,170))
boxplot(PERsugar8trials$PER.sugar.test ~ PERsugar8trials$Species, las = 2, main= "PER sugar test", ylab= "Time", ylim = c(0,170))

par(cex.axis=1)


#Dotchart for the means of the time until PER sugar for each species
par(mfrow = c(3,4))

for (n in 1:nrow(PERsugar.success.mean)) {
plot(t(PERsugar.success.mean[n,(4:11)]), xlab="Trial number", ylab = "Time", main = (PERsugar.success.mean$Species[n]))
lines(t(PERsugar.success.mean[n,(4:11)]))
}

par(mfrow = c(1,1))


##################



##Time PER sugar - Time PER water----

View(Beeh.data)

par(mfrow=c(2,2))
plot((Beeh.data$PER.sugar1 - Beeh.data$PER.water1), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 1", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)

plot((Beeh.data$PER.sugar2 - Beeh.data$PER.water2), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 2", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)

plot((Beeh.data$PER.sugar3 - Beeh.data$PER.water3), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 3", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)


plot(abs(Beeh.data$PER.sugar4 - Beeh.data$PER.water4), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 4", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)


plot(abs(Beeh.data$PER.sugar5 - Beeh.data$PER.water5), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 5", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)


plot(abs(Beeh.data$PER.sugar6 - Beeh.data$PER.water6), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 6", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)

plot(abs(Beeh.data$PER.sugar7 - Beeh.data$PER.water7), row.names(Beeh.data), xlab="Time PER sugar minus time water PER", ylab="Individual", main = "Trial 7", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)

plot(abs(Beeh.data$PER.sugar.test - Beeh.data$PER.water.test), row.names(Beeh.data), ylab="Individual", xlab="Time PER sugar minus time water PER", main = "Test", ylim = c(0,nrow(Beeh.data)),xlim=c(-150,175))
abline(v = 120)
abline(v = -120)

par(mfrow=c(1,1))


#melt----
#Let's transform data.frames to proper data form to extract slopes and do models
Beeh.PER.sugar
melt.Beeh.PER.sugar<-melt(Beeh.PER.sugar)
colnames(melt.Beeh.PER.sugar)<-c("ID","Species","Trial","Time")

melt.Beeh.PER.sugar<-melt.Beeh.PER.sugar[order(melt.Beeh.PER.sugar$ID),]

temp.melt.sugar<-replace(melt.Beeh.PER.sugar, c("PER.sugar1","PER.sugar2","PER.sugar3","PER.sugar4","PER.sugar5","PER.sugar6","PER.sugar.test"), c(1,2,3,4,5,6,7,8))
melt.Beeh.PER.sugar$Trial<-temp.melt.sugar$PER.sugar1

melt.Beeh.PER.sugar

#PHYLOGENETIC TREES----
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
install.views('Phylogenetics')
update.views('Phylogenetics')

#Librería para leer y modificar árboles filogenéticos
library(ape)

#LOAD IN YOUR TREE, by Hedtke et al 2013
getwd()
apoidea<-read.tree(file = "data/phylogeny_genus_level.txt")

#We have 10 possible trees
str(apoidea)
apoidea[[1]]
t1<-apoidea[[1]]
t2<-apoidea[[2]]
t3<-apoidea[[3]]
t4<-apoidea[[4]]
t5<-apoidea[[5]]
t6<-apoidea[[6]]
t7<-apoidea[[7]]
t8<-apoidea[[8]]
t9<-apoidea[[9]]
t10<-apoidea[[10]]

str(t1)
plot(t1)
t1$tip.label




# We drop tips except "Andrena", "Anthophora", "Apis", "Bombus", "Lasioglossum","Megachile", "Eucera", "Osmia", "Panurgus", "Rhodanthidium", "Xylocopa"
# We don't have Flavipanurgus, Psithyrus
tree1<-drop.tip(t1, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola"))

tree1$tip.label

tree2<-drop.tip(t2, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree2$tip.label

tree3<-drop.tip(t3, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree3$tip.label

tree4<-drop.tip(t4, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree4$tip.label

tree5<-drop.tip(t5, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree5$tip.label

tree6<-drop.tip(t6, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree6$tip.label


tree7<-drop.tip(t7, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree7$tip.label


tree8<-drop.tip(t8, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))


tree8$tip.label

tree9<-drop.tip(t9, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                            "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                            "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                            "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                            "Ctenocolletes", "Alocandrena", "Megandrena",      
                            "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                            "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                            "Perdita", "Clavipanurgus", "Panurginus",        
                            "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                            "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                            "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                            "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                            "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                            "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                            "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                            "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                            "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                            "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                            "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                            "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                            "Sphecodopsis", "Pasites", "Oreopasites",     
                            "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                            "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                            "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                            "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                            "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                            "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                            "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                            "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                            "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                            "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                            "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                            "Peponapis", "Xenoglossa", "Tetraloniella",   
                            "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                            "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                            "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                            "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                            "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                            "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                            "Aglae", "Eulaema", "Eufriesea",            
                            "Tetragonilla", "Tetragonula", "Platytrigona",    
                            "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                            "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                            "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                            "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                            "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                            "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                            "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                            "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                            "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                            "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                            "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                            "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                            "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                            "Ctenoplectra", "Macrogalea", "Allodapula",      
                            "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                            "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                            "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                            "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                            "Dioxys", "Noteriades", "Radoszkowskiana", 
                            "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                            "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                            "Haetosmia", "Wainia", "Hoplosmia",           
                            "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                            "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                            "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                            "Dianthidium", "Anthidiellum", "Paranthidium",  
                            "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                            "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                            "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                            "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                            "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                            "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                            "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                            "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                            "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                            "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                            "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                            "Thrincohalictus", "Halictus", "Homalictus",   
                            "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                            "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                            "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                            "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                            "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                            "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                            "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                            "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                            "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                            "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                            "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                            "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                            "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                            "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                            "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                            "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                            "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                            "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"     
                            ))

tree9$tip.label

tree10<-drop.tip(t10, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                              "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                              "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                              "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                              "Ctenocolletes", "Alocandrena", "Megandrena",      
                              "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                              "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                              "Perdita", "Clavipanurgus", "Panurginus",        
                              "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                              "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                              "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                              "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                              "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                              "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                              "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                              "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                              "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                              "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                              "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                              "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                              "Sphecodopsis", "Pasites", "Oreopasites",     
                              "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                              "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                              "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                              "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                              "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                              "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                              "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                              "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                              "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                              "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                              "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                              "Peponapis", "Xenoglossa", "Tetraloniella",   
                              "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                              "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                              "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                              "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                              "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                              "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                              "Aglae", "Eulaema", "Eufriesea",            
                              "Tetragonilla", "Tetragonula", "Platytrigona",    
                              "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                              "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                              "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                              "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                              "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                              "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                              "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                              "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                              "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                              "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                              "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                              "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                              "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                              "Ctenoplectra", "Macrogalea", "Allodapula",      
                              "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                              "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                              "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                              "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                              "Dioxys", "Noteriades", "Radoszkowskiana", 
                              "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                              "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                              "Haetosmia", "Wainia", "Hoplosmia",           
                              "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                              "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                              "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                              "Dianthidium", "Anthidiellum", "Paranthidium",  
                              "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                              "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                              "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                              "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                              "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                              "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                              "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                              "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                              "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                              "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                              "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                              "Thrincohalictus", "Halictus", "Homalictus",   
                              "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                              "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                              "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                              "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                              "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                              "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                              "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                              "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                              "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                              "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                              "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                              "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                              "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                              "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola",
                              "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                              "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                              "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                              "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides"))

tree10$tip.label

#Are the trees the same, except tree1, use any other
str(tree1)

par(mfrow=c(1,1))
plot(tree1, main = "Tree 1")

plot(tree2, main = "Tree 2")
plot(tree3, main = "Tree 3")
plot(tree4, main = "Tree 4")
plot(tree5, main = "Tree 5")
plot(tree6, main = "Tree 6")
plot(tree7, main = "Tree 7")
plot(tree8, main = "Tree 8")
plot(tree9, main = "Tree 9")
plot(tree10, main = "Tree 10")




#Queen brain----
which(Bombus.terrestris$Sex == "Queen")
Queen.bombus<-Bombus.terrestris[which(Bombus.terrestris$Sex == "Queen"),]
Queen.bombus$Brain.weight
which(is.na(Bombus.terrestris$Brain.weight))
mean.bombus.brain<-mean(Bombus.terrestris.woqueen[-which(is.na(Bombus.terrestris.woqueen$Brain.weight)),]$Brain.weight)
Bombus.terrestris.woqueen$IT..mm.
mean.bombus.IT<-mean(Bombus.terrestris.woqueen$IT..mm.)
#IT comparison
mean.bombus.IT
Queen.bombus$IT..mm.
#Brain weight comparison 
mean.bombus.brain
Queen.bombus$Brain.weight
#Here we have an interesting comparison between workers and males and queens,
par(mfrow=c(1,2))
boxplot(Bombus.terrestris.woqueen[-which(is.na(Bombus.terrestris.woqueen$Brain.weight)),]$Brain.weight,Queen.bombus$Brain.weight, names=c("Males and Workers", "Queen"), ylab="Brain weight", main = "Brain weight comparison \nBombus terrestris")
boxplot(Bombus.terrestris.woqueen$IT..mm.,Queen.bombus$IT..mm., ylab="Intertegular distance", names=c("Males and Workers", "Queen"), main = "Intertegular distance comparison \nBombus terrestris")
par(mfrow=c(1,1))
