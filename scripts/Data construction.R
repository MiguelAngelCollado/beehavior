library(reshape2)
library(dplyr)
library(visreg)
library(phytools)
library(DHARMa)
library(effects)
library(lme4)
library(ggplot2)
library(reshape2)
library(rsq)
library(optiRum)
library(survival)
require(MuMIn)
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
library(ape)
library(stringi)
library("sjstats")
#if (!requireNamespace("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")
#BiocManager::install("ggtree", version = "3.8")
library(ggtree)



Beeh.data<-read.csv("data/Behavior comparison.csv")
nrow(Beeh.data)

#DATA CONSTRUCTION--------

#If we don't have the correct cue, we can't use that bee
Beeh.data<-Beeh.data[-which(Beeh.data$Correct.cue == ""),]
#If we don't have the genus, we can't use that bee
Beeh.data<-Beeh.data[-which(is.na(Beeh.data$Genus)),]
#Check
which(Beeh.data$Correct.cue == "")
which(is.na(Beeh.data$Genus))

Beeh.data<-Beeh.data[-which(Beeh.data$ID == "D41"),]





Lasioglossum.malachurum<-subset(Beeh.data, subset = (Beeh.data$Species == "Lasioglossum malachurum"))
#D139 has the bigger brain of all Lasioglossum malachurum, we could remove it
#D139 has outlier residuals
boxplot(Lasioglossum.malachurum$Brain.weight/Lasioglossum.malachurum$IT..mm.)
boxplot(Lasioglossum.malachurum$Brain.weight)
boxplot(Lasioglossum.malachurum$IT..mm.)

boxplot(Lasioglossum.malachurum)

#Beeh.data<-Beeh.data[-which(Beeh.data$ID == "D139"),]


#The number of individuals identified is
nrow(Beeh.data)


#Psithyrus is actually a Bombus
Beeh.data$Genus<-replace(Beeh.data$Genus, Beeh.data$Genus == "Psithyrus", "Bombus")

#We don't have flavipanurgus as a genus in the phylo tree, 
#so we replace it with panurgus
Beeh.data$Genus<-replace(Beeh.data$Genus, Beeh.data$Genus == "Flavipanurgus", "Panurgus")


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

Beeh.PER.sugar
Beeh.PER.water
Beeh.success
Beeh.water.exploring
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


nrow(useless.bees)


useless.bees.export<-data.frame(useless.bees$Genus,
useless.bees$Species,
useless.bees$Notes)
colnames(useless.bees.export)<-c("Genus","Species","Notes")

useless.bees.export

setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/figures")
#write.csv(useless.bees.export, "list of useless individuals.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")


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

#QUESTION: we count NA's as NO-----
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
Success8trials$Species<-droplevels(Success8trials$Species)
#Success8trials<-as.factor(Success8trials)

str(Success8trials)
Success8trials$Success.test<-as.factor(Success8trials$Success.test)
Success8trials$Success1<-as.factor(Success8trials$Success1)
Success8trials$Success2<-as.factor(Success8trials$Success2)
Success8trials$Success3<-as.factor(Success8trials$Success3)
Success8trials$Success4<-as.factor(Success8trials$Success4)
Success8trials$Success5<-as.factor(Success8trials$Success5)
Success8trials$Success6<-as.factor(Success8trials$Success6)
Success8trials$Success7<-as.factor(Success8trials$Success7)
str(Success8trials)

brainandIT<-data.frame(Beeh.data$ID,
Beeh.data$Brain.weight,
Beeh.data$IT..mm.)
colnames(brainandIT)<-c("ID","Brain.weight","IT..mm.")
brainandIT$brain.IT<-(brainandIT$Brain.weight/brainandIT$IT..mm.)


Success8trials<-merge(Success8trials, brainandIT)

#There some species that did not showed interest for more than 3 times, we filter
#Osmia caerulescens, Panurgus dargius, Eucera elongatula, Andrena labialis

Success8trials<-Success8trials[-c(as.numeric(rownames(subset(Success8trials, subset = (Success8trials$Species == "Osmia caerulescens")))),
                     as.numeric(rownames(subset(Success8trials, subset = (Success8trials$Species == "Panurgus dargius")))),
                     as.numeric(rownames(subset(Success8trials, subset = (Success8trials$Species == "Eucera elongatula")))),
                     as.numeric(rownames(subset(Success8trials, subset = (Success8trials$Species == "Andrena labialis"))))
),]


Genus<-data.frame(Beeh.data$ID,
Beeh.data$Genus)
colnames(Genus)<-(c("ID","Genus"))
Success8trials<-merge(Success8trials,Genus)
dev.off()

#We filter NA in brain/IT
Success8trials
Success8trials.ITf<-Success8trials[-which(is.na(Success8trials$brain.IT)),]


plot(Success.test ~ brain.IT, data = Success8trials.ITf, main="Success related to brain size")

#Create new variable for graphs only
temp<-as.numeric(Success8trials.ITf$Success.test)
temp<-replace(temp, temp == 1, 0)
temp<-replace(temp, temp == 2, 1)
Success8trials.ITf$Success.test.as.numeric<-temp

#We extract RESIDUALS from Brain ~ IT models
par(mfrow=c(2,2))
plot(Brain.weight ~ IT..mm.,data = Success8trials.ITf, xlab="Body Size")
abline(lm(Brain.weight ~ IT..mm.,data = Success8trials.ITf), col="pink")
plot(log(Brain.weight) ~ log(IT..mm.),data = Success8trials.ITf, xlab="log(Body Size)")
abline(lm(log(Brain.weight) ~ log(IT..mm.),data = Success8trials.ITf), col="pink")
plot(log(Brain.weight) ~ IT..mm.,data = Success8trials.ITf, xlab="Body Size", ylab = "Log(Brain)")
abline(lm(log(Brain.weight) ~ IT..mm.,data = Success8trials.ITf), col="pink")
par(mfrow=c(1,1))


summary(lm(Brain.weight ~ IT..mm.,data = Success8trials.ITf))
summary(lm(log(Brain.weight) ~ log(IT..mm.),data = Success8trials.ITf))
summary(lm(log(Brain.weight) ~ IT..mm.,data = Success8trials.ITf))

#The best model is the log-transformed
BIT<-(lm(log(Brain.weight) ~ log(IT..mm.), data = Success8trials.ITf))
summary(BIT)
BIT$residuals



##residuals by species
dev.off()
plot(log(Brain.weight) ~ log(IT..mm.),data = Success8trials.ITf, xlab="log(Body Size)")
abline(lm(log(Brain.weight) ~ log(IT..mm.),data = Success8trials.ITf), col="Darkgreen")

AAAAAXXX<-lmer(log(Brain.weight) ~ log(IT..mm.) + (1|Species), data = Success8trials.ITf)
AAAAAXXX@beta
abline(AAAAAXXX@beta, col="purple")
summary(AAAAAXXX)


AAAAAYYY<-lmer(log(Brain.weight) ~ log(IT..mm.) + (1|Genus), data = Success8trials.ITf)
abline(AAAAAYYY@beta, col="red")



#We add the residuals to our dataframe
Success8trials.ITf$residuals<-BIT$residuals
plot(Success8trials.ITf$Success.test ~ Success8trials.ITf$residuals)

#We add censored data, and time until PER
PER.sugar.test.censored<-replace(Beeh.data$PER.sugar.test, is.na(Beeh.data$PER.sugar.test), 121)

PER.merge<-data.frame(Beeh.data$ID,
                      Beeh.data$PER.sugar.test,
                      PER.sugar.test.censored)
colnames(PER.merge)<-c("ID","PER.sugar.test","PER.sugar.test.censored")

Success8trials.ITf<-merge(Success8trials.ITf, PER.merge)

#Cox analysis need results as logical, we add a logical variable for success

Success8trials.ITf$PER.sugar.test.censored

success.test.logi<-vector()
n=1
for (n in 1:(nrow(Success8trials.ITf))) {
  if (Success8trials.ITf$Success.test[n] == 1) {
    success.test.logi[n]<-TRUE    
  }else{
    success.test.logi[n]<-FALSE
  }
}
is.logical(success.test.logi)

Success8trials.ITf$success.test.logi<-success.test.logi


Success7trials.ITf<-Success8trials.ITf
Success7trials.ITf<-Success7trials.ITf[,-(which(colnames(Success7trials.ITf)=="Success1"))]

Success7trials.ITf$n.of.success


Success7trials.ITf$n.of.success<-(as.numeric(Success7trials.ITf$Success2)-1) + 
  (as.numeric(Success7trials.ITf$Success3)-1) + 
  (as.numeric(Success7trials.ITf$Success4)-1) + 
  (as.numeric(Success7trials.ITf$Success5)-1) + 
  (as.numeric(Success7trials.ITf$Success6)-1) + 
  (as.numeric(Success7trials.ITf$Success7)-1) + 
  (as.numeric(Success7trials.ITf$Success.test)-1)

#MAIN ANALYSIS-----
#Success 8 block----
#Success8 ~ brain/IT----

#plot
plot(Success.test.as.numeric ~ brain.IT, data = Success8trials.ITf, main="Success related to brain size", xlab="Encephalization (Brain/IT)", ylab = "Success learning test")
xweight <- seq(0, 2, 0.01)
fit <- glm(Success.test ~ brain.IT, family = binomial, data = Success8trials.ITf)
yweight <- predict(fit, list(brain.IT = xweight), type="response")
lines(xweight, yweight)


lm.succ.brain.it<-lm(as.numeric(Success.test) ~ brain.IT, data = Success8trials.ITf)
summary(lm.succ.brain.it)

succ.brain.it<-glm(Success.test ~ brain.IT, data = Success8trials.ITf, family = binomial)
summary(succ.brain.it)
allEffects(succ.brain.it)
#Add random factor Species, still significant
succ.brain.itr<-glmer(Success.test ~ brain.IT + (1|Species), data = Success8trials.ITf, family = binomial)
summary(succ.brain.itr)

#Add nested random factor Genus/Species, still significant

#Percentage of success

length(which(Success8trials.ITf$Success.test==1))
length(which(Success8trials.ITf$Success.test==0))

(length(which(Success8trials.ITf$Success.test==1))/(length(which(Success8trials.ITf$Success.test==1))+length(which(Success8trials.ITf$Success.test==0))))*100


#Linearizing?
Success8trials.ITf$log.brain.it<-log(Success8trials.ITf$Brain.weight)/log(Success8trials.ITf$IT..mm.)

succ.brain.itrg.log<-glmer(Success.test ~ log.brain.it + (1|Genus/Species), data = Success8trials.ITf, family = binomial)
summary(succ.brain.itrg.log)




#result
succ.brain.itrg<-glmer(Success.test ~ brain.IT + (1|Genus/Species), data = Success8trials.ITf, family = binomial)
allEffects(succ.brain.itrg)
summary(succ.brain.itrg)


#Success8 ~ brain/IT residuals----


#plot
plot(Success.test.as.numeric ~ residuals, data = Success8trials.ITf, main= "Learning success related to IT/Brain residuals", ylab="No success / Success", xlab= "IT/Brain residuals")
min(Success8trials.ITf$residuals)
max(Success8trials.ITf$residuals)
xweight <- seq(-1, 2, 0.01)
fit <- glm(Success.test ~ residuals, family = binomial, data = Success8trials.ITf)
yweight <- predict(fit, list(residuals = xweight), type="response")
lines(xweight, yweight)


summary(glm(as.numeric(Success8trials.ITf$Success.test) ~ BIT$residuals))

#Controlar por especie

succ8.res<-glm(Success.test ~ residuals, data = Success8trials.ITf, family = binomial)
summary(succ8.res)

succ8.ress<-glmer(Success.test ~ residuals + (1|Species), data = Success8trials.ITf, family = binomial)
summary(succ8.ress)


#result
succ8.ressg<-glmer(Success.test ~ residuals + (1|Genus/Species), data = Success8trials.ITf, family = binomial)
summary(succ8.ressg)


#Slope differences for success/no success----
#Let's see tendency lines for success and no success
plot(Brain.weight ~ Success.test, data = Success8trials.ITf, notch = TRUE)

ggplot(Success8trials.ITf, aes(x=IT..mm., y=Brain.weight, color=Success.test)) +
  geom_point() 

ggplot(Success8trials.ITf, aes(x=IT..mm., y=Brain.weight, color=Success.test)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Success.test), se = FALSE)

ggplot(Success8trials.ITf, aes(x=IT..mm., y=Brain.weight, color=Success.test)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Success.test))

#Linearlized with log
ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=Success.test)) +
  geom_point() 

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=Success.test)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Success.test), se = FALSE)

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=Success.test)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Success.test))
  

brain.IT.succ.sp<-lmer(log(Brain.weight) ~ log(IT..mm.) * Success.test + (1|Species), data = Success8trials.ITf)
summary(brain.IT.succ.sp)

#Slopes
succ81<-subset(Success8trials.ITf, subset = (Success8trials.ITf$Success.test == "1"))
succ80<-subset(Success8trials.ITf, subset = (Success8trials.ITf$Success.test == "0"))

succ81.lm<-lm(Brain.weight ~ IT..mm., data = succ81)
succ80.lm<-lm(Brain.weight ~ IT..mm., data = succ80)

#Slope for the model with 
#Slope for success
succ81.lm$coefficients[2]
#Slope for no success
succ80.lm$coefficients[2]


#Only IT.mm. is significative, logically
brain.IT.succ.sp<-lmer(log(Brain.weight) ~ log(IT..mm.) * Success.test + (1|Genus/Species), data = Success8trials.ITf)
summary(brain.IT.succ.sp)









#Success8 ~ Absolute brain size--------
plot(Success.test.as.numeric ~ Brain.weight, data = Success8trials.ITf, main="Success related to brain size", xlab="Absolute brain size", ylab = "Success learning test")
xweight <- seq(0, 7, 0.05)
fit <- glm(Success.test ~ Brain.weight, family = binomial, data = Success8trials.ITf)
yweight <- predict(fit, list(Brain.weight = xweight), type="response")
lines(xweight, yweight)


succ.globalbrain<-glmer(Success.test ~  Brain.weight + (1|Genus/Species), data = Success8trials.ITf, family = binomial)
summary(succ.globalbrain)

allEffects(succ.globalbrain)


brm.succ8brains








#n.of.success block-----
#n.of.success ~ brain/IT-----
hist(Success8trials.ITf$n.of.success)
plot(n.of.success~brain.IT,data = Success8trials.ITf)
abline(lm(n.of.success~brain.IT,data = Success8trials.ITf), col = "purple")

n.succ.brainit.lm<-lm(n.of.success~brain.IT,data = Success8trials.ITf)
summary(n.succ.brainit.lm)
#control by species and genus
n.succ.brainit.lm.sp<-lmer(n.of.success~brain.IT + (1|Species),data = Success8trials.ITf)
summary(n.succ.brainit.lm.sp)

glm(brain.IT~n.of.success, data = Success8trials.ITf)
allEffects(glm(brain.IT~n.of.success, data = Success8trials.ITf))


#Brain.IT is significative related to n.of.success
n.succ.brainit.lm.g<-lmer(n.of.success~brain.IT + (1|Genus/Species),data = Success8trials.ITf)
summary(n.succ.brainit.lm.g)


#n.of.success ~ residuals------
plot(n.of.success~residuals,data = Success8trials.ITf, main= "Number of learning success compared with residuals", xlab="IT/Brain residuals", ylab="Number of success")
abline(lm(n.of.success~residuals,data = Success8trials.ITf), col = "purple")


n.succ.res.lm<-lm(n.of.success~residuals,data = Success8trials.ITf)
summary(n.succ.res.lm)

#No effects
n.succ.res.lmer<-lmer(n.of.success~residuals + (1|Genus/Species),data = Success8trials.ITf)
summary(n.succ.res.lmer)



#n.of.success ~ absolute brain size-----
plot(n.of.success~Brain.weight,data = Success8trials.ITf)
abline(lm(n.of.success~Brain.weight,data = Success8trials.ITf), col = "purple")

n.succ.brain.lm<-lm(n.of.success~Brain.weight,data = Success8trials.ITf)
summary(n.succ.brain.lm)
#control by species and genus
n.succ.brain.lm.sp<-lmer(n.of.success~Brain.weight + (1|Species),data = Success8trials.ITf)
summary(n.succ.brain.lm.sp)

glm(Brain.weight~n.of.success, data = Success8trials.ITf)
allEffects(glm(Brain.weight~n.of.success, data = Success8trials.ITf))


#Brain.IT is significative related to n.of.success
n.succ.brain.lm.g<-lmer(n.of.success~Brain.weight + (1|Genus/Species),data = Success8trials.ITf)
summary(n.succ.brain.lm.g)



#Slope differences for number of success----

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=as.factor(n.of.success))) +
  geom_point() 


ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=as.factor(n.of.success))) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=as.factor(n.of.success)), se = FALSE)

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=as.factor(n.of.success))) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=as.factor(n.of.success)))


brain.IT.succ.sp<-lmer(log(Brain.weight) ~ log(IT..mm.) * n.of.success + (1|Species), data = Success8trials.ITf)
summary(brain.IT.succ.sp)

#effects only for IT.mm
brain.IT.succ.spg<-lmer(log(Brain.weight) ~ log(IT..mm.) * n.of.success + (1|Genus/Species), data = Success8trials.ITf)
summary(brain.IT.succ.spg)







#n.of.success (starting in trial 2) block----
#n.of.success (starting in trial 2) ~ brain/IT-----
hist(Success7trials.ITf$n.of.success)
plot(n.of.success~brain.IT,data = Success8trials.ITf)
abline(lm(n.of.success~brain.IT,data = Success7trials.ITf), col = "purple")

n.succ7.brainit.lm<-lm(n.of.success~brain.IT,data = Success7trials.ITf)
summary(n.succ7.brainit.lm)
#control by species and genus
n.succ7.brainit.lm.sp<-lmer(n.of.success~brain.IT + (1|Species),data = Success7trials.ITf)
summary(n.succ7.brainit.lm.sp)

glm(brain.IT~n.of.success, data = Success7trials.ITf)
allEffects(glm(brain.IT~n.of.success, data = Success7trials.ITf))


#Brain.IT is significative related to n.of.success
n.succ7.brainit.lm.g<-lmer(n.of.success~brain.IT + (1|Genus/Species),data = Success7trials.ITf)
summary(n.succ7.brainit.lm.g)


#n.of.success (starting in trial 2)~ residuals------
plot(n.of.success~residuals,data = Success7trials.ITf, main= "Number of learning success compared with residuals", xlab="IT/Brain residuals", ylab="Number of success")
abline(lm(n.of.success~residuals,data = Success7trials.ITf), col = "purple")


n.succ7.res.lm<-lm(n.of.success~residuals,data = Success7trials.ITf)
summary(n.succ.res.lm)

#light effects
n.succ7.res.lmer<-lmer(n.of.success~residuals + (1|Genus/Species),data = Success7trials.ITf)
summary(n.succ7.res.lmer)


#n.of.success (starting in trial 2) ~ absolute brain size-----
plot(n.of.success~Brain.weight,data = Success7trials.ITf)
abline(lm(n.of.success~Brain.weight,data = Success7trials.ITf), col = "purple")

n.succ7.brain.lm<-lm(n.of.success~Brain.weight,data = Success7trials.ITf)
summary(n.succ7.brain.lm)
#control by species and genus
n.succ7.brain.lm.sp<-lmer(n.of.success~Brain.weight + (1|Species),data = Success7trials.ITf)
summary(n.succ7.brain.lm.sp)

glm(Brain.weight~n.of.success, data = Success7trials.ITf)
allEffects(glm(Brain.weight~n.of.success, data = Success7trials.ITf))


#Brain.IT is significative related to n.of.success
n.succ7.brain.lm.g<-lmer(n.of.success~Brain.weight + (1|Genus/Species),data = Success7trials.ITf)
summary(n.succ7.brain.lm.g)

#per.sugar.test.block----

#Only count succeeders as PER.sugar.test
Success.only<-subset(Success8trials.ITf, subset = (Success8trials.ITf$Success.test == 1))


#PER.sugar.test ~ Brain.IT (only succeeders)----


plot(PER.sugar.test ~ brain.IT,data = Success.only, main= "Time until success ~ Brain.IT", ylim=c(0,120))
abline(lm(PER.sugar.test ~ brain.IT,data = Success.only), col = "purple")

PERsugar.lm.s<-lm(PER.sugar.test ~ brain.IT,data = Success.only)
summary(PERsugar.lm.s)


PERsugar.lmer.s<-lmer(PER.sugar.test ~ brain.IT + (1|Genus/Species),data = Success.only)
summary(PERsugar.lmer.s)
#Possion distribution
PERsugarc.glmer.sp<-glmer(PER.sugar.test ~ brain.IT + (1|Genus/Species),family = poisson,data = Success.only)
summary(PERsugarc.glmer.sp)




#PER.sugar.test ~ Brain.IT----


#Acumulation in low time until success
par(mfrow=c(2,1))
plot(PER.sugar.test ~ brain.IT,data = Success8trials.ITf, main= "Time until success ~ Brain.IT", ylim=c(0,120))
abline(lm(PER.sugar.test ~ brain.IT,data = Success8trials.ITf), col = "purple")
plot(PER.sugar.test.censored ~ brain.IT,data = Success8trials.ITf, main= "Censored time until success ~ Brain.IT", ylim=c(0,120))
abline(lm(PER.sugar.test.censored ~ brain.IT,data = Success8trials.ITf), col = "purple")
par(mfrow=c(1,1))

PERsugar.lm<-lm(PER.sugar.test ~ brain.IT,data = Success8trials.ITf)
summary(PERsugar.lm)

#There is some effect, yay


PERsugarc.lmer<-lmer(PER.sugar.test.censored ~ brain.IT + (1|Genus/Species),data = Success8trials.ITf)
summary(PERsugarc.lmer)
#Possion distribution?
PERsugarc.glmer<-glmer(PER.sugar.test.censored ~ brain.IT + (1|Genus/Species),family = poisson,data = Success8trials.ITf)
summary(PERsugarc.glmer)


##survival curves


cox.cue.time<- coxph(Surv(PER.sugar.test.censored, success.test.logi) ~ brain.IT, na.action = na.exclude, data = Success8trials.ITf) 
cox.cue.time



PERsugar.lmer<-lmer(PER.sugar.test ~ brain.IT + (1|Genus/Species),data = Success8trials.ITf)
summary(PERsugar.lmer)



#Per.sugar.test ~ residuals (only succeeders)----

plot(PER.sugar.test ~ residuals, data=Success.only, main="Time until learning success ~ residuals", xlab="Brain/IT residuals", ylab="Time until learning success")
abline(lm(PER.sugartest ~ residuals, data=Success8trials.PER), col="purple")

PERsugar.res.lm<-lm(PER.sugar.test ~ residuals,data=Success.only)
summary(PERsugar.res.lm)




PERsugar.res.glmer.s<-glmer(PER.sugar.test ~ residuals + (1|Genus/Species), family = poisson, data=Success.only)
summary(PERsugar.res.glmer.s)


#Per.sugar.test ~ residuals----

plot(PER.sugartest ~ residuals, data=Success8trials.PER, main="Time until learning success ~ residuals", xlab="Brain/IT residuals", ylab="Time until learning success")
abline(lm(PER.sugartest ~ residuals, data=Success8trials.PER), col="purple")

PERsugar.res.lm<-lm(PER.sugartest ~ residuals,data=Success8trials.PER)
summary(PERsugar.res.lm)

plot(PER.sugar.test.censored ~ residuals, data = Success8trials.ITf)
abline(lm(PER.sugar.test.censored ~ residuals, data = Success8trials.ITf), col="purple")

#survival curves
cox.cue.time2<- coxph(Surv(PER.sugar.test.censored, success.test.logi) ~ residuals, na.action = na.exclude, data = Success8trials.ITf) 
cox.cue.time2


PERsugar.res.glmer<-glmer(PER.sugar.test ~ residuals + (1|Genus/Species), family = poisson, data=Success8trials.ITf)
summary(PERsugar.res.glmer)

#Poisson distribution?
PERsugar.res.glmer<-glmer(PER.sugar.test ~ residuals + (1|Genus/Species), family = poisson, data=Success8trials.ITf)
#Terrible
summary(PERsugar.res.glmer)


PERsugar.c.res.glmer<-glmer(PER.sugar.test.censored ~ residuals + (1|Genus/Species), family = poisson, data=Success8trials.ITf)
#Terrible
summary(PERsugar.c.res.glmer)



PERsugar.res.lmer<-lmer(PER.sugar.test ~ residuals + (1|Genus/Species), data=Success8trials.ITf)
#Terrible
summary(PERsugar.res.lmer)


#PER.sugar.test absolute brain size (only succeeders)-------


plot(PER.sugar.test ~ Brain.weight,data = Success.only, main= "Time until success ~ Brain.IT", ylim=c(0,120))
abline(lm(PER.sugar.test ~ Brain.weight,data = Success.only), col = "purple")

PERsugar.lm.abs<-lm(PER.sugar.test ~ Brain.weight,data = Success.only)
summary(PERsugar.lm.abs)


PERsugar.lmer.abs<-lmer(PER.sugar.test ~ Brain.weight + (1|Genus/Species),data = Success.only)
summary(PERsugar.lmer.abs)
#Possion distribution
PERsugarc.glmer.absp<-glmer(PER.sugar.test ~ Brain.weight + (1|Genus/Species),family = poisson,data = Success.only)
summary(PERsugarc.glmer.absp)


#PER.sugar.test absolute brain size-------

#Acumulation in low time until success
par(mfrow=c(2,1))
plot(PER.sugar.test ~ Brain.weight,data = Success8trials.ITf, main= "Time until success ~ Brain.weight", ylim=c(0,120))
abline(lm(PER.sugar.test ~ Brain.weight,data = Success8trials.ITf), col = "purple")
plot(PER.sugar.test.censored ~ Brain.weight,data = Success8trials.ITf, main= "Censored time until success ~ Brain.weight", ylim=c(0,120))
abline(lm(PER.sugar.test.censored ~ Brain.weight,data = Success8trials.ITf), col = "purple")
par(mfrow=c(1,1))

PERsugar.lm.abs<-lm(PER.sugar.test ~ Brain.weight,data = Success8trials.ITf)
summary(PERsugar.lm.abs)



PERsugarc.lmer.abs<-lmer(PER.sugar.test.censored ~ Brain.weight + (1|Genus/Species),data = Success8trials.ITf)
summary(PERsugarc.lmer.abs)
#Possion distribution?
PERsugarc.glmer.abs<-glmer(PER.sugar.test.censored ~ Brain.weight + (1|Genus/Species),family = poisson,data = Success8trials.ITf)
summary(PERsugarc.glmer.abs)


##survival curves


cox.cue.time.abs<- coxph(Surv(PER.sugar.test.censored, success.test.logi) ~ Brain.weight, na.action = na.exclude, data = Success8trials.ITf) 
cox.cue.time.abs






#Slope differences for Per.sugar.test----

#Don't know how to plot that
ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=PER.sugar.test)) +
  geom_point() 

per.sugar.lmer.slopes<-lmer(Brain.weight ~ IT..mm. * PER.sugar.test + (1|Genus/Species), data=Success8trials.ITf)
summary(per.sugar.lmer.slopes)



#individual.slopes.block---- 
#Individual.slopes ~ brain.IT----
Success8trials.ITf

PER<-data.frame(Beeh.data$ID,
Beeh.data$PER.sugar1,
Beeh.data$PER.sugar2,
Beeh.data$PER.sugar3,
Beeh.data$PER.sugar4,
Beeh.data$PER.sugar5,
Beeh.data$PER.sugar6,
Beeh.data$PER.sugar7,
Beeh.data$PER.sugar.test)

colnames(PER)<-c("ID", "PER.sugar1", "PER.sugar2", "PER.sugar3", "PER.sugar4"
                 , "PER.sugar5", "PER.sugar6", "PER.sugar7", "PER.sugartest")

Success8trials.PER<-merge(Success8trials.ITf, PER)



touch.sugar1<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar1)[n]){
    touch.sugar1[n]<-0  
  }else{
    touch.sugar1[n]<-1  
  }}

touch.sugar2<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar2)[n]){
    touch.sugar2[n]<-0  
  }else{
    touch.sugar2[n]<-1  
  }}

touch.sugar3<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar3)[n]){
    touch.sugar3[n]<-0  
  }else{
    touch.sugar3[n]<-1  
  }}

touch.sugar4<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar4)[n]){
    touch.sugar4[n]<-0  
  }else{
    touch.sugar4[n]<-1  
  }}

touch.sugar5<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar5)[n]){
    touch.sugar5[n]<-0  
  }else{
    touch.sugar5[n]<-1  
  }}

touch.sugar6<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar6)[n]){
    touch.sugar6[n]<-0  
  }else{
    touch.sugar6[n]<-1  
  }}

touch.sugar7<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar7)[n]){
    touch.sugar7[n]<-0  
  }else{
    touch.sugar7[n]<-1  
  }}

touch.sugar.test<-NULL
for (n in 1:nrow(Success8trials.PER)) {
  if(is.na(Success8trials.PER$PER.sugar.test)[n]){
    touch.sugar.test[n]<-0  
  }else{
    touch.sugar.test[n]<-1  
  }}

PERactivityevents<-(touch.sugar1 + touch.sugar2 + touch.sugar3 + touch.sugar4 + 
                      touch.sugar5 + touch.sugar6 + touch.sugar7 + touch.sugar.test)
Success8trials.PER$PERactivityevents<-(touch.sugar1 + touch.sugar2 + touch.sugar3 + touch.sugar4 + 
                                         touch.sugar5 + touch.sugar6 + touch.sugar7 + touch.sugar.test)

#We filter for having done PER in more than 3 events 
Success8trials.PER<-subset(Success8trials.PER, subset = (Success8trials.PER$PERactivityevents>3))



test.numbers<-(1:8)
n=25
colnames(Success8trials.PER)
which(colnames(Success8trials.PER)=="PER.sugar1")
which(colnames(Success8trials.PER)=="PER.sugartest")
slope<-NULL

par(mfrow = c(3,4))
for (n in 1:nrow(Success8trials.PER)) {
  plot(t(Success8trials.PER[n,((which(colnames(Success8trials.PER)=="PER.sugar1")
):(which(colnames(Success8trials.PER)=="PER.sugartest")
))]), xlab="Trial number", ylab = "Time", main = (Success8trials.PER$Species[n]))
  lines(t(Success8trials.PER[n,(which(colnames(Success8trials.PER)=="PER.sugar1"):which(colnames(Success8trials.PER)=="PER.sugartest"))]))
  abline(lm(t(Success8trials.PER[n,(which(colnames(Success8trials.PER)=="PER.sugar1"):which(colnames(Success8trials.PER)=="PER.sugartest"))]) ~ test.numbers), col="purple")
  slope[n]<-(lm(t(Success8trials.PER[n,(which(colnames(Success8trials.PER)=="PER.sugar1")
:which(colnames(Success8trials.PER)=="PER.sugartest"))]) ~ test.numbers)$coefficients)[2]
  
}
par(mfrow = c(1,1))

slope
Success8trials.PER$slope<-slope

plot(slope ~  brain.IT, data=Success8trials.PER, main = "Learning slopes related to brain/IT", ylab="Individual Slopes", xlab="Brain/IT")
abline(lm(slope ~  brain.IT, data=Success8trials.PER), col="purple")

individual.slopes.lm<-lm(slope ~  brain.IT, data=Success8trials.PER)
summary(individual.slopes.lm)

#Too much control??
individual.slopes.lmerID<-lmer(slope ~  brain.IT + (1|Genus/Species/ID), data=Success8trials.PER)
summary(individual.slopes.lmerID)


individual.slopes.lmer<-lmer(slope ~  brain.IT + (1|Genus/Species), data=Success8trials.PER)
summary(individual.slopes.lmer)



#Individual.slopes ~ residuals-----
Success8trials.PER.res<-Success8trials.PER

Success8trials.PER.res$brain.IT
lm(Brain.weight ~ IT..mm., data=Success8trials.PER)$residuals
Success8trials.PER.res$residuals<-lm(Brain.weight ~ IT..mm., data=Success8trials.PER)$residuals

Success8trials.PER<-Success8trials.PER.res

plot(slope ~ residuals,data=Success8trials.PER)
abline(lm(slope ~ residuals,data=Success8trials.PER), col="purple")

summary(lm(slope ~ residuals,data=Success8trials.PER))
summary(lmer(slope ~ residuals + (1|Genus/Species),data=Success8trials.PER))


#Slope differences for individual slopes------

ggplot(Success8trials.PER, aes(x=log(IT..mm.), y=log(Brain.weight), color=slope)) +
  geom_point() 

per.slopes.lmer.slopes<-lmer(Brain.weight ~ IT..mm. * slope + (1|Genus/Species), data=Success8trials.PER)
summary(per.slopes.lmer.slopes)




#per.time.trials.block-----
#PERtime ~ trial number-----
#We create the dataframe
#For all the bees that reacted to some trial
Beeh.PER.sugar
melt.Beeh.PER.sugar<-melt(Beeh.PER.sugar)
colnames(melt.Beeh.PER.sugar)<-c("ID","Species","Trial","Time")

melt.Beeh.PER.sugar<-melt.Beeh.PER.sugar[order(melt.Beeh.PER.sugar$ID),]

temp.melt.sugar<-replace(melt.Beeh.PER.sugar, c("PER.sugar1","PER.sugar2","PER.sugar3","PER.sugar4","PER.sugar5","PER.sugar6","PER.sugar.test"), c(1,2,3,4,5,6,7,8))
melt.Beeh.PER.sugar$Trial<-temp.melt.sugar$PER.sugar1

melt.Beeh.PER.sugar

str(melt.Beeh.PER.sugar)
boxplot(melt.Beeh.PER.sugar$Time ~ melt.Beeh.PER.sugar$Trial)
abline(lm(Time ~ Trial, data = melt.Beeh.PER.sugar), col="purple")

plot(Time ~ Trial, data = melt.Beeh.PER.sugar)
abline(lm(Time ~ Trial, data = melt.Beeh.PER.sugar), col="purple")

#Just the bees that reached trial8
ID<-last.test.done.bees$ID

ID<-as.data.frame(ID)
melt.Beeh.PER.sugar
melt.last.test.done.bees<-merge(ID,melt.Beeh.PER.sugar)

plot(Time ~ Trial, data = melt.last.test.done.bees)
abline(lm(Time ~ Trial, data = melt.last.test.done.bees), col="purple")

boxplot(melt.last.test.done.bees$Time ~ melt.last.test.done.bees$Trial)
abline(lm(Time ~ Trial, data = melt.last.test.done.bees), col="purple")

summary(lm(Time ~ Trial, data = melt.last.test.done.bees))

summary(lmer(Time ~ Trial + (1|Species), data = melt.last.test.done.bees))

genus<-data.frame(Beeh.data$ID,
Beeh.data$Genus)
colnames(genus)<-c("ID", "Genus")

melt.last.test.done.bees<-merge(melt.last.test.done.bees, genus)

#SD differences?
aggregate(Time ~ Trial, FUN=sd, data = melt.last.test.done.bees)

#mean differences?
aov.t<-aov(Time ~ as.factor(Trial), data = melt.last.test.done.bees)
summary(aov.t)
TukeyHSD(aov.t)




#I think there are differences along time

summary(lmer(Time ~ Trial + (1|Genus/Species/ID), data = melt.last.test.done.bees))

#PERtime ~ trial number * Brain.IT-----
ID.BIT<-data.frame(Beeh.data$ID,
(Beeh.data$Brain.weight/Beeh.data$IT..mm.))
Success8trials.ITf
colnames(ID.BIT)<-c("ID", "brain.IT")

melt.last.test.done.bees<-merge(melt.last.test.done.bees, ID.BIT)



ggplot(melt.last.test.done.bees, aes(x=Trial, y=Time, color=brain.IT)) +
  geom_point()+
  ggtitle("b")

PERtrial.lmer.bit<-lmer(Time ~ Trial * brain.IT + (1|Genus/Species), data = melt.last.test.done.bees)
PERtrial.lmer.bit<-lmer(Time ~ Trial * brain.IT + (1|Genus/Species/ID), data = melt.last.test.done.bees)

summary(PERtrial.lmer.bit)
#PERtime ~ trial.number * residuals----


measured.bit<-subset(melt.last.test.done.bees, subset = (is.na(melt.last.test.done.bees$Brain.weight) == FALSE))


BD.BIT<-data.frame(Beeh.data$ID,
Beeh.data$Brain.weight,
Beeh.data$IT..mm.)

colnames(BD.BIT)<-c("ID","Brain.weight","IT..mm.")

melt.last.test.done.bees2<-merge(melt.last.test.done.bees, BD.BIT)

nrow(melt.last.test.done.bees2)

melt.res<-lm(Brain.weight~IT..mm. ,data=melt.last.test.done.bees2)$residuals
melt.last.test.done.bees2<-subset(melt.last.test.done.bees2, subset=(is.na(melt.last.test.done.bees2$brain.IT)==FALSE))
melt.last.test.done.bees2$residuals<-melt.res



PERtrial.lmer.bit2<-lmer(Time ~ Trial * residuals + (1|Genus/Species/ID), data = melt.last.test.done.bees2)
summary(PERtrial.lmer.bit2)



ggplot(melt.last.test.done.bees2, aes(x=Trial, y=Time, color=residuals)) +
  geom_point() +
  ggtitle("c")







#success8 ~ n.of.success (Does bees learn?)----
#Does number of success conditionate success in the test?
plot(n.of.success ~ Success.test, notch = TRUE, data = Success8trials.ITf)
plot(as.numeric(Success.test) ~ n.of.success, data = Success8trials.ITf)
abline(lm(as.numeric(Success.test) ~ n.of.success, data = Success8trials.ITf), col="purple")

summary(lm(as.numeric(Success.test) ~ n.of.success, data = Success8trials.ITf))
n.succ.succ8<-glm(Success.test ~ n.of.success, data = Success8trials.ITf, family = binomial)
summary(n.succ.succ8)
allEffects(n.succ.succ8, xlevels=list(n.of.success=c(0:8)))


#mixed model
n.succ.succ8.glmm<-glmer(Success.test ~ n.of.success + (1|Genus/Species), data = Success8trials.ITf, family = binomial)
allEffects(n.succ.succ8.glmm, xlevels=list(n.of.success=c(0:8)))

summary(n.succ.succ8.glmm)


#Success ~ Trial-----
Beeh.succs<-data.frame(Success8trials.ITf$ID,Success8trials.ITf$Genus,Success8trials.ITf$Species,Success8trials.ITf$Success1,
Success8trials.ITf$Success2,Success8trials.ITf$Success3,Success8trials.ITf$Success4,
Success8trials.ITf$Success5,Success8trials.ITf$Success6,Success8trials.ITf$Success7,
Success8trials.ITf$Success.test)
colnames(Beeh.succs)<-c("ID","Genus","Species","Trial1","Trial2",
                        "Trial3","Trial4","Trial5","Trial6",
                        "Trial7","Test")

Beeh.succeeses<-melt(Beeh.succs, id.vars = c("ID","Genus","Species"))
Beeh.succeeses<-Beeh.succeeses[order(Beeh.succeeses$ID),]
Beeh.succeeses$value<-as.factor(Beeh.succeeses$value)

colnames(Beeh.succeeses)<-c("ID","Genus","Species","Trial","Success")
plot(Beeh.succeeses$Success ~ Beeh.succeeses$Trial, xlab="Trial", ylab = "Success")

levels(Beeh.succeeses$Trial)<-c(levels(Beeh.succeeses$Trial),1,2,3,4,5,6,7,8)
Trial.as.numeric<-replace(Beeh.succeeses$Trial, Beeh.succeeses$Trial == "Trial1", 1)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial2", 2)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial3", 3)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial4", 4)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial5", 5)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial6", 6)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Trial7", 7)
Trial.as.numeric<-replace(Trial.as.numeric, Trial.as.numeric == "Test", 8)
Beeh.succeeses$Trial.as.numeric<-Trial.as.numeric
Beeh.succeeses$Trial.as.numeric<-as.numeric(Beeh.succeeses$Trial.as.numeric)-8

plot(as.numeric(Beeh.succeeses$Success)-1~Beeh.succeeses$Trial.as.numeric,
     xlab="Trial", ylab = "No success / Success")
abline(lm(as.numeric(Beeh.succeeses$Success)-1~Beeh.succeeses$Trial.as.numeric))

summary(lm(as.numeric(Beeh.succeeses$Success)-1~Beeh.succeeses$Trial.as.numeric))

str(Beeh.succeeses)

succ.trial.binomial<-glmer(Success~Trial.as.numeric + (1|Genus/Species), family = binomial, data = Beeh.succeeses)
summary(succ.trial.binomial)
allEffects(succ.trial.binomial, xlevels=list(Trial.as.numeric=c(0:8)))


#SPECIES COMPARISON-------
#Species success
fraction.species<-data.frame(Success8trials.ITf$ID,
                             Success8trials.ITf$Genus,
                             Success8trials.ITf$Species,
                             Success8trials.ITf$Success.test)

colnames(fraction.species)<-c("ID","Genus","Species","Success.test")
fraction.species$Species<-droplevels(fraction.species$Species)
fraction.species$Success.test.as.numeric<-as.numeric(fraction.species$Success.test)-1
plot(fraction.species$Success.test ~ fraction.species$Species, 
     xlab="Species", ylab = "Proportion of success")

exitos<-as.data.frame(aggregate(fraction.species$Success.test.as.numeric ~ fraction.species$Species, FUN = sum))
total<-as.data.frame(summary(fraction.species$Species))
total$Species<-row.names(total)
colnames(total)<-c("All", "Species")
colnames(exitos)<-c("Species","Sum")
species.successes<-merge(total,exitos)
species.successes

#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/data")
#write.csv(species.successes, "Species_successes.csv")
#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")



#First trial
tomerge<-data.frame(Beeh.data$ID,
Beeh.data$Genus,
Beeh.data$Species,
Beeh.data$PER.sugar1,
Beeh.data$PER.water1)
colnames(tomerge)<-c("ID","Genus","Species","PER.sugar1","PER.water1")


succ1<-data.frame(Success8trials.ITf$ID,
Success8trials.ITf$Genus,
Success8trials.ITf$Species,
Success8trials.ITf$Success1)
colnames(succ1)<-c("ID","Genus","Species","Success1")
tomerge
succ1

first.trial<-merge(succ1,tomerge)



aggregate(first.trial$PER.sugar1~first.trial$Species, FUN = min)
fs<-data.frame(aggregate(Beeh.data$PER.sugar1~Beeh.data$Species, FUN = min))
fw<-data.frame(aggregate(Beeh.data$PER.water1~Beeh.data$Species, FUN = min))

fsw<-as.data.frame(merge(fs,fw, all = TRUE))

colnames(fsw)<-c("Species", "PER.sugar1", "PER.water1")

fu<-data.frame(
useless.species$Species,
useless.species$PER.sugar1,
useless.species$PER.water1)
colnames(fu)<-c("Species","PER.sugar1","PER.water1")
fu<-unique(fu)
minimost1<-rbind(fsw,fu)


minimost1$PER.sugar1[is.na(minimost1$PER.sugar1)] <- 999
minimost1$PER.water1[is.na(minimost1$PER.water1)] <- 999
reaction.time<-vector()
for(n in 1:nrow(minimost1)){
  reaction.time[n]<-min(minimost1[n,2:3])
  }
reaction.time[(reaction.time)==999] <- NA
minimost1$reaction.time<-reaction.time
minimost<-data.frame(minimost1$Species, minimost1$reaction.time)
minimost


minimost<-minimost[order(minimost$minimost1.reaction.time),]
colnames(minimost)<-c("Species","Reaction.time")


#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior/data")
#write.csv(minimost, "Reaction_times.csv")
#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/beehavior")


#OTHER ANALYSIS---------
#Encephalization index is related to body size-------
#If body weight still correlates with encephalization index cannot be used 
#because it means that the allometric effects is not fully removed.
Success8trials.ITf$brain.IT
Success8trials.ITf$IT..mm.
plot(Success8trials.ITf$brain.IT~Success8trials.ITf$IT..mm.)
abline(lm(brain.IT~IT..mm., data = Success8trials.ITf))
summary(lm(brain.IT~IT..mm., data = Success8trials.ITf))

##Does bees learn?-----
Success8trials.ITf$Success.test.as.numeric
observed<-c(nrow(subset(Success8trials.ITf, subset = (Success8trials.ITf$Success.test.as.numeric == 1))),
nrow(subset(Success8trials.ITf, subset = (Success8trials.ITf$Success.test.as.numeric == 0))))
expected<-c(0.5,0.5)

#Our proportion of learners is different from the expected values
chisq.test(x = observed,
           p = expected)



#Data construction: means for each species dataframe-----

#Let's add mean time of each trial

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
#We extract slopes of the lm as well and add it to the data.frame
par(mfrow = c(3,4))
slope<-NULL
test.numbers<-(1:8)
for (n in 1:nrow(PERsugar.success.mean)) {
plot(t(PERsugar.success.mean[n,(4:11)]), xlab="Trial number", ylab = "Time", main = (PERsugar.success.mean$Species[n]))
lines(t(PERsugar.success.mean[n,(4:11)]))
abline(lm(t(PERsugar.success.mean[n,(4:11)]) ~ test.numbers), col="purple")
slope[n]<-(lm(t(PERsugar.success.mean[n,(4:11)]) ~ test.numbers)$coefficients)[2]

}
par(mfrow = c(1,1))
PERsugar.success.mean$slope<-slope

#Let's add brain weight and IT to data
cerebros.species<-data.frame<-(aggregate(Brain.weight ~ Species, FUN = mean, data = Beeh.data))
ITs.species<-data.frame<-(aggregate(IT..mm. ~ Species, FUN = mean, data = Beeh.data))

PERsugar.success.mean<-merge(merge(PERsugar.success.mean, cerebros.species), ITs.species)
PERsugar.success.mean$brain.IT<-(PERsugar.success.mean$Brain.weight / PERsugar.success.mean$IT..mm.)
#Add genus
PERsugar.success.mean
add.genus<-data.frame(Beeh.data$Genus, Beeh.data$Species)
colnames(add.genus)<-c("Genus","Species")
add.genus
Add.genus<-unique(add.genus)
Add.genus
PERsugar.success.mean<-merge(PERsugar.success.mean, Add.genus)


#success8 ~ brain.it----

plot(PER.sugar.test ~ brain.IT, data = PERsugar.success.mean)
abline(lm(PER.sugar.test ~ brain.IT, data = PERsugar.success.mean), col="purple")

succ8sp.lm<-lm(PER.sugar.test ~ brain.IT, data = PERsugar.success.mean)
summary(succ8sp.lm)

#(add genus)

#Slope ~ brain.it----
#(No correlation)
plot(slope ~ brain.IT, data = PERsugar.success.mean)
abline(lm(slope ~ brain.IT, data = PERsugar.success.mean), col="purple")
summary(lm(slope ~ brain.IT, data = PERsugar.success.mean))

#Number of success ~ brain.it----
#(Correlation)
str(PERsugar.success.mean)
plot(mean.of.success ~ brain.IT, data = PERsugar.success.mean)
abline(lm(mean.of.success ~ brain.IT, data = PERsugar.success.mean), col="purple")

hist(PERsugar.success.mean$brain.IT)
shapiro.test(PERsugar.success.mean$brain.IT)
n.succ.brain.IT<-lm(mean.of.success ~ brain.IT, data = PERsugar.success.mean)
summary(n.succ.brain.IT)

#Control by genus? with this little n?
summary(lmer(mean.of.success ~ brain.IT + (1|Genus), data = PERsugar.success.mean))

n.succ.brain.IT.glm<-glm(mean.of.success ~ brain.IT, data = PERsugar.success.mean)
summary(n.succ.brain.IT.glm)

lm(mean.of.success ~ residuals, data= PERsugar.success.mean)



#residuals----
#residuals are ok
plot(log(Brain.weight) ~ log(IT..mm.), data = PERsugar.success.mean)
abline(lm(log(Brain.weight) ~ log(IT..mm.), data = PERsugar.success.mean), col = "purple")

brain.it.species.lm<-lm(log(Brain.weight) ~ log(IT..mm.), data = PERsugar.success.mean)
summary(brain.it.species.lm)

brain.it.species.lm$residuals

PERsugar.success.mean$residuals <- brain.it.species.lm$residuals

meansucc.res<-lm(mean.of.success ~ residuals, data= PERsugar.success.mean)
plot(mean.of.success ~ residuals, data= PERsugar.success.mean)
abline(meansucc.res, col = "purple")
summary(meansucc.res)

Success8trials.ITf

PER.sugar.test.mean

#TimePER8 ~ trial * brain ----

Success8trials.ITf

#We take 
ID<-Success8trials.ITf$ID 
ID<-as.data.frame(ID)
colnames(Beeh.data)
PER.sugar<-data.frame(Beeh.data$ID, Beeh.data$PER.sugar1, Beeh.data$PER.sugar2, 
           Beeh.data$PER.sugar3, Beeh.data$PER.sugar4, Beeh.data$PER.sugar5,
           Beeh.data$PER.sugar6, Beeh.data$PER.sugar7)

colnames(PER.sugar)<-c("ID", "PER.sugar1", "PER.sugar2", "PER.sugar3", "PER.sugar4",
                       "PER.sugar5", "PER.sugar6", "PER.sugar7")

#What should we do with NAs?
ID.PER.sugar<-merge(ID, PER.sugar)

melted.ID.PER.sugar<-melt(ID.PER.sugar)

ID.test<-data.frame(Beeh.data$ID, Beeh.data$PER.sugar.test)
colnames(ID.test)<-c("ID","PER.sugar.test")

ID.PER.sugar<-merge(melted.ID.PER.sugar, ID.test)

ID.PER.sugar

rest<-data.frame(Success8trials.ITf$ID,Success8trials.ITf$Genus,Success8trials.ITf$Species, Success8trials.ITf$Brain.weight,
Success8trials.ITf$IT..mm., Success8trials.ITf$brain.IT, Success8trials.ITf$residuals)

colnames(rest)<-c("ID","Genus","Species","Brain.weight","IT..mm.","brain.IT","residuals")

merge(ID.PER.sugar, rest)

Success8trials.melt<-merge(ID.PER.sugar, rest)

#Model for test~values
succ8.values.brainIT<-lm(PER.sugar.test ~ value * brain.IT, data = Success8trials.melt)
summary(succ8.values.brainIT)

#Control by ID, something fails
Success8trials.melt$PER.sugar.test
Success8trials.melt$value

succ8.values.brainIT.ID<-lmer(PER.sugar.test ~ value * brain.IT + (1|ID), data = Success8trials.melt)
summary(succ8.values.brainIT.ID)

#Why the fixed effects are different?
succ8.values.brainIT.sp<-lmer(PER.sugar.test ~ value * brain.IT + (1|Species), data = Success8trials.melt)
summary(succ8.values.brainIT.sp)

succ8.values.brainIT.g<-lmer(PER.sugar.test ~ value * brain.IT + (1|Genus), data = Success8trials.melt)
summary(succ8.values.brainIT.g)

r.squaredGLMM(succ8.values.brainIT.g)
#(Controlar por todo a la vez)




##QUESTION: Time PER sugar - Time PER water----


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

#Brain ~ Urban/Natural-----
Beeh.data$ID
Beeh.data$Place
levels(Beeh.data$Place)
n=1
Place.type<-NULL
for (n in 1:length(Beeh.data$Place)) {
if (Beeh.data$Place[n] == levels(Beeh.data$Place)[1] | Beeh.data$Place[n] == levels(Beeh.data$Place)[2] | Beeh.data$Place[n] == levels(Beeh.data$Place)[3] | Beeh.data$Place[n] == levels(Beeh.data$Place)[5] | Beeh.data$Place[n] == levels(Beeh.data$Place)[6] | Beeh.data$Place[n] == levels(Beeh.data$Place)[7]| Beeh.data$Place[n] == levels(Beeh.data$Place)[8]| Beeh.data$Place[n] == levels(Beeh.data$Place)[9]| Beeh.data$Place[n] == levels(Beeh.data$Place)[11]) {
  Place.type[n] <- "Natural" }else{
  Place.type[n] <- "Urban"
}}  
Place.type  


Place.type.data<-data.frame(Beeh.data$ID, Place.type)
colnames(Place.type.data)<-c("ID", "Place.type")



place.brain<-merge(Success8trials.ITf, Place.type.data)
boxplot(place.brain$brain.IT ~ place.brain$Place.type, notch = TRUE, ylab = "Brain/IT")
TukeyHSD(aov(place.brain$brain.IT ~ place.brain$Place.type))

#PHYLOGENETIC TREES----

##LOAD TREE
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
#We treat the data differently, mostly because species names, we do a "new" data.frame
data<-Success8trials.ITf
#Species dataframe, 
species=c("Rhodanthidium_sticticum" ,"Osmia_latreillei"   ,    
          "Megachile_willughbiella", "Bombus_terrestris"      ,
          "Bombus_pratorum"         ,"Bombus_pascuorum"       ,
          "Bombus_vestalis"         ,"Apis_mellifera"         ,
          "Lasioglossum_malachurum" ,"Lasioglossum_immunitum", 
          "Flavipanurgus_venustus" , "Andrena_angustior"   ,   
          "Andrena_hispania"       , "Andrena_flavipes"  ,     
          "Andrena_pilipes"        , "Andrena_sp." )

##Use tree 1 (376 genera) #Genera-level phylogney I used the first one
bee.mcmc=bee.trees[[1]]

###root with apoid wasp outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)

##Make ultrametric
bee.mcmc=chronos(bee.mcmc)

bee.tree=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
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
                                    "Samba", "Capicola", "Hesperapis",      
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
                                    "Hypanthidium","Anthodioctes", "Hypanthidioides", 
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
                                    "Hemicotelles", "Colletes", "Mourecotelles", 
                                    "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                    "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                    "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                    "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides","Anthophora",
                                    "Eucera", "Chilicola", "Duckeanthidium",
                                    
                                    "Xylocopa", "Tachysphex" 
))
bee.tree
plot(bee.tree)
nodelabels()
## Add species tips to genera tips

#change panurgus to flavipanurgus
bee.tree$tip.label[2]
bee.tree$tip.label[2]=c("Flavipanurgus")

#add dummy species labels
bee.tree$tip.label<-paste(bee.tree$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
  bee.tree<-add.species.to.genus(bee.tree,species[i],
                                 where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree$tip.label)
bee.tree<-drop.tip(bee.tree,bee.tree$tip.label[ii])
#Our tree
plot(bee.tree)

##Check for missing species
setdiff(species,bee.tree$tip.label)

#Remove node labels, or the model will fail
bee.tree$node.label=NULL


##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)



#if we compare A with our species column, they have different names
A
dataformcmc=data
dataformcmc$Species
#Let's fix it
dataformcmc$Species<-stri_replace_first_regex(dataformcmc$Species,pattern = " ", replacement = "_")
dataformcmc[dataformcmc$Species%in%"Psithyrus_vestalis","Species"]=c("Bombus_vestalis")
dataformcmc.success<-dataformcmc
dataformcmc.success<-subset(dataformcmc, subset = (dataformcmc$Success.test == 1))

#no differences. good
setdiff(rownames(A),dataformcmc$Species)
#Success8 ~ brain.it mcmcglmm----

#This is the formula of the first model I want to do
#I changed binomial to bernoulli as it is more efficient for data with just 0's and 1's
brm.prueba<-brm(Success.test ~ brain.IT + (1|Species), data = dataformcmc,
                cores=4,
                family = bernoulli, cov_ranef = list("Species" = A),
                control = list(adapt_delta = 0.99,max_treedepth=15))


##Posterior predictive checks - with these you are looking at how well yrep replicates y - if they are very different, it suggests your model is misspecified
# if a model is a good fit then we should be able to use it to generate data that looks a lot like the data we observed
pp_check(brm.prueba,nsamples=1000)

#we create the prio object, with the formula of our model
bprior1<-get_prior(Success.test ~ brain.IT + (1 |Species), data = dataformcmc, family = bernoulli(), autocor = NULL,
                   sparse = FALSE, internal = FALSE)

#How to create priors by Liam Kendall
#this isn't how I would create priors - this is just giving you the default priors
#you can create priors using the following - although your model is performing well even with default
#I would use `normal` priors and use the output of the model with the defaults to define these 
#Usually you should use your domain expertise i.e. what do you know about what the baseline of the intercept and coefficient be given what you know about the data
#this is difficult however with non-gaussian families

bprior <- prior(normal(0,5), class = b) +
  prior(normal(0,2), class = Intercept) +
  prior(normal(0,1), class = sd) 

brm.prueba2<-brm(Success.test ~ brain.IT + (1|Species), data = dataformcmc,
                 cores = 4,
                 prior = bprior,
                 family = bernoulli, 
                 cov_ranef = list("Species" = A),
                 control = list(adapt_delta = 0.99,max_treedepth=15))

pp_check(brm.prueba2,nsamples=1000)


#Add information criterion to model
brm.prueba=add_ic(brm.prueba,ic=c("waic"))
brm.prueba2=add_ic(brm.prueba2,ic=c("waic"))

#compare
compare_ic(brm.prueba,brm.prueba2,ic=c("waic"))

#see they are much the same
#brms is very good at fitting binomial/bernoulli models


#Bayesian R2
bayes_R2(brm.prueba)


# Intraclass Correlation Coefficient:
# ICC is the proportion of the variance explained by the grouping structure 
# in the population
# Close to 1 indicates high similarity between values from the same group.

icc(brm.prueba, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)




#Success8 ~ residuals glmmm----

brm.succ8res<-brm(Success.test ~ residuals + (1|Species), data = dataformcmc,
                cores=4,
                family = bernoulli, cov_ranef = list("Species" = A),
                control = list(adapt_delta = 0.99,max_treedepth=15))


brm.succ8res
brm.succ8res=add_ic(brm.succ8res,ic=c("waic"))
pp_check(brm.succ8res,nsamples=1000)
bayes_R2(brm.succ8res)
icc(brm.succ8res, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.succ8res)
#Success8 ~ absolute brain size mcmcglmm----

brm.succ8brains<-brm(Success.test ~ Brain.weight + (1|Species), data = dataformcmc,
                  cores=4,
                  family = bernoulli, cov_ranef = list("Species" = A),
                  control = list(adapt_delta = 0.99,max_treedepth=15))


brm.succ8brains
brm.succ8brains=add_ic(brm.succ8brains,ic=c("waic"))
pp_check(brm.succ8brains,nsamples=1000)
bayes_R2(brm.succ8brains)
marginal_effects(brm.succ8brains)
icc(brm.succ8brains, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)



allEffects(brm.succ8brains)


#n.of.success~brain.it glmm------
brm.nsuccbrain.it<-brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc,
                        cores=4,
                        family = gaussian, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))
brm.nsuccbrain.it

brm.nsuccbrain.it=add_ic(brm.nsuccbrain.it,ic=c("waic"))
pp_check(brm.nsuccbrain.it,nsamples=1000)
bayes_R2(brm.nsuccbrain.it)
icc(brm.nsuccbrain.it, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#n.of.success~residuals glmm------
brm.nsuccresiduals<-brm(n.of.success ~ residuals + (1|Species), data = dataformcmc,
                  cores=4,
                  family = gaussian, cov_ranef = list("Species" = A),
                  control = list(adapt_delta = 0.99,max_treedepth=15))

brm.nsuccresiduals
brm.nsuccresiduals=add_ic(brm.nsuccresiduals,ic=c("waic"))
pp_check(brm.nsuccresiduals,nsamples=1000)
bayes_R2(brm.nsuccresiduals)
marginal_effects(brm.nsuccresiduals)

icc(brm.nsuccresiduals, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)



#n.of.success~absolute brain size mcmcglmmm------

brm.nsuccrbrain<-brm(n.of.success ~ Brain.weight + (1|Species), data = dataformcmc,
                        cores=4,
                        family = gaussian, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))

brm.nsuccrbrain=add_ic(brm.nsuccrbrain,ic=c("waic"))
pp_check(brm.nsuccrbrain,nsamples=1000)
bayes_R2(brm.nsuccrbrain)
brm.nsuccrbrain
icc(brm.nsuccrbrain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)





#n.of.success (starting in trial 2)~brain.it mcmcglmmm------

dataformcmc7trials<-dataformcmc
dataformcmc7trials$brain.IT


dataformcmc7trials$n.of.success<-(as.numeric(dataformcmc7trials$Success2)-1) + 
  (as.numeric(dataformcmc7trials$Success3)-1) + 
  (as.numeric(dataformcmc7trials$Success4)-1) + 
  (as.numeric(dataformcmc7trials$Success5)-1) + 
  (as.numeric(dataformcmc7trials$Success6)-1) + 
  (as.numeric(dataformcmc7trials$Success7)-1) + 
  (as.numeric(dataformcmc7trials$Success.test)-1)

brm.nsucc7rbrain.it<-brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc7trials,
                     cores=4,
                     family = gaussian, cov_ranef = list("Species" = A),
                     control = list(adapt_delta = 0.99,max_treedepth=15))

brm.nsucc7rbrain.it=add_ic(brm.nsucc7rbrain.it,ic=c("waic"))
pp_check(brm.nsucc7rbrain.it,nsamples=1000)
bayes_R2(brm.nsucc7rbrain.it)
icc(brm.nsucc7rbrain.it, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


brm.nsucc7rbrain.it




#n.of.success (starting in trial 2)~residuals mcmcglmmm------
brm.nsucc7res<-brm(n.of.success ~ residuals + (1|Species), data = dataformcmc7trials,
                      cores=4,
                      family = gaussian, cov_ranef = list("Species" = A),
                      control = list(adapt_delta = 0.99,max_treedepth=15))

brm.nsucc7res=add_ic(brm.nsucc7res,ic=c("waic"))
pp_check(brm.nsucc7res,nsamples=1000)
bayes_R2(brm.nsucc7res)
brm.nsucc7res
icc(brm.nsucc7res, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#n.of.success (starting in trial 2)~absolute brain size mcmcglmmm------

brm.nsuccrabsbrain<-brm(n.of.success ~ Brain.weight + (1|Species), data = dataformcmc7trials,
                     cores=4,
                     family = gaussian, cov_ranef = list("Species" = A),
                     control = list(adapt_delta = 0.99,max_treedepth=15))

brm.nsuccrabsbrain=add_ic(brm.nsuccrabsbrain,ic=c("waic"))
pp_check(brm.nsuccrabsbrain,nsamples=1000)
bayes_R2(brm.nsuccrabsbrain)
icc(brm.nsuccrabsbrain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


brm.nsuccrabsbrain

#PER.sugar.test ~ Brain.IT-------
brm.persugartest.brain.IT<-brm(PER.sugar.test ~ brain.IT + (1|Species), data = dataformcmc,
                               cores=4,
                               family = gaussian, cov_ranef = list("Species" = A),
                               control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.brain.IT=add_ic(brm.persugartest.brain.IT,ic=c("waic"))
#Not a good model mate
pp_check(brm.persugartest.brain.IT,nsamples=1000)
bayes_R2(brm.persugartest.brain.IT)
icc(brm.persugartest.brain.IT, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


brm.persugartest.brain.IT<-brm(PER.sugar.test ~ brain.IT + (1|Species), data = dataformcmc,
                               cores=4,
                               family = poisson, cov_ranef = list("Species" = A),
                               control = list(adapt_delta = 0.99,max_treedepth=15))

brm.persugartest.brain.IT=add_ic(brm.persugartest.brain.IT,ic=c("waic"))
brm.persugartest.brain.IT
pp_check(brm.persugartest.brain.IT,nsamples=1000)
bayes_R2(brm.persugartest.brain.IT)
#ICC high!
icc(brm.persugartest.brain.IT, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)



#PER.sugar.test ~ residuals glmm-----
brm.persugartest<-brm(PER.sugar.test ~ residuals + (1|Species), data = dataformcmc,
                        cores=4,
                        family = gaussian, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest=add_ic(brm.persugartest,ic=c("waic"))
pp_check(brm.persugartest,nsamples=1000)
bayes_R2(brm.persugartest)

brm.persugartest<-brm(PER.sugar.test ~ residuals + (1|Species), data = dataformcmc,
                      cores=4,
                      family = poisson, cov_ranef = list("Species" = A),
                      control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest
brm.persugartest=add_ic(brm.persugartest,ic=c("waic"))
pp_check(brm.persugartest,nsamples=1000)
bayes_R2(brm.persugartest)
icc(brm.persugartest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#PER.sugar.test ~ absolute brain size mcmcglmm-----
brm.persugarbrain<-brm(PER.sugar.test ~ Brain.weight + (1|Species), data = dataformcmc,
                      cores=4,
                      family = poisson, cov_ranef = list("Species" = A),
                      control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugarbrain
brm.persugarbrain=add_ic(brm.persugarbrain,ic=c("waic"))
pp_check(brm.persugarbrain,nsamples=1000)
bayes_R2(brm.persugarbrain)
icc(brm.persugarbrain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)




#Per.sugar.test ~ absolute brain size (only succeeders) brm-----
brm.persugarbrain.s<-brm(PER.sugar.test ~ Brain.weight + (1|Species), data = dataformcmc.success,
                       cores=4,
                       family = poisson, cov_ranef = list("Species" = A),
                       control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugarbrain.s
brm.persugarbrain.s=add_ic(brm.persugarbrain.s,ic=c("waic"))
pp_check(brm.persugarbrain.s,nsamples=1000)
bayes_R2(brm.persugarbrain.s)
icc(brm.persugarbrain.s, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#Per.sugar.test~ absolute brain size (capped) brm-----
hist(dataformcmc.success$PER.sugar.test)
plot(dataformcmc.success$PER.sugar.test)
is.numeric(dataformcmc$PER.sugar.test)
dataformcmc.success40s<-subset(dataformcmc.success, subset = (dataformcmc.success$PER.sugar.test<40))

brm.persugarbrain.40s<-brm(PER.sugar.test ~ Brain.weight + (1|Species), data = dataformcmc.success40s,
                         cores=4,
                         family = poisson, cov_ranef = list("Species" = A),
                         control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugarbrain.40s
brm.persugarbrain.40s=add_ic(brm.persugarbrain.s,ic=c("waic"))
pp_check(brm.persugarbrain.40s,nsamples=1000)
bayes_R2(brm.persugarbrain.40s)
icc(brm.persugarbrain.40s, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#Per.sugar.test ~ residuals (only succeeders) brm-----

brm.persugartest.rs<-brm(PER.sugar.test ~ residuals + (1|Species), data = dataformcmc.success,
                      cores=4,
                      family = poisson, cov_ranef = list("Species" = A),
                      control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.rs
brm.persugartest.rs=add_ic(brm.persugartest.rs,ic=c("waic"))
pp_check(brm.persugartest.rs,nsamples=1000)
bayes_R2(brm.persugartest.rs)
icc(brm.persugartest.rs, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#Per.sugar.test~ residuals (capped) brm-----
hist(dataformcmc.success$PER.sugar.test)
plot(dataformcmc.success$PER.sugar.test)
is.numeric(dataformcmc$PER.sugar.test)
dataformcmc.success40s<-subset(dataformcmc.success, subset = (dataformcmc.success$PER.sugar.test<40))


brm.persugartest.40rs<-brm(PER.sugar.test ~ residuals + (1|Species), data = dataformcmc.success40s,
                         cores=4,
                         family = poisson, cov_ranef = list("Species" = A),
                         control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.40rs
brm.persugartest.40rs=add_ic(brm.persugartest.rs,ic=c("waic"))
pp_check(brm.persugartest.40rs,nsamples=1000)
bayes_R2(brm.persugartest.40rs)
icc(brm.persugartest.40rs, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)



#Per.sugar.test ~ brain.it (only succeeders) brm-----
brm.persugarbrainit.s<-brm(PER.sugar.test ~ brain.IT + (1|Species), data = dataformcmc.success,
                         cores=4,
                         family = poisson, cov_ranef = list("Species" = A),
                         control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugarbrainit.s
brm.persugarbrainit.s=add_ic(brm.persugarbrain.sit,ic=c("waic"))
pp_check(brm.persugarbrainit.s,nsamples=1000)
bayes_R2(brm.persugarbrainit.s)
icc(brm.persugarbrainit.s, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#PER.sugar.test.censored ~ Brain.IT-------
brm.persugartest.c.brainIT<-brm(PER.sugar.test.censored ~ brain.IT + (1|Species), data = dataformcmc,
                        cores=4,
                        family = gaussian, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.c.brainIT=add_ic(brm.persugartest.c.brainIT,ic=c("waic"))
#This is not a good model either
pp_check(brm.persugartest.c.brainIT,nsamples=1000)
bayes_R2(brm.persugartest.c.brainIT)
icc(brm.persugartest.c.brainIT, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)



brm.persugartest.c.brainIT<-brm(PER.sugar.test.censored ~ brain.IT + (1|Species), data = dataformcmc,
                                cores=4,
                                family = poisson, cov_ranef = list("Species" = A),
                                control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.c.brainIT=add_ic(brm.persugartest.c.brainIT,ic=c("waic"))
#This is not a good model either
pp_check(brm.persugartest.c.brainIT,nsamples=1000)
bayes_R2(brm.persugartest.c.brainIT)
icc(brm.persugartest.c.brainIT, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)





#PER.sugar.test.censored ~ residuals mcmcglmmm----
brm.persugartest.c<-brm(PER.sugar.test.censored ~ residuals + (1|Species), data = dataformcmc,
                      cores=4,
                      family = gaussian, cov_ranef = list("Species" = A),
                      control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugartest.c=add_ic(brm.persugartest.c,ic=c("waic"))
#This is not a good model either
pp_check(brm.persugartest.c,nsamples=1000)
bayes_R2(brm.persugartest.c)
brm.persugartest.c
#PER.sugar.test.censored ~ absolute brain size mcmcglmmm----
brm.persugarbrain.c<-brm(brm.persugarbrain.c ~ Brain.weight + (1|Species), data = dataformcmc,
                        cores=4,
                        family = gaussian, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))
brm.persugarbrain.c=add_ic(brm.persugarbrain.c,ic=c("waic"))
#This is not a good model either
pp_check(brm.persugarbrain.c,nsamples=1000)
bayes_R2(brm.persugarbrain.c)
brm.persugarbrain.c
icc(brm.persugartest.c, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)







#Time ~ Trial glmmm-------
summary(lmer(Time ~ Trial + (1|Genus/Species/ID), data = melt.last.test.done.bees))
melt.last.test.done.beesforglmm<-melt.last.test.done.bees


melt.last.test.done.beesforglmm$Species<-stri_replace_first_regex(melt.last.test.done.beesforglmm$Species,pattern = " ", replacement = "_")
melt.last.test.done.beesforglmm[melt.last.test.done.beesforglmm$Species%in%"Psithyrus_vestalis","Species"]=c("Bombus_vestalis")
melt.last.test.done.beesforglmm<-na.omit(melt.last.test.done.beesforglmm)
melt.last.test.done.beesforglmm<-melt.last.test.done.beesforglmm[-(which(melt.last.test.done.beesforglmm$Species == "Panurgus_dargius")),]
melt.last.test.done.beesforglmm<-melt.last.test.done.beesforglmm[-(which(melt.last.test.done.beesforglmm$Species == "Osmia_caerulescens")),]


setdiff(melt.last.test.done.beesforglmm$Species,bee.tree$tip.label)

brm.time.trial.negbinomial<-brm(Time ~ Trial + (1|Species), data = melt.last.test.done.beesforglmm,
                            cores=4,
                            family = negbinomial, cov_ranef = list("Species" = A),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.time.trial.negbinomial=add_ic(brm.time.trial.negbinomial,ic=c("waic"))
pp_check(brm.time.trial.negbinomial,nsamples=1000)
bayes_R2(brm.time.trial.negbinomial)
icc(brm.time.trial.negbinomial, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)
brm.time.trial.negbinomial


marginal_effects(brm.time.trial.negbinomial)




#success8 ~ n.of.success (Does bees learn?)----

dataformcmc$n.of.success
dataformcmc$Success.test

brm.succnsucc<-brm(Success.test ~ n.of.success + (1|Species), data = dataformcmc,
                        cores=4,
                        family = bernoulli, cov_ranef = list("Species" = A),
                        control = list(adapt_delta = 0.99,max_treedepth=15))
brm.succnsucc=add_ic(brm.succnsucc,ic=c("waic"))
pp_check(brm.succnsucc,nsamples=1000)
bayes_R2(brm.succnsucc)
brm.succnsucc
icc(brm.succnsucc, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#Success ~ Trials brms--------
dataformcmc
melt.last.test.done.beesforglmm
Beeh.succeeses
Beeh.succeesesformcmc<-Beeh.succeeses
Beeh.succeesesformcmc$Species<-stri_replace_first_regex(Beeh.succeesesformcmc$Species,pattern = " ", replacement = "_")
Beeh.succeesesformcmc[Beeh.succeesesformcmc$Species%in%"Psithyrus_vestalis","Species"]=c("Bombus_vestalis")


Beeh.succeesesformcmc




brm.succtrial<-brm(Success ~ Trial.as.numeric + (1|Species), data = Beeh.succeesesformcmc,
    cores=4,
    family = bernoulli, cov_ranef = list("Species" = A),
    control = list(adapt_delta = 0.99,max_treedepth=15))
brm.succtrial=add_ic(brm.succtrial,ic=c("waic"))
pp_check(brm.succtrial,nsamples=1000)
bayes_R2(brm.succtrial)
brm.succtrial
icc(brm.succtrial, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#negbiomial??------
Beeh.succeesesformcmc2<-Beeh.succeesesformcmc
Beeh.succeesesformcmc2$Success<-as.numeric(as.character(Beeh.succeesesformcmc2$Success))
brm.succtrial2<-brm(Success ~ Trial.as.numeric + (1|Species), data = Beeh.succeesesformcmc2,
                   cores=4,
                   family = negbinomial, cov_ranef = list("Species" = A),
                   control = list(adapt_delta = 0.99,max_treedepth=15))

icc(brm.succtrial2, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)


#bayesian vs mixed models-----
#success8~brain.it
summary(succ.brain.itrg)
success8brain.itglmmm
#success8~residuals
summary(succ8.ressg)
brm.succ8res
#n.of.success~brain.it
summary(n.succ.brainit.lm.g)
brm.nsuccbrain.it
#n.of.success~residuals
summary(n.succ.res.lmer)
brm.nsuccresiduals
#PER.sugar.time~brain.IT
summary(PERsugarc.glmer)
brm.persugartest.brain.IT
#PER.sugar.time~residuals
summary(PERsugar.res.glmer)
brm.persugartest
summary(PERsugar.c.res.glmer)
PERsugarc.glmer

#Time~Trial
summary(lmer(Time ~ Trial + (1|Species), data = melt.last.test.done.bees))
brm.time.trial.poisson

#other-------------------
#I just had a bit of fun with your dataset below haha...

##I imagine your next model could be the number of successes?
# you can model this with poisson or negative binomial
# maybe even with an offset of log(total number of tests)
#Sounds like a cool model!!

#with default priors

#poisson
brm_successes=brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc,
                  cores = 4,
                  family = poisson, 
                  cov_ranef = list("Species" = A),
                  control = list(adapt_delta = 0.99,max_treedepth=15))

#negative binomial
brm_successes2=brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc,
                   cores = 4,
                   family = negbinomial, 
                   cov_ranef = list("Species" = A),
                   control = list(adapt_delta = 0.99,max_treedepth=15))

#Add information criterion to model
brm_successes=add_ic(brm_successes,ic=c("waic"))
brm_successes2=add_ic(brm_successes2,ic=c("waic"))

#compare
compare_ic(brm_successes,brm_successes2,ic=c("waic"))

#poisson is better but best to read more about posterior predictive checks
pp_check(brm_successes2,nsamples=1000)

#You can use marginal_effects to plot your model - it gives posterior median +- 95% credible interval
marginal_effects(brm_successes)

#This was fun! let me know if you need more help
#are you also going to calculate phylogenetic signal of your traits/response variables?




#Queen brain----
Bombus.terrestris<-(subset(Beeh.data, subset = (Beeh.data$Species == "Bombus terrestris")))
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





#Figures article----

#Figure 2
par(mfrow=c(1,2))

plot(Success.test.as.numeric ~ brain.IT, data = Success8trials.ITf, main="Success related to brain size (a)", xlab="Encephalization (Brain/IT)", ylab = "No success / success", yaxt='n')
xweight <- seq(0, 2, 0.01)
fit <- glm(Success.test ~ brain.IT, family = binomial, data = Success8trials.ITf)
yweight <- predict(fit, list(brain.IT = xweight), type="response")
lines(xweight, yweight)

plot(Success.test.as.numeric ~ residuals, data = Success8trials.ITf, main= "Success related to brain-body size residuals (b)", ylab="", xlab= "brain-body size residuals", yaxt='n')
xweight <- seq(-1, 2, 0.01)
fit <- glm(Success.test ~ residuals, family = binomial, data = Success8trials.ITf)
yweight <- predict(fit, list(residuals = xweight), type="response")
lines(xweight, yweight)

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=Success.test)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=Success.test))+
  ggtitle("Success related to encephalization (c)") 
  
  
par(mfrow=c(1,1))

#Figure 3

par(mfrow=c(1,2))


plot(n.of.success~brain.IT,data = Success8trials.ITf, xlab="Encephalization (Brain/IT)", ylab="Number of success", main="Number of success related to brain size (a)")
abline(lm(n.of.success~brain.IT,data = Success8trials.ITf))

plot(n.of.success~residuals,data = Success8trials.ITf, main= "Number of success related to residuals (b)", xlab="Encephalization residuals", ylab="Number of success")
abline(lm(n.of.success~residuals,data = Success8trials.ITf))

ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=as.factor(n.of.success))) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=as.factor(n.of.success)))+
  ggtitle("Number of success related to encephalization (c)")


par(mfrow=c(1,1))

#Figure 4

par(mfrow=c(1,2))

plot(PER.sugar.test.censored ~ brain.IT,data = Success8trials.ITf, main= "Censored time until PER in the test ~ encephalization (a)", ylim=c(0,120), xlab="Encephalization (Brain/IT)", ylab="Censored time until PER")
abline(lm(PER.sugar.test.censored ~ brain.IT,data = Success8trials.ITf))

plot(PER.sugar.test.censored ~ residuals, data = Success8trials.ITf, main= "Censored time until PER in the test ~ brain-body size residuals (b)", ylab = "", xlab="Brain ~ Body size residuals")
abline(lm(PER.sugar.test.censored ~ residuals, data = Success8trials.ITf))

par(mfrow=c(1,1))

plot(PER.sugar.test ~ brain.IT,data = Success8trials.ITf, main= "Time until PER in the test ~ encephalization (a)", ylim=c(0,120), xlab="Encephalization (Brain/IT)", ylab="Time until PER")
abline(lm(PER.sugar.test ~ brain.IT,data = Success8trials.ITf))

plot(PER.sugar.test ~ residuals,data=Success8trials.ITf, main="Time until PER in the test ~ encephalization residuals (c)", xlab="Encephalization residuals", ylab="Time until PER")
abline(lm(PER.sugar.test ~ residuals,data=Success8trials.ITf))



ggplot(Success8trials.ITf, aes(x=log(IT..mm.), y=log(Brain.weight), color=PER.sugar.test)) +
  geom_point()+
  ggtitle(" (d)") 

#Figure 5

par(mfrow=c(1,2))

plot(slope ~  brain.IT, data=Success8trials.PER, main = "Learning slopes related to encephalization (a)", ylab="Learning slopes values", xlab="Encephalization (Brain/IT)")
abline(lm(slope ~  brain.IT, data=Success8trials.PER))

plot(slope ~ residuals,data=Success8trials.PER, main = "Learning slopes related to encephalization residuals (b)", ylab="Learning slopes values", xlab="Encephalization residuals")
abline(lm(slope ~ residuals,data=Success8trials.PER))

ggplot(Success8trials.PER, aes(x=log(IT..mm.), y=log(Brain.weight), color=slope)) +
  geom_point()+
  ggtitle(" (c)") 

par(mfrow=c(1,1))

#Figure 6

boxplot(melt.last.test.done.bees$Time ~ melt.last.test.done.bees$Trial, main="Time until PER throughout the trials (a)", xlab="Trial number", ylab="Time until PER")
abline(lm(Time ~ Trial, data = melt.last.test.done.bees))

ggplot(melt.last.test.done.bees, aes(x=Trial, y=Time, color=brain.IT)) +
  geom_point()+
  ggtitle("Time until PER throughout the trials by encephalization (b)")+
  xlab("Trial number")+
  ylab("Time until PER")

ggplot(melt.last.test.done.bees2, aes(x=Trial, y=Time, color=residuals)) +
  geom_point() +
  ggtitle("Time until PER throughout the trials by encephalization residuals (c)") +
  xlab("Trial number")+
  ylab("Time until PER")



#Figures google drive-----


#Figure 2

#pdf("Figure 2.pdf")

boxplot(Time ~ Trial, data = melt.Beeh.PER.sugar, las = 1, xlab = "Trial", ylab="Time", xaxt="n",
        main="Time until touching the rewarded strip", outline=FALSE, ylim=c(0,190), col="Grey90", border = "Grey99", names=NULL)
axis(side=1,at=c(1,2,3,4,5,6,7,8),labels=c("1","2","3","4","5","6","7","Test"))
fit<-marginal_effects(brm.time.trial.negbinomial)
fits<-as.data.frame(fit$Trial)
fits$Trial
lines(fits$Trial, fits$estimate__, lwd=3, lty = "dashed")
lines(fits$Trial, fits$lower__, col = "purple", lty = "dashed")
lines(fits$Trial, fits$upper__, col = "purple", lty = "dashed")

mylevels<-levels(as.factor(melt.Beeh.PER.sugar$Trial))
levelProportions<-summary(as.factor(melt.Beeh.PER.sugar$Trial))/nrow(melt.Beeh.PER.sugar)

for(i in 1:length(mylevels)){
  
  thislevel<-mylevels[i]
  thisvalues<-melt.Beeh.PER.sugar[as.factor(melt.Beeh.PER.sugar$Trial)==thislevel, "Time"]
  
  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter<-jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.2)) 
  
}


dev.off()



#or
#marginal_effects(brm.time.trial.negbinomial)
#plot(marginal_effects(brm.time.trial.negbinomial), points = T)




#Figure 3
plot(bee.tree)
p<-ggtree(bee.tree)
p + geom_tiplab()
nodelabels()
tiplabels()

p + 
  geom_tiplab(aes(label=paste0('italic(', label, ')~italic(', bee.tree$tip.label, ')')), 
              parse=TRUE)+
  geom_cladelabel(node=16, label="0.11", align=T, color='red', offset = 0.1315, fontsize = 5.5)

p + 
  geom_tiplab(aes(label=paste0('italic(', label, ')~italic(', " ", ')')), 
              parse=TRUE)+
  geom_cladelabel(node=16, label="0.11", align=T, color='red', offset = 0.1315, fontsize = 5.5)


#pdf("Figure 3.pdf", width = 15, height = 10)
p + geom_tiplab(aes(label=paste0('italic(', label, ')~italic(', " ", ')')), 
                parse=TRUE) + ggtitle("Phylogenetic tree:", subtitle = "Body size - brain weight residuals")+
geom_cladelabel(node=16, label="0.11", align=T, color='red', offset = 0.1315, fontsize = 5.5) +
geom_cladelabel(node=1, label="-0.16", align=T, color='red', offset = 0.13, fontsize = 4.5) +
geom_cladelabel(node=2, label="0.20", align=T, color='red', offset = 0.1315, fontsize = 6) +
geom_cladelabel(node=3, label="-0.59", align=T, color='red', offset = 0.13, fontsize = 3.5) +
geom_cladelabel(node=4, label="0.02", align=T, color='red', offset = 0.1315, fontsize = 5) +
geom_cladelabel(node=5, label="0.17", align=T, color='red', offset = 0.1315, fontsize = 6) +
geom_cladelabel(node=6, label="0.66", align=T, color='red', offset = 0.1315, fontsize = 7) +
geom_cladelabel(node=7, label="-0.30", align=T, color='red', offset = 0.13, fontsize = 4) +
geom_cladelabel(node=8, label="-0.15", align=T, color='red', offset = 0.13, fontsize = 4.5) +
geom_cladelabel(node=9, label="0.50", align=T, color='red', offset = 0.1315, fontsize = 7) +
geom_cladelabel(node=10, label="0.02", align=T, color='red', offset = 0.1315, fontsize = 5) +
geom_cladelabel(node=11, label="0.02", align=T, color='red', offset = 0.1315, fontsize = 5) +
geom_cladelabel(node=12, label="-0.09", align=T, color='red', offset = 0.13, fontsize = 5) +
geom_cladelabel(node=13, label="-0.04", align=T, color='red', offset = 0.13, fontsize = 5) +
geom_cladelabel(node=14, label="0.23", align=T, color='red', offset = 0.1315, fontsize = 6) +
geom_cladelabel(node=15, label="-0.15", align=T, color='red', offset = 0.13, fontsize = 4.5) +
  geom_cladelabel(node=19, label="Halictidae", align=T, color='darkgrey', offset = 0.08, fontsize = 4.5) + 
  geom_cladelabel(node=20, label="Andrenidae", align=T, color='darkgrey', offset = 0.08, fontsize = 4.5) +
  geom_cladelabel(node=23, label="Apidae", align=T, color='darkgrey', offset = 0.08, fontsize = 4.5) +
  geom_cladelabel(node=25, label="Megachilidae", align=T, color='darkgrey', offset = 0.08, fontsize = 4.5) 
  
dev.off()


#Figure 4


dev.off()
pdf("Figure 4.pdf", height = 7, width = 7.5)
par(mfrow=c(2,2))
#1/4
unique(Success8trials.ITf$Genus)
unique(Success8trials.ITf$Species)

Success8trials.ITf$Genus<-droplevels(Success8trials.ITf$Genus)
colors()
plot(Success.test.as.numeric ~ Brain.weight, data = Success8trials.ITf, 
     main="Success related to \nbrain size (a)", xlab="Absolute brain size", cex.lab= 1.3 ,ylab = "Success learning test",
     col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus], las = 1)
#legend(x=4.2, y=0.55, legend = levels(Success8trials.ITf$Genus),
#       col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink"), pch=19, cex=0.6,ncol = 1)
box(which = "plot", lty = "solid")
fit <- marginal_effects(brm.succ8brains)
fits<-as.data.frame(fit$Brain.weight)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)

lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")
points(Success.test.as.numeric ~ Brain.weight, data = Success8trials.ITf,col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus])
#2/4
unique(Success8trials.ITf$Genus)
Success8trials.ITf$Genus<-droplevels(Success8trials.ITf$Genus)
colors()
plot(Success.test.as.numeric ~ residuals, data = Success8trials.ITf, cex.lab= 1.3,
     main="Success related to\n brain-body size residuals (b)", xlab="Brain-body size residuals", ylab = "Success learning test",
     col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus], las = 1)
fit<-marginal_effects(brm.succ8res)
fits<-as.data.frame(fit$residuals)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)

lines(fits$residuals, fits$estimate__, lwd=2)
lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")
#3/4
#capped
plot(PER.sugar.test ~ Brain.weight, data = dataformcmc.success40s, cex.lab= 1.3,
     col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus],
     main="Success time related to\n brain size (c)", xlab="Absolute brain size", ylab = "Success time learning test", ylim=c(0,75), las = 1)
fit<-marginal_effects(brm.persugarbrain.40s)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)
box(which = "plot", lty = "solid")
lines(fits$Brain.weight, fits$estimate__, lwd=2)

lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")

points(PER.sugar.test ~ Brain.weight,
       
       col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus]
       ,data = dataformcmc.success40s)
#4/4
#capped

brm.persugartest.40rs
Success8trials.ITf$Genus<-droplevels(Success8trials.ITf$Genus)

plot(PER.sugar.test ~ residuals, data = dataformcmc.success40s,cex.lab= 1.3,
     main="Success time related to \nbrain-body size residuals (d)", xlab="Brain-body size residuals", ylab = "Success time learning test",
     col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus],ylim=c(0,75), las = 1)
fit<-marginal_effects(brm.persugartest.40rs)
fits<-as.data.frame(fit$residuals)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)
box(which = "plot", lty = "solid")

lines(fits$residuals, fits$estimate__, lwd=2)

lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")
points(PER.sugar.test ~ residuals, data = dataformcmc.success40s,
       col = c("red", "blue", "darkgreen", "black","darkblue","gold","khaki","pink")[Genus])
dev.off()
##########end-----

#not capped, not presented##
brm.persugarbrain.s
unique(Success8trials.ITf$Genus)
Success8trials.ITf$Genus<-droplevels(Success8trials.ITf$Genus)
colors()

plot(PER.sugar.test ~ Brain.weight, data = dataformcmc.success, 
     main="Success time related to brain size", xlab="Brain weight", ylab = "Success learning test",
     col = c("red", "blue", "green", "black","darkblue","gold","khaki","pink")[Genus])
legend(x=5, y=70, legend = levels(Success8trials.ITf$Genus),
       col=c("red", "blue", "green", "black","darkblue","gold","khaki","pink"), pch=1)
fit<-marginal_effects(brm.persugarbrain.s)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")

###
brm.persugartest.rs
unique(Success8trials.ITf$Genus)
Success8trials.ITf$Genus<-droplevels(Success8trials.ITf$Genus)
colors()

plot(PER.sugar.test ~ residuals, data = dataformcmc.success, 
     main="Success time related to brain-body size residuals", xlab="Brain-body size residuals", ylab = "Success learning test",
     col = c("red", "blue", "green", "black","darkblue","gold","khaki","pink")[Genus])
legend(x=0.45, y=100, legend = levels(Success8trials.ITf$Genus),
       col=c("red", "blue", "green", "black","darkblue","gold","khaki","pink"), pch=1)
fit<-marginal_effects(brm.persugartest.rs)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$estimate__, lwd=2)
lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")


#Autocorrelation?--------
cor(as.numeric(Success8trials.ITf$n.of.success), as.numeric(Success8trials.ITf$Success.test))
Success8trials.ITf$Success.test
Success8trials.ITf$PER.sugar.test


plot(log(Success8trials.ITf$Brain.weight) ~ log(Success8trials.ITf$IT..mm.), xlab="Inter-tegular distance", ylab="Brain weight", 
     main="Linearized correlation")
abline(lm(log(Success8trials.ITf$Brain.weight) ~ log(Success8trials.ITf$IT..mm.)), 
       col = "purple")



View(Success8trials.ITf)
as.data.frame(summary(Success8trials.ITf$Species))
