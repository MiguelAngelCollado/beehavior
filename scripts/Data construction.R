
library(dplyr)
library(visreg)
library(phytools)
library(DHARMa)
library(effects)
library(lme4)
library(ggplot2)
library(reshape2)
library(rsq)
library(MuMlm)
library(optiRum)
library(survival)
require(lme4)
require(MuMIn)

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
Success8trials<-as.factor(Success8trials)

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







#per.sugar.test.block----
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

##survival curves


cox.cue.time<- coxph(Surv(PER.sugar.test.censored, success.test.logi) ~ brain.IT, na.action = na.exclude, data = Success8trials.ITf) 
cox.cue.time



PERsugar.lmer<-lmer(PER.sugar.test ~ brain.IT + (1|Genus/Species),data = Success8trials.ITf)
summary(PERsugar.lmer)

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


PERsugar.res.lmer<-lmer(PER.sugar.test ~ residuals + (1|Genus/Species), data=Success8trials.ITf)
#Terrible
summary(PERsugar.res.lmer)


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


#OTHER ANALYSIS---------
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
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
#install.views('Phylogenetics')
#update.views('Phylogenetics')

#Librería para leer y modificar árboles filogenéticos
library(ape)

#LOAD IN YOUR TREE, by Hedtke et al 2013
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
plot(t1)
#Structure, plot and genus of the phylo tree
str(t1)
plot(t2)
t2$tip.label

PERsugar.success.mean
unique(Success8trials.ITf$Genus)
levels(Success8trials.ITf$Genus)

# We drop tips except "Andrena", "Apis", "Bombus", "Lasioglossum", 
# "Osmia", "Rhodanthidium"


#All trees are the same for our genus levels, except from tree 1, we pick tree2

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
                            "Eucera", "Chilicola",
                            
                            "Xylocopa" 
))

tree2$tip.label


#Create tree for our data
plot(tree2)
treep<-tree2
plot(treep)

treep$edge.length


#The model to change the tree
#treep<-bind.tip(tree = treep, tip.label = "", where = , position = )

#Tree modification, we add polytomies
plot(treep)
summary(Success8trials.ITf$Species)
nodelabels()
tiplabels()

#This is the branch lengths
treep$edge.length
#And this is the position of those branch lengths
treep$edge

#So now we add the politomies with the same branch lengths 
treep<-bind.tip(tree = treep, tip.label = "Andrena", where = 8, edge.length = 0)
treep<-bind.tip(tree = treep, tip.label = "Andrena", where = 8, edge.length = 0)
treep<-bind.tip(tree = treep, tip.label = "Andrena", where = 8, edge.length = 0)
treep<-bind.tip(tree = treep, tip.label = "Andrena", where = 8, edge.length = 0)



plot(treep)
nodelabels()
tiplabels()
treep$edge.length
treep$edge
summary(Success8trials.ITf$Species)

treep<-bind.tip(tree = treep, tip.label = "Lasioglossum", where = 6, edge.length = 0)

plot(treep)
nodelabels()
tiplabels()
treep$edge.length
treep$edge


summary(Success8trials.ITf$Species)

treep<-bind.tip(tree = treep, tip.label = "Bombus", where = 4, edge.length = 0)
treep<-bind.tip(tree = treep, tip.label = "Bombus", where = 4, edge.length = 0)
treep<-bind.tip(tree = treep, tip.label = "Bombus", where = 4, edge.length = 0)

plot(treep)
nodelabels()
tiplabels()
treep$edge.length
#Let's rename our personal phylotree
tree<-treep
plot(tree)
summary(Success8trials.ITf$Species)

tree$tip.label[1]<-"Rhodanthidium sticticum"
tree$tip.label[2]<-"Osmia latreillei"
tree$tip.label[3]<-"Megachile willughbiella"
tree$tip.label[4]<-"Bombus terrestris"
tree$tip.label[5]<-"Bombus pratorum"
tree$tip.label[6]<-"Bombus pascuorum"
tree$tip.label[7]<-"Psithyrus vestalis"
tree$tip.label[8]<-"Apis mellifera"
tree$tip.label[9]<-"Lasioglossum malachurum"
tree$tip.label[10]<-"Lasioglossum immunitum"
tree$tip.label[11]<-"Flavipanurgus venustus"
tree$tip.label[12]<-"Andrena angustior"
tree$tip.label[13]<-"Andrena hispania"
tree$tip.label[14]<-"Andrena flavipes"
tree$tip.label[15]<-"Andrena pilipes"
tree$tip.label[16]<-"Andrena sp."
tree$tip.label[17]<-"Duckeanthidium sp."
plot(tree)

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

#Autocorrelation?
cor(as.numeric(Success8trials.ITf$n.of.success), as.numeric(Success8trials.ITf$Success.test))
Success8trials.ITf$Success.test
Success8trials.ITf$PER.sugar.test



