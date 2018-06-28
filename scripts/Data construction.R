library(dplyr)
Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")
View(Beeh.data)

#Useless bees and useful bees----

######
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

######


#Initial exploration----
summary(Beeh.data$Genus)
summary(Beeh.data$Species)
which(summary(Beeh.data$Species)>2)

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
Beeh.data$PER.water.test<-as.numeric(as.character(Beeh.data$PER.water.test))


Beeh.data
nrow(Beeh.data)
nrow(Beeh.PER.sugar)
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

plot(t(Beeh.PER.sugar[16,(3:10)]), xlab="Trial number", ylab = "Time")
lines(t(Beeh.PER.sugar[16,(3:10)]))

View(Beeh.PER.sugar)
is.na(Beeh.PER.sugar)
par(mfrow = c(3,3))
n=1

#Number of individuals that reacted to the test#

reactors<-NULL
for (n in 1:(nrow(Beeh.PER.sugar))) {
  if(is.na(Beeh.PER.sugar$PER.sugar1[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar2[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar3[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar4[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar5[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar6[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar7[n]) & 
     is.na(Beeh.PER.sugar$PER.sugar.test[n])==TRUE){
    reactors[n]<-NA}else{                                                                                                                                                                                                                       
    reactors[n]<-Beeh.PER.sugar$Species[n]  
    }}
reactors
#The number is
length(which(is.na(reactors)==FALSE))

#Graphs of every individual for PER sugar
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



