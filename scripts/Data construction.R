Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")
View(Beeh.data)

#Data treatment-----
#PER variables must be numerical
str(Beeh.data)
Beeh.data$PER.sugar1<-as.numeric(Beeh.data$PER.sugar1)
Beeh.data$PER.sugar2<-as.numeric(Beeh.data$PER.sugar2)
Beeh.data$PER.sugar3<-as.numeric(Beeh.data$PER.sugar3)
Beeh.data$PER.sugar4<-as.numeric(Beeh.data$PER.sugar4)
Beeh.data$PER.sugar5<-as.numeric(Beeh.data$PER.sugar5)
Beeh.data$PER.sugar6<-as.numeric(Beeh.data$PER.sugar6)
Beeh.data$PER.sugar7<-as.numeric(Beeh.data$PER.sugar7)
Beeh.data$PER.sugar.test<-as.numeric(Beeh.data$PER.sugar.test)
Beeh.data$PER.water1<-as.numeric(Beeh.data$PER.water1)
Beeh.data$PER.water2<-as.numeric(Beeh.data$PER.water2)
Beeh.data$PER.water3<-as.numeric(Beeh.data$PER.water3)
Beeh.data$PER.water4<-as.numeric(Beeh.data$PER.water4)
Beeh.data$PER.water5<-as.numeric(Beeh.data$PER.water5)
Beeh.data$PER.water6<-as.numeric(Beeh.data$PER.water6)
Beeh.data$PER.water.test<-as.numeric(Beeh.data$PER.water.test)

#####

which(summary(Beeh.data$Species)>2)

Beeh.data
Beeh.PER.sugar<-data.frame(Beeh.data$ID,Beeh.data$Species,Beeh.data$PER.sugar1,Beeh.data$PER.sugar2,Beeh.data$PER.sugar3,Beeh.data$PER.sugar4,Beeh.data$PER.sugar5,Beeh.data$PER.sugar6,Beeh.data$PER.sugar7,Beeh.data$PER.sugar.test)
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

par(mfrow = c(3,3))
for (n in 1:(nrow(Beeh.PER.sugar))) {
  print(plot(t(Beeh.PER.sugar[n,(3:10)]), xlab="Trial number", ylab = "Time"))
  lines(t(Beeh.PER.sugar[n,(3:10)]))
  
}
plot(t(Beeh.PER.sugar[1,(3:10)]), xlab="Trial number", ylab = "Time")
lines(t(Beeh.PER.sugar[1,(3:10)]))
plot(t(Beeh.PER.sugar[2,(3:10)]), xlab="Trial number", ylab = "Time")
lines(t(Beeh.PER.sugar[2,(3:10)]))



print(plot(t(activity.decline[n,]), ylim = c(0,1), xlab = "Trial", ylab = "activity.prop", main = row.names(activity.decline[n,])))
lines(t(activity.decline[n,]), ylim = c(0,1))

plot()
points(Beeh.data[16,14],Beeh.data[16,18],Beeh.data[16,22])
colnames(Beeh.data)


innovation.curve<- survfit(Surv(virtual.success.time5, success5) ~ experiment.type, na.action = na.exclude, data = explain.innovation1.full) 
plot(innovation.curve, lty = 1:2, xlab="Virtual success time 5", ylab="% of no success in trial 5", main= "Kapdddlan-Meier Curves\nfor innovation") 
legend(10000, .4, c("Control","Treatment"), lty = 1:2) 

#There is no difference in Treatment vs Control curves
survdiff (Surv(virtual.success.time5, success5) ~ experiment.type, na.action = na.exclude, data = explain.innovation1.full)

points(Beeh.data[1,1])
