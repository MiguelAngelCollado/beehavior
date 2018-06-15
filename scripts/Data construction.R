Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")
View(Beeh.data)

summary(Beeh.data$Species)
summary(Beeh.data$Place)

which(summary(Beeh.data$Species)>2)
