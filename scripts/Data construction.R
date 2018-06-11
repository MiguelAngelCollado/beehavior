Beeh.data<-read.csv("~/Desktop/Tesis/R/beehavior/data/Behavior comparison.csv")
View(Beeh.data)

summary(Beeh.data$Species)
which(summary(Beeh.data$Species)>2)
