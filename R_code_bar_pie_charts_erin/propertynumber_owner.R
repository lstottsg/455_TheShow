property_counts <- read.csv("paris_listed3.csv", header=TRUE, sep=",", as.is=TRUE)

pdf(file = "paris_barchart.pdf", width = 11, height = 8.5)

ggplot(data=property_counts, aes(x=properties, y=owners)) +
  geom_bar(color="LightGreen", fill="Gray", width=0.8,stat="identity") +
  xlab("Numbers of Properties Owned") + ylab("Numbers of Owners") +
  ggtitle("The Total Numbers of AirBnB Rental Properties Owned in Paris")

dev.off()

svg(file = "paris_barchart.svg", width = 11, height = 8.5)

ggplot(data=property_counts, aes(x=properties, y=owners)) +
  geom_bar( color="LightGreen", fill="Gray", width=0.8,stat="identity") +
  xlab("Numbers of Properties Owned") + ylab("Numbers of Owners") +
  ggtitle("The Total Numbers of AirBnB Rental Properties Owned in Paris")

dev.off()

png(file = "paris_barchart.png", width = 800, height = 600)

ggplot(data=property_counts, aes(x=properties, y=owners)) +
  geom_bar(color="LightGreen", fill="Gray", width=0.8,stat="identity") +
  xlab("Numbers of Properties Owned") + ylab("Numbers of Owners") +
  ggtitle("The Total Numbers of AirBnB Rental Properties Owned in Paris")

dev.off()



slices <- c(2854, 524872) 
lbls <- c("Owned Multiple Propertities", "Owned Single Property)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Owned Single Property Versus Multiple Properties | AirBnB in Paris")

pdf(file = "/Volumes/WD/NU/predict_455/Group_assignments/CheckPointC/working_files/paris_piechart.pdf", width = 11, height = 8.5)

slices <- c(2854, 524872) 
lbls <- c("Owned Multiple Propertities", "Owned Single Property)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Owned Single Property Versus Multiple Properties | AirBnB in Paris")
dev.off()