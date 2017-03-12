#Boxplot
paris_data <- read.csv(file = "cities/paris/Paris_data_revised.csv")
boxplot(paris_data$X..listings.per.id, main="Box Plot of Number of Rentals by Host ID",horizontal = TRUE,xlab = "Number of listings per host")


png(file = "graphs/placeholders/png/Box Plot Number of Rentals by Host ID.png", width = 800, height = 600)
boxplot(paris_data$X..listings.per.id, main="Box Plot of Number of Rentals by Host ID",horizontal = TRUE,xlab = "Number of listings per host")
dev.off()

svg(file = "graphs/placeholders/SVG/Box Plot Number of Rentals by Host ID.svg", width = 8.5, height = 7.0)
boxplot(paris_data$X..listings.per.id, main="Box Plot of Number of Rentals by Host ID",horizontal = TRUE,xlab = "Number of listings per host")
dev.off()


library(extrafontdb)
library(ggplot2)
library(waffle)
library(Rttf2pt1)

#Waffle Chart to indicate proportion of Paris Host IDs with more than 1 listing
host_counts <- c(41547,3019,165,126)
names(host_counts) <- c("1","2-4","5-10",">10")
host_counts

number_listings <- read.csv(file="Number_listings.csv")
waffle(host_counts/100, title="Number of Rentals Per Host ID", xlab="One Square Equals 100 Host IDs")

png(file = "graphs/placeholders/png/Waffle Number of Rentals Per Host ID.png", width = 800, height = 600)
waffle(host_counts/100, title="Number of Rentals Per Host ID", xlab="One Square Equals 100 Host IDs")
dev.off()

svg(file = "graphs/placeholders/SVG/Waffle Number of Rentals Per Host ID.svg", width = 8.5, height = 7.0)
waffle(host_counts/100, title="Number of Rentals Per Host ID", xlab="One Square Equals 100 Host IDs")
dev.off()

#Time Series
rental_numbers <- read.csv(file="cities/paris/Rental Numbers.csv")
ggplot(rental_numbers, aes(x=rental_numbers$Year, y=rental_numbers$Number.Rentals))+geom_line()+geom_point() +
  ylab("Number of Rentals Listed") +
  xlab("Year") +
  scale_x_continuous(breaks=c(2015,2016,2017)) +
  ggtitle("Number of AirBnB Rentals Listed in Paris by Year")

png(file = "graphs/placeholders/png/Number of Rentals in Paris by Year.png", width = 800, height = 600)
ggplot(rental_numbers, aes(x=rental_numbers$Year, y=rental_numbers$Number.Rentals))+geom_line()+geom_point() +
  ylab("Number of Rentals Listed") +
  xlab("Year") +
  scale_x_continuous(breaks=c(2015,2016,2017)) +
  ggtitle("Number of AirBnB Rentals Listed in Paris by Year")
dev.off()

svg(file = "graphs/placeholders/SVG/Number of Rentals in Paris by Year.svg", width = 8.5, height = 7.0)
ggplot(rental_numbers, aes(x=rental_numbers$Year, y=rental_numbers$Number.Rentals))+geom_line()+geom_point() +
  ylab("Number of Rentals Listed") +
  xlab("Year") +
  scale_x_continuous(breaks=c(2015,2016,2017)) +
  ggtitle("Number of AirBnB Rentals Listed in Paris by Year")
dev.off()

