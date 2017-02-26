#Boxplot
paris_data <- read.csv(file = "Paris_data_revised.csv")
boxplot(Paris_data_revised$X..listings.per.id, main="Box Plot of Number of Rentals by Host ID",horizontal = TRUE)

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

#Time Series
rental_numbers <- read.csv(file="Rental Numbers.csv")
ggplot(rental_numbers, aes(x=rental_numbers$ï..Year, y=rental_numbers$Number.Rentals))+geom_line()+geom_point() +
xlab("Number of Rentals Listed") +
ylab("Year") +
ggtitle("Number of AirBnB Rentals Listed in Paris by Year")

