

#Predict-455
#The Show
#Travel & Tourism
#AirBnB

#Visualizations:
#1) Text visualization of reviews
#2) Basic summary graphs (average price by area, number of rentals, etc.)
#3) AirBnB rentals versus total residences
#4) Time series Paris housing index versus AirBnB growth

######################################################################################################################
setwd("C:/Users/lstottsg/Desktop/Laura School/Winter 2017/Show Data Files")
#load data
Paris_L <- read.csv("Paris Listings.csv")
Paris_R <- read.csv("Paris Reviews.csv")
ParisHousingIndex <- read.csv("Paris Housing Index.csv")

#########################################################################################
#VISUALIZATION #1: Review Text Word Frequency Bar Chart
#########################################################################################
library(wordcloud)
library(tm) 
library(quanteda)
library(tidytext)
library(SnowballC)   
library(cluster)  
library(fpc)
library(plyr)
library(dplyr)

ParisReviews <- scan("Paris Detailed Reviews.csv.gz", what = "char", sep = "\n") 
class(ParisReviews)

#PREPROCESS DATA
ParisReviews <- tolower(ParisReviews) 
ParisReviews <- stemDocument(ParisReviews) 
ParisReviews <- stripWhitespace(ParisReviews) 
ParisReviews <- removeNumbers(ParisReviews) 
ParisReviews <- removePunctuation(ParisReviews) 
ParisReviews <- removeWords(ParisReviews, stopwords("english")) 
ParisReviews <- removeWords(ParisReviews, stopwords("portuguese")) 
ParisReviews <- removeWords(ParisReviews, stopwords("french")) 
ParisReviews <- removeWords(ParisReviews, stopwords("spanish")) 
ParisReviews <- removeWords(ParisReviews, stopwords("german")) 
ParisReviews <- removeWords(ParisReviews, stopwords("italian"))

#remove words associated with rental type
ParisReviews <- removeWords(ParisReviews,c("paris","apartment", "appartement", "studio",
                                           "flat", "house", "room", "small", "clean", "bed", "lappartement")) 
#remove general descriptive words not having to do with city/community
ParisReviews <- removeWords(ParisReviews,c("comfortable", "perfect", "easy", "stay", "nice", "made",
                                           "good", "great", "recommend", "excellent", "wonderful", "lovely", "bien")) 
#remove non-descript words or possible codes
ParisReviews <- removeWords(ParisReviews,c("ã", "ì", "ñ", "ðñ", "ì", "ððñ", "æ", "ñ",
                                           "ë", "ð", "å", "í", "el", "ê", "é")) 

#translate some frequent words
ParisReviews <- gsub("trãs", "back", ParisReviews)
ParisReviews <- gsub("lokal", "local", ParisReviews)
ParisReviews <- gsub("located", "location", ParisReviews)


# strip out all non-letters and return vector 
ParisReviews.preword.vector <- unlist(strsplit(ParisReviews, "\\W")) 
# drop all empty words 
ParisReviews.vector <- ParisReviews.preword.vector[which(nchar(ParisReviews.preword.vector) > 0)] 

#word frequency
ParisReview.freq <- as.data.frame(table(unlist(ParisReviews.vector)))
ParisReview.freq <- ParisReview.freq[order(-ParisReview.freq$Freq),] 

library(ggplot2)
#plot words that appear at least 50000 times
pdf(file = "Paris Review Word Freq Bar Chart.pdf", width = 8.5, height = 8.5) 
PRfreq.plot <- ggplot(subset(ParisReview.freq, Freq>50000), aes(reorder(Var1, -Freq), Freq))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Paris Reviews: Top Words") +
  xlab("Words")
print(PRfreq.plot)
dev.off()


png(file = "Paris Review Word Freq Bar Chart.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
PRfreq.plot <- ggplot(subset(ParisReview.freq, Freq>50000), aes(reorder(Var1, -Freq), Freq))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Paris Reviews: Top Words") +
  xlab("Words")
print(PRfreq.plot)
dev.off()
 


#########################################################################################
#VISUALIZATION #1: Review Text Word Cloud
#########################################################################################

#TODO - this is not working...perhaps need to subset

#same color palette for wordcloud
my.pal <- brewer.pal(12, "Paired") 

#plot wordcloud
set.seed(1234) 
wordcloud(ParisReview.freq, min.freq = 40000, max.words = 150, random.order=FALSE, random.color=FALSE, rot.per=0.0, 
          color=my.pal, ordered.colors=FALSE, use.r.layout=FALSE, fixed.asp=TRUE) +
  title(main="Paris AirBnB Reviews")



#########################################################################################
#Non-Text Data
#########################################################################################

#set up eda data sets to play with
Paris_L.eda <- Paris_L
Paris_R.eda <- Paris_R

#Check out summary for overview and identify missing data
summary(Paris_L.eda)
summary(Paris_R.eda)

#handle NA - remove since we have a lot of data to work with
Paris_L.eda[is.na(Paris_L.eda)] <- 0
Paris_R.eda[is.na(Paris_R.eda)] <- 0

#view data dimensions
dim(Paris_L.eda)

#check out neighborhoods
neighborhoods <- as.data.frame(unique(Paris_L.eda$neighbourhood)) 

#########################################################################################
#VISUALIZATION #2: Bar Charts
#########################################################################################

Paris_L.eda
attach(Paris_L.eda)

#Mean price per neighborhood
#y-axis = mean price
#x-axis = neighborhood

#calculate mean price per neighborhood
mean.price <- data.frame(aggregate(price, by=list(neighbourhood), FUN=mean))

Paris_L.eda <- transform(Paris_L.eda, neighbourhood=reorder(neighbourhood, -price) ) 

#bar chart - mean price per neighborhood
pdf(file = "Paris Mean Price per Neighborhood.pdf", width = 8.5, height = 8.5) 
ggplot.pxbyloc <- ggplot(mean.price, aes(x = reorder(mean.price$Group.1, -mean.price$x),y = mean.price$x))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Mean Price per Neighborhood") +
  xlab("Neighborhood") +
  ylab("Mean Price in USD")
print(ggplot.pxbyloc)
dev.off()

png(file = "Paris Mean Price per Neighborhood.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot.pxbyloc <- ggplot(mean.price, aes(x = reorder(mean.price$Group.1, -mean.price$x),y = mean.price$x))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Mean Price per Neighborhood") +
  xlab("Neighborhood") +
  ylab("Mean Price in USD")
print(ggplot.pxbyloc)
dev.off()

#############################################################################################################################
#GGPlot: Stacked Bar Charts
#y-axis = price
#x-axis = minimum nights
#category = neighborhood group

visalength <- Paris_L.eda[ which(minimum_nights>=90 ), ] #no rentals with a min of 90 nights (visa requirments)
longterm <- Paris_L.eda[ which(minimum_nights<90 & minimum_nights>=30), ]
midterm <- Paris_L.eda[ which(minimum_nights<30 & minimum_nights>=20), ]
shortterm <- Paris_L.eda[ which(minimum_nights<20 & minimum_nights>=4), ]
quicktrip <- Paris_L.eda[ which(minimum_nights<4 ), ]

#Quick Trip Rental
pdf(file = "Paris Quick Trip Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.quicktrip <- ggplot(quicktrip, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Quick Trips Allowed\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night")
print(ggplot.quicktrip)
dev.off()

png(file = "Paris Quick Trip Rental Price by Room Type.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot.quicktrip <- ggplot(quicktrip, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Quick Trips Allowed\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night")
print(ggplot.quicktrip)
dev.off()

#Short Term Rental
pdf(file = "Paris Short Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.shortterm <- ggplot(shortterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Short Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.shortterm)
dev.off()

png(file = "Paris Short Term Rental Price by Room Type.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot.shortterm <- ggplot(shortterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Short Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.shortterm)
dev.off()

#Midterm Rental
pdf(file = "Paris Medium Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.midterm <- ggplot(midterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Medium Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.midterm)
dev.off()

png(file = "Paris Medium Term Rental Price by Room Type.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot.midterm <- ggplot(midterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Medium Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.midterm)
dev.off()

#Longterm Rental
pdf(file = "Paris Long Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.longterm <- ggplot(longterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Longer Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.longterm)
dev.off()

png(file = "Paris Long Term Rental Price by Room Type.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot.longterm <- ggplot(longterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Longer Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.longterm)
dev.off()


#########################################################################################
#VISUALIZATION #4: Time Series
#########################################################################################
ParisHI.eda <- ParisHousingIndex
summary(ParisHI.eda)

#########################################################################################
#VISUALIZATION #5: Dotplot
#########################################################################################

#simple lattice connected dotplot
lattice.pxbyloc <- dotplot(mean.price$Group.1 ~ mean.price$x , data = Paris_L.eda, type = "o",
                           auto.key = list(space = "right", points = TRUE, lines = FALSE), xlab = "Mean Price", 
                           ylab = "Neighborhood", main = "Mean Price by Location")
print(lattice.pxbyloc)

#########################################################################################
#VISUALIZATION #7: Pie Chart (making same look as Erin's)
#########################################################################################
property_counts <- read.csv("paris_listed3.csv", header=TRUE, sep=",", as.is=TRUE)

listingIds <- length(unique(Paris_L$id))
listingIds
#gives 52,725 listing ids

#per Kayla's stats, 1,356,074 total Paris residences in 2011

#so non-AirBnB residences 1303349 and AirBnB 52725 (making huge assumptions given year difference and no clue what areas are included)

#Unlisted Paris residences versus AirBnB properties
pdf(file = "Unlisted Paris residences versus AirBnB properties.pdf", width = 8.5, height = 8.5) 
slices <- c(1303349, 52725) 
lbls <- c("Non-AirBnB", "AirBnB Listed")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Paris Residences: Unlisted vs AirBnB Listed Properties | AirBnB in Paris")
dev.off()

png(file = "Unlisted Paris residences versus AirBnB properties.png",width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
slices <- c(1303349, 52725) 
lbls <- c("Non-AirBnB", "AirBnB Listed")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Paris Residences: Unlisted vs AirBnB Listed Properties | AirBnB in Paris")
dev.off()

#******************************************************************************************************************************************
#Erin's Code
#******************************************************************************************************************************************
property_counts <- read.csv("/Volumes/WD/NU/predict_455/Group_assignments/CheckPointC/working_files/paris_listed3.csv", header=TRUE, sep=",", as.is=TRUE)

pdf(file = "/Volumes/WD/NU/predict_455/Group_assignments/CheckPointC/working_files/paris_barchart.pdf", width = 11, height = 8.5)

ggplot(data=property_counts, aes(x=properties, y=owners)) +
  geom_bar(colour="chocolate4", fill="chocolate1", width=0.8,stat="identity") +
  xlab("Numbers of Properties Owned") + ylab("Numbers of Owners") +
  ggtitle("The Total Numbers of AirBnB Rental Properties Owned in Paris")

dev.off()

slices <- c(2854, 524872) 
lbls <- c("Owned Multiple Propertities", "Owned Single Property")
          pct <- round(slices/sum(slices)*100)
          lbls <- paste(lbls, pct)
          lbls <- paste(lbls,"%",sep="")
          pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Owned Single Property Versus Multiple Properties | AirBnB in Paris")
          
          pdf(file = "/Volumes/WD/NU/predict_455/Group_assignments/CheckPointC/working_files/paris_piechart.pdf", width = 11, height = 8.5)
          
          slices <- c(2854, 524872) 
          lbls <- c("Owned Multiple Propertities", "Owned Single Property")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Owned Single Property Versus Multiple Properties | AirBnB in Paris")
dev.off()








