
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
#VISUALIZATION #1: Review Text WordCloud
#########################################################################################
library(wordcloud)
library(tm) 
library(quanteda)
library(tidytext)
library(SnowballC)   
library(cluster)  
library(fpc)
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
PRfreq.plot <- ggplot(subset(ParisReview.freq, Freq>50000), aes(reorder(Var1, -Freq), Freq))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Paris Reviews: Top Words") +
  xlab("Words")

PRfreq.plot 


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

#and now we have no missing values
sum(is.na(valid_survey_input.eda))

#view data dimensions
dim(Paris_L.eda)

#check out neighborhoods
neighborhoods <- as.data.frame(unique(Paris_L.eda$neighbourhood)) 

#########################################################################################
#VISUALIZATION #2: Bar Charts
#########################################################################################
library(ggplot2)
library(plyr)
library(tibble)
library(reshape)
library(dplyr)
library(stringr)
library(readr)
library(corrplot)

#Mean price per neighborhood
#y-axis = mean price
#x-axis = neighborhood

#calculate mean price per neighborhood
mean.price <- data.frame(aggregate(price, by=list(neighbourhood), FUN=mean))

Paris_L.eda <- transform(Paris_L.eda, neighbourhood=reorder(neighbourhood, -price) ) 

#bar chart - mean price per neighborhood
ggplot.pxbyloc <- ggplot(mean.price, aes(x = reorder(mean.price$Group.1, -mean.price$x),y = mean.price$x))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Mean Price per Neighborhood") +
  xlab("Neighborhood") +
  ylab("Mean Price in USD")

print(ggplot.pxbyloc)

#############################################################################################################################
#GGPlot: Stacked Bar Charts

Paris_L.eda
attach(Paris_L.eda)

visalength <- Paris_L.eda[ which(minimum_nights>=90 ), ] #no rentals with a min of 90 nights (visa requirments)
longterm <- Paris_L.eda[ which(minimum_nights<90 & minimum_nights>=30), ]
midterm <- Paris_L.eda[ which(minimum_nights<30 & minimum_nights>=20), ]
shortterm <- Paris_L.eda[ which(minimum_nights<20 & minimum_nights>=4), ]
quicktrip <- Paris_L.eda[ which(minimum_nights<4 ), ]

#Stacked bar chart showing the proportion of student preference in software language, stacked by interest type
#y-axis = price
#x-axis = minimum nights
#category = neighborhood group

#Quick Trip Rental
ggplot.quicktrip <- ggplot(quicktrip, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Quick Trips Allowed\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.quicktrip)

#Short Term Rental
ggplot.shortterm <- ggplot(shortterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Short Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.shortterm)

#Midterm Rental
ggplot.midterm <- ggplot(midterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Medium Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.midterm)

#Longterm Rental
ggplot.longterm <- ggplot(longterm, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity") +
  ggtitle("Price by Room Type: Longer Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night") 
print(ggplot.longterm)


#########################################################################################
#VISUALIZATION #: Dotplot
#########################################################################################

#simple lattice connected dotplot
lattice.pxbyloc <- dotplot(mean.price$Group.1 ~ mean.price$x , data = Paris_L.eda, type = "o",
                           auto.key = list(space = "right", points = TRUE, lines = FALSE), xlab = "Mean Price", 
                           ylab = "Neighborhood", main = "Mean Price by Location")
print(lattice.pxbyloc)


#########################################################################################
#VISUALIZATION #4: Time Series
#########################################################################################
ParisHI.eda <- ParisHousingIndex
summary(ParisHI.eda)




