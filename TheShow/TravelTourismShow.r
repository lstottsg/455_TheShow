
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
Paris_L <- read.csv("Paris Listings.csv", encoding = "UTF-8")
Paris_R <- read.csv("Paris Reviews.csv", encoding = "UTF-8")
ParisHousingIndex <- read.csv("Paris Housing Index.csv")
ParisHousingIndex_Adj <- read.csv("Paris Housing Index_adj.csv")
ParisReview_Short <- read.csv("ParisReviews_Short.csv", encoding = "UTF-8")
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
library(textcat)

ParisReviews <- scan("Paris Detailed Reviews.csv.gz", what = "char", sep = "\n", encoding = "UTF-8") 
class(ParisReviews)
head(ParisReviews)

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
                                           "good", "great", "recommend", "excellent", "wonderful", "lovely", "bien", "can",
                                           "get", "just", "definitely", "even", "très", "us")) 
#remove non-descript words or possible codes
ParisReviews <- removeWords(ParisReviews,c("ã", "ì", "ñ", "ðñ", "ì", "ððñ", "æ", "ñ",
                                           "ë", "ð", "å", "í", "el", "ê", "é")) 

#what languages are these???
textcat(c("ã", "ì", "ñ", "ðñ", "ì", "ððñ", "æ", "ñ",
          "ë", "ð", "å", "í", "el", "ê", "é"))

#can we see percent reviews per language?
class(ParisReview_Short)
languages <- as.data.frame(textcat(ParisReview_Short$comments))
unique_lang <- unique(languages)
summary(languages)

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

head(ParisReview.freq)

library(ggplot2)
#plot words that appear at least 50000 times
pdf(file = "Paris Review Word Freq Bar Chart.pdf", width = 8.5, height = 8.5) 
PRfreq.plot <- ggplot(subset(ParisReview.freq, Freq>50000), aes(reorder(Var1, -Freq), Freq))  +  
  geom_bar(stat="identity", color="LightGreen", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Paris Reviews: Top Words") +
  xlab("Words Used in Reviews") +
  ylab("Word Frequency")
print(PRfreq.plot)
dev.off()


svg(file = "Paris Review Word Freq Bar Chart.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
PRfreq.plot <- ggplot(subset(ParisReview.freq, Freq>50000), aes(reorder(Var1, -Freq), Freq))  +  
  geom_bar(stat="identity", color="LightGreen", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Paris Reviews: Top Words") +
  xlab("Words Used in Reviews") +
  ylab("Word Frequency")
print(PRfreq.plot)
dev.off()
 


#########################################################################################
#VISUALIZATION #1: Review Text Word Cloud
#########################################################################################

#TODO - this is not working...perhaps need to subset

corp <- corpus(VectorSource(ParisReviews.vector))
dtm <- DocumentTermMatrix(corp)

#tell R this is a plaintext doc       
docs <- tm_map(ParisReviews, PlainTextDocument)   

#STAGE AND EXPLORE DATA

#create matrix from corpus
dtm <- DocumentTermMatrix(docs)   
dtm   
#inspect(dtm)
reviews.dtm <- DocumentTermMatrix(ParisReviews.vector)  

#same color palette for wordcloud
my.pal <- brewer.pal(12, "Paired") 

#plot wordcloud
set.seed(1234) 
wordcloud(subset(ParisReview.freq, Freq>40000), min.freq = 40000, max.words = 150, random.order=FALSE, random.color=FALSE, rot.per=0.0, 
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
  geom_bar(stat="identity", color="LightGreen", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Mean Price per Neighborhood") +
  xlab("Neighborhood") +
  ylab("Mean Price in USD")
print(ggplot.pxbyloc)
dev.off()

svg(file = "Paris Mean Price per Neighborhood.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
ggplot.pxbyloc <- ggplot(mean.price, aes(x = reorder(mean.price$Group.1, -mean.price$x),y = mean.price$x))  +  
  geom_bar(stat="identity", color="LightGreen", fill="Gray")   +
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
attach(Paris_L.eda)
plot.df <- data.frame(minimum_nights, room_type, price)
detach(Paris_L.eda)
attach(plot.df)

visalength <- plot.df[ which(minimum_nights>=90 ), ] #no rentals with a min of 90 nights (visa requirments)
longterm <- plot.df[ which(minimum_nights<90 & minimum_nights>=30), ]
midterm <- plot.df[ which(minimum_nights<30 & minimum_nights>=20), ]
shortterm <- plot.df[ which(minimum_nights<20 & minimum_nights>=4), ]
quicktrip <- plot.df[ which(minimum_nights<4 ), ]
detach(plot.df)
#this gives us NA values for all missing minimum_night prices (convert minimum_nights and room_type to factor to retrieve levels)
longterm.ex <- cbind(expand.grid(minimum_nights=levels(as.factor(longterm$minimum_nights)), room_type=levels(longterm$room_type), price=0))
midterm.ex <- cbind(expand.grid(minimum_nights=levels(as.factor(midterm$minimum_nights)), room_type=levels(midterm$room_type), price=NA))
shortterm.ex <- cbind(expand.grid(minimum_nights=levels(as.factor(shortterm$minimum_nights)), room_type=levels(shortterm$room_type), price=NA))

#now add rows for all NA values to hold space in dodged bar chart
longterm.plot <- rbind(longterm, longterm.ex)
midterm.plot <- rbind(midterm, midterm.ex)
shortterm.plot <- rbind(shortterm, shortterm.ex)

#Quick Trip Rental
pdf(file = "Paris Quick Trip Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.quicktrip <- ggplot(quicktrip, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity", width = 0.8, alpha=.25) +
  ggtitle("Price by Room Type: Quick Trips Allowed\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") 
print(ggplot.quicktrip)
dev.off()

svg(file = "Paris Quick Trip Rental Price by Room Type.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
ggplot.quicktrip <- ggplot(quicktrip, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity", width = 0.8, alpha=.25) +
  ggtitle("Price by Room Type: Quick Trips Allowed\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type")
print(ggplot.quicktrip)
dev.off()

#Short Term Rental
pdf(file = "Paris Short Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.shortterm <- ggplot(shortterm.plot, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity", width=.8, alpha=.25) +
  ggtitle("Price by Room Type: Short Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type")
print(ggplot.shortterm)
dev.off()

svg(file = "Paris Short Term Rental Price by Room Type.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
ggplot.shortterm <- ggplot(shortterm.plot, aes( x = reorder(minimum_nights, as.integer(minimum_nights)), y = price, fill = room_type )) + 
  geom_bar( position = "dodge", width = 0.8, stat = "identity", alpha=.25) +
  ggtitle("Price by Room Type: Short Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") +
  scale_x_discrete()
print(ggplot.shortterm)
dev.off()

#Midterm Rental
pdf(file = "Paris Medium Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.midterm <- ggplot(midterm.plot, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity", width=.8, alpha=.25) +
  ggtitle("Price by Room Type: Medium Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") 
print(ggplot.midterm)
dev.off()

svg(file = "Paris Medium Term Rental Price by Room Type.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
ggplot.midterm <- ggplot(midterm.plot, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", width = 0.8, stat = "identity", alpha=.25) +
  ggtitle("Price by Room Type: Medium Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") 
print(ggplot.midterm)
dev.off()

#Longterm Rental
pdf(file = "Paris Long Term Rental Price by Room Type.pdf", width = 8.5, height = 8.5) 
ggplot.longterm <- ggplot(longterm.plot, aes( x = minimum_nights, y = price, fill = room_type, width=.9 )) + 
  geom_bar( position = "dodge", width = 0.8, stat = "identity", alpha=.25) +
  ggtitle("Price by Room Type: Longer Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") 
print(ggplot.longterm)
dev.off()


svg(file = "Paris Long Term Rental Price by Room Type.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
ggplot.longterm <- ggplot(longterm.plot, aes( x = minimum_nights, y = price, fill = room_type )) + 
  geom_bar( position = "dodge", stat = "identity", alpha=.25) +
  ggtitle("Price by Room Type: Longer Term Rentals\n") +
  xlab("\nMinimum Nights") +
  ylab("Price per Night USD") +
  scale_fill_discrete(name = "Room Type") 
print(ggplot.longterm)
dev.off()


#########################################################################################
#VISUALIZATION #4: Time Series
#########################################################################################
library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(zoo)  # utilities for working with time series
library(reshape)
library(grid)
library(scales)

ParisHI.eda <- ParisHousingIndex[1,]
rownames(ParisHI.eda) <- c("ParisFlats")

summary(ParisHI.eda)

#remove the 'X' from the year in the colname
colnames(ParisHI.eda) <- gsub('X', '', colnames(ParisHI.eda))

ParisHI.t <- setNames(data.frame(t(ParisHI.eda[,-1])),ParisHI.eda[,1])

ParisHI.ts <- ts(ParisHI.t$`Paris - Flats`, start=c(2006), end=c(2016),frequency = 4)
ParisHI.xts <- as.xts(ParisHI.ts)

chartSeries(ParisHI.xts,theme="white", major.ticks="years", minor.ticks=FALSE, show.grid=FALSE, line.type="l", title="Paris Housing Index")

# plot series using standard R graphics 
plot(ParisHI.ts,main="Paris Housing Index - Flats 2006 to 2016", col="blue", xlab="Year", ylab="Index Level")

# plot series using ggplot 

ParisHI.df <- as.data.frame(ParisHousingIndex_Adj)
class(ParisHI.df)

paris.index <- ggplot(ParisHI.df, aes(Quarter,IndexLevelAdj)) + 
  geom_line() +
  ggtitle("Paris Housing Index: 2006 to 2016\n") +
  xlab("\nQuarter") +
  ylab("Index Level - Adjusted for Inflation")

print(paris.index)  

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

summary(languages)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

slices <- c(275558, 124200, 92925, 28531, 23359, 119026) 
lbls <- c("English", "French", "Scots", "Catalan", "Afrikaans", "Other")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
lines(c(1.45, 1.45, 1.45, 1.85, 1.35, 1.35))

svg(file = "Paris Review Language Pie Chart.svg",
    bg = "white")
pie(slices,labels = lbls, clockwise=TRUE, col=adjustcolor(gg_color_hue(length(lbls)),alpha.f = .4),main="Paris AirBnB Number of Reviews by Language", radius=1.4)
dev.off()

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

svg(file = "Unlisted Paris residences versus AirBnB properties.svg",width = 7, height = 7, pointsize = 12,
    bg = "white")
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
