
#Predict-455
#The Show
#Travel & Tourism
#AirBnB

#Visualizations:
#1) 
#2) 
#3) 

#########################################################################################
#Set up data for exploration
#########################################################################################

#load libraries
library(lattice) 
library(ggplot2)
library(plyr)
library(tibble)
library(reshape)
library(dplyr)
library(stringr)
library(readr)
library(corrplot)
library(wordcloud)
library(tm) 
library(quanteda)
library(tidytext)
library(SnowballC)   
library(cluster)  
library(fpc)
library(dplyr)
library(tidyr)


# cities focused on in HBR article
#Lisbon <- read.csv("Lisbon.csv")
#JoshuaTree <- read.csv("JoshuaTree.csv")
#Reykjavik <- read.csv("Reykjavik.csv")

# supplementary cities mentioned in HBR article
Paris_L <- read.csv("./cities/paris/listings.csv")
Paris_R <- read.csv("./cities/paris/reviews.csv")
NewYork_L <- read.csv("./cities/new york/listings.csv")
NewYork_R <- read.csv("./cities/new york/reviews.csv")
Barcelona_L <- read.csv("./cities/barcelona/listings.csv")
Barcelona_R <- read.csv("./cities/barcelona/reviews.csv")

#detailed reviews for text analysis
#ParisDetRev <- read.csv("Paris Detailed Reviews.csv.gz")
#NewYorkDetRev <- read.csv("New York Detailed Reviews.csv.gz")
#BarcelonaDetRev <- read.csv("Barcelona Detailed Reviews.csv.gz")

######################################################################################################################
#Begin EDA
######################################################################################################################

#set up eda data sets to play with
#Lisbon.eda <- Lisbon
#JoshuaTree.eda <- JoshuaTree
#Reykjavik.eda <- Reykjavik

Paris_L.eda <- Paris_L
Paris_R.eda <- Paris_R
NewYork_L.eda <- NewYork_L
NewYork_R.eda <- NewYork_R
Barcelona_L.eda <- Barcelona_L
Barcelona_R.eda <- Barcelona_R

ParisDetRev.eda <- ParisDetRev
NewYorkDetRev.eda <- NewYorkDetRev
BarcelonaDetRev.eda <- BarcelonaDetRev
  
#Check out summary for overview and identify missing
#summary(Lisbon.eda)
#summary(JoshuaTree.eda)
#summary(Reykjavik.eda)
summary(Paris_L.eda)
summary(Paris_R.eda)
summary(NewYork_L.eda)
summary(NewYork_R.eda)
summary(Barcelona_L.eda)
summary(Barcelona_R.eda)


