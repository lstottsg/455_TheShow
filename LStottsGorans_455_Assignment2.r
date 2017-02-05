#Laura Stotts Gorans
#Predict-455
#Assignment 2

#Goals of R Code:
#1)

#########################################################################################
#Set up data for exploration
#########################################################################################

# install R packages for use in text proessing and visualization
library(wordcloud)
library(tm) 
library(quanteda)
library(tidytext)
library(SnowballC)   
library(ggplot2) 
library(cluster)  
library(fpc)
library(dplyr)
library(tidyr)
library(ggplot2)

########################################################################################################################

#PREPROCESS DATA

#pull in all texts as corpus
cname <- file.path("C:/Users/lstottsg/Desktop/Laura School/Winter 2017/StottsGorans_455_Ind2", "Texts")   
cname   
dir(cname)  

docs.c <- Corpus(DirSource(cname))   

summary(docs.c) 

################################    
#get tidy corpus for use later? 
docs_tidy <- tidy(docs.c)
docs_tidy
################################   

docs <- tm_map(docs.c, removePunctuation)   

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("\\(", " ", docs[[j]])   
  docs[[j]] <- gsub("\\)", " ", docs[[j]])   
  docs[[j]] <- gsub("---", " ", docs[[j]])   
}   

docs <- tm_map(docs, removeNumbers)   

docs <- tm_map(docs, tolower)  

docs <- tm_map(docs, stemDocument) 

docs <- tm_map(docs, stripWhitespace) 

docs <- tm_map(docs, removeWords, stopwords("english"))  

#removing most common/patriotic terms to get to unique
docs <- tm_map(docs, removeWords, c("can", "will", "must", "applause", "today",
                                    "america", "american", "nation", "nations", 
                                    "president", "americas", "people", "applaus")) 

#joining some phrases and splitting some contractions
for (j in seq(docs))
{
  docs[[j]] <- gsub("god bless", "god_bless", docs[[j]])
  docs[[j]] <- gsub("my fellow", "my_fellow", docs[[j]])
  docs[[j]] <- gsub("we the people", "we_the_people", docs[[j]])
  docs[[j]] <- gsub("we'll", "we will", docs[[j]])
  docs[[j]] <- gsub("we've", "we have", docs[[j]])
  docs[[j]] <- gsub("we're", "we are", docs[[j]])
}

#tell R this is a plaintext doc       
docs <- tm_map(docs, PlainTextDocument)   

#STAGE AND EXPLORE DATA

#create matrix from corpus
dtm <- DocumentTermMatrix(docs)   
dtm   
#inspect(dtm)

#organize words by frequency
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)  

#remove sparse terms
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  

freq[tail(ord)]   

head(table(freq), 20)  

#frequency of frequencies
freq <- colSums(as.matrix(dtms))   
freq   

#look at most frequent (words that are used at least 5 times)
findFreqTerms(dtm, lowfreq=5) 

wf <- data.frame(word=names(freq), freq=freq)  
wf <- wf[order(-freq),] 
head(wf)  

#PLOT (using ggplot2)

#plot words that appear at least 20 times
pdf(file = "fig_text_freq_plot_of_AllInaugurationSpeeches.pdf", width = 8.5, height = 8.5) 
freq.plot <- ggplot(subset(wf, freq>20), aes(word, freq))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Chart 1: Inaugural Addresses 1997-2013: Words Appearing > 20 Times")

freq.plot 
dev.off()

#FIND CORRELATIONS

#correlations with a chosen word, specifying a correlation limit of 0.90 
findAssocs(dtm, c("freedom"), corlimit=0.985)   
findAssocs(dtm, c("country"), corlimit=0.95) 
findAssocs(dtm, c("great"), corlimit=0.90) 

#prep for plotting
corr1 <- findAssocs(dtm, c("freedom"), corlimit=0.985)[[1]] # specifying a correlation limit of 0.95
corr1 <- cbind(read.table(text = names(corr1), stringsAsFactors = FALSE), corr1)
corr2 <- findAssocs(dtm, c("country"), corlimit=0.95)[[1]] # specifying a correlation limit of 0.90 
corr2 <- cbind(read.table(text = names(corr2), stringsAsFactors = FALSE), corr2)

#use dyplr to join correlations
terms_corrs <- full_join(corr1, corr2)

# gather for plotting
two_terms_corrs_gathered <- gather(terms_corrs, term, correlation, corr1:corr2)

# insert the actual terms of interest so they show up on the legend
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr1", "freedom", "country")

pdf(file = "fig_text_corr_plot_freedom_country.pdf", width = 8.5, height = 8.5) 

ggplot(two_terms_corrs_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 3) +
  ggtitle(paste0("Chart 3: Correlations With ", "\"", "freedom",  "\"", " and ",  "\"", "country", "\"")) +
  ylab("Significance") +
  xlab("Word") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

dev.off()

#CREATE WORDCLOUD

my.pal <- brewer.pal(12, "Paired")  

pdf(file = "fig_text_wordcloud_of_AllInaugurationSpeeches.pdf", width = 8.5, height = 8.5) 
wordcloud(names(freq), freq, random.color=FALSE, color=my.pal, rot.per=.0) +
  title(main="Frequent Words: Inaugural Addresses 1997-2013")

dev.off()

#CLUSTER DENDROGRAM (using cluster)

#cluster words by similarity
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)  

d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
fit   

pdf(file = "fig_text_cluster_dend_of_AllInaugurationSpeeches.pdf", width = 8.5, height = 8.5) 

plot(fit, hang=-1,main="Chart 4: Cluster Dendrogram of Inaugural Addresses 1997-2013")
groups <- cutree(fit, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red") # draw dendogram with red borders around the 5 clusters   

dev.off()
#############################################################################################################
#Now look at Trump's individual text

T2017Trump <- scan("2017Trump_Inauguration.txt", what = "char", sep = "\n") 
class(T2017Trump)
# -----------------------------------# word cloud for Trump's Inauguration Speech 2017# -----------------------------------


# replace uppercase with lowercase letters 
T2017Trump <- tolower(T2017Trump) 
T2017Trump <- stemDocument(T2017Trump) 
T2017Trump <- removeNumbers(T2017Trump) 
T2017Trump <- removePunctuation(T2017Trump) 
T2017Trump <- stripWhitespace(T2017Trump) 
T2017Trump <- removeWords(T2017Trump, stopwords("english")) 
T2017Trump <- gsub("\\(", " ", T2017Trump)   
T2017Trump <- gsub("\\)", " ", T2017Trump)   
T2017Trump <- gsub("---", " ", T2017Trump) 

#removing most common/patriotic terms to get to unique
T2017Trump <- removeWords(T2017Trump,c("can", "will", "must", "applause", "today",
                                 "america", "american", "nation", "nations", 
                                 "president", "americas", "people", "applaus")) 


T2017Trump <- gsub("god bless", "god_bless", T2017Trump)
T2017Trump <- gsub("my fellow", "my_fellow", T2017Trump)
T2017Trump <- gsub("we the people", "we_the_people", T2017Trump)
T2017Trump <- gsub("we'll", "we will", T2017Trump)
T2017Trump <- gsub("we've", "we have", T2017Trump)
T2017Trump <- gsub("we're", "we are", T2017Trump)


# strip out all non-letters and return vector 
T2017Trump.preword.vector <- unlist(strsplit(T2017Trump, "\\W")) 
# drop all empty words 
T2017Trump.vector <-T2017Trump.preword.vector[which(nchar(T2017Trump.preword.vector) > 0)] 

#word frequency
Trump.freq <- as.data.frame(table(unlist(T2017Trump.vector)))
Trump.freq <- Trump.freq[order(-Trump.freq$Freq),] 

#plot words that appear at least 20 times
pdf(file = "fig_text_freq_plot_of_TrumpSpeech.pdf", width = 8.5, height = 8.5) 
Tfreq.plot <- ggplot(subset(Trump.freq, Freq>5), aes(Var1, Freq))  +  
  geom_bar(stat="identity", color="LightBlue", fill="Gray")   +
  theme(axis.text.x=element_text(angle=45, hjust=1))   +
  ggtitle("Chart 2: Words Frequency >5 In Trump's Speech") +
  xlab("Words")

Tfreq.plot 
dev.off()

#same color palette for wordcloud
my.pal <- brewer.pal(12, "Paired") 

#plot wordcloud
pdf(file = "fig_text_wordcloud_of_Trump_InaugurationSpeech.pdf", width = 8.5, height = 8.5) 

set.seed(1234) 
wordcloud(T2017Trump, min.freq = 5, max.words = 150, random.order=FALSE, random.color=FALSE, rot.per=0.0, 
          color=my.pal, ordered.colors=FALSE, use.r.layout=FALSE, fixed.asp=TRUE) +
  title(main="Trump Inauguration 2017")

dev.off()     
