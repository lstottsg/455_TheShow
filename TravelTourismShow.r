
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
Lisbon <- read.csv("Lisbon.csv")
JoshuaTree <- read.csv("JoshuaTree.csv")
Reykjavik <- read.csv("Reykjavik.csv")

# supplementary cities mentioned in HBR article
Paris_L <- read.csv("Paris Listings.csv")
Paris_R <- read.csv("Paris Reviews.csv")
NewYork_L <- read.csv("New York Listings.csv")
NewYork_R <- read.csv("New York Reviews.csv")
Barcelona_L <- read.csv("Barcelona Listings.csv")
Barcelona_R <- read.csv("Barcelona Reviews.csv")

#detailed reviews for text analysis
ParisDetRev <- read.csv("Paris Detailed Reviews.csv.gz")
NewYorkDetRev <- read.csv("New York Detailed Reviews.csv.gz")
BarcelonaDetRev <- read.csv("Barcelona Detailed Reviews.csv.gz")

######################################################################################################################
#Begin EDA
######################################################################################################################

#set up eda data sets to play with
Lisbon.eda <- Lisbon
JoshuaTree.eda <- JoshuaTree
Reykjavik.eda <- Reykjavik

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
summary(Lisbon.eda)
summary(JoshuaTree.eda)
summary(Reykjavik.eda)
summary(Paris_L.eda)
summary(Paris_R.eda)
summary(NewYork_L.eda)
summary(NewYork_R.eda)
summary(Barcelona_L.eda)
summary(Barcelona_R.eda)


#handle NA - remove
valid_survey_input.eda[is.na(valid_survey_input.eda)] <- 0

#and now we have no missing values
sum(is.na(valid_survey_input.eda))

#view data dimensions
dim(valid_survey_input.eda)


#########################################################################################
#VISUALIZATION 1
#########################################################################################

#GGPlot: Stacked Bar Chart

#Stacked bar chart showing the proportion of student preference in software language, stacked by interest type
#y-axis = student interest count
#x-axis = language
#category = interest type

#Working with columns 2:16

#create transposed data.frame for use as stacked chart fill
eda.pivot <- data.frame(t(valid_survey_input.eda[2:16]))

#create a new column for rownames
eda.pivot[ "Pref_cat" ] <- rownames(eda.pivot)

#melt data frame
eda.molten <- melt.data.frame( eda.pivot, id.vars="Pref_cat", variable_name="Student" )
colnames(eda.molten)[3] <- "Points"


#create column with just language type
eda.molten$Language <- ifelse(
  grepl("Scala", eda.molten$Pref_cat, ignore.case = F), "JavaScalaSpark", 
  ifelse(
    grepl("Script", eda.molten$Pref_cat, ignore.case = F), "JavaScriptHTMLCSS",
    ifelse(
      grepl("Python", eda.molten$Pref_cat, ignore.case = F), "Python", 
      ifelse(
        grepl("SAS", eda.molten$Pref_cat, ignore.case = F), "SAS", "R")
    )))

#create column with just preference type
eda.molten$Pref_Type <- ifelse(
  grepl("Personal", eda.molten$Pref_cat, ignore.case = F), "Personal", 
  ifelse(
    grepl("Professional", eda.molten$Pref_cat, ignore.case = F), "Professional","Industry")
)
#order
eda.molten <- eda.molten[with(eda.molten, order(-Pref_Type)), ]

head(eda.molten)

#plot 
pdf(file = "fig_ggplot2_stacked_bar_chart_for_lang_pref.pdf", 
    width = 7.5, height = 7.5, paper = "letter") 
ggplot.langpref <- ggplot(eda.molten[3:5], aes( x = Language, y = Points, fill = Pref_Type )) + 
  geom_bar( position = "dodge", stat = "identity", alpha = .3 ) +
  ggtitle("Chart 1: Student Programming Language Preference\n") +
  xlab("\nProgramming Language") +
  ylab("Points Given by Students") 
print(ggplot.langpref)
dev.off()


#########################################################################################
#VISUALIZATION #2
#########################################################################################

#Lattice: Connected Dot Plot

#Student preference in new courses by graduation term
#y-axis = student interest count
#x-axis = Graduation Term
#color = Course


#Working with columns 17:20 and last column

movecols <- c("Graduate_Date", "Python_Course_Interest", "Foundations_DE_Course_Interest", 
             "Analytics_App_Course_Interest", "Systems_Analysis_Course_Interest")

valid_survey_input.eda <- valid_survey_input.eda[, c(movecols, setdiff(names(valid_survey_input.eda), movecols))]

names(valid_survey_input.eda)
head(valid_survey_input.eda)

#create transposed data.frame for use as stacked chart fill
vis.2 <- data.frame(valid_survey_input.eda[1:5])
head(vis.2)

#parse Graduate Date to extract Year
Year <- parse_number(vis.2$Graduate_Date)
Term <- gsub('[[:digit:]]+', '', vis.2$Graduate_Date)
Year <- as.character(Year)
Year[Year==""] <- "NA"
Year <- as.factor(Year)

vis.2$Year <- Year


#calculate mean values for course interest per Graduate Year
Python.Course <- aggregate(vis.2$Python_Course_Interest, by=list(Graduate_Year=vis.2$Year), FUN=mean)
Foundations_DE.Course <- aggregate(vis.2$Foundations_DE_Course_Interest, by=list(Graduate_Year=vis.2$Year), FUN=mean)
Analytics_App.Course <- aggregate(vis.2$Analytics_App_Course_Interest, by=list(Graduate_Year=vis.2$Year), FUN=mean)
Systems_Analysis.Course <- aggregate(vis.2$Systems_Analysis_Course_Interest, by=list(Graduate_Year=vis.2$Year), FUN=mean)


#create new data frame for summary variables
Course.AggregateInterest <- data.frame(Graduate_Year=Python.Course$Graduate_Year,
                                       Python.Course=Python.Course$x, 
                                       Foundations_DE.Course=Foundations_DE.Course$x, 
                                       Analytics_App.Course=Analytics_App.Course$x, 
                                       Systems_Analysis.Course=Systems_Analysis.Course$x)

#assign years to rownames for easier sorting
rownames(Course.AggregateInterest) <- Course.AggregateInterest$Graduate_Year


#melt data frame
course.molten <- melt.data.frame( Course.AggregateInterest, id.vars="Graduate_Year", variable_name="Course")
colnames(course.molten)[3] <- "Points"


#simple lattice connected dotplot showing interest in courses given years to graduate
pdf(file = "fig_lattice_dotplot_course_interest.pdf", 
    width = 7.5, height = 7.5, paper = "letter") 
lattice.courseint <- dotplot(Points ~ Graduate_Year, data = course.molten, groups = Course, type = "o",
                            auto.key = list(space = "right", points = TRUE, lines = TRUE), xlab = "Graduation Year", 
                            ylab = "Mean Student Interest in Points", main = "Chart 2: Mean Interest in New Courses by Graduation Year")
print(lattice.courseint)

dev.off()  


#########################################################################################
#VISUALIZATION #3: Review Text WordCloud
#########################################################################################

#WordCloud: WordCloud

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

#PREPROCESS DATA

#pull in all detailed review texts as corpus
corpus.r <- file.path("C:/The Show", "All Reviews")   
corpus.r   
dir(corpus.r)  

docs.c <- Corpus(DirSource(corpus.r))   

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

#CREATE WORDCLOUD

my.pal <- brewer.pal(12, "Paired")  

wordcloud(names(freq), freq, random.color=FALSE, color=my.pal, rot.per=.0) +
  title(main="Frequent Words: ")



#########################################################################################
#VISUALIZATION #4
#########################################################################################
#4) Correlation of interest in courses given previous classes taken or terms to graduate

names(valid_survey_input.eda)

#narrow our data down to courses completed by students and course interest
course.interest <- data.frame(valid_survey_input.eda[, 2:5])
course.names <- data.frame(valid_survey_input.eda[, 22:40])
course.complete <- data.frame(course.interest, course.names)

names(course.complete)

# software count variables to add to the data frame
course.complete$Python_Courses <- numeric(nrow(course.complete))
course.complete$R_Courses <- numeric(nrow(course.complete))
course.complete$SAS_Courses <- numeric(nrow(course.complete))

for (istudent in seq(along = course.complete$Python_Courses)) {
  
  if (valid_survey_input$PREDICT400[istudent] != "")
    course.complete$Python_Courses[istudent] <-
      course.complete$Python_Courses[istudent] + 1
  if (valid_survey_input$PREDICT420[istudent] != "")
    course.complete$Python_Courses[istudent] <-
      course.complete$Python_Courses[istudent] + 1
  if (valid_survey_input$PREDICT452[istudent] != "")
    course.complete$Python_Courses[istudent] <-
      course.complete$Python_Courses[istudent] + 1
  if (valid_survey_input$PREDICT453[istudent] != "")
    course.complete$Python_Courses[istudent] <-
      course.complete$Python_Courses[istudent] + 1
  if (valid_survey_input$OtherPython[istudent] != "")
    course.complete$Python_Courses[istudent] <-
      course.complete$Python_Courses[istudent] + 1
  
  if (valid_survey_input$PREDICT401[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1
  if (valid_survey_input$PREDICT413[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1
  if (valid_survey_input$PREDICT450[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1        
  if (valid_survey_input$PREDICT451[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1        
  if (valid_survey_input$PREDICT454[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1        
  if (valid_survey_input$PREDICT455[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1        
  if (valid_survey_input$PREDICT456[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1        
  if (valid_survey_input$PREDICT457[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1 
  if (valid_survey_input$OtherR[istudent] != "")
    course.complete$R_Courses[istudent] <-
      course.complete$R_Courses[istudent] + 1 
  
  if (valid_survey_input$PREDICT410[istudent] != "")
    course.complete$SAS_Courses[istudent] <-
      course.complete$SAS_Courses[istudent] + 1
  if (valid_survey_input$PREDICT411[istudent] != "")
    course.complete$SAS_Courses[istudent] <-
      course.complete$SAS_Courses[istudent] + 1
  if (valid_survey_input$OtherSAS[istudent] != "")
    course.complete$SAS_Courses[istudent] <-
      course.complete$SAS_Courses[istudent] + 1
  
}      
head(course.complete)

# it is often useful to set up a data frame just for plotting
plotting_data_frame <- 
  course.complete[, c("Courses_Completed", "Prog_Lang")]
plotting_data_frame$Prog_Lang <- 
  factor(plotting_data_frame$Prog_Lang,
         levels = c("R", "SAS", "Python"), 
         labels = c("R Courses", "SAS Courses", "Python Courses"))



# it is often useful to set up a data frame just for plotting
plotting_data_frame <- 
  course.complete[, c("Courses_Completed", "Python_Course_Interest", "Foundations_DE_Course_Interest", 
                      "Analytics_App_Course_Interest", "Systems_Analysis_Course_Interest",
                      "Python_Courses", "R_Courses", "SAS_Courses")]


pdf(file = "fig_baseR_Python_Interest_Progression.pdf", 
    width = 7.5, height = 8, paper = "letter")
base_plot_object <- boxplot(Python_Course_Interest~Python_Courses, data = course.complete, horizontal = TRUE, 
                            range = 0, col = HEX_BLUE, boxwex = 0.5, show.names = TRUE,
                            main = "Chart 4: Python Course Interest Given Python Courses Taken",
                            xlab = "Student Interest in New Python Course",
                            ylab = "Python Courses Taken")

print(base_plot_object) 

dev.off()


