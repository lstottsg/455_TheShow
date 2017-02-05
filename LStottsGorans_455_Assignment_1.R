#Laura Stotts Gorans
#Predict-455
#Assignment 1

#Goals of R Code:
#1) Visualization of current software preferences
#2) Visualization of interest in new courses
#3) Correlation of interest in python and interest in python-related courses
#4) Correlation of interst in python given previous classes taken or terms to graduate

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

# set up hash fuctions similar to Python dictionary data structure
library(hash)  

# set color constants for visualizations
HEX_BLUE <- "#3953A4"  
HEX_GREEN <- "#13A049" 
HEX_RED <- "#EE3166"  
HEX_WHITE <- "#FFFFFF"  
HEX_GREY <- "#C2C2D6"  
HEX_BLACK <- "#000000"

# read in comma-delimited text file
# note that IPAddress is formatted as an actual IP address
# but is actually a random-hash of the original IP address
valid_survey_input <- 
  read.csv("mspa_survey_data.csv")

#set up an eda data set to play with
valid_survey_input.eda <- valid_survey_input
attach(valid_survey_input.eda)

#Check out summary for overview and identify missing
summary(valid_survey_input.eda)
#1 missing for Python-Course_Interest
#7 missing for Foundations_DE_Course_Interest
#4 missing for Analytics _App_Course_Interest
#7 missing for Systems_Analysis_Course_Interest
#20 missing for Courses_Completed


#because NA is most likely zero interest, I will replace NA with zero
valid_survey_input.eda[is.na(valid_survey_input.eda)] <- 0

#and now we have no missing values
sum(is.na(valid_survey_input.eda))

#view data dimensions
dim(valid_survey_input.eda)

#print out number of respondents
cat("\nNumber of Respondents =", nrow(valid_survey_input.eda))

#########################################################################################
#VISUALIZATION #1: CURRENT PROGRAMMING LANGUAGE PREFERENCES
#########################################################################################
#1) Visualization of current software preferences

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
#VISUALIZATION #2: INTEREST IN NEW COURSES
#########################################################################################
#2) Visualization of interest in new courses

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
#VISUALIZATION #3: CORRELATION BETWEEN LANGUAGE INTEREST AND COURSE INTEREST
#########################################################################################
#3) Correlation of interest in programming languages and interest in courses

#Corrplot: Correlation Heatmap

#set up data for viewing in correlation matrix heatmap      
cormat.pref <- cor(valid_survey_input.eda[, 2:20])
short_prefvar_names <- c("Pers_Java", "Pers_JavaSript", "Pers_Python", "Pers_R", "Pers_SAS",
                         "Prof_Java", "Prof_JavaSript", "Prof_Python", "Prof_R", "Prof_SAS",
                         "Ind_Java", "Ind_JavaSript", "Ind_Python", "Ind_R", "Ind_SAS","Python_Int", "DataEng_Int", 
                         "AnalyticApp_Int", "SysAna_Int")
rownames(cormat.pref) <- short_prefvar_names
colnames(cormat.pref) <- short_prefvar_names

# examine intercorrelations among software preference and course interest variables
pdf(file = "fig_cor_heat_map_language_course_corr.pdf", 
    width = 8, height = 7.5, paper = "letter")
M <- cor(cormat.pref,use = "na.or.complete")
corrplot(M, method="color", tl.col ="black", order = "FPC", type="lower")
title("Chart 3: Correlations Amongst Programming Language & Course Interest", line = +3)
dev.off()



#########################################################################################
#VISUALIZATION #4: CORRELATION BETWEEN INTEREST IN PYTHON AND PREVIOUS CLASSES TAKEN + TERMS TO GRADUATE
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


