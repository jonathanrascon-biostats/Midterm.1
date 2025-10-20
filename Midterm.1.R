#MIDTERM 1--------

#The given data set is psychiatric symptoms, secondary to an acute spinal injury
#and associated to age. The columns are: Record ID- the unique identifier, Age 
#Group (two- 18-35, 65-80), BSI total (brief symptom indicator), and Sig.Scale
#which is a score associated with clinical significannce. Record ID is nominal,
#Age Group is categorical, and BSI and Sig.Scale are ordinal--for our purposes,
#these two should be stored as numeric.

#set working directory and create Git repository------
  #I created a repo on the github as "Midterm.1" and created a git project in R
  #ands connected it to my online repo. Just to check the directory, I ran:
getwd() #which returns my repo directory

#libraries------- 
  #For convenience, I'll add all libraries here:

library(readxl)
library(tidyverse)
library(pastecs)
library(reshape2)

#read in data------- 
  #First, I copied and pasted the data set into an excel file before
  #upload. I also will store this in github.

BSI.sig.data <- read_xlsx("midterm.dataset.xlsx")

#Check data types.-------
  #First, I want to see if my data is stored as the type of data I want:
  #numeric, character, etc.
names(BSI.sig.data)
  #After a quick check, I found that all my columns were pulled with a weird
  #naming format; they all had a space after them, giving them the weird naming
  #convention you see below. I renamed them all.
BSI.sig.data <- BSI.sig.data %>% rename(Age.Group=`Age.Group `, 
    Record = ` Record `, Sig.Scale=`Sig.Scale `, BSI.Total=`BSI.Total `) 

typeof(BSI.sig.data$Record) ; typeof(BSI.sig.data$Age.Group)
typeof(BSI.sig.data$BSI.Total) ; typeof(BSI.sig.data$Sig.Scale)
  #After checking all data types, I see that ID and Age are stored as "double". I will
  #change Record and Age to character.

BSI.sig.data <- BSI.sig.data %>% mutate_at(c("Record", "Age.Group"), as.character)

#using the mutate_at function with concatenate, I change both to character strings.

#Melt data into long(tidy) format
BSI.sig.data.long <- melt(BSI.sig.data, id.vars = c("Record", "Age.Group"), 
          variable.name = "Test.Type", value.name = "Score")  

BSI.sig.data.long %>% group_by(Age.Group, Test.Type) %>% 
  summarise(mean = mean(Score), sd(Score))

