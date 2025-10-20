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

#Pull statistical descriptions-------
  #Using the by function, I can stratify each variable by age group
by(data = BSI.sig.data$BSI.Total, BSI.sig.data$Age.Group,
   FUN = function(x) round(stat.desc(x, norm = TRUE), 3))

by(data = BSI.sig.data$Sig.Scale, BSI.sig.data$Age.Group,
   FUN = function(x) round(stat.desc(x, norm = TRUE), 3))
  #The first thing to notice is the extreme difference in mean! The BSI total 
  #and the Sig score are both much higher for the 18-35 group.

#Plot BSI mean data-------

BSI.plot <- BSI.sig.data %>% 
  ggplot(aes(x = Age.Group, y = BSI.Total, fill = Age.Group)) + 
  stat_summary(fun = mean, geom = "bar", width = .7) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, color = "black")+
  scale_y_continuous(limits = c(0, 140), breaks = seq(from = 0, to = 140, by = 5))+
  scale_fill_manual(values = c("forestgreen", "blue")) +
  labs(title = "BSI Scores by Age Group", x = "Age Group" , y = "Mean Score by Age") + 
  theme(legend.position = "none")

BSI.plot  

#Plot Sig.Scale mean data-------

Sig.Scale.plot <- BSI.sig.data %>% 
  ggplot(aes(x = Age.Group, y = Sig.Scale, fill = Age.Group)) +
  stat_summary(fun = mean, geom = "bar", width = .7) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, color = "tomato")+
  scale_y_continuous(limits = c(-1, 10), breaks = seq(from = 0, to = 10, by = 1))+
  scale_fill_manual(values = c("skyblue", "green")) +
  labs(title = "SIG Scores by Age Group", x = "Age Group" , y = "Mean Score by Age") + 
  theme(legend.position = "none")

Sig.Scale.plot

#Melt data into long(tidy) format
BSI.sig.data.long <- melt(BSI.sig.data, id.vars = c("Record", "Age.Group"), 
          variable.name = "Test.Type", value.name = "Score")  

BSI.sig.data.long %>% group_by(Age.Group, Test.Type) %>% 
  summarise(mean = mean(Score), sd(Score), var(Score))

