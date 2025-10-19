#MIDTERM 1

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

#libraries. For convenience, I'll add all libraries here:

library(readxl)

#read in data. First, I copied and pasted the data set into an excel file before
#upload. I also will store this in github.

BSI.sig.data <- read_xlsx("midterm.dataset.xlsx")
