#imports the data and assigns it to the variable Data.Set
USArrests <- read.csv("D:/0_Datascience/Datasets/OA 2.2 - USArrests.csv")
View(OA.2.2...USArrests)

#installs most of the packages we will be using over the class
install.packages("tidyverse")

#imports dplyr and tidyr into our project
library(tidyr)
library(dplyr)



#dplyr has 5 basic verbs: filer, arrange, select, mutate, summarize, group_by



#Filter
#Filters out all values that do not full fill the listed logic
#First argument is the tibble/data_frame, the second is the filter logic
Only.A.States <- filter(USArrests, substring(X,0,1) == "A")
View(Only.A.States)



#Arrange
#Reorders the rows of a tibble/data frame by the listed parameters
By.Murder <- arrange(USArrests, Murder)
View(By.Murder)
#You can use the desc() to switch to descending
By.Murder.Descending <- arrange(USArrests, desc(Murder))
View(By.Murder.Descending)



#slice 
#removes data by row numbers
Rows.5.to.10 <- slice(USArrests, 5:10)
View(Rows.5.to.10)
#Use Slice_head() and Slice_tail() to remove the first and last rows respectively 
First.5 = slice_head(USArrests, n=5)
Last.5 = slice_tail(USArrests, n=5)
View(First.5)
View(Last.5)
#Select random rows
Random.5 = slice_sample(USArrests, n = 5)
View(Random.5)


#Select
#Removes selects different columns
State.and.Murder = select(USArrests, X, Murder)
Not.Murder = select(USArrests, !Murder)
View(State.and.Murder)
View(Not.Murder)


#Mutate
#Changes changes values with in a table
Murders.Times.10 = mutate(USArrests, Murder.10 = Murder * 10)
View(Murders.Times.10)
#transmute, is the same as mutate but only keeps the new values
Only.Murders.time.10 = transmute(USArrests, Murder.10 = Murder * 10)
View(Only.Murders.time.10)


#Summarise
#creates a single row of data based upon the logic
Total.Murders = summarise(USArrests, murders = sum(Murder))
View(Total.Murders)


#group_by
#rearranges the data into groups by the designated values
Group.By.First.Letter = group_by(USArrests, substring(X, 0, 1))
Sum.murder.by.first.letter = summarise(Group.By.First.Letter, murders = sum(Murder))
View(Sum.murder.by.first.letter)
View(Group.By.First.Letter)
class(Group.By.First.Letter)
