library(dplyr)
library(ggplot2)
library(animation)

#get the data and asign it to Data
Data = USArrests
#clean the data set
cleaned_USARRESTS = Data %>% 
  mutate(scaled_murder = scale(Murder), Scaled_Assault = scale(Assault), scaled_pop = scale(UrbanPop)) %>% 
  filter(!is.na(Assault), !is.na(UrbanPop), !is.na(Murder)) %>%
  select(-c(Murder, Assault, UrbanPop))
#K-cluster algorithm with 4 clusters
kmeans(Data,4)
#visualize the algorithm with 5 clusters 
kmeans.ani(cleaned_USARRESTS, 5)

#function that provides the amount of the data encapsulated
kmean_withinss <- function(k) {
  cluster <- kmeans(cleaned_USARRESTS, k)
  return (cluster$tot.withinss)
}
#example of the use of ^ algorithm with 2 clusters
kmean_withinss(2)

#define max_k as 20
max_k = 20
#instantiates a data frame
x = data.frame(Num = c(0), value = c(0))
#iterates through 2:max_k and records the number of clusters and the tot.withinss data
for(loop in 2:max_k){
  x = data.frame(Num = c(x[,1], loop), value = c(x[,2], kmean_withinss(loop)))
}
#look at x
View(x)
#graph x
ggplot(x, aes(Num, value)) + geom_point() + geom_line()

