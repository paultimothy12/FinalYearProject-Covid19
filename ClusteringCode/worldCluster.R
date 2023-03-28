rm(list = ls())
gc()
covid_data <-read.csv("C:\\Program Files\\R\\R3.3.2\\datasets\\covidworldfeb25.csv", header = TRUE)
print(covid_data)
org_data = covid_data
org_data
head(covid_data)
head(covid_data, n= 2)
str(covid_data)
dim(covid_data)
length(covid_data)
covid_data[1:4,]
covid_data[1:4,2]
covid_data[,4]
tail(covid_data)
tail(covid_data, n=10)
print(covid_data$Name)
summary(covid_data)
#Data Preprocessing
x1<- covid_data$Name
x1
x2 <- covid_data$WHO.Region
x2
x3 <- covid_data$Cases.cumulative
x3
x4 <- covid_data$Cases.new_7
x4
summary(x4)
left = subset(covid_data,Cases.new_7 > 200000, select = c(Cases.new_7))
print(left)
left = subset(covid_data,Cases.new_7 > 100000, select =
c(Name,Cases.new_7))
print(left)
left = subset(covid_data,Cases.new_7 > 50000, select =
c(Name,Cases.new_7))
print(left)
x5 <- covid_data$Cases.new24hrs
x5
x6 <- covid_data$Deaths.cumulative
x6
x7 <- covid_data$Deaths.new_7
x7
x8 <- covid_data$Deaths.new24hrs
x8
hist(x4, main = " New Cases",xlab="No. of New Cases", ylab="No.of
Countries",col = "brown")
hist(x7, main = " New Deaths",xlab="No. of New Deaths", ylab="No.of
Countries",col = "green")
new_data <- cbind(x1,x2,x3,x4,x5)
covid_data = covid_data[,c(3,4,5,6,7,8)]
print(covid_data)
head(covid_data)
summary(covid_data)
# Normalization
normalize <- function(x) {
return((x-min(x))/ (max(x) - min(x)))
}
covid_datan <-
as.data.frame(lapply(covid_data[,c(1,2,3,4,5,6)],normalize))
print(covid_datan)
summary(covid_datan)
kmeans.result <- kmeans(covid_datan, 3)
print(kmeans.result$cluster)
print(kmeans.result$centers)
plot(covid_datan$Cases.new_7~
covid_datan$Deaths.new_7,covid_datan,col =
kmeans.result$cluster,xlab="No. of New Deaths", ylab="No.of New
Cases",main = "New Cases Vs Death Cases")
with(covid_datan,text(covid_datan$Cases.new_7~
covid_datan$Deaths.new_7, labels=org_data$Name,pos=4, cex = .6,font =
1))
org_data <- cbind(org_data,kmeans.result$cluster)
View(org_data)
write.csv(org_data,file = "D:\\cluster1.csv")
#Hierarchical CLustering
rm(list = ls())
gc()
covid_data <-read.csv("C:\\Program Files\\R\\R3.3.2\\datasets\\covidworldfeb25.csv", header = TRUE)
print(covid_data)
org_data = covid_data
org_data
covid_data = covid_data[,c(3,4,5,6,7,8)]
print(covid_data)
head(covid_data)
summary(covid_data)
# Normalization
normalize <- function(x) {
return((x-min(x))/ (max(x) - min(x)))
}
covid_datan <-
as.data.frame(lapply(covid_data[,c(1,2,3,4,5,6)],normalize))
print(covid_datan)
summary(covid_datan)
distance <-dist(covid_datan)
distance
hc <- hclust(dist(covid_datan), method="ave")
print(hc)
plot(hc, hang = -1, labels=org_data$Name)
rect.hclust(hc, k=3, border = "green")
groups <- cutree(hc, k=3)
print(groups)
org_data <- cbind(org_data,groups)
print(org_data)
View(org_data)
write.csv(org_data,file = "D:\\cluster2.csv")
#Performance Measurement of K-Means and Hierarchical Clustering
rm(list = ls())
gc()
library(cluster)
covid_data <-read.csv("C:\\Program Files\\R\\R3.3.2\\datasets\\covidworldfeb25.csv", header = TRUE)
print(covid_data)
org_data = covid_data
org_data
covid_data = covid_data[,c(3,4,5,6,7,8)]
print(covid_data)
head(covid_data)
summary(covid_data)
# Normalization
normalize <- function(x) {
return((x-min(x))/ (max(x) - min(x)))
}
covid_datan <-
as.data.frame(lapply(covid_data[,c(1,2,3,4,5,6)],normalize))
print(covid_datan)
summary(covid_datan)
#Kmeans Clustering Algorithm
kmeans.result <- kmeans(covid_datan, 3)
print(kmeans.result)
print(kmeans.result$totss)
print(kmeans.result$betweenss)
Kmeans_Qua = kmeans.result$betweenss/kmeans.result$totss
cat("Quality of Kmeans Clustering = ",Kmeans_Qua*100,"%")
#Hierarchical Clustering Algorithm
hc <- hclust(dist(covid_datan), method="ave")
cat("Quality of Hierarchical Clustering = ",100*coef(hc),"%")
