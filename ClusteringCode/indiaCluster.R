rm(list = ls())
gc()
covid_data <-read.csv("C:\\Program Files\\R\\R3.3.2\\datasets\\state_wisemar.csv", header = TRUE)
print(covid_data)
org_data = covid_data
org_data
head(covid_data)
head(covid_data, n= 7)
str(covid_data)
dim(covid_data)
length(covid_data)
covid_data[3:6,]
covid_data[3:6,5]
covid_data[,3]
tail(covid_data)
tail(covid_data, n=7)
print(covid_data$Name)
summary(covid_data)
#Data Preprocessing
x1<- covid_data$Name
x1
x2 <- covid_data$State_code
x2
x3 <- covid_data$Confirmed
x3
x4 <- covid_data$Recovered
x4
x5 <- covid_data$Deaths
x5
x6 <- covid_data$Active
x6
summary(x4)
new_data <- cbind(x1,x2,x3,x4,x5,x6)
covid_data = covid_data[,c(3,4,5,6)]
print(covid_data)
head(covid_data)
summary(covid_data)
# Normalization
normalize <- function(x) {
return((x-min(x))/ (max(x) - min(x)))
}
covid_datan <- as.data.frame(lapply(covid_data[,c(1,2,3,4)],normalize))
print(covid_datan)
summary(covid_datan)
kmeans.result <- kmeans(covid_datan, 3)
print(kmeans.result$cluster)
print(kmeans.result$centers)
plot(covid_datan$Active~ covid_datan$Deaths,covid_datan,col =
kmeans.result$cluster,xlab="No. of Deaths", ylab="No.of Active
Cases",main = "Active Vs Deaths")
with(covid_datan,text(covid_datan$Active~ covid_datan$Deaths,
labels=org_data$Name,pos=4, cex = .6,font = 1))
org_data <- cbind(org_data$Name,kmeans.result$cluster)
View(org_data)
write.csv(org_data,file = "D:\\kcluster.csv")
#Hierarchical Clustering
rm(list = ls())
gc()
covid_data <-read.csv("C:\\Program Files\\R\\R3.3.2\\datasets\\state_wisemar.csv", header = TRUE)
print(covid_data)
org_data = covid_data
org_data
covid_data = covid_data[,c(3,4,5,6)]
print(covid_data)
summary(covid_data)
# Normalization
normalize <- function(x) {
return((x-min(x))/ (max(x) - min(x)))
}
covid_datan <- as.data.frame(lapply(covid_data[,c(1,2,3,4)],normalize))
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
org_data <- cbind(org_data$Name,groups)
View(org_data)
write.csv(org_data,file = "D:\\hcluster.csv")
