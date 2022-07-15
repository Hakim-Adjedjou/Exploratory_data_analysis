#load the dataset into R : 

dt<-read.csv("./state.csv")

#libraries to load  : 
library(dplyr)


#Quick overview of the dataset :

dim(dt)
summary(dt)
names(dt)

#the dataset is already tidy and there s no NA values , time for an EDA ! 

#starting with the estimated tendency or location of data :

mean(dt$Population)
mean(dt$Population,trim = 0.1)
median(dt$Population)

#the mean and trimmed mean differs widely suggesting the presence of outliers for "Population" feature

weighted.mean(dt$Murder.Rate,w=dt$Population)

#for the murder rate we use a weighted mean since each state has different population

quantile(dt$Murder.Rate)

#the median is 4 murders per 100.000 people , the range however is 9.4 suggesting a certain variability in
# the "Murder Rate" distribution .

IQR(dt$Murder.Rate)

#the IQR is 3.125  , to see more about the variability we can plot  : 

#for the murder Rate feature : 

boxplot(dt$Murder.Rate)

#for the population feature : 

boxplot(dt$Population/1000000,ylab="Population in millions")

#as we can see , there 4 outliers in the boxplot of population ,confirming the reason why the mean and trimmed mean are different
#to spot them ( outliers ) in the dataframe we can use : 

metric<-quantile(dt$Population,probs = 0.75)+IQR(dt$Population) * 1.5

filter(dt,dt$Population>=metric)

#now , let reform the dataset : 

breaks<-seq(from=min(dt$Population),to=max(dt$Population),length=11)

population_freq<-cut(dt$Population, breaks = breaks , right = T , include.lowest = T, dig.lab = 6)

res<-data.frame(population_freq,dt$State)

res<-res %>% group_by(population_freq, .drop=F) %>% summarise(count=n(),states=toString(unique(dt.State)))
View(res)

# the population feature was transformed to categorical data with interval with another column containing the states that falls in that interval

#now for the murder rate feature , we can plot a density plot : 
hist(dt$Murder.Rate , freq = F)

#to check the precise value as data frame , you can run : 
murder_freq<-cut(dt$Murder.Rate, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11) , right = T , include.lowest = T, dig.lab = 2)
res2<-data.frame(murder_freq,dt$State)
res2<-res2 %>% group_by(murder_freq, .drop=F) %>% summarise(count=n()/50)

#
lines(density(dt$Murder.Rate),lwd=3 ,col="blue")
