#Import Dataset
getwd()
setwd("C:/Users/ADMIN/Downloads")
melbourne<-read.csv("C:/Users/ADMIN/Downloads/Melbourne_housing.csv")

#Packages & libraries 
install.packages('tidyverse')
install.packages('janitor')
install.packages("dplyr")
install.packages("ggalt")
install.packages('GGally')
install.packages('ggridges')
install.packages('ggcorrplot')
install.packages('corrplot')
install.packages('treemapify')
install.packages("mosaic")
install.packages('visreg')
library("RColorBrewer")
library(tidyverse)
library(treemapify)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggalt)
library(GGally)
library(ggridges)
library(ggcorrplot)
library(corrplot)
library(lubridate)
library(visreg)
library(treemapify)

mycolors <- c( "#AEBDCA", "#DFF6FF", "#B4CDE6", "#98A8F8", "#5F9DF7",
              "#CDFCF6","#2146C7" ,"#7DE5ED")

mytheme <- theme(axis.text.x = element_text(angle = 90, size = 10, vjust = .4),
                 plot.title = element_text(size = 15, vjust = 2),
                 axis.title.x = element_text(size = 12, vjust = -.35))

mytheme2 <- theme(axis.text.x = element_text(size = 10, vjust = .4),
                  plot.title = element_text(size = 15, vjust = 2),
                  axis.title.x = element_text(size = 12, vjust = -.35))


#DATA CLEANING

#checking for null values
colSums(is.na(melbourne))
str(melbourne)

#removing unwanted columns
house <- subset( melbourne, select = -c(Car,Lattitude, YearBuilt, BuildingArea,Longtitude , Landsize, Longtitude) )
dim(house)

house2 <- subset( melbourne, select = c(Rooms, Distance, Price, Bedroom2, Bathroom, Car, Landsize, Lattitude, Longtitude, YearBuilt ) )
dim(house2)

#Replace missing value Price with 0
#house$Price[is.na(house$Price)] <- 0

#Replace missing value Price with NA
house$Price[is.na(house$Price)] <- NA
house$Price

#replacing empty values with NA
house[house==""]<-NA
colSums(is.na(house))

#removing rows with NA values 
data <- na.omit(house) 
colSums(is.na(data))
dim(data) #checking if removed

#renaming columns name
names(data) 
names(data) <- c("Suburb","Address","Rooms","Type","Price","Sales_Method",
                 "Agent","Date_Sold","Distance","Postcode","No_of_Bedroom",
                 "Bathroom","Council_Area","Region","Property_Count")

names(data)   #Checking for changes made

#Changing datatypes of columns
data$Rooms <- as.factor(data$Rooms)
data$Type <- as.factor(data$Type)
data$Sales_Method <- as.factor(data$Sales_Method)
data$Region <- as.factor(data$Region)
data$Property_Count <- as.integer(data$Property_Count)
data$Postcode <- as.integer(data$Postcode)
data$Distance <- as.numeric(data$Distance)
data$Date_Sold <- as.Date(data$Date_Sold, format = "%d/%m/%Y")

#renaming level of factor
levels(data$Type) <- list(House = "h", Townhouse = "t", Unit = "u")
levels(data$Sales_Method) <- list(`property sold` = "S",
                                      `property priorly sold` = "SP",
                                      `passed in` = "PI",
                                      `sold prior not disclosed` = "PN",
                                      `sold not disclosed` = "SN",
                                      `no bid` = "NB",
                                      `vendor bid` = "VB",
                                      `withdrawn prior to auction` = "W",
                                      `sold after auction` = "SA",
                                      `sold after auction price not disclosed` = "SS")

str(data)         #Checking for changes

summary(data)


#DATA VISUALIZATION


#Proportions type of property
pie(xtabs(~Type, data))

#treemap spred of Sales Method
plotdata <- data %>%
  count(Sales_Method)
ggplot(plotdata, 
       aes(fill = Sales_Method, 
           area = n)) +
  geom_treemap() + 
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Melbourne property Sales Method")

#treemap of Regions
plotdata <- data %>%
  count(Region)
ggplot(plotdata, 
       aes(fill = Region, 
           area = n)) +
  geom_treemap() + 
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Melbourne Regions")

#Spread of types of property
ggplot(data, aes(x=Type))+
  geom_bar(stat="count", width=0.7, fill="#7DE5ED")+
  theme_minimal()

#kernel density plot of year built
ggplot(house2, aes(x = YearBuilt)) +
  geom_density(fill = "#7DE5ED") +  
  labs(title = "Spread of year in which property built in melbourne")

#Spread of Rooms
ggplot(data, aes(x=Rooms))+
  geom_bar(stat="count", width=0.7, fill="#7DE5ED")+
  theme_minimal()




#Spread of no.of Rooms with respect to type of property
table(data$Type, data$Rooms)
barplot(table(data$Type, data$Rooms),
        main = "No.of rooms with respect to type of property",
        col=brewer.pal(n = 3, name = 'Blues')
)
legend("topright",
       c("House","Townhouse","unit"),
       fill=brewer.pal(n = 3, name = 'Blues'))

#Which type of real estate has highest Price based on the numbers of rooms?
Price_high <- aggregate(Price~Type+Rooms, data, FUN = max)
Price_high[order(Price_high$Price, decreasing = T),][1,]

ggplot(Price_high, aes(x=Type, y=Price))+
  geom_bar(stat="identity", width=0.7, fill="#7DE5ED")+
  theme_minimal()
#OR
ggplot(data, aes(x=Type, y=Price)) + geom_point()
ggplot(data, aes(x=Type, y=Price)) +
  scale_y_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000),
                     labels = c("$1m","$2m","$3m","$4m","$5m","$6m","$7m","$8m","$9m"))+
  geom_point(size=1.5, shape=23, colour="#5837D0")

#Property Price of region wrt to its price
table(data$Region, data$Type) %>%head(7)

barplot(table(data$Type, data$Region),
        main = "Region and it's property price",
        las=2,srt=45,xpd=TRUE,
        beside = TRUE,
        col=brewer.pal(n = 3, name = 'Blues'))
legend("topright",
       c("House","Townhouse","Unit"),
       fill=brewer.pal(n = 3, name = 'Blues'))

#scatter plot region vs Price
ggplot(data, aes(x=Price, y=Region)) + geom_point()
ggplot(data, aes(x=Price, y=Region)) +
  scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000),
  labels = c("$1m","$2m","$3m","$4m","$5m","$6m","$7m","$8m","$9m"))+
  geom_point(size=2, shape=23, colour="#5837D0")

#Co relation of data 
df <- dplyr::select_if(data, is.numeric)
corG <- cor(df, use="complete.obs")
round(corG,2)
ggcorrplot(corG)

#Region with Price Distributions
ggplot(data, aes(Region, Price)) +
  geom_boxplot(fill="#5837D0") +
  scale_y_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000),
  labels = c("$1m","$2m","$3m","$4m","$5m","$6m","$7m","$8m","$9m"))+
  theme(legend.position = "none") +
  xlab("Region") +
  ylab("Price") +
  ggtitle("Region with Price Distributions") +
  coord_flip()

#Rate price of townhouse every suburb
townhouse <- data[data$Type == "Townhouse",]
townhouse_2 <- aggregate(Price~Type+Suburb, townhouse, mean)
townhouse_2[order(townhouse_2$Price, decreasing = T), ][1:3,]
#Rate price of house every suburb
house1 <- data[data$Type == "House",]
house_2 <- aggregate(Price~Type+Suburb, house1, mean)
house_2[order(house_2$Price, decreasing = T), ][1:3,]
#Rate price of unit every suburb
unit <- data[data$Type == "Unit",]
unit_2 <- aggregate(Price~Type+Suburb, unit, mean)
unit_2[order(unit_2$Price, decreasing = T), ][1:3,]


#expensive Suburb with it's region wrt price
table(data$Price, data$Region)
region_high <- aggregate(Price~Region+Suburb, data, FUN = max)
region_high[order(region_high$Price, decreasing = T),][1:5,]

#Which suburb region has the most property count?
table(data$Property_Count, data$Region)
table(data$Suburb, data$Price) %>% head(5)
sub_high <- aggregate(Property_Count~Suburb+Region, data, sum)
sub_high[order(sub_high$Property_Count, decreasing = T),][1:5,]

#Council area with suburb
data %>%
  group_by(Suburb) %>%
  summarise(Count = n()) %>% 
  head(10) %>%
  ggplot(aes(reorder(Suburb, Count), Count)) +
  geom_bar(stat = 'identity', fill="#5837D0") +
  mytheme+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  coord_flip() +
  xlab("Council Area") +
  ylab("Count") +
  ggtitle("Council Area Frequencies")

#The highest of total price based suburb and type of real estate ?
total_price <- aggregate(Price~Suburb+Type, data, sum)
total_price[order(total_price$Price, decreasing = T),][1:3,]


#How much rate price of unit in 2018?
data$year = as.numeric(format(data$Date_Sold, "%Y"))
unit_18 <- aggregate(Price~Type+Date_Sold, data, mean)
unit_18[(unit_18$Type == "u" & unit_18$Date_Sold == 2017),]

#Which of council area is closest to Central Business Development?
council <- aggregate(Distance~Council_Area, data, min)
council[order(council$Distance, decreasing = F),][1:10,]



#Total price every year?
year_total <- xtabs(Price~year, data)
options(scipen = 999)
barplot(year_total,
        col=brewer.pal(n = 3, name = 'Blues'))
title(main = "Total Price Household Market per Year",
      ylab = "Total Price")

#Method of Sales every Type of real estate?
table(droplevels(data$Sales_Method), data$Type)
barplot(xtabs(~Type + Sales_Method, data), las=2,
        ylim = c(0,50000), ylab = "Total",
        col=brewer.pal(n = 3, name = 'Blues'))
legend("topright",
       c("House","Townhouse","unit"),
       fill=brewer.pal(n = 3, name = 'Blues'))

#Which property agent has the highest rate price for house?
mean_house <- data[(data$Type == "House"),]
mean_high <- aggregate(Price~Agent+Type, mean_house, mean)
mean_high[order(mean_high$Price, decreasing = T),][1,]
#Which property agent has the highest rate price for unit?
mean_unit <- data[(data$Type == "Unit"),]
mean_unit <- aggregate(Price~Agent+Type, mean_unit, mean)
mean_unit[order(mean_unit$Price, decreasing = T),][1,]
#Which property agent has the highest rate price for townhouse?
mean_Thouse <- data[(data$Type == "Townhouse"),]
mean_Thouse<- aggregate(Price~Agent+Type, mean_Thouse, mean)
mean_Thouse[order(mean_Thouse$Price, decreasing = T),][1,]

#Top10 Agent wrt property sold
top10_agent <- data %>% group_by(Agent) %>%
  summarise(Number = n()) %>% arrange(desc(Number)) %>%
  head(5)
ggplot(top10_agent, aes(reorder(Agent, Number), Number, fill = Agent))+
  geom_bar(stat = "identity")+
  mytheme2+
  theme(legend.position = "none")+
  labs(x = "Agent Names", y = "Number of Property Sold",
       title = "Top 10 Agent")+
  scale_fill_manual(values = mycolors)+
  coord_flip()



#Price Distribution of property Type
ggplot(data, aes(Type, Price)) +
  geom_boxplot(outlier.colour = "black", fill="#5837D0") + 
  scale_x_discrete(labels = c('Houses','Townhouses','Units')) +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Type of Property") +
  ylab("Price") +
  ggtitle("Price Distribution of Property Type")


#Council Area that has most property
data %>%
  group_by(Council_Area) %>%
  summarise(Count = n()) %>% 
  head(10) %>%
  ggplot(aes(reorder(Council_Area, Count), Count)) +
  geom_bar(stat = 'identity', fill="#5837D0") +
  mytheme+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  coord_flip() +
  xlab("Council Area") +
  ylab("Count") +
  ggtitle("Council Area with most property")

#Top10 Suburbs by the Number of Houses
top10sub_by_houses <- data %>% group_by(Suburb) %>%
  summarise(Number = n()) %>% arrange(desc(Number)) %>%
  head(5)

ggplot(top10sub_by_houses, aes(reorder(Suburb, Number), Number, fill = Suburb))+
  geom_bar(stat = "identity")+
  mytheme2+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Number of Houses",
       title = "Top10 Suburbs by the Number of Houses")+
  scale_fill_manual(values = mycolors)+
  coord_flip()

suburb_vs_price <- data[c("Suburb","Price")] %>% na.omit() 
top10sub_by_averprice <- suburb_vs_price %>% group_by(Suburb) %>%
  summarise(Average = sum(Price)/n()) %>%
  arrange(desc(Average)) %>% head(6)

ggplot(top10sub_by_averprice, aes(reorder(Suburb, Average), Average, fill = Suburb))+
  geom_bar(stat = "identity")+
  mytheme2+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Average Price of House",
       title = "Top 10 Suburbs by the Average Price of House")+
  scale_fill_manual(values = mycolors)+
  coord_flip()

#Suburb vs Price
p <- ggplot(data, aes(x=Suburb, 
                     y=Price,
                     las=2,srt=45,xpd=TRUE,
                     color=Suburb)) +
  geom_point(size=3) +
  scale_y_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000),
        labels = c("$1m","$2m","$3m","$4m","$5m","$6m","$7m","$8m","$9m"))+
  labs(color = "Car Class") +
  theme_bw()
ggplotly(p)


#Linear Regression
houseDist <- lm(Price ~ Distance + Bathroom + Bedroom2 + Landsize,
                data = house2)
visreg(houseDist, "Bedroom2", gg = TRUE) 
visreg(houseDist, "Landsize", gg = TRUE) 
