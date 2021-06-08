rm(list=ls())   
install.packages("tidyverse")
library(ggplot2)
library(dplyr)

       
data_covid <- read.csv("C:/Users/priyo/Downloads/covid_R/COVID19_line_list_data.csv")

describe(data_covid)  

data_covid$death_dummy <- as.integer(data_covid$death != 0)  
View(data_covid)

unique(data_covid$death_dummy)


total_count = data_covid %>%
  group_by(country)%>%
  count(country)

ggplot(total_count,aes(x = reorder(country, -n) , y = n, fill=country)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), vjust= -0.50) + 
  labs(x="Countries",y = "Count" , title = "Total cases across the globe")

total_number = data_covid %>%
  count(death_dummy)

ggplot(total_number,aes(x = reorder(death_dummy, -n) , y = n)) + 
  geom_bar(stat = "identity", fill = "green" , color = "yellow", alpha = 0.5 ) + 
  geom_text(aes(label = n), vjust= -0.50) + 
  theme_dark() +
  labs(x="Alive = 0  Dead = 1",y = "Count" , title = "Total cases")

total_deaths = sum(data_covid$death_dummy)
death_rate = (total_deaths/nrow(data_covid))*100
death_rate



alive = subset(data_covid, death_dummy == 0) 
View(alive)                                 
nrow(alive)                                 

dead = subset(data_covid, death_dummy == 1)  
nrow(dead)                                   


mean(alive$age , na.rm = TRUE) 
mean(dead$age, na.rm = TRUE)
boxplot(alive$age,dead$age,main ="Box plot by Age" , xlab= "Green - Alive   Red - Dead",ylab="Age", col = c("green","red"))      



Alive_age_na=na.omit(alive$age)               
plot(density(Alive_age_na),col = "green",main = "Alive density")  

Dead_age_na=na.omit(dead$age)
plot(density(Dead_age_na),col = "red",main = "Dead density")


ggplot(alive, aes(x = age)) + 
  geom_density(fill = "purple" , alpha = 0.5) +
  geom_vline(xintercept = 25 , size = 1 , color = "yellow",linetype = "dashed")+
  theme_dark()+expand_limits(x=c(20,105), y=0)

ggplot(dead, aes(x = age)) +
  geom_density(fill = "blue" , alpha = 0.5)+
  geom_vline(xintercept = 25 , size = 1 , color = "yellow",linetype = "dashed")+
  theme_dark()+expand_limits(x=c(20,105), y=0)


t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)


# claim: gender has no effect

men = subset(data_covid, gender == "male")  
View(men)                                   
women = subset(data_covid, gender == "female")



gender_specific = data_covid %>%
  count(gender)

ggplot(gender_specific,aes(x = reorder(gender, -n) , y = n)) + 
  geom_bar(stat = "identity", fill = c("blue","red","yellow") , color = "black", alpha = 0.8 ) + 
  geom_text(aes(label = n), vjust= -0.50) + 
  theme_linedraw() +
  labs(x="Gender", y = "Count", title = "Gender Count")

men_total = men %>%
  count(death_dummy)

ggplot(men_total,aes(x = reorder(death_dummy, -n) , y = n)) + 
  geom_bar(stat = "identity", fill = c("green","red") , color = "black", alpha = 0.5 ) + 
  geom_text(aes(label = n), vjust= -0.50) + 
  theme_grey() +
  labs(x="Green - Alive  Red -Dead",y = "Count" , title = "Total cases")

mean(men$death_dummy, na.rm = TRUE)*100



women_total = women %>%
  count(death_dummy)
ggplot(women_total,aes(x = reorder(death_dummy, -n) , y = n)) + 
  geom_bar(stat = "identity", fill = "blue" , color = "black", alpha = 0.5 ) + 
  geom_text(aes(label = n), vjust= -0.50) + 
  theme_grey() +
  labs(x="Alive = 0  Dead = 1",y = "Count" , title = "Total cases")

mean(women$death_dummy, na.rm = TRUE)*100


t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)

