install.packages("mapdata")
install.packages("countrycode")
install.packages("maps")
install.packages("ggmap")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(countrycode)
library(maps)
library(ggmap)
library(mapdata)

ranking <- read.csv("C:/Users/moharahman/Downloads/times_world_university_rankings.csv")
View(ranking)

attach(ranking)
ranking_a <- data.frame(Country, Teaching_Rating, Inter_Outlook_Rating, Research_Rating, Citations_Rating, Industry_Income_Rating, Total_Score, Num_Students, percent_Inter_Students, Year)

ranking_b <- set_names(ranking_a, tolower(names(ranking_a[1:10])))

View(ranking_b)

count(ranking_b, country)
ranking_b <- mutate(ranking_b, country = recode(.x=country, "Unisted States of America"="United States of America"))
ranking_b <- mutate(ranking_b, country = recode(.x=country, "Unted Kingdom"="United Kingdom"))
str(ranking_b)

ranking_b$country <- as.factor(ranking_b$country)
ranking_b$inter_outlook_rating <- as.numeric(ranking_b$inter_outlook_rating)
ranking_b$industry_income_rating <- as.numeric(ranking_b$industry_income_rating)
ranking_b$total_score <- as.numeric(ranking_b$total_score)
ranking_b$year <- as.factor(ranking_b$year)

summary(ranking_b)

round(colMeans(ranking_b[2:9], na.rm = T), digits = 3)
round(cor(ranking_b[2:9], use = "complete.obs"), digits = 3)

#Let's add a new column continent (countrycode package us installed above)

ranking_b$continent <- countrycode(sourcevar = ranking_b[,"country"], 
                         origin = "country.name",
                         destination = "continent")

str(ranking_b)
ranking_b$continent <- as.factor(ranking_b$continent)

#1.Teaching Rating & Total Score

ggplot(data = ranking_b, aes(x=teaching_rating, y=total_score, color=continent))+
  geom_point(shape= 15, size=2)+
  labs(x= "Teaching Rating",
       y= "Total Score",
       title = "Relationship Between Teaching Rating & Total Score",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016 ")+
  stat_smooth(method="lm", se=F)+
  facet_wrap(~year)+
  theme_bw() + 
  theme(legend.position="right")+
  scale_colour_discrete("Continent")

#2.Research Rating & Total Score

ggplot(data = ranking_b, aes(x=research_rating, y=total_score, color=continent))+
  geom_point(shape= 20, size=3)+
  labs(x= "Research Rating",
       y= "Total Score",
       title = "Relationship Between Research Rating & Total Score",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016 ")+
  stat_smooth(method="lm", se=F)+
  facet_wrap(~year)+
  theme_bw() + 
  theme(legend.position="right")+
  scale_colour_discrete("Continent")

#3.Citation Rating & Total Score

ggplot(data = ranking_b, aes(x=citations_rating, y=total_score, color= continent))+
  geom_point()+
  labs(x= "Citation Rating",
       y= "Total Score",
       title = "Relationship Between Citation Rating & Total Score",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016")+
  stat_smooth(method="lm", se=F)+
  facet_wrap(~year)+
  theme_bw() + 
  theme(legend.position="right")+
  scale_colour_discrete("Year")

#4.International Students & International Outlook Rating

ggplot(data = ranking_b, aes(x=percent_inter_students, y=inter_outlook_rating, color= continent))+
  geom_point()+
  labs(x= "Percentage(%) of International Students",
       y= "International Outlook Rating",
       title = "Relationship Between Percentage of International Students & International Outlook Rating",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016")+
  stat_smooth(method="lm", se=F)+
  facet_wrap(~year)+
  theme_bw() + 
  theme(legend.position="right")+
  scale_colour_discrete("Year")

#5.Ranking Scores in Different Continents

ggplot(data = ranking_b, aes(x=total_score, fill=continent))+
  geom_boxplot()+
  labs(x= "Total Score",
       title = "Ranking Score in Different Continents",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016", fill="Continent")+
  facet_wrap(~year)+
  theme_gray() + 
  theme(legend.position="right")

#6.Number of Universities from different Continents

ggplot(data = ranking_b, aes(x=continent, fill=continent))+
  geom_bar(aes())+
  labs(x= "Continent",
       y= "Number of Universities",
       title = "Universities from different Continents",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016", fill="Continent")+
  facet_wrap(~year)+
  theme_gray() + 
  theme(legend.position="right")

#7.Number of Universities From Different Countries

ggplot(data = ranking_b, aes(x=country, fill= continent))+
  geom_bar(aes(), show.legend = T)+
  labs(x= "Country",
       y= "Number of Universities",
       title = "Number of Universities From Different Countries",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016", fill="Continent")+
  theme_gray() + 
  theme(legend.position="right")+
  theme(axis.text.x = element_text(angle = 90))

#8.Ranking Score in the World Map: 

world <- map_data("world")
View(world)

world_b <- mutate(world, region = recode(.x=region, "USA"="United States of America"))
world_c <- mutate(world_b, region = recode(.x=region, "UK"="United Kingdom"))
world_d <- mutate(world_c, region = recode(.x=region, "Russia"="Russian Federation"))
world_e <- mutate(world_d, region = recode(.x=region, "Ireland"="Republic of Ireland"))

rank <- ranking_b
names(rank) [1] <- "region"

world_1 <- left_join(world_e, rank, by = "region")
world_2 <- world_1 %>%
  filter(!is.na(world_1$teaching_rating))
View(world_2)

ggplot(data = world_1, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=teaching_rating))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill=NA, color = "red", size=.1) + 
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", title = "Ranking Score in the World Map",
       subtitle = "World University Ranking",
       caption = "Dataset: Times World University Ranking 2011-2016", fill="Teaching Rating(instead of total score)")+
  theme(legend.position="right")
  

