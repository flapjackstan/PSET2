library(tidyverse)

rm(list=ls())
wd = setwd("C:/Users/Elmer/Documents/R/Statistical Modeling/PSET2")

#a
#movies <- read.csv("data/movie_metadata.csv")

#b
movies <-read.csv("data/movie_metadata.csv")

movies <- movies %>% filter(budget<4e+08) #get rid of anomolies

movies <- movies %>% mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),
                                                           "\\|"), 1)), grossM = gross/1e+06, budgetM = budget/1e+06)

movies <- movies %>% mutate(genre_main = factor(genre_main)%>%
                            fct_drop())

#c
movies <- movies %>% 
  mutate(profitM = grossM - budgetM,
         ROI = profitM / budgetM)

#d
sum(is.na(movies$ROI))

#movies <- na.omit(movies) #omits all NA values in a df
movies <- movies %>% drop_na(ROI) #omits NA values in a column 

sum(is.na(movies$ROI))

cat('average ROI is', mean(movies$ROI))

hgp1<-ggplot(movies, aes(x=ROI)) + 
  geom_histogram(color="black", fill="white", binwidth = 500)
hgp1

ggsave("output/histogram1.png", width = 8, height = 8)

#e
count(movies, vars = ROI > 10)

movies_filt <- movies %>% filter(ROI < 10) #we want/keep everything < 10 

count(movies_filt, vars = ROI > 10)

ggplot(data = movies_filt, aes(ROI))+
  geom_histogram(color="black", fill="white", binwidth = 1) 
ggsave("output/histogram2.png", width = 8, height = 8)

#f

average_roi_bycat <- movies_filt %>% 
  group_by(genre_main) %>% 
  summarize(mean(ROI))

average_roi_bycat

#g

genre_meanROI <- average_roi_bycat$`mean(ROI)`
genre <- average_roi_bycat$genre_main

sp1 <- ggplot( data = average_roi_bycat)+ 
  geom_point(mapping = aes(x = genre, y = genre_meanROI)) +
  labs(x= "Genre",
       y= "Average(ROI)",
       title= "Average ROI by Genre")

ggsave("output/scatterplot1.png", width = 10, height = 8)

#h
test3 <- group_by(movies_filt,actor_1_name)

df2 <- summarise(test3,mean(ROI),mean(profitM),num_films = n())
x2 <- df2$`mean(ROI)`
df2 <- df2 %>% arrange(desc(x2))


df2 <- df2 %>% slice(1:20)

df2

#i


df3 <- summarise(test3,mean(ROI))
y3 <- df3$actor_1_name
x3 <- df3$`mean(ROI)`
df3 <- df3 %>% arrange(desc(x3))
df3 <- df3 %>% slice(1:30)
y3 <- df3$actor_1_name
x3 <- df3$`mean(ROI)`
df3

ggplot(df3 = df3 %>% top_n(30, wt = x3),
       mapping = aes(x = x3, y = reorder(y3, x3)))+
       geom_point()+
       labs(x= "ROI",
            y= "Actors",
            title= "Top 30 Actors by ROI")
ggsave("output/scatterplot2.png", width = 8, height = 8)



#j

df4 <- summarise(test3,mean(ROI))
y4 <- df4$actor_1_name
x4 <- df4$`mean(ROI)`
df4 <- df4 %>% arrange((x4))
df4 <- df4 %>% slice(30:1)
y4 <- df4$actor_1_name
x4 <- df4$`mean(ROI)`
df4

ggplot(df4 = df4 %>% top_n(30, wt = x4), 
       mapping = aes(x = x4, y = reorder(y4, x4)))+
       geom_point()+
       labs(x= "ROI",
            y= "Actors",
            title= "Lowest 30 Actors by ROI")
ggsave("output/scatterplot3.png", width = 8, height = 8)