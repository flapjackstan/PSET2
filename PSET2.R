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

movies_filt <- group_by(movies_filt, actor_1_name)
actors_and_mean_roi <- summarize(movies_filt, mean(ROI), profitM)
actor_meanROI <- actors_and_mean_roi$`mean(ROI)`
act_and_roi_slice <- actors_and_mean_roi %>% arrange(desc(actor_meanROI)) %>%
                                             slice(1:20)  
                                        
act_and_roi_slice