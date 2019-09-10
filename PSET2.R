library(tidyverse)
output_dir = "/output"
wd = setwd(here::here("data"))

#a
#movies <- read.csv("data/movie_metadata.csv")

#b
movies <-read.csv("movie_metadata.csv")

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

mean(movies$ROI)
max(movies$ROI)

# sp1 <- ggplot(movies, aes(x = grossM, y = genre_main, fill = genre_main)) + 
#   geom_density_ridges() + 
#   scale_x_continuous(limits = c(0, 500)) +
#   labs(x = "Box Office Gross (USD Millions)", 
#        y = "Main Genre")

hgp1<-ggplot(movies, aes(x=ROI)) + 
  geom_histogram(color="black", fill="white")
hgp1
