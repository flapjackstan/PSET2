getwd()
setwd("C:/Users/Elmer/Documents/R/Statistical Modeling/PSET2")

# Question One (ISLR Q2)

# a) Regression. n(sample) = whatever subset we pick, p(predictors) = the vars

# b) classification
  # n = 20 similair products, p = sucees, falire, price, mark budget,comp price, and 10 other vars

# c) TBD...

# Question Two (ISLR Q4)
# a,b,c) TBD, easy


# Question 3, Plotting IMDB's Top 5000 Movies

# a) csv downloaded
# b) 

library('tidyverse')
movies <- read.csv("data/movie_metadata.csv")
movies <- movies %>% filter(budget <4e+08)
movies <- movies %>% mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),
                                                             "\\|"),1)), grossM = gross /1e+06, budgetM = budget/1e+06)
movies <- movies %>% mutate(genre_main = factor(genre_main) %>% fct_drop())

# c)

names(movies)


movies <- movies %>% mutate(
  profitM = grossM - budgetM,
  ROI = profitM / budgetM 
)




# d)
 ggplot(data = movies, aes(ROI))+geom_histogram()
#^gives a really shitty grapg :(
 
 movies <- movies %>% drop_na(ROI)
 
 sum(is.na(movies$ROI))
 
 mean(movies$ROI) 
 max(movies$ROI)
 
ggplot(movies, aes(x=ROI))+geom_histogram(color="black",fill="white")
 # ^no mapping? no data = ?
 
# e) 
 # ?count
count(movies, vars = ROI >= 10)
# ^not sure if this is counting properly, don't understand the output
# still need to filter out ROI >10 

movies_filt <- movies %>% filter(ROI <= 10)

count(movies_filt, vars = ROI >= 10)

ggplot(data = movies_filt, aes(ROI))+geom_histogram()

?count

# f)
?group_by()
# (???) se_ROI_genre = sd(ROI, na.rm = TRUE)/sqrt(n)))

test1 <-group_by(movies_filt, genre_main)

df <- summarise(test1, mean(ROI))
average_roi <- df[2]
df
?summarise
movies_filt_2 <- movies_filt %>% group_by(genre_main)

   
movies_filt_2 <-movies_filt_2 %>%  group_by(genre_main)
movies_filt_2 <-movies_filt_2 %>% select(genre_main)
# g)

y <- df$`mean(ROI)`
x <- df$genre_main
ggplot( data = df, mapping = aes(x = x, y = y)) +geom_point() +labs(x="Genre_Main", y="Average(ROI)")

# h)

test3 <- group_by(movies_filt,actor_1_name)

df2 <- summarise(test3,mean(ROI),mean(profitM),num_films = n())
x2 <- df2$`mean(ROI)`
df2 <- df2 %>% arrange(desc(x2))


df2 <- df2 %>% slice(1:20)

df2


# i)
ggplot(data = movies, aes(x = actor_1_name , y = ROI))+geom_point()
# ^it prints

ggplot(data = movies, aes(x = actor_1_name , y = ROI))+geom_point()

movies_h <- movies %>% slice(1:30)
?top_n

# ggplot(data = test3 %>% top_n(30, wt = x2), mapping = aes(x=fct_reorder(actor_1_name,x2) %>% fct_drop))

df3 <- summarise(test3,mean(ROI))
y3 <- df3$actor_1_name
x3 <- df3$`mean(ROI)`
df3 <- df3 %>% arrange(desc(x3))
df3 <- df3 %>% slice(1:30)
y3 <- df3$actor_1_name
x3 <- df3$`mean(ROI)`
df3
ggplot(df3,aes(x=x3,y=y3))+geom_point()


ggplot(df3 = df3 %>% top_n(30, wt = x3), mapping = aes(x=x3,y=y3))+geom_point()


ggplot(df3 = df3 %>% top_n(30, wt = x3), mapping = aes(x = x3, y = reorder(y3, x3)))+geom_point()


?reorder


plot(x3,y3, main = "Top 30 Actors")
df3


x3
y3

# j)

df4 <- summarise(test3,mean(ROI))
y4 <- df4$actor_1_name
x4 <- df4$`mean(ROI)`
df4 <- df4 %>% arrange((x4))
df4 <- df4 %>% slice(30:1)
y4 <- df4$actor_1_name
x4 <- df4$`mean(ROI)`
df4
ggplot(df4,aes(x=x4,y=y4))+geom_point()
ggplot(df4 = df4 %>% top_n(30, wt = x4), mapping = aes(x = x4, y = reorder(y4, x4)))+geom_point()