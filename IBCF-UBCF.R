library(tidyverse)
library(splitstackshape)
library(tidytext)
library(shiny)
library(DT)
library(shinyWidgets)
library(shinythemes)


# Set working directory to file location

movies <- read_csv("movies.csv")
ratings <- read_csv("ratings.csv")
genome_scores <- read_csv("genome-scores.csv")
genome_tags <- read_csv("genome-tags.csv")
links <- read_csv("links.csv")
tags <- read_csv("tags.csv")


#No of movies in each genres
movies %>% 
  cSplit('genres', '|', 'long') %>% 
  group_by(genres) %>%
  summarize(No_of_movies = n()) %>% 
  mutate(reorder_within(No_of_movies,No_of_movies,genres)) %>% 
  ggplot()+
  geom_col(aes(x = genres, y = No_of_movies, fill = genres)) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x ="Genres", y = "Number of Movies", title = "Number of movies in each genres") +
  scale_x_reordered()


# Data Cleaning

movies_list<- movies %>% 
  cSplit('genres', '|', 'long') 

genre1<- c("Adventure")

movies_list %>% 
  filter(genres == genre1) %>% 
  select(title)


#--
ratings %>% 
  group_by(userId) %>% 
  summarize(average = mean(rating)) %>% 
  ggplot()+
  geom_point(aes(x=userId,y=average))

#--
movies %>% 
  cSplit('genres', '|', 'long') %>%
  count(movieId) 

#Average movie rating
movies %>% 
  cSplit('genres', '|', 'long') %>%
  left_join(ratings) %>% 
  group_by(movieId) %>% 
  summarize(No_of_genres = n_distinct(genres), Average_rating = mean(rating))

ratings %>% 
  count(userId) %>% 
  filter(n>1)

# Remove duplicate movies

#movies[duplicated(movies$title),'title']
#movies[movies$title == 'War of the Worlds (2005)',]
movies_new %>% 
  cSplit('genres', '|', 'long') %>% 
  filter(genres == 'Adventure') %>% 
  right_join(ratings) %>% 
  select('userId','movieId','rating','title') %>% 
  head(100000) %>% 
  select('userId','title','rating') %>% 
  group_by(userId) %>% 
  spread(key=title, value = rating)



########################################### IBCF #############################################

genre_list <- movies %>% 
  cSplit('genres', '|', 'long') %>% 
  select(genres) %>% 
  distinct(genres)

movies_new <- movies[!duplicated(movies$title),]
userList <- ratings %>% select(userId) %>%  
  unique() %>% head(10000)

movies_split <- movies_new %>% 
  cSplit('genres', '|', 'long') 

ratingsMovies <- 
  userList %>% inner_join(ratings) %>%
  left_join(movies_split) 

cf_matrix<- ratingsMovies %>%   
  #filter(genres == "Thriller") %>% 
  select('userId','title','rating') %>% 
  cast_dtm(userId,title,rating)


ibcf_matrix <- cf_matrix %>% 
  as.matrix() %>% 
  as.data.frame()

cor_values<-ibcf_matrix%>% 
  cor(ibcf_matrix[,c('Sudden Death (1995)','Young Guns (1988)','Inside (1996)')],use="pairwise.complete.obs") 
print(typeof(cor_values))
typeof(final_matrix)
final_matrix <-tibble(a=colnames(ibcf_matrix), b=cor_values)
typeof(result)
result<-final_matrix %>% 
  filter(!(a %in% c('Sudden Death (1995)','Young Guns (1988)','Inside (1996)'))) 
typeof(ibcf_output)
ibcf_output<-tibble(title = result$a,sum = result$b %>% as.data.frame() %>% 
                      mutate(sum = V1+V2+V3) %>% 
                      select(sum) %>% 
                      arrange(desc(sum))) %>% 
  select(title) %>% 
  rename(Movie = title) %>% 
  head(10)

########################################### UBCF #############################################

ubcfMatrix <- t(ibcf_matrix)

cor_values_ubcf<-ubcfMatrix %>% 
  cor(ubcfMatrix[,1],use="pairwise.complete.obs") 

final_matrixUBCF <-tibble(colnames(ubcfMatrix),cor_values) %>% 
  rename("userId"=`colnames(ubcfMatrix)`)

### Select the 4 most similar users
ubcfSelectUsers <- 
  final_matrixUBCF[order(-cor_values),] %>% head(4)

### Add userId as a column
tempUsers <- cbind(userId= rownames(ibcf_matrix),ibcf_matrix)

### Random Error- May fail later
tempUsers <- tempUsers[,-c(2886)]

### Join with IBCF
tempUserSelectIbcf <- tempUsers %>%
  filter(userId %in% ubcfSelectUsers$userId)

### Add userId as the rownames
rownames(tempUserSelectIbcf) <- tempUserSelectIbcf$userId 

### Transpose and add movies as a column
tempTransposeUserIbcf <- tempUserSelectIbcf %>% select(-c(userId)) %>% t()

MoviesSimilarUsers <- cbind(title= rownames(tempTransposeUserIbcf),tempTransposeUserIbcf)

MoviesSimilarUsers <- MoviesSimilarUsers %>% as.data.frame()

### Select those movies which have not been seen by the selected user and 
### seen by atleast one of the similar users

MoviesRecommendUbcf <- MoviesSimilarUsers %>% 
  filter(`1` == 0, MoviesSimilarUsers[,3]!=0 | MoviesSimilarUsers[,4]!=0 |
           MoviesSimilarUsers[,5]!=0)

### Add their ratings, and take the top 10 movies as per the ratings' sum

MoviesRecommend <- MoviesRecommendUbcf %>% 
  mutate(movieScore = as.numeric(levels(MoviesRecommendUbcf[,3])[MoviesRecommendUbcf[,3]]) +
           as.numeric(levels(MoviesRecommendUbcf[,4])[MoviesRecommendUbcf[,4]])+
           as.numeric(levels(MoviesRecommendUbcf[,5])[MoviesRecommendUbcf[,5]])) %>%
  arrange(desc(movieScore)) %>% head(10) %>% rename(`Movie`= title) %>%
  select(`Movie`)

################################### Evaluate Metrics ##############################


### Select those movies which have not been seen by the selected user and 
### seen by atleast one of the similar users

MoviesRecommendUbcfValidate <- MoviesSimilarUsers %>% 
  filter(`26` != 0, MoviesSimilarUsers[,3]!=0 | MoviesSimilarUsers[,4]!=0 |
           MoviesSimilarUsers[,5]!=0)

MoviesRecommendUbcfValidate[MoviesRecommendUbcfValidate == 0] <- NA

MoviesRecommendUbcfValidate<- MoviesRecommendUbcfValidate %>% mutate(
  userActualRating= as.numeric(levels(MoviesRecommendUbcfValidate[,2])[MoviesRecommendUbcfValidate[,2]]),
  user1= as.numeric(levels(MoviesRecommendUbcfValidate[,3])[MoviesRecommendUbcfValidate[,3]]),
  user2= as.numeric(levels(MoviesRecommendUbcfValidate[,4])[MoviesRecommendUbcfValidate[,4]]),
  user3= as.numeric(levels(MoviesRecommendUbcfValidate[,5])[MoviesRecommendUbcfValidate[,5]]))

MovieRecoPredRating <- MoviesRecommendUbcfValidate %>% mutate(
  predictedRating=rowMeans(MoviesRecommendUbcfValidate[,7:9],na.rm = TRUE)) %>%
  select(-c(2:5),-c(7:9))

MovieRecoPredRating <- MovieRecoPredRating %>% mutate(`How did the user rate the movie?`= 
                                                        ifelse(userActualRating>=3.5,"Good","Bad"),
                                                      `Prediction: Will the user like the movie?`=ifelse(predictedRating>=3.5,"Good","Bad")) %>%
  mutate(Error=ifelse(`How did the user rate the movie?`!=`Prediction: Will the user like the movie?`,1,0)) 

MovieRecoPredRating

MovieRecoPredRating %>%
  summarise(Accuracy=1-(sum(Error)/n()))
