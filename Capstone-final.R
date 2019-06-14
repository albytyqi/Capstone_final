#############################################################
# The Project: Recommendation System for Google Playstore App
#############################################################

####Analysing the structure of the data#####
#install.packages("kableExtra")
#library(kableExtra)
library(tidyverse)
library(readr)
library(stringr)
#Extracting the data#


googleplaystore <- read_csv("googleplaystore.csv")

print("dimension:")
dim(googleplaystore)

print("Structure:")
str(googleplaystore)


#how many non-numeric in numeric columns
sum(is.na(as.numeric(googleplaystore$Rating)))
x<-str_which(googleplaystore$Rating,"NaN") # list of all rows with Rating =NaN
googleplaystore[c(x),] # since other entries seem to be correct, at this stage we will not delete any of the rows with NaN as Rating


sum(is.na(as.numeric(googleplaystore$Reviews)))
googleplaystore %>% filter(is.na(as.numeric(googleplaystore$Reviews))) #this line will be taken out later  when we clean the dataframe for the "Installs" column

pattern <- "^[A-z]"
sum(str_detect(googleplaystore$Size,pattern)) # so there are 9146 out of 10473 entries that  match with a size description
googleplaystore%>% group_by(Size)%>% summarise(count=n()) %>% arrange(desc(count)) # we see that the one is not a mistake but netry corresponds to "varies by device"

googleplaystore %>% group_by(Type)%>% summarise(count=n()) # there is one NaN and one 0, and the line with Type=0 is the same line that will be taken out when clearing for Reviews
s<-"NaN"

googleplaystore[str_which(googleplaystore$Type,s),]#since data are unrelevant, this line will be take out
str_which(googleplaystore$Type,s)

pattern_I<-"^[a-z,A-Z]"
sum(str_detect(googleplaystore$Installs,pattern_I))
str_which(googleplaystore$Installs,pattern_I)#	Returns the subset of strings that contain the pattern

googleplaystore[c(str_which(googleplaystore$Installs,pattern_I)),] #we see that genrally this App has wrong entries in nearly all columns

New_googleplaystore<-googleplaystore[-c(str_which(googleplaystore$Type,s),str_which(googleplaystore$Installs,pattern_I)),] #we took out the line with wrong inputs since it is also correlating the wrong Reviews
str(New_googleplaystore)

#Since for validation set and test set we need the ratings to be numerical and none Nan, we will need to take out the NaN entries from the dataframe
x<-str_which(New_googleplaystore$Rating,"NaN") # list of all rows with Rating =NaN
New_googleplaystore<-New_googleplaystore[-c(x),]

New_googleplaystore %>% group_by(Type)%>% summarise(count=n())
New_googleplaystore %>% group_by(Installs)%>% summarise(count=n()) 
New_googleplaystore %>% group_by(Price)%>% summarise(count=n()) %>% arrange(desc(count))
New_googleplaystore %>% group_by(`Content Rating`)%>% summarise(count=n()) 
New_googleplaystore %>% separate_rows(Genres)%>%group_by(Genres)%>% summarise(count=n()) %>% print(n=119)
New_googleplaystore %>% group_by(App) %>% summarize(count=n())%>%arrange(desc(count))
l<-New_googleplaystore %>% group_by(App) %>% summarize(count=n())%>% filter(count>1) %>%arrange(desc(count))
dim(l)[1]
#it seems that 798 Apps are repeated, see below example of most frequently repated Apps
New_googleplaystore %>% filter(App== "Helix Jump")

New_googleplaystore %>% filter(App %in% (l[1:798,1])) #next step is to take out the duplicated App entris
New_googleplaystore<-New_googleplaystore %>% distinct(App, .keep_all = TRUE) 

#change the variable type for Installs from chr to number 
New_googleplaystore<-New_googleplaystore %>% mutate(Installs= as.numeric(str_replace_all(Installs,"[+/,]","")))
str(New_googleplaystore)

####rating distribution####


n_App <- n_distinct(New_googleplaystore$App)
n_Category <- n_distinct(New_googleplaystore$Category)
n_CRating  <- n_distinct(New_googleplaystore$`Content Rating`)
n_version  <-  n_distinct(New_googleplaystore$`Android Ver`)
n_Genres  <-  n_distinct(New_googleplaystore$Genres)

Data_structure_table <- data_frame(analyse="Distinct  App", total = n_App )
Data_structure_table <-bind_rows(Data_structure_table,data_frame(analyse="Distinct Category ", total = n_Category ))
Data_structure_table <-bind_rows(Data_structure_table,data_frame(analyse="Distinct Content Rating ", total = n_CRating ))
Data_structure_table <-bind_rows(Data_structure_table,data_frame(analyse="Distinct Android Version ", total = n_version ))
Data_structure_table <-bind_rows(Data_structure_table,data_frame(analyse="Distinct Genres ", total = n_Genres ))

print("Data set table structure")
Data_structure_table %>% knitr::kable()

### list of different genres####
print("list of all  genres")
New_googleplaystore %>% separate_rows(Genres) %>%
  group_by(Genres) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>% print (n=69)



#now, we have to find all non-regular App names and clear them out.
library(stringr)



#rating distribution plot by Ratings####

New_googleplaystore %>% filter(!is.na(Rating)& (Rating<=5)) %>%
  group_by(Rating) %>%
  summarize(count = n()) %>% # print(n=41)
  ggplot(aes(x = Rating, y = count)) +
  geom_line(color="blue", size=2) 

#rating distribution plot by Installs####
New_googleplaystore %>% 
  group_by(Installs) %>% 
 summarize(count = n()) %>% filter(count>100)%>%
  ggplot(aes(x = Installs,  y = count)) +
  geom_smooth(alpha=0.1)

New_googleplaystore %>% group_by(Reviews,Installs) %>% arrange(desc(Installs)) %>%
  ggplot(aes(x=Reviews, y=Installs)) +
  geom_smooth(alpha=0.1)+ ggtitle("Installs vs Reviews")

New_googleplaystore  %>% group_by(Category)%>%arrange(desc(Installs)) %>%
  ggplot(aes(x=Installs, y=Category)) +
  geom_line()


####Recommender ####
# Validation set will be 10% of GooglePlaystore data
library(caret)
set.seed(1)
test_index <- createDataPartition(y = New_googleplaystore$Rating, times = 1, p = 0.1, list = FALSE)
Capstone <- New_googleplaystore[-test_index,]
temp <- New_googleplaystore[test_index,] #%>%print(n=739)

# Make sure Category,Installs and Genres in validation set are also in Capstone set

validation <- temp %>% 
  semi_join(Capstone, by = "Category") %>%
  semi_join(Capstone, by = "Installs") %>%
  semi_join(Capstone, by="Genres")

# Add rows removed from validation set back into Capstone set

removed <- anti_join(temp, validation)
Capstone <- rbind(Capstone, removed)

rm(test_index, temp,  removed) 

#after the model is set to provide a full prediction table

#rating distribution plot by Category####
Capstone %>% 
  count(Category) %>% arrange(n)%>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("rating distribution plot by Category")

#rating distribution plot by Genres ####
Capstone %>%   count(Genres) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("rating distribution plot by Genres ") 


#10 top reviewed App####
print("10 top reviewed App")
Capstone %>% group_by(App, Reviews) %>%
  #summarize(Rating) %>%
  arrange(desc(Reviews))

#10 top least reviewed App####
print("10 top least rated App")
Capstone %>% group_by(App, Reviews) %>%  arrange((Reviews)) 

#creating RMSE function####

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#processing the first step of the Recomandation System by
#applying naively the mu_hat( average ratings of all App) and by this process
#the naive rmse####
mu_hat <- mean(Capstone$Rating)
naive_rmse <- RMSE(validation$Rating, mu_hat) 

print("mu_hat")
mu_hat
print("naive_rmse")
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
#this table will be used to evaluate the improvement of each model

rmse_results %>% knitr::kable()

#str(Capstone)
#adding the Category effect to the naive model (Category)
#naive rmse+Category effect which will be called model_1_rmse and added to the table of RMSE

mu <- mean(Capstone$Rating) 
Category_avgs <- Capstone %>% 
  group_by(Category) %>% 
  summarize(b_i = mean(Rating - mu))
Category_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + validation %>% 
  left_join(Category_avgs, by='Category') %>%
  .$b_i
#output: Table with RMSE
model_1_rmse <- RMSE(predicted_ratings, validation$Rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Category Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

#naive rmse+Category effect+Installs effect

Installs_avgs <- Capstone %>%
  left_join(Category_avgs, by='Category') %>%
  group_by(Installs) %>%
  summarize(b_u = mean(Rating - mu - b_i))

predicted_ratings <- validation %>% 
  left_join(Category_avgs, by='Category') %>%
  left_join(Installs_avgs, by='Installs') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$Rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Category + Installs Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#naive rmse+category effect+installs effect +Genres effect
genres_avgs <- Capstone %>% 
  left_join(Category_avgs, by='Category') %>%
  left_join(Installs_avgs, by='Installs') %>%
  group_by(Genres) %>%
  summarize(b_g = mean(Rating - mu - b_i - b_u))

predicted_ratings <- validation %>% 
  left_join(Category_avgs, by='Category') %>%
  left_join(Installs_avgs, by='Installs') %>%
  left_join(genres_avgs, by='Genres') %>%
  mutate(pred = mu + b_i + b_u +b_g) %>%
  .$pred
model_g_rmse <- RMSE(predicted_ratings, validation$Rating)
#output: Table with RMSE####
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Category + Installs +genres Effects Model",  
                                     RMSE = model_g_rmse ))
rmse_results %>% knitr::kable()

#########################
##regularization model ####
#########################

####creating a sequence oflambda for evaluating best lambda as penalty factor

lambdas <- seq(0, 400, 0.25)

#regularizing by Category effect####
#creating function to assess best RMSE for category effect with optimal Lambda
rmses_b_i <- sapply(lambdas, function(l){
  
  mu <- mean(Capstone$Rating)
  
  b_i <- Capstone %>% 
    group_by(Category) %>%
    summarize(b_i = sum(Rating - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "Category") %>%
    mutate(pred = mu + b_i ) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$Rating))
})

#plotting the result of Lambda vs RMSE for better visualiation
qplot(lambdas, rmses_b_i)

#best lambda for Category effect model

lambda_i <- lambdas[which.min(rmses_b_i)]
lambda_i

#results incoporating to RMSE_results table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Category Effect Model",  
                                     RMSE = min(rmses_b_i)))
#printing the table of rmse results
rmse_results %>% knitr::kable()


#regularizing by Install effect####
#creating function to assess best RMSE for Installs effect with optimal Lambda (Category effect already set to the model)

rmses_b_u <- sapply(lambdas, function(l){
  
  mu <- mean(Capstone$Rating)
  
  b_i <- Capstone %>% 
    group_by(Category) %>%
    summarize(b_i = sum(Rating - mu)/(n()+lambda_i))
  
  b_u <- Capstone %>% 
    left_join(b_i, by="Category") %>%
    group_by(Installs) %>%
    summarize(b_u = sum(Rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "Category") %>%
    left_join(b_u, by = "Installs") %>%
    mutate(pred = mu + b_i+ b_u ) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$Rating))
})

qplot(lambdas, rmses_b_u)  

lambda_u <- lambdas[which.min(rmses_b_u)]
print("lambda_u:")
lambda_u

#output: Table with RMSE

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Category + Installs Effect Model",  
                                     RMSE = min(rmses_b_u)))
rmse_results %>% knitr::kable()

#regularizing by genre ####
#creating function to assess best RMSE for Genre effect with optimal Lambda (Category and Installs effect already set to the model)

rmses_b_g <- sapply(lambdas, function(l){
  
  mu <- mean(Capstone$Rating)
  
  b_i <- Capstone %>% 
    group_by(Category) %>%
    summarize(b_i = sum(Rating - mu)/(n()+lambda_i))
  
  b_u <- Capstone %>% 
    left_join(b_i, by="Category") %>%
    group_by(Installs) %>%
    summarize(b_u = sum(Rating - b_i - mu)/(n()+lambda_u))
  
  b_g <- Capstone %>% 
    left_join(b_i, by="Category") %>%
    left_join(b_u, by="Installs")%>%
    group_by(Genres) %>%
    summarize(b_g = sum(Rating - b_i- b_u - mu)/(n()+l))
  
  predicted_ratings <-     validation %>% 
    left_join(b_i, by = "Category") %>%
    left_join(b_u, by = "Installs") %>%
    left_join(b_g, by= "Genres") %>%
    mutate(pred=mu + b_i+ b_u+ b_g)  %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$Rating))
})

#plotting the results of lambda_g vs RMSE by genre effect model
qplot(lambdas, rmses_b_g)  
#optimal lambda for genre effect model
lambda_g <- lambdas[which.min(rmses_b_g)]
print("lambda_g")
lambda_g

#table with final RMSE results including regularized Category, Installs and Genres effect model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Category + Installs + Genres Effect Model",  
                                     RMSE = min(rmses_b_g)))
rmse_results %>% knitr::kable()


# checking results #creating predicted ratings #

b_i <- validation %>%   group_by(Category) %>%  summarize(b_i = sum(Rating - mu)/(n()+lambda_i))

b_u <- validation %>%   left_join(b_i, by="Category") %>%  group_by(Installs) %>%
  summarize(b_u = sum(Rating - b_i - mu)/(n()+lambda_u))

b_g <- validation %>%   left_join(b_i, by="Category") %>%
  left_join(b_u, by="Installs")%>%  group_by(Genres) %>%
  summarize(b_g = sum(Rating - b_i- b_u - mu)/(n()+lambda_g))

predicted_ratings_rep <-   validation %>%   left_join(b_i, by = "Category") %>%
  left_join(b_u, by = "Installs") %>%  left_join(b_g, by = "Genres") %>%
  mutate(pred=mu + b_i+b_u+b_g)

#check prediction vs Rating#

Cat_rating_diff<-predicted_ratings_rep %>%  group_by(Category)%>%  summarize(P=mean(pred),R=mean(Rating),diff=R-P)

Cat_rating_diff%>%ggplot(aes(R,P,colour=diff))+ geom_point()+ ggtitle("The App rating prediction by Category")

App_rating_diff<-predicted_ratings_rep %>%  group_by(App)%>%  summarize(P=mean(pred),R=mean(Rating),diff=R-P)
App_rating_diff%>%ggplot(aes(R,P,colour=diff))+ geom_point()+ ggtitle("The App rating prediction ") +scale_color_gradient(low="blue", high="yellow")

#whole table with predicted ratings with allrows (predicted ratings=mu+b_i+b_u+b_g)####
predicted_ratings_rep <- 
  New_googleplaystore %>% 
  left_join(b_i, by = "Category") %>%
  left_join(b_u, by = "Installs") %>%
  left_join(b_g, by = "Genres") %>%
  mutate(pred_c=mu + b_i+b_u+b_g)

#whole table with predicted ratings for each App s####
predicted_ratings_rep_byApp <- 
  predicted_ratings_rep %>%group_by(Category) %>% filter(!is.na(pred_c)) %>%
summarize(n = n(), avg_ratings = mean(Rating), pred_ratings = mean(pred_c)) %>%
  arrange(desc(n)) 


#snapshot of 20 App prediction####
print("snapshot of 20 App prediction")
head(predicted_ratings_rep_byApp,n=20L)
#snapshot of predicted rating for 20 first ratings by Category####
print("snapshot of predicted rating for 20 first ratings by Category")
predicted_ratings_rep%>%arrange(Category) %>% filter(!is.na(pred_c))%>%select(App,Rating, Category,Type,Genres,pred_c)%>%head(n=20L)


###END###
