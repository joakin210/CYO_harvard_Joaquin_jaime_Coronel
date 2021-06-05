

#title: "HarvardX: PH125.9x Data Science \n My Own Jokes Recommendation System Project Submission--CYO"
#author: "Joaquin Emilio Jaime Coronel"
#date:     "May  25 2021"

##############################################################

# Introduction


#Recommendation systems are very important on many enterprises and social spheres 
#around the world.They are used to help sell or offer products to users that have 
#not seen them or have bought them.

#In this ocassion we are recommending jokes to read, using the Jester5k data that 
#contains the whole info to do this.The variables affected by the code are connected
#to those learned on the course series, that take us to know the best joke to
#recommend to others to read it and observe other characteristics.

#The goal  of  this project is to highlight the importance of recommending systems 
#that are taking an interesting place on our daily life and businesses.

#The key steps to follow on this project were to have crystal clear the R package 
#for recommendation, choose the data (Jester5k), prepare it, explore it, show the 
#models used, visualize the most of the data to make clearer the understanding of 
#each section,show results, comment it and conclude with an interesting paragraph 
#telling to others the importance of the Recommending Systems on this modern life.


##############################################################
## Installing packages.
##############################################################


if(!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

if(!require("data.table")) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!"recommenderlab" %in% rownames(installed.packages())){
  install.packages("recommenderlab")}
library("recommenderlab")
library("ggplot2")
library("data.table")
data(Jester5k)
Jester5k


data_to_use <- Jester5k

# Line code to reproduce random components of recommenderlab.

set.seed(1)

# Datasets that can be used to play with Recommenderlab functions.

data_package <- data(package = "recommenderlab")

# Look the methods we use with this kind of objects

 methods(class = class(data_to_use))

# Making a comparison of the size of data_to_use with  R matrix:
  
object.size(data_to_use)

object.size(as(data_to_use, "matrix")) 

# Collaborative filtering algorithms use measuring the similarity between users and items. For this purpose, we use the similarity function.To do this we compute this using the cosine distance

# Now compute the  matrix of similarity.

similarity_users <- similarity(data_to_use[1:4, ], method = 
                                 "cosine", which = "users")

similarity_users


# Let큦 explore the dissimilarities.

# Let큦 convert  similarity_users into a matrix.

as.matrix(similarity_users)

# Use image to visualize the matrix. Rows and columns corresponds to a user, and cells corresponds to similarity between two users.

image(as.matrix(similarity_users), main = "User similarity")


# We can compute and visualize the similarity between the first four item.

similarity_items <- similarity(Jester5k[, 1:4], method = 
                                 "cosine", which = "items")

as.matrix(similarity_items)


# Now, we can visualize the matrix using this image.

image(as.matrix(similarity_items), main = "Item similarity")



##############################################################
# Exploring the data
##############################################################

# Extracting the size of the data, show us 5000 users and 100 jokes.

dim(data_to_use)

# Exploring  values of rating.

vector_ratings <- as.vector(data_to_use@data)

unique(vector_ratings)


# As you can see a rating equal to 0 represents a missing value, so remove them from vector_ratings:
  
vector_ratings <- vector_ratings[vector_ratings != 0]

# Now, we can  plot the ratings. In order to visualize a bar plot. Let's convert them into categories using factors and to see chart:

vector_ratings <- factor(vector_ratings)

vector_ratings

#Now visualize their distribution with qplot:

qplot(vector_ratings) + ggtitle("Distribution of the ratings")


views_per_joke <- colCounts(data_to_use)

views_per_joke


#Knowing which jokes have been viewed.

table_views <- data.frame(
  jokes = names(views_per_joke),
  views = views_per_joke
)
table_views <- table_views[order(table_views$views, decreasing = 
                                   TRUE), ]
table_views


#We can classify by number of views.

#Which are the jokes most viewed?

views_per_joke <- colCounts(data_to_use)
views_per_jokeviews_per_joke <- colCounts(data_to_use)
views_per_joke

 #Let큦 see the first six rows through a histogram.

ggplot(table_views[1:6, ], aes(x = jokes, y = views)) + 
  geom_bar(stat="identity") + theme(axis.text.x = 
                                      element_text(angle = 45, hjust = 1)) + ggtitle("Number of views 
of the top jokes")

# We can visualize the top-rated jokes by computing the average rating of each of them. For this we can use colMeans that automatically ignores the 0s. Now, let큦 see the average ratings.

average_ratings <- colMeans(data_to_use)
average_ratings


 # As we see the highest value is  3, and there are a few movies whose rated 1 or 5 Maybe, these jokes received were rated from a few people, so we don큧 take them into account. We can remove the jokes whose number of views is  below 100.

average_ratings_relevant <- average_ratings [views_per_joke > 100] 

average_ratings_relevant

 # Now, let큦 visualize it.

qplot(average_ratings_relevant) + stat_bin(fill ="blue", binwidth = 0.1) + 
  ggtitle(paste("Distribution of the relevant average ratings"))


#It is possible visualize the matrix through a heat map using colors that represent the ratings. Rows correspond to a user, columns to a joke, and cells to its rating..

# Let큦 do visualize the matrix.

image(data_to_use, main = "Heatmap of the rating matrix")


# Due to there are too many users and items,We can build another chart just showings rows and columns.

image(data_to_use[1:10, 1:15], main = "Heatmap of the first rows and 
columns")


##############################################################################
# Data preparation
##############################################################################
# Let큦 see how to prepare the data to be used in the recommending system.models. For doing this, we have to select the most important data and normalized it.Exploring the data we find that jokes have been seen and rated few times.Let큦 determine the number of users per joke.Now, we define ratings_jokes that are  contained the matrix that we will use.

ratings_jokes <- data_to_use[rowCounts(data_to_use) > 50, 
                          colCounts(data_to_use) > 100] 
ratings_jokes 


#Let큦 explore the most important data.

# let's visualize the top 2 percent of users and jokes in a new matrix:
 
min_jokes <- quantile(rowCounts(ratings_jokes), 0.98)
min_jokes

min_users <- quantile(colCounts(ratings_jokes), 0.98)
min_users


# Now, let큦 see the distribution.

average_ratings_per_user <- rowMeans(ratings_jokes)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) + 
  ggtitle("Distribution of the average rating per user")


# As you can see the last plot shows that the average rating varies between different users.

# Now let큦 normalize the data.Taking into account that some users have given high (or low) ratings to all their jokes could change  the   results. Let큦 remove this effect to normalize the average rating of each user being 0. 

ratings_jokes_norm <- normalize(ratings_jokes)
ratings_jokes_norm

# Let's see now the average rating done by users:

sum(rowMeans(ratings_jokes_norm) > 0.00001)


##############################################################################
# Models
##############################################################################

# Model I---Item-based collaborative filtering model

#Collaborative filtering  takes into account of the information about different users. It refers to the fact that users 
#collaborate with each other to recommend items.  A mean element is a rating matrix in which rows belongs to users and columns to items.The men algorithm is based on:  measure how similar they are in terms of having received similar ratings by similar users in two items, identify the k-most similar items in each one and  the items that are most similar to the user's preferences

#Defining the training and test sets.

#We will be using a part of the data_to_us dataset (the training set) and apply it on the other part (the test set).With this We will 
#recommend jokes to the users in the test set. These sets are defined in this way: Training sets: include users from which the model learn. 
#Test sets : include users to whom we recommends jokes. We will set the training set in 80 percent and 20 percent on the test set as this:
  
 data_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_jokes), 
                     replace = TRUE, prob = c(0.8, 0.2))
 head(data_train)



#Let's define the training and the test sets:


r_data_train <- ratings_jokes[data_train, ]


r_data_test <- ratings_jokes[!data_train, ]


# AS we want to recommend items to each user, we will use the k-fold for doing this we have to split the users randomly into 5 groups,
#use a group as a test set and the other groups as training sets Repeat it in  each group.

get_set <- sample(x = 1:5, size = nrow(ratings_jokes), replace = 
                      TRUE)
for(i_model in 1:5) {
  get_train <- get_set == i_model
  r_data_train <- ratings_jokes[data_train, ]
  r_data_test <- ratings_jokes[!data_train, ]
}


# Building "IBCF" recommendation model. 

# Let큦 take into account the next info.The model is item-based collaborative filtering (IBCF).

recom_models <- recommenderRegistry$get_entries(dataType = 
                                                        "realRatingMatrix")


recom_models$IBCF_realRatingMatrix$parameters


#In order to show how to change parameters, we set k = 30, which is the default, this computes the similarities among each pair of items.


r_model <- Recommender(data = r_data_train, method = "IBCF", 
                          parameter = list(k = 30))

r_model

class(r_model)


#Now, let큦 the show the recommendation model.

#We will use getModel to extract some details such as its description and parameters:

model_details <- getModel(r_model)
model_details$description


#We will use The model_details$sim matrix component to find similarities.

#Let큦 see what this show us.

class(model_details$sim)


dim(model_details$sim)

#As you can see, model_details$sim is a square matrix whose size is equal to the number of items. We can explore a part of it using image:

n_items_top <- 20 


# Now let's see what the heat map shows us:

image(model_details$sim[1:n_items_top, 1:n_items_top], 
      main = "Heatmap of the first rows and columns") 


#If we check the heatmap most of the values are equal to 0. The reason is that each row contains only k elements. 

model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)


#Now, let's check the distribution  of elements by column.

col_sums <- colSums(model_details$sim > 0)

col_sums


#Now , let큦 write the code to build the distribution chart:

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of 
the column count")

#Watching the chart  there are a few jokes that are similar to many others. Let's see which are the jokes with the most elements:
  
 
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]


# Now, we apply the recommending model on the test set.We are on the capacity to recommend jokes to the users in the test set. So define n_recommended to specify the number of items to recommend to each user.

n_recommended <- 10


#The above algorithm identifies the top n recommendations

r_predicted <- predict(object = r_model, newdata = r_data_test, n = n_recommended)
r_predicted


#To ilustrate I say that, the r_predicted object contains the recommendations, you can check it with this piece ofcode.

class(r_predicted)


# Model II---User-based collaborative filtering

#In this model we will use a new user to identify its similar users. Then, we will recommend the top-rated items.
#Things to have into account in this module:
#--Measure similarities of each user are to the new one. They are correlation and cosine.To identify the most similar we use,
#-(k-nearest_neighbors)and the similarity that is above a defined threshold
#--Apply average and Weighted average rating, using the similarities as weights.
#--In this model we  will build a training and a test set, too.

# Building "UBCF" recommendation model

#To start, we present this piece of code.

r_models <- recommenderRegistry$get_entries(dataType = 
                                              "realRatingMatrix")

r_models$UBCF_realRatingMatrix$parameters


# Parameters to have into account--- method: It computes the similarity between users--- nn: It큦 the number of similar users

# Now Let's build the recommending model.

r_model <- Recommender(data = r_data_train, method = "UBCF")

r_model


#Now, check details of the model using getModel:
 
model_details <- getModel(r_model)


#Now, let큦 see the model components.

names(model_details)

#model_details contains a dataslot too.

model_details$data

######################################################################
# Results
######################################################################

# Evaluating the models

#To recommend items to new users, collaborative systems estimates the ratings that are not yet seen, then, it
#recommends the top-rated. Now, let큦  evaluate the model by comparing the estimated ratings with real users.

# Data preparation for validation using k-fold

n_fold <- 6

items_to_keep <- 12

rating_threshold <- 3

eval_sets <- evaluationScheme(data = ratings_jokes, method = "cross-validation", 
                              k = n_fold, given = items_to_keep, goodRating = rating_threshold)


# Now, let큦 define the model to evaluate and list parameters.

model_to_evaluate <- "IBCF"

model_parameters <- NULL


# Now, let큦 construct the model using the next chunk of code.

eval_recommender <- Recommender(data = getData(eval_sets, "train"), 
                                method = model_to_evaluate, parameter = model_parameters)


# Now, we specify the number of items to recommend.

items_to_recommend <- 10


# Now, let큦 make the matrix using the predict function.

eval_prediction <- predict(object = eval_recommender, newdata = 
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings") 


class(eval_prediction)


# Now, let큦  see the number of jokes to recommend to each user, visualizing them.

qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 10) + 
  
  ggtitle("Distribution of jokes per user")


# Now, let큦 measure the accuracy and compute(RMSE,MSE and MAE)

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = 
    TRUE)

head(eval_accuracy)


#Now, let's check the RMSE by a user.

qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of the RMSE by user")


# Now, let's check the MSE by a user.


qplot(eval_accuracy[, "MSE"]) + geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of the MSE by user")


# Now, let's check the MAE by a user.

qplot(eval_accuracy[, "MAE"]) + geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of the MAE by user")


# Having a performance index in the whole model.

# Most of the RMSEs are in the range of 0.8 to 1.4. The model was evaluated in each user. We use this code.

eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = 
                                          FALSE) 
eval_accuracy



#########################################################################
# Discussing the models performance.
#########################################################################

#With these measures we can compare the performance of different models with the same data, 
#as shown using qplot, geom_histogram and ggtitle.

# Conclusion

#This project dealing to Item and users-based collaborative filtering, would help to know the importance of 
#recommendation systems in our everyday  life and business. It leave us as a big content of learning that I 
#am sure will impact when applying to other kind of data becoming in a powerful tool to recommend items.

#The IBCF and the UBCF Collaborative filtering have some limitations When dealing with new users and/or new 
#items.

#Taking into account the limitations that have the IBCF and the UBCF Collaborative filtering models leave us 
#the  necessity to explore new models to apply in a future work and get the 100% of effectiveness to achieve 
#better results.