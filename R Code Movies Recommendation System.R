# Install and Load required libraries 

install.packages("tidyverse") 

install.packages("recommenderlab") 

install.packages("ggplot2") 



library(tidyverse) 

library(recommenderlab) 

library(ggplot2) 


## Download and unzip the dataset 

download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", "movielens.zip") 

unzip("movielens.zip") 


# Load the data into R 

ratings <- read.csv("ml-latest-small/ratings.csv") 

movies <- read.csv("ml-latest-small/movies.csv") 


# Clean the data: remove NA rows 

movies_clean <- na.omit(movies) 

ratings_clean <- ratings[complete.cases(ratings), ] 


# Create data frame with user IDs, item IDs, and ratings 

ratings_data <- data.frame( 
  
  UserID = c(1, 1, 2, 2, 3),    # User IDs 
  
  ItemID = c(101, 102, 101, 103, 102),  # Item IDs 
  
  Rating = c(4, 3, 5, 2, 4)      # Ratings 
  
) 



# Convert data frame to a realRatingMatrix 

rating_matrix <- as(ratings_data, "realRatingMatrix") 

# review the data
str(rating_matrix) 



# Build a recommendation model using user-based collaborative filtering with k-nearest neighbors 

model <- Recommender(rating_matrix, method = "UBCF") 



# Build a recommendation model using item-based collaborative filtering 

model_item <- Recommender(rating_matrix, method = "IBCF") 



# Split the data into training and test sets 

set.seed(123) 

train_indices <- sample(1:nrow(rating_matrix), 0.8 * nrow(rating_matrix)) 

train_data <- rating_matrix[train_indices, ] 

test_data <- rating_matrix[-train_indices, ] 



# Generate recommendations for the test set 

recommendations <- predict(model, test_data) 





# Convert recommendations to a matrix 

recommended_matrix <- as(recommendations, "matrix") 



# Clean NAs from the recommended matrix 

recommended_matrix[is.na(recommended_matrix)] <- 0 





# Extract actual ratings from test_data 

actual_ratings <- as(test_data, "matrix") 



# Calculate RMSE only for the recommended items 

common_items <- intersect(colnames(recommended_matrix), colnames(actual_ratings)) 

rmse <- sqrt(mean((recommended_matrix[, common_items] - actual_ratings[, common_items])^2, na.rm = TRUE)) 





# Print RMSE 

print(rmse) 


# Plot the distribution of ratings
ggplot(data = ratings_clean, aes(x = rating)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Frequency")

# Calculate the number of ratings and average rating for each movie
movie_stats <- ratings_clean %>%
  group_by(movieId) %>%
  summarise(num_ratings = n(), avg_rating = mean(rating))

# Plot the relationship between number of ratings and average rating
ggplot(data = movie_stats, aes(x = num_ratings, y = avg_rating)) +
  geom_point(color = "skyblue") +
  labs(title = "Relationship Between Number of Ratings and Average Rating",
       x = "Number of Ratings",
       y = "Average Rating") +
  theme_minimal()




