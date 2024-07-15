
set.seed(123)  

num_users = 10
num_movies = 10

ratings = matrix(sample(c(1:5, NA), num_users * num_movies, replace = TRUE), nrow = num_users)

print(ratings)


new_user_ratings = c(NA, 4, NA, NA, 3, NA, 5, NA, NA, 2)

print(new_user_ratings)


imputed_ratings = apply(ratings, 2, function(x) {
  na_idx = is.na(x)
  if (any(na_idx)) {
    mean_val = mean(x[!na_idx])
    x[na_idx] = mean_val
  }
  x
})


library(class)  

predict_ratings = function(imputed_ratings, new_user_ratings, k = 3) {
  unseen_movies = is.na(new_user_ratings)
  new_user_ratings_unseen = new_user_ratings[unseen_movies]
  
  neighbors = knn(imputed_ratings[!unseen_movies, , drop = FALSE], 
                   imputed_ratings[unseen_movies, , drop = FALSE], 
                   new_user_ratings[!unseen_movies], k = k)
  
  predicted_ratings = sapply(1:length(new_user_ratings_unseen), function(i) {
    if (is.na(new_user_ratings_unseen[i])) {
      neighbor_ratings = imputed_ratings[unseen_movies, neighbors]
      mean_ratings = apply(neighbor_ratings, 1, mean, na.rm = TRUE)
      if (any(is.na(mean_ratings))) {
        mean_ratings[is.na(mean_ratings)] = mean(imputed_ratings[, neighbors], na.rm = TRUE)
      }
      mean_ratings[i]
    } else {
      new_user_ratings_unseen[i]
    }
  })
  
  predicted_results = data.frame(movie_index = which(unseen_movies), predicted_rating = predicted_ratings)
  return(predicted_results)
}


predicted_results = predict_ratings(imputed_ratings, new_user_ratings)

cat("\nMovies recommended for the new user:\n")
print(predicted_results)

recommended_movies = predicted_results[predicted_results$predicted_rating >= 3, ]

cat("\nMovies recommended for the new user (predicted rating >= 3):\n")
print(recommended_movies)