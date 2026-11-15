# Movie Ratings Analyzer
# CSE 310 - Applied Programming
# This program keeps track of movies and looks at their ratings.

# ------------------------------------------
# 1. Movie collection
# ------------------------------------------

# Each movie has a title, genre, year, runtime, rating, and watched status.
movies <- data.frame(
  title = c(
    "The Notebook",
    "Interstellar",
    "Shrek",
    "The Dark Knight",
    "La La Land",
    "Titanic",
    "Inception",
    "Coco",
    "Pride and Prejudice",
    "The Lion King",
    "Parasite",
    "Finding Nemo",
    "Little Women",
    "The Grand Budapest Hotel",
    "10 Things I Hate About You"
  ),
  genre = c(
    "Romance", "Sci-Fi", "Animation", "Action", "Musical",
    "Romance", "Sci-Fi", "Animation", "Romance", "Animation",
    "Thriller", "Animation", "Drama", "Comedy", "Romance"
  ),
  year = as.integer(c(
    2004, 2014, 2001, 2008, 2016,
    1997, 2010, 2017, 2005, 1994,
    2019, 2003, 2019, 2014, 1999
  )),
  runtime = as.numeric(c(
    123, 169, 90, 152, 128,
    195, 148, 105, 129, 88,
    132, 100, 135, 99, 97
  )),
  rating = as.numeric(c(
    7.8, 8.7, 7.9, 9.0, 8.0,
    7.9, 8.8, 8.4, 7.8, 8.5,
    8.5, 8.2, 7.8, 8.1, 7.3
  )),
  watched = as.logical(c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, TRUE, TRUE, FALSE, TRUE,
    FALSE, TRUE, FALSE, TRUE, TRUE
  ))
)

# A factor is useful for grouping movies by genre.
movies$genre <- as.factor(movies$genre)

# ------------------------------------------
# 2. Save and load movie data
# ------------------------------------------

# Save the collection so it can also be used as a CSV file.
write.csv(movies, "movies.csv", row.names = FALSE)

# Load the information back into R.
movies <- read.csv("movies.csv", stringsAsFactors = FALSE)

# Change genre back to a factor after importing the file.
movies$genre <- as.factor(movies$genre)

cat("\nMovie collection loaded from movies.csv!\n")

# ------------------------------------------
# 3. Display the movie collection
# ------------------------------------------

display_movies <- function(movie_data) {
  cat("\n--- My Movie Collection ---\n")
  print(movie_data)
}

# ------------------------------------------
# 4. Calculate the average rating
# ------------------------------------------

calculate_average <- function(movie_data) {
  average <- mean(movie_data$rating)
  return(average)
}

# ------------------------------------------
# 5. Find the highest-rated movie
# ------------------------------------------

find_highest_rated <- function(movie_data) {
  highest_index <- which.max(movie_data$rating)
  highest_movie <- movie_data[highest_index, ]

  cat("\n--- Highest-Rated Movie ---\n")
  print(highest_movie)
}

# ------------------------------------------
# 6. Compare ratings by genre
# ------------------------------------------

analyze_genres <- function(movie_data) {
  cat("\n--- Average Rating by Genre ---\n")

  # Get the different genres in the collection.
  genre_list <- levels(movie_data$genre)

  # Check each genre and calculate its average rating.
  for (current_genre in genre_list) {
    genre_movies <- movie_data[
      movie_data$genre == current_genre,
    ]

    genre_average <- mean(genre_movies$rating)

    cat(
      current_genre, ":",
      round(genre_average, 2),
      "\n"
    )
  }
}

# ------------------------------------------
# 7. Create charts
# ------------------------------------------

create_charts <- function(movie_data) {

  # Find the average rating for each genre.
  genre_averages <- aggregate(
    rating ~ genre,
    data = movie_data,
    FUN = mean
  )

  # Compare the genres in a bar chart.
  barplot(
    genre_averages$rating,
    names.arg = genre_averages$genre,
    main = "Average Movie Rating by Genre",
    xlab = "Movie Genre",
    ylab = "Average Rating",
    ylim = c(0, 10),
    col = "lightblue",
    las = 2
  )

  # Wait so the first chart can be viewed.
  readline("Press Enter to see the ratings histogram...")
  
  # Show how the ratings are spread out.
  hist(
    movie_data$rating,
    main = "Distribution of Movie Ratings",
    xlab = "Rating",
    ylab = "Number of Movies",
    col = "lightgreen",
    breaks = 5
  )
}

# ------------------------------------------
# 8. Run the program
# ------------------------------------------

display_movies(movies)

cat(
  "\nOverall average rating:",
  round(calculate_average(movies), 2),
  "\n"
)

find_highest_rated(movies)
analyze_genres(movies)
create_charts(movies)
