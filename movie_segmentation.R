library(tidyverse)
library(caret)

# Clustering for similar movie recommend
movies <- read_csv("IMDB_movies_and_people_Extract.csv")

# Remove duplicate and filter for recent movies
recent_movies <- movies %>% distinct() %>% 
  filter(`Year of Release` >= 1970)

# Segment by genre, country, director, rating, year
sub_recent <- recent_movies %>% 
  filter(`What did they do ?` == "director") %>% 
  select(country = Country,
         genre = `Genres (1st)`,
         director_name = `Person Name`,
         rating = `IMDB Rating`,
         year = `Year of Release`)

sub_recent <- sub_recent %>% 
  na.omit()

# See distinct value in columns
# Too many categories in one column
sub_recent %>% summarise_all(n_distinct)

# Filter to top 10 country and top 10 director

top_10_country_df <- sub_recent %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!is.na(country)) %>% 
  select(country) %>% 
  head(10)

top_10_country <- top_10_country_df$country


top_30_direct_df <- sub_recent %>% 
  group_by(director_name) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!is.na(director_name)) %>% 
  select(director_name) %>% 
  head(30)

top_30_director <- top_30_direct_df$director_name

process_sub_recent <- sub_recent %>% 
  mutate(country = if_else(country %in% top_10_country,country,"Other"),
         director_name = if_else(director_name %in% top_30_director,director_name,"Other"))
View(process_sub_recent)

# Apply one hot encoding 
categorical_columns <- c("country","genre","director_name")

dmy <- dummyVars(" ~ .", data = process_sub_recent[categorical_columns])
data_transformed <- data.frame(predict(dmy, newdata = process_sub_recent[categorical_columns]))

# Combine data
# Remove the original categorical columns and add the transformed ones.

model_data <- process_sub_recent %>% 
  select(-all_of(categorical_columns)) %>% 
  bind_cols(data_transformed)

View(model_data)

# Apply kmeans clustering
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(data_transformed, centers = 10, nstart = 10)

# View size of the groups
kmeans_result$size

# Assign cluster back to model data
model_data$cluster <- kmeans_result$cluster

# Reverse one hot encoding 
original_country <- c(top_10_country,"Other")
original_director  <- c(top_30_director,"Other")
original_genre <- unique(process_sub_recent$genre)

one_hot_country <- character()
one_hot_genre <- character()
one_hot_direct <- character()

for( country in original_country){
  one_hot_country <- c(one_hot_country,paste0("country",country))
}
one_hot_country <- gsub(" ", ".", one_hot_country)


for( genre in original_genre){
  one_hot_genre <- c(one_hot_genre,paste0("genre",genre))
}

one_hot_genre <- gsub("-", ".", one_hot_genre)

for( direct in original_director){
  one_hot_direct <- c(one_hot_direct,paste0("director_name",direct))
}

one_hot_direct<- gsub(" ", ".", one_hot_direct)
one_hot_direct<- gsub("-", ".", one_hot_direct)
one_hot_direct<- gsub("'", ".", one_hot_direct)
# Function to reverse one hot.
reverse_one_hot <- function(encoded_columns, categories) {
  apply(encoded_columns, 1, function(row) {
    categories[which.max(row)]
  })
}

# Reverse one hot for genre, country, and director
model_data$original_genre <- reverse_one_hot(model_data[,one_hot_genre], original_genre)
model_data <- model_data[, !names(model_data) %in% one_hot_genre]

model_data$original_country <- reverse_one_hot(model_data[,one_hot_country], original_country)
model_data <- model_data[, !names(model_data) %in% one_hot_country]

model_data$original_director <- reverse_one_hot(model_data[,one_hot_direct], original_director)
model_data <- model_data[, !names(model_data) %in% one_hot_direct]



# There are unexpected many-to-many relationship sue to other 
# Apply function across rows
rows_with_other <- apply(model_data, 1, function(x) any(x == "Other"))

# Subset dataframe to remove rows with "Other"
no_other_model_data <- model_data[!rows_with_other, ]

    
# Join back with recent movie 
# But there are many-to-maany warning  investigate furthur
combined_result <- sub_recent %>% 
                        inner_join(no_other_model_data,by = c("country" = "original_country",
                                                    "genre" = "original_genre",
                                                    "director_name" = "original_director",
                                                    "rating" = "rating","year" = "year"))


# For now visualize the model_data

model_data %>% 
  group_by(cluster) %>% 
  summarise(mean_rating = mean(rating,na.rm = T)) %>% 
  arrange(-mean_rating) %>% 
  ggplot(aes(x = reorder(cluster, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity",fill = "#A9CCE3") +
  labs(title = "Mean Rating by Cluster",
       x = "Cluster",
       y = "Mean Rating") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  
    axis.title.x = element_text(margin = margin(t = 15))  
  )

model_data %>% 
  filter(original_country != "Other" & cluster %in% c(1,4,5)) %>% 
  ggplot(aes(x = as.factor(cluster), fill = original_country)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "100% Stacked Bar Chart of Clusters by Country",
       x = "Cluster",
       y = "Percentage",
       fill = "Country") +
  scale_fill_brewer(palette = "Pastel1") +  # You can choose a different palette
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  
    axis.title.x = element_text(margin = margin(t = 15))  
  )
  