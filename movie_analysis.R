library(tidyverse)
library(caret)
library(rpart)

df <- read_csv("IMDB_movies_and_people_Extract.csv")

# Remove duplicate
df <- df %>% distinct() 

# Check # of null in each column

sapply(df, function(x) sum(is.na(x)))

glimpse(df)

# Check rating distribution
df %>% 
  ggplot(aes(x = `IMDB Rating`)) + 
  geom_density() + 
  facet_wrap(~`Genres (1st)`) +
  theme_minimal()

# Summary stat fro rating
df %>%
  summarise(
    Mean = mean(`IMDB Rating`, na.rm = TRUE),
    Median = median(`IMDB Rating`, na.rm = TRUE),
    SD = sd(`IMDB Rating`, na.rm = TRUE),
    Min = min(`IMDB Rating`, na.rm = TRUE),
    Max = max(`IMDB Rating`, na.rm = TRUE)
  )

df %>% 
  ggplot(aes(x = `IMDB Rating`)) + 
  geom_density() + 
  theme_minimal()

df %>% 
  filter(is.na(`Contains Genre?`),is.na(`Country`)) %>% 
  select(`Contains Genre?`,`Country`,`Year of Release`) %>% 
  group_by(`Year of Release`) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Number of movie for each year
df %>% group_by(year = `Year of Release`) %>% 
  summarise(count =n()) %>% 
  arrange(-count) %>% 
  ggplot(aes(x = year,y = count)) +
    geom_line() +
    theme_minimal()


# Consider from 1905 outward since there're more movie
# Genre with hgihest imdb score for each year

years_popular_genre <- df %>% 
    distinct(Title,.keep_all = TRUE) %>% 
    filter(!is.na(`IMDB Rating`),`Year of Release` > 1904,!is.na(`Genres (1st)`)) %>% 
    select(year = `Year of Release`,
           genre = `Genres (1st)`,
           score = `IMDB Rating`) %>% 
    group_by(year,genre) %>% 
    summarise(avg_score = mean(score)) %>% 
    group_by(year) %>% 
    mutate(Rank = rank(-avg_score)) %>% 
    arrange(year,Rank) %>% 
    filter(Rank < 4)

years_popular_genre %>%
  filter(year > 2017) %>% 
  ggplot(aes(x = factor(year), y = avg_score, fill = reorder(genre, -avg_score))) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  theme_minimal() +
  labs(title = "Average Score by Genre for Each Year",
       x = "Year",
       y = "Average IMDB Score",
       fill = "Genre") +
  theme_minimal()

# Find Overall number of shows across genre top 4 
df %>% 
  filter(!is.na(`Genres (1st)`)) %>% 
  distinct(Title,.keep_all = TRUE) %>% 
  select(year = `Year of Release`,
         genre = `Genres (1st)`,
         score = `IMDB Rating`) %>% 
  group_by(genre) %>% 
  summarise(films = n()) %>% 
  arrange(desc(films)) %>% 
  head(5) %>% 
  ggplot(aes(x = reorder(genre, -films), y = films,fill = factor(films))) +
  geom_col() + 
  scale_fill_manual(values = c("#CCEBC5",
                               "#FBB4AE",
                               "#B3CDE3",
                               "#DECBE4",
                               "#FED9A6")) + 
  theme_minimal() +
  labs(title = "Top 5 Genres by Film Count",
        x = "Genre",
        y = "Number of Films") +
  theme(legend.position = "none")+ 
  theme(axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20)))


df %>% 
  filter(!is.na(`IMDB Rating`),`Genres (1st)` %in% c("Drama","Documentary","Comedy","Action","Crime")) %>% 
  distinct(Title,.keep_all = TRUE) %>%  # Very important remov duplicate title 
  select(year = `Year of Release`,
         genre = `Genres (1st)`,
         score = `IMDB Rating`,) %>% 
  filter(year > 2014) %>% 
  group_by(year,genre) %>% 
  summarise(avg_score = mean(score)) %>% 
  ggplot(aes(year,avg_score,col = genre)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#FBB4AE",
                               "#B3CDE3",
                               "#CCEBC5",
                               "#DECBE4",
                               "#FED9A6")) +
  theme_minimal() + 
  theme(axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))) +
  labs(title = "Average IMDB Score by Promminent Genre for Each Year",
       x = "Release Year",
       y = "Average IMDB Score")

# Comparing drop in crime genre 2019 to 2020
df %>% 
  filter(`Genres (1st)` == "Crime" & `Year of Release` >= 2015 
         &  `Year of Release` <= 2020) %>% 
  distinct(Title,.keep_all = TRUE) %>% 
  group_by(`Year of Release`) %>% 
  summarise(mean_score = mean(`IMDB Rating`,na.rm = T)) %>% 
  mutate(score_last_year = lag(mean_score, order_by = `Year of Release`), # Find the previous year's score
         percent_change = (mean_score - score_last_year) / score_last_year * 100) %>% 
  
  filter(!is.na(percent_change)) %>% 
  ggplot(aes(x = as.factor(`Year of Release`), y = percent_change, fill = percent_change > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#6BCB77", "FALSE" = "#E63946")) +
  theme_minimal() +
  labs(title = "Yearly Percentage Change in Rating of Crime Movie",
       x = "Year",
       y = "Percent Change",
       fill = "Growth/Drop") +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  
    axis.title.x = element_text(margin = margin(t = 15))  
  )
  
# Investigte what movie cause huge drop in crime movie rating in 2020
df %>% 
  filter(`Genres (1st)` == "Crime" & `Year of Release` == 2020 
         & `IMDB Rating` < 5.65) %>% 
  distinct(Title,.keep_all = TRUE) %>%
  select(Title,`IMDB Rating`) %>% 
  arrange(`IMDB Rating`) 
  



# Separate into 10 years 1900 -1910 and so on.
# Find  Top 3 company produce film with high average score 
# IN each decade
df_decade <- df %>% 
            mutate(decade = paste0(floor(`Year of Release` / 10) * 10, "s"))


df_decade %>% 
  distinct(Title,.keep_all = TRUE) %>% 
  filter(!is.na(`IMDB Rating`)) %>% 
  group_by(decade,`Production Companies (1st)`) %>% 
  summarise(avg_rating = mean(`IMDB Rating`),
            films = n()) %>% 
  filter(films > 5) %>% 
  group_by(decade) %>% 
  mutate(ranking = row_number(-avg_rating)) %>% 
  filter(ranking < 4) %>% 
  arrange(desc(decade),ranking) %>% 
  View()
 
 # Find  Top director with highest IMDB score 
df %>% 
  distinct(Title,.keep_all = TRUE) %>%
  filter(`What did they do ?` == "director" & !is.na(`IMDB Rating`)) %>% 
  select(name = `Person Name`,
         imdb_score = `IMDB Rating`) %>% 
  group_by(name) %>% 
  summarise(avg_imdb_score = mean(imdb_score, na.rm = TRUE),
            films = n()) %>%
  filter(films > 20) %>% 
  arrange(-avg_imdb_score) %>% 
  head(5) %>% 
  ggplot(aes(x = reorder(name, -avg_imdb_score), y = avg_imdb_score, fill = avg_imdb_score)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "lightcoral",name = "Average IMDB Score") +  # Set pastel color gradient
  theme_minimal() +
  labs(x = "Director",
       y = "Average IMDB Score",
       title = "Top Directors by IMDB Score with more than 20 films.") +
  theme(
    axis.title.y = element_text(margin = margin(r = 15)),  
    axis.title.x = element_text(margin = margin(t = 15))  
  )




# 5. Find correlation between rating and runtime
# Corr of numerical variable
df %>% 
  distinct(Title,.keep_all = TRUE) %>%
  filter(`Genres (1st)` == "Drama") %>% 
  select(`IMDB Rating`,`Runtime (Minutes)`) %>% 
  na.omit() %>% 
  cor()
# Maybe between IMDB score and runtime 
 
df %>%
  distinct(Title,.keep_all = TRUE) %>%
  filter(`Genres (1st)`  %in% c("Drama"))  %>% 
  sample_frac(0.01) %>% 
  filter(`Runtime (Minutes)` < 500) %>% 
  select(`IMDB Rating`,`Runtime (Minutes)`,genre = `Genres (1st)`) %>% 
  ggplot(aes(`IMDB Rating`,`Runtime (Minutes)`)) +
  geom_point(color = "salmon",alpha = 0.5) +
  geom_smooth() + 
  theme_minimal() + 
  labs(title = "Drama movie runtime and IMDB rating" )
  
set.seed(42)
small_df <- df %>% 
  sample_frac(0.01)

write_csv(small_df,"small_df.csv")


## Data Preparation for ML ##

top_10_genre_df <- df %>% 
            group_by(`Genres (1st)`) %>% 
            summarise(n = n()) %>% 
            arrange(-n) %>% 
            filter(!is.na(`Genres (1st)`)) %>% 
            select(`Genres (1st)`) %>% 
            head(10)

top_10_genre <- top_10_genre_df$`Genres (1st)`

top_10_com_df <- df %>% 
  group_by(`Production Companies (1st)`) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!is.na(`Production Companies (1st)`)) %>% 
  select(`Production Companies (1st)`) %>% 
  head(10)

top_10_com <- top_10_com_df$`Production Companies (1st)`

top_10_contry_df <- df %>% 
  group_by(Country) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!is.na(Country)) %>% 
  select(Country) %>% 
  head(10)

top_10_country <- top_10_contry_df$Country

small_df <- read_csv("model_data.csv")
cleaned_data <- na.omit(small_df[c("IMDB Rating", "Country", "Genres (1st)",
                                   "Language","Person Name",
                                   "Production Companies (1st)","What did they do ?",
                                   "Year of Release")])
colnames(cleaned_data) <- make.names(colnames(cleaned_data))
cleaned_data <- cleaned_data %>% 
      mutate(genre = if_else(Genres..1st. %in% top_10_genre,Genres..1st.,"Other"),
             company = if_else(Production.Companies..1st. %in% top_10_com,Production.Companies..1st.,"Other")) %>% 
      select(-Genres..1st.,-Production.Companies..1st.)

View(cleaned_data)

cleaned_data <- cleaned_data %>% 
    mutate(Country = if_else(Country %in% top_10_country,Country,"Other")) 
  

##------------------ML------------------------------------------##
library(caret)
# Train test Split data

split_df <- function(df,train_size=0.7){
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n,size= n*train_size)
  train_df <- df[id,]
  test_df <- df[-id,]
  list(train=train_df,test=test_df)
}


cleaned_data <- cleaned_data %>% 
      select(IMDB.Rating,
             Country,genre,
             company,
             Year.of.Release)

prep_data <- split_df(cleaned_data)
train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

# Function for evaluation
cal_mae <- function(actual,prediction){
  error <- actual - prediction
  mean(abs(error))
}

cal_mse <- function(actual,prediction){
  error <- actual - prediction
  mean(error **2)
}

cal_rmse <- function(actual,prediction){
  error <- actual - prediction
  sqrt(mean(error **2))
}

# Initialize train control and cv
library(rpart)
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

dt_model <- train(IMDB.Rating ~ .,
                  data = train_data,
                  method = "rpart")
                   #tuneGrid = expand.grid(cp = c(0.01,0.05,0.1)),
                  #trControl = ctrl)


# Training set
p_train <- predict(dt_model,newdata = train_data)

cal_rmse(train_data$IMDB.Rating,p_train)
cal_mse(train_data$IMDB.Rating,p_train)
cal_mae(train_data$IMDB.Rating,p_train)



# Testing set
p <- predict(dt_model,newdata = test_data)

cal_rmse(test_data$IMDB.Rating,p)
cal_mse(test_data$IMDB.Rating,p)
cal_mae(test_data$IMDB.Rating,p)

saveRDS(dt_model,"dt_model.RDS")

# Test on full dataframe 
test_data %>% 
  group_by(Country) %>% 
  summarise(n = n())
