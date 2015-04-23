##################### Merge data sets together ##################### 

# Step 1: 
# Make sure the data frames have meaningful column names, and there is a mutual column name which helps to merge the data frames.
names(film_db) <- c("Title", "imdb_Rating", "Genres", "Runtime", "imdb_Votes")
names(rt_db) <- c("Title", "MPAA_rating", "Critics_score", "Year")

# Step 2:
# Get rid of upper characters in the title, so as there is a higher chance to find the most common titles
film_db$Title <- as.character(film_db$Title)
film_db$Title <- tolower(film_db$Title)
rt_db$Title <- tolower(rt_db$Title)
bo_db$Title <- tolower(bo_db$Title)

# Step 3:
# Merge the three data frames together. Since merge happens between two data frames, do this action twice.
film_merged <- unique(merge(bo_db, film_db, by="Title"))
film_merged <- unique(merge(film_merged, rt_db, by="Title"))

##################### Feature Engineering ##################### 

# Step 1: 
# Create three new variables: Opening_percentage tells the ration of first weekend's gross compared to the total; Opening_per_theater tells the average gross in the first weekend per theater; Theater_change tells what was the magnitude of theater change after the first weekend.
film_merged <- mutate(film_merged, Opening_percentage=round(Opening/Total_gross * 100, 2))
film_merged <- mutate(film_merged, Opening_per_theater=round(Opening/Theaters_2, 1))
film_merged <- mutate(film_merged, Theater_change=Theaters - Theaters_2)

# Step 2:
# Replace Open and Close variables to more meaningful ones.
which_year <- 2013
# Replace MM/DD to YYYY/MM/DD
film_merged$Open <- paste0(which_year, "/", film_merged$Open)
film_merged$Close <- paste0(which_year, "/", film_merged$Close)
film_merged$Open <- as.Date(film_merged$Open)
film_merged$Close <- as.Date(film_merged$Close)
# If showing a film is ended the following year, add +1 year to Close.
for (i in 1:length(film_merged$Close)) {
    if (film_merged$Close[i] < film_merged$Open[i]) {
        film_merged$Close[i] <- film_merged$Close[i] + years(1)
    }
}
# New column Runtime_duration tells how many days a film was in theaters before stopping.
film_merged <- mutate(film_merged, Runtime_duration=Close - Open)

# Step 3:
# Change IMDB votes to numeric and scale Rotten Tomatoes critics score to the same as IMDB votes.
film_merged$imdb_Votes <- gsub(",", "", film_merged$imdb_Votes)
film_merged$imdb_Votes <- as.numeric(film_merged$imdb_Votes)
film_merged$Critics_score <- film_merged$Critics_score / 10

# Step 4:
# Remove the year variable coming from Rotten Tomatoes, as it is redundant.
film_merged <- film_merged[,-16]

# Step 5:
# Create two new variables, one for IMDB votes and one for Total gross in order to show the magnitude of these values in a smaller scale.
film_merged$imdb_votes_groups <- sapply(film_merged$imdb_Votes, function(x) 
    if (x > 152000) {5} 
    else if (x < 152000 & x > 113000) {4}
    else if (x < 113000 & x > 36000) {3}
    else if (x < 36000 & x > 15000) {2}
    else {1})
film_merged$total_gross_groups <- sapply(film_merged$Total_gross, function(x) 
    if (x > 3e+08) {6} 
    else if (x < 3e+08 & x > 2e+08) {5}
    else if (x < 2e+08 & x > 1e+08) {4}
    else if (x < 1e+08 & x > 5e+07) {3}
    else if (x < 5e+07 & x > 1e+07) {2}
    else {1})

# Optional Step:
# Save what we have so far.
write.csv(film_merged,"film_merged.csv", row.names = FALSE)
