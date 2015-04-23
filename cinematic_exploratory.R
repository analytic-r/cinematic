##################### Exploratory data analysis ##################### 

# Step 1:
# Open the .csv file containing our data.
film_merged <- read.csv("film_merged.csv", header=TRUE, stringsAsFactors=FALSE,
                        colClasses=c("Open"="Date", "Close"="Date", "MPAA_rating"="factor"))

# Step 2:
# Invoke the necessary libraries.
require(ggplot2) # ggplot
require(RColorBrewer) # brewer.pal
require(gridExtra) # grid.arrange
require(stringr) # str_trim

# Step 3: 
# Explore the data
# Number of variables
dim(film_merged)

# Variable names
names(film_merged)

# Checking which items are missing from the data frame from 1-50 places in terms of boxoffice results ranking.
setdiff(1:50, head(sort(film_merged$Rank), 50))

# What are those films' title and how much money they stand for.
paste0(film_merged$Title[24], ": $", round(film_merged$Total_gross[24] / 1000000, 2), "M")
paste0(film_merged$Title[42], ": $", round(film_merged$Total_gross[42] / 1000000, 2), "M")

# What are the titles of the movies having the most and less number of characters.
film_merged$Title[which.max(nchar(film_merged$Title))]
film_merged$Title[which.min(nchar(film_merged$Title))]

# How much money my data frame stand for and what is the percentage compared to the reality.
year_total <- sum(as.numeric(film_merged$Total_gross))

round(year_total / y_2013 * 100, 2)

# The top 10 highest grossing movie stand for this percentage of the whole year:
top10_bo <- sum(as.numeric(tail(sort(film_merged$Total_gross), 10)))
round(top10_bo / y_2013 * 100, 2)

# The highest grossing one:
number_1 <- data.frame(Title=film_merged$Title[film_merged$Rank==1], 
                       Gross=film_merged$Total_gross[film_merged$Rank==1])

# This data frame has information about the total, my data frame's total, top 10 and highest grossing film.
bo_to_plot <- data.frame(type=c(1, 2, 3, 4),
                         values=c(y_2013, year_total, top10_bo, number_1[,2]))