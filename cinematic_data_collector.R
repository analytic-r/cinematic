##################### Acquire data from IMDB ##################### 

# Step 1:
# Invoke the necessary libraries.
library(XML) # to use: readHTMLTable
require(stringr) # to use str_trim

# Step 2: 
# First I start with defining a new function (imdb_func), which gets one attribute, that is the year I am interested in. This piece of code contains the URL that I will invoke. The function's attribute is the year, and that's going to replace the which_year object. This URL also tells what is the minimum number of votes a movie should have to be present in this list. Additionally, a number that corresponds to the records being displayed (page_1 <- 1-50; page_2 <- 51-100; page_3 <- 101-150). I defined six pages altogether, that is 300 titles at most.
imdb_func <- function(which_year) {
    page_1 <- paste0("http://www.imdb.com/search/title??t=0&num_votes=1000,&sort=num_votes&&title_&year=", 
                     which_year, ",", which_year)
    
    # Step 3:
    # After this, I define a few data frames that will become useful later on.
    film_db <- data.frame(); film_db_temp <- data.frame(); votes_all <- data.frame()
    
    # Step 4:
    # If you as an imdb user are located in a country where English is not the first language, the default setting is that you see movie titles in your own language. I can see them in Hungarian, and I want to see them in English. No matter if you change this in your imdb profile, R won't recognize it, and it will download html files in your native language. This is when becomes handy to use a proxy within R. You will find plenty of proxies with a Google search. Chances are, you will need to define an other proxy when you read this.
    curl <- getCurlHandle()    
    curlSetOpt(.opts=list(proxy="xxx.xxx.xxx.xxx"), curl=curl)
    
    # Step 5:
    # Having a proxy defined, now I can download html files from imdb in English. I wrote a for loop, here it is with my embedded comments.
    # The for loop will go through all the six imdb URLs I specified above. The following tasks will be performed six times.
    for (i in 1:6) {
        # Using the defined proxy, we download the html from imdb.
        imdb <- getURL(get(paste0("page_", i)), curl=curl)
        # Extract the number of user votes each movie got and store in object votes. Next, bind the votes coming with the for loop to object votes_all.
        votes <- readHTMLTable(imdb)[[1]][,4]
        votes_all <- rbind(votes_all, as.matrix(votes))
        # Extract the table which contains all other details we need.
        imdb <- readHTMLTable(imdb)[[1]][,3]
        # The for loop checks how many movies we have in the actual html, and use all of them, one-by-one.
        for (k in 1:length(imdb)) {
            film <- imdb
            # Split up the movie's details by the separator. In the next line, keep only what we need.
            film <- str_split_fixed(film[k], "\n", 32)
            film <- data.frame(t(str_trim(film[,c(1, 20, 30, 32)], side="both")))
            # Some feature engineering: change classes, remove unnecessary strings.
            film$X2 <- as.numeric(substr(film$X2, 1, 3))
            film$X4 <- as.numeric(gsub(" mins.", "", film$X4))
            # Bind each film's details into one object.
            film_db_temp <- rbind(film_db_temp, film) 
        }
        # Bind the data frames we got from each imdb pages.
        film_db <- rbind(film_db, film_db_temp)
        # Empty one of our data frame we use in each for loop.
        film_db_temp <- data.frame()
    }
    
    # Step 6:
    # As the last step, let's bind together the movie title, rating, genres, runtime with number of votes, close the function, and specify meaningful variable names.
    film_db <<- cbind(film_db, votes_all$V1)
}
names(film_db) <- c("Title", "imdb_Rating", "Genres", "Runtime", "imdb_Votes")

##################### Acquire data from Rotten Tomatoes ##################### 

# Step 1:
Invoke the necessary libraries.
library(XML) # to use: readHTMLTable
library(stringr) # to use str_trim

# Step 2: 
# Start defining Rotten Tomatoes function
rt_func <- function(which_year) {
    film_list <- film_db$Title
    
    # Step 3:
    # Next, I create a data frame which I will need in a following for loop, and provide the API key that I got when registering to the site. 
    rt_crawled <- data.frame()
    apikey <- "xxxxxxxxxxxxxxxxx"
    
    # Step 4:
    # Here comes a for loop that includes an if-else statement too. I am providing some explanations as comments inside the R code.
    # A for loop that runs as many times as the number of titles I have
    for (i in 1:length(film_list)) { 
        # Follows the RT URL that includes the API key and the film title
        rt_crawl_json <- paste0("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey=",
                                apikey, "&q=", film_list[i], "&page_limit=50")
        # The film title has spaces, so I change them to + character.
        rt_crawl_json <- gsub(" ", "+", rt_crawl_json)
        # With getURL, I load the site content to memory
        rt_crawl_json <- getURL(rt_crawl_json)
        # I create a data frame from the json file I received
        rt_crawl_json <- fromJSON(rt_crawl_json, flatten=TRUE)$movies
        # If a title is found, I get meaningful data with the above code. If a title not found, I can't include to the data frame.
        # The following if-else statement inserts a line with the title and NA values if a title was not found in RT.
        if (length(names(rt_crawl_json)) > 0) {
            rt_crawl_json <- rt_crawl_json[,c("title", "mpaa_rating", "ratings.critics_score", "year")]
        } else {
            rt_crawl_json <- data.frame("title"=film_list[i],  "mpaa_rating"=NA, "ratings.critics_score"=NA, "year"=0)
        }
        # As production year is not specified in the query, I got results from another years as well. I want to remove them.
        # Here I tell R to extract entries that equals the specified year, and critics score is higher than zero. 
        rt_crawl_json <- rt_crawl_json[rt_crawl_json$year==which_year & rt_crawl_json$ratings.critics_score > 0,] 
        # Finally, bind together the results.
        rt_crawled <- rbind(rt_crawled, rt_crawl_json)
    }
    
    # Step 5:
    # In the end, I want to change the header for the first column from "title" to "Title", so as both Imdb, Rotten Tomatoes and Boxofficemojo wil have the same. A function doesn't make its inside object available to the global environment. There are a few ways to make this happen, I use the <<- assignment.
    names(rt_crawled)[1] <- "Title"
    rt_db <<- rt_crawled
}

##################### Acquire data from Boxofficemojo ##################### 

# Step 1:
Invoke the necessary libraries.
library(XML) # to use: readHTMLTable
library(stringr) # to use str_trim

# Step 2: 
# Create a function, which I named as b_mojo_func. It has one attribute: which_year, and when I start the function later on, I can specify the year I am interested in. Since I will access URLs, I immediately define three of them. Each URL contains 100 titles.
b_mojo_func <- function(which_year) {
    page_1 <- paste0("http://www.boxofficemojo.com/yearly/chart/?page=1&view=releasedate&view2=domestic&yr=", which_year, "&p=.htm")
    page_2 <- paste0("http://www.boxofficemojo.com/yearly/chart/?page=2&view=releasedate&view2=domestic&yr=", which_year, "&p=.htm")
    page_3 <- paste0("http://www.boxofficemojo.com/yearly/chart/?page=3&view=releasedate&view2=domestic&yr=", which_year, "&p=.htm")
    
    # Step 3:
    # Create a data frame that I will be using in the for loop. Here they are with my embedded comments.
    bo_db <- data.frame()
    # Having the URLs, I run these thrice.
    for (i in 1:3) {
        # Read the sites into objects, and I already specify parts of the input, the rest are skipped
        tables <- readHTMLTable(get(paste0("page_", i)), header=TRUE, skip.rows=1, trim=TRUE, which=7)
        # I only need the first 100 rows, and I skip some summary lines from the end
        tables <- tables[1:100,]
        # Bind the three URLs result into one data frame
        bo_db <- rbind(bo_db, tables)
    } 
    
    # Step 4:
    # Give more meaningful names to the header. At the end of this post, I explain what they are exactly.
    names(bo_db) <- c("Rank", "Title", "Studio", "Total_gross", "Theaters", "Opening", "Theaters_2", "Open", "Close")
    
    # Step 5:
    # Clean four columns
    # Replace comma and dollar signs in two columns, I want to see only numbers
    bo_db$Total_gross <- gsub("[,\\$]", "", bo_db$Total_gross)
    bo_db$Opening <- gsub("[,\\$]", "", bo_db$Opening)
    # This is the same, except now only commas removed
    bo_db$Theaters <- gsub(",", "", bo_db$Theaters)
    bo_db$Theaters_2 <- gsub(",", "", bo_db$Theaters_2)
    
    # Step 6:
    # Although the above columns now have only numbers, the class of them are still characters. I change their classes to numeric with the following for loop.
    for (i in 4:7) {
        bo_db[,i] <- as.numeric(bo_db[,i])
    }
    
    # Step 7:
    # The Title column is factor at the moment, so I change it to character. Then there is a tricky line with multiple commands. It's better to separate them into multiple lines, so as others easily understand. This time I don't follow this practice, so here is what I do. In some cases, the title contains when the movie was released. The best example: 2001: Space Odyssey (2013 release). If I want to merge all data frames from different sources using title as the common variable, it's better to remove the strings following the original title. I do this with the strsplit command. As I get a list, I use the unlist command. As I get blank characters around the title, I remove them with str_trim.
    bo_db$Title <- as.character(bo_db$Title)
    for(i in 1:length(bo_db$Title)) {
        bo_db$Title[i] <- str_trim(unlist(strsplit(bo_db$Title[i], "\\("))[1], side="both")
    }
    
    # Step 8:
    # Finally, make the data frame available in the global environment, and close the function.
    bo_db <<- bo_db
}