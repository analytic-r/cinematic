##################### Plots for cinematic retrospective ##################### 

setwd("C:/Attila/Documents/_education/R/solutions/boxoffice/")

library("ggplot2")
library("RColorBrewer")

# Plot 1:
ggplot(bo_to_plot, aes(x = type, y = values, fill = type)) + geom_bar(position="dodge", stat="identity") + theme_minimal() +
    scale_fill_continuous(name="Categories", labels=c("Year sum", "Data frame sum", "Top 10 films", "Number 1 film")) +
    scale_y_continuous(breaks = c(seq(0, 12e+9, by = 3e+9))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank())

# Plot 2-3:
# IMDB ratings.
ggplot(film_merged, aes(x=Open, y=imdb_Rating)) + 
    geom_point(aes(size=-Rank, color=-imdb_Votes)) +
    scale_size("Rank",breaks=c(-10, -50, -100,-200, -300),labels=c(10, 50, 100,200, 300)) +
    scale_colour_gradient("Votes", breaks=c(-600000, -500000, -400000, -300000, -200000, -100000, 0), labels=c("600k", "500k", "400k", "300k", "200k", "100k", "0"))
# Rotten Tomatoes ratings.
ggplot(film_merged, aes(x=Open, y=Critics_score)) + 
    geom_point(aes(size=-Rank, color=-imdb_Votes)) +
    scale_size("Rank",breaks=c(-10, -50, -100,-200, -300),labels=c(10, 50, 100,200, 300)) +
    scale_colour_gradient("Votes", breaks=c(-600000, -500000, -400000, -300000, -200000, -100000, 0), labels=c("600k", "500k", "400k", "300k", "200k", "100k", "0"))

# Plot 4-5:
labelle_r <- function(variable,value){
    return(labels[value])
}
labels <- list("1"="<10M", "2"="10-50M", "3"="50-100M", "4"="100-200M", "5"="200-300M","6"=">300M")
# IMDB ratings
ggplot(film_merged, aes(imdb_Rating, Open)) +
    geom_point(aes(size=imdb_votes_groups, alpha=0.5, colour=total_gross_groups)) +
    theme_bw() + facet_grid(. ~total_gross_groups, labeller=labelle_r)+
    scale_colour_gradientn(colours=cols) + scale_alpha(guide="none")
# Rotten Tomatoes ratings
ggplot(film_merged, aes(Critics_score, Open)) +
    geom_point(aes(size=imdb_votes_groups, alpha=0.5, colour=total_gross_groups)) +
    theme_bw() + facet_grid(. ~total_gross_groups, labeller=labelle_r)+
    scale_colour_gradientn(colours=cols) + scale_alpha(guide="none")

# Plot 6:
cols <- brewer.pal(6, "Dark2")
ggplot(film_merged[film_merged$total_gross_groups==2,], aes(Total_gross, Open)) +
    geom_point(aes(size=imdb_votes_groups, alpha=0.5, colour=total_gross_groups)) +
    theme_bw()+ scale_colour_gradientn(colours=cols[5]) +
    geom_text(data=film_merged[film_merged$total_gross_groups==2 & film_merged$imdb_votes_groups==5,], 
              aes(label=Title), hjust=-0, vjust=2) + coord_flip() +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    scale_alpha(guide="none")

# Plot 7:
a <- ggplot(film_merged, aes(x = Rank, y = imdb_Rating)) + geom_boxplot() + geom_jitter() +
    scale_y_continuous(limits=c(0,10), breaks=seq(0, 10, 1)) + theme_bw() +
    geom_text(data = film_merged[c(which.min(film_merged$imdb_Rating),
                                   which.max(film_merged$imdb_Rating),
                                   which.min(film_merged$Critics_score), 
                                   which.max(film_merged$Critics_score)),], 
              aes(label = Title), hjust=0, vjust=0, size = 4, colour = c("red", "green", "blue", "blue"))
b <- ggplot(film_merged, aes(x = Rank, y = Critics_score)) + geom_boxplot() + geom_jitter() +
    scale_y_continuous(limits=c(0,10), breaks=seq(0, 10, 1)) + theme_bw() +
    geom_text(data = film_merged[c(which.min(film_merged$imdb_Rating),
                                   which.max(film_merged$imdb_Rating),
                                   which.min(film_merged$Critics_score), 
                                   which.max(film_merged$Critics_score)),], 
              aes(label = Title), hjust=0, vjust=0, size = 4, colour = c("blue", "blue", "red", "green"))
grid.arrange(a, b, ncol=2)

# Plot 8:
genres_all <- str_trim(unlist(strsplit(film_merged$Genres, "|", fixed = TRUE)),
                       side = "both")
genres_all <- gsub("-", "", genres_all)
genres_list <- unique(genres_all)
genre_stat <- data.frame()
for (i in genres_list) {
    genre_temp <- data.frame(genre = i, occurences = length(grep(i, genres_all)))
    genre_stat <- rbind(genre_stat, genre_temp)
}
genre_stat <- arrange(genre_stat, occurences)
genre_levels <- names(sort(table(genres_all)))
genre_stat$genre <- factor(genre_stat$genre, levels = genre_levels)
cols = c("#66C2A5", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#3288BD")
ggplot(genre_stat, aes(genre, occurences)) + geom_bar(stat = "identity", fill = cols) + coord_flip()  + theme_bw()

# Plot 9:
run_min <- which(film_merged$Runtime_duration %in% head(sort(film_merged$Runtime_duration),3))
run_max <- which(film_merged$Runtime_duration %in% tail(sort(film_merged$Runtime_duration),3))
ggplot(film_merged, aes(x=Open, y=as.numeric(Runtime_duration))) + geom_jitter() +
    scale_y_continuous(limits=c(0,260), breaks=seq(0, 260, 30)) + theme_bw() +
    geom_text(data=rbind(film_merged[run_min,], film_merged[run_max,]),
              aes(label=Title), hjust=0, vjust=0, size=4)

# Plot 10:
ggplot(film_merged, aes(x = Critics_score, fill = MPAA_rating)) + geom_bar(position = "fill")