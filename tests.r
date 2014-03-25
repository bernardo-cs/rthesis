# source() loads a file into the R console
# history() shows command line history on the R console
# sink() diverts the R consle input to a file

library(stringr)
library(Matrix)
library(kohonen)
# Load file from disk
data.csv.tweets <- file.path('fixtures/50_mb_tweets/tweets_english.csv')
data.csv.binmatrix <- file.path('fixtures/50_mb_tweets/bin_matrix.csv')

tweets <- read.csv(data.csv.tweets, header=FALSE, sep = ",", stringsAsFactors = FALSE)
names(tweets) <- c('username','tweet text')

tweets.bin.matrix <- read.csv(data.csv.binmatrix, header=TRUE, sep = ",", stringsAsFactors = FALSE)
tweets.bin.matrix <- as.matrix(tweets.bin.matrix)

# Bin matrix must be square and have an adequate amount of words 
if(nrow(tweets.bin.matrix) != ncol(tweets.bin.matrix)) message("bin matrix dimensions missmatch") 
if(length(colnames(tweets.bin.matrix)) != ncol(tweets.bin.matrix)) message("Dimension problem while creating the matrixes")

# Receives a dataframe with the format "username" "tweet_text"
# retuns a binary data frame of the tweet_text
df.iterator <- function(df){
  for(tweet.rows.text in df[2]){
    current.line = 1

    for(tweet.text in tweet.rows.text){
      tweet.vector <- str_split(tweet.text, " ") 

      # add new columns to the data.frame
      for (new.row.name in setdiff(tweet.vector[[1]], names(df))){
        if(nchar(new.row.name) > 0){
          df[,new.row.name] <- 0
          df[current.line, new.row.name] <- 1 
        }
      }

      # add value to exsiting cell if word exists
      for(new.row.name in intersect(tweet.vector[[1]], names(df))){
        if(nchar(new.row.name) > 0){
          df[current.line, new.row.name] <- 1 
        }
      }
      current.line = current.line + 1
    }

  }
  return(df)
}
## Converting the DF to a matrix
#tweets.binary.df <- df.iterator(tweets)
#tweets.binary.df.without.names <- subset(tweets.binary.df, select = c(-V1,-V2))
#tweets.binary.matrix <- as.matrix(tweets.binary.df.without.names, dimnames=c(,names(tweets.binary.df.without.names)))

## Converte to sparse matrix
#tweets.binary.sparse <- as(tweets.binary.matrix, "Matrix")

## Run SOMS
tweets.som1 <- som(data = tweets.bin.matrix, grid = somgrid(5, 5, "hexagonal"))
tweets.som2 <- som(data = tweets.bin.matrix, grid = somgrid(5, 5, "hexagonal"))
tweets.som3 <- som(data = tweets.bin.matrix, grid = somgrid(5, 5, "hexagonal"))

## Plot SOMS
pdf("50mb_6457tweets_dataset.pdf")
par(mfrow = c(3,2))
plot(tweets.som1, type = "counts", main = "50mb/6457 Tweets: counts")
plot(tweets.som1, type = "quality", main = "50mb/6457 Tweets: quality")
plot(tweets.som2, type = "counts", main = "50mb/6457 Tweets: counts")
plot(tweets.som2, type = "quality", main = "50mb/6457 Tweets: quality")
plot(tweets.som3, type = "counts", main = "50mb/6457 Tweets: counts")
plot(tweets.som3, type = "quality", main = "50mb/6457 Tweets: quality")
dev.off()

