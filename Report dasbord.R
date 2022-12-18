

#Load library for analysis
library(syuzhet)
library(ggplot2)
library(plotly)
library(plyr)
library(wordcloud)
library(tm)
library(tidyverse)
library(lessR)
library(dplyr)
library(psych)



#read file
library(readr)
jumia_1 <- read_csv("C:/Users/Elite/Desktop/Rclasess/jumia 1.csv")
View(jumia_1)

#restored data in charater format
review1 <-as.character(jumia_1$reviews)

#obtain setiment scores
get_nrc_sentiment("happy")
get_nrc_sentiment("abuse")

#store data into new variable

jumai2<-get_nrc_sentiment(review1)

#combine text and sentiment column
review_sentiment1 <- cbind(jumia_1$reviews)

#barplot for sentiment analysis
barplot(colSums(jumai2), col = rainbow(10),ylab ="count", 
        main = 'Jumai feedback of oraimo freepod 3 True Wireless Earbuds ENC')


Corpus <- Corpus(VectorSource(jumia_1$reviews))
Corpus[[1]] [1]

#covert text to lowercase
Corpus <- tm_map(Corpus,content_transformer(tolower))
#REMOVE NUMBERS
Corpus <- tm_map(Corpus,removeNumbers)
#REMOVE PUTATUATIONS
Corpus <- tm_map(Corpus,removePunctuation)

#ELIMINATE  EXTRA WHITE SPACE
Corpus <- tm_map(Corpus,stripWhitespace)

#REMOVE COMMON ENGLISH STOP WORDS
Corpus <- tm_map(Corpus,removeWords,stopwords('english'))
#creae tdm
tdm <- TermDocumentMatrix(Corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame( word = names(v), freq = v)



wordcloud(d$word,d$freq,
          random.order = TRUE, rot.per = 0.3,
          scale=c(4,.5),max.words = 129,
          colors = brewer.pal(8,"Dark2"))
#to export csv file

write.csv(jumai2,file = 'jumai2.csv')
save(jumai2,file = "jumai.RData")


#inferential analysis
max(review1)
min(review1)
min(reviews)
max(reviews)
max(Star)
min(Star)

# mporting  newfile
library(readr)
p_jumai <- read_csv("p jumai.csv")
View(p_jumai)
attach(p_jumai)
colnames(jumaiproduct)
jumaiproduct <- p_jumai
attach(jumaiproduct)
#examine the mean difference between actual price and discounted price
t.test(`Actual_Price`,Discount_price,
       alt="Two.sided", conf=0.95, data = jumaiproduct, alternative =  'greater')

t.test(`Actual_Price`,Discount_price,
       alt="Two.sided", conf=0.95, data = jumaiproduct, alternative =  'less')

cor.test(Actual_Price,Discount_price)
