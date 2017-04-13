## the source for this script is adapted from Ben Marwick's analysis of the Day of Archaeology,
## https://github.com/benmarwick/dayofarchaeology/blob/master/004_generate_topic_model.r
## The reader might want to consider instead using Marwick's more recent illustrative script on using the tidytext package for topic modeling,
## https://gist.github.com/benmarwick/c02226e2e1774e1c2719f19369e16b56
## We are grateful that Marwick shares his knowledge and expertise with R so freely!

options(java.parameters = "-Xmx6144m")
#install.packages("mallet")
#install.packages("rJava")
require(mallet)
require(rJava)
library(mallet)
setwd("/Insta-dead/")


captionstext <- read.csv("posts-formatted-for-topicmodelling.csv", stringsAsFactors = FALSE)
## kludge for when username comesthrough as numeric rather than character
captionstext$username <-as.character(captionstext$username)

documents <- data.frame(text = captionstext$text,
                        id = make.unique(captionstext$username),
                        class = captionstext$year, 
                        stringsAsFactors=FALSE)
mallet.instances <- mallet.import(documents$id, documents$text, "en.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
## the stopwords file isi added humanbone and humanskull to the stopwords since those were original search terms
## and because skull, skulls, human are extremely prominent after so doing, we add those in too.
## the stopwords list is that which comes bundeled with MALLET, http://mallet.cs.umass.edu/


## Create a topic trainer object.
n.topics <- 25
topic.model <- MalletLDA(n.topics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(1000)

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)

write.csv(doc.topics, file = "25topicsinposts.csv")

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=3)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

# create data.frame with columns as authors and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

# find top n topics for a certain author
df1 <- t(topic_docs[,grep("234396855", names(topic_docs))])

#8963295 is a person who has 'for sale' in her post
#255766488 natural_selections - skullshop.ca
#361451583 ryan matthew cohn
#234396855 pandora's box, York
colnames(df1) <- topics.labels
require(reshape2)
topic.proportions.df <- melt(cbind(data.frame(df1),
                                   document=factor(1:nrow(df1))),
                             variable.name="topic",
                             id.vars = "document") 
# plot for each doc by that author
require(ggplot2)
dpi=600    #pixels per square inch
png("a-figure.png", width=14*dpi, height=14*dpi, res=dpi)

ggplot(topic.proportions.df, aes(topic, value, fill=document)) +
  geom_bar(stat="identity") +
  ylab("proportion") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol=5)
dev.off()


## cluster based on shared words
dpi=600    #pixels per square inch
png("another-figure.png", width=14*dpi, height=14*dpi, res=dpi)
plot(hclust(dist(topic.words)), labels=topics.labels)
dev.off()

dpi=600    #pixels per square inch
png("still-another-figure.png", width=14*dpi, height=14*dpi, res=dpi)
plot(as.dendrogram(hclust(dist(topic.words))),horiz=F,cex=1, main="Dendrogram of topics within Instagram Bone Trade posts")
dev.off()

## How do topics differ across different years?

topic_docs_t <- data.frame(t(topic_docs))
topic_docs_t$year <- documents$class
# now we have a data frame where each row is a topic and 
# each column is a document. The cells contain topic 
# proportions. The next line computes the average proportion of
# each topic in all the posts in a given year. Note that in 
# topic_docs_t$year there is one FALSE, which dirties the data
# slightly and causes warnings
df3 <- aggregate(topic_docs_t, by=list(topic_docs_t$year), FUN=mean)
# this next line transposes the wide data frame created by the above
# line into a tall data frame where each column is a year. The 
# input data frame is subset using the %in% function 
# to omit the last row because this
# last row is the result of the anomalous FALSE value that 
# is in place of the year for one captions post. This is probably
# a result of a glitch in the captions page format. I also exclude
# the last column because it has NAs in it, a side-effect of the
# aggregate function above. Here's my original line:
# df3 <- data.frame(t(df3[-3,-length(df3)]), stringsAsFactors = FALSE)
# And below is an updated version that generalises this in case 
# you have more than two years:
years <- sort(as.character(na.omit(as.numeric(as.character(unique(topic_docs_t$year))))))
df3 <- data.frame(t(df3[(df3$Group.1 %in% years),-length(df3)]), stringsAsFactors = FALSE)
# now we put on informative column names
# names(df3) <- c("y2012", "y2013")
# Here's a more general version in case you have more than two years
# or different years to what I've got:
names(df3) <- unname(sapply(years, function(i) paste0("y",i)))
# the next line removes the first row, which is just the years
df3 <- df3[-1,]
# the next line converts all the values to numbers so we can 
# work on them
df3 <- data.frame(apply(df3, 2, as.numeric, as.character))
df3$topic <- 1:n.topics

# which topics differ the most between the years? 

# If you have 
# more than two years you will need to do things differently
# by adding in some more pairwise comparisons. Here is one 
# pairwise comparison:
df3$diff16_15 <- df3[,6] - df3[,5] # 2015-2016
df3[with(df3, order(-abs(diff15_16))), ]
# # then if you had three years you might then do
# # a comparison of yrs 1 and 3
df3$diff15_14 <- df3[,5] - df3[,4] #2015-2014
df3[with(df3, order(-abs(diff15_14))), ]
# # and the other pairwise comparison of yrs 2 and 3
df3$diff14_13 <- df3[,4] - df3[,3] #2014-2013
df3[with(df3, order(-abs(diff14_13))), ]

df3$diff13_12 <- df3[,3] - df3[,2] #2013-2012
df3[with(df3, order(-abs(diff13_12))), ]

df3$diff12_11 <- df3[,2] - df3[,1] #2012-2011
df3[with(df3, order(-abs(diff12_11))), ]

## and so on

## need to give those diff variables better names.

# plot
library(reshape2)
# we reshape from long to very long! and drop the 
# 'diff' column that we computed above by using a negatve 
# index, that's the -4 in the line below. You'll need to change
# that value if you have more than two years, you might find
# replacing it with -ncol(df3) will do the trick, if you just
# added one diff column. 
df3m <- melt(df3[,-13], id = 7) # the first number is the diff you want, the second is the topics

dpi=600    #pixels per square inch
png("a-figure-showing-topics-over-time.png", width=14*dpi, height=14*dpi, res=dpi)

ggplot(df3m, aes(fill = as.factor(topic), topic, value)) +
  geom_bar(stat="identity") +
  coord_flip()  +
  facet_wrap(~ variable)

dev.off()



