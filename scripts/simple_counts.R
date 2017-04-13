## adapting https://github.com/programminghistorian/ph-submissions/blob/gh-pages/lessons/basic-text-processing-in-r.md
## the repository for that lesson is the source used here for the word_frequency.csv file

install.packages("tidyverse")
install.packages("tokenizers")
library(tidyverse)
library(tokenizers)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)
setwd("/Insta-dead/")
text <- paste(readLines("textforcountsscript.txt"), collapse = "\n")
words <- tokenize_words(text)
length(words[[1]])

tab <- table(words[[1]])
tab <- data_frame(word = names(tab), count = as.numeric(tab))
tab <- arrange(tab, desc(count))
tab

wf <- read_csv(sprintf("rdata/word_frequency.csv"))
wf

tab <- inner_join(tab, wf)
tab

filter(tab, frequency < 0.5)
print(filter(tab, frequency < 0.0005), n = 10)

## see https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html for next bit

bing <- get_sentiments("bing")
bing_word_counts <- tab %>% 
  inner_join(bing) %>%
  count(word, count, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

#code to setup output plot to file
dpi=600    #pixels per square inch
png("figure.png", width=9*dpi, height=9*dpi, res=dpi)
##

bing_word_counts %>%
  filter(count > 50) %>%
  mutate(count = ifelse(sentiment == "negative", -count, count)) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#code to write to file, and then to stop. won't write without this.
dev.off()

###
nrcpos <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
nrcposcount <- tab %>%
  semi_join(nrcpos) %>%
  count(word, count, sort = TRUE)

nrcneg <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
nrcnegcount <- tab %>%
  semi_join(nrcneg) %>%
  count(word, count, sort = TRUE)

library(wordcloud)

tab %>%
  inner_join(bing) %>%
  count(word, count, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "count", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 200)

##