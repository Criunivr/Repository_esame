# SA_advanced_1

# Use UDpipe for multi-language NLP
# https://bnosac.github.io/udpipe/en/

install.packages("udpipe")
library(udpipe)


library(tidyverse)

load("corpora/Pasticceria.RData")

# find models in the resources folder
list.files(path = "resources/udpipe", pattern = ".udpipe", full.names = T)

# if you like, you can download a udpipe model for another language, with the following command:
# udpipe_download_model(language = "my_language", model_dir = "resources/udpipe")
# just change "my_language" to the language of interest

# load the (english) model
udmodel <- udpipe_load_model(file = "resources/udpipe/italian-isdt-ud-2.4-190531.udpipe")

# then process the text
text_annotated <- udpipe(object = udmodel, x = my_df$review, doc_id = rownames(my_df), trace = T)

View(text_annotated)

# now everything is ready to perform (multi-language) SA!

# Example: multi-dimensional SA with SentiArt
# info: https://github.com/matinho13/SentiArt

# read SentiArt from resources folder
#cambia questo per file colab, metti percorso, fai upload 
my_dictionary <- read.csv("resources/sentiment_dictionaries/SA_SVM_dictionary_it (1).csv", stringsAsFactors = F)
View(my_dictionary)

# note: Sentiart includes values per word (not lemma) in lowercase, so we need to lowercase the tokens in our text and perform the analysis on them

text_annotated$token_lower <- tolower(text_annotated$token)

# to avoid annotating stopwords, limit the analysis to meaningful content words
POS_sel <- c("NOUN", "VERB", "ADV", "ADJ", "INTJ") # see more details here: https://universaldependencies.org/u/pos/
text_annotated$token_lower[which(!text_annotated$upos %in% POS_sel)] <- NA

# use left_join to add multiple annotations at once
text_annotated <- left_join(text_annotated, my_dictionary, by = c("token_lower" = "word")) 

# now that the sentiment annotation is done, let's keep just the useful info 
text_annotated <- text_annotated[c(1,19:length(text_annotated))]
text_annotated$doc_id <- as.numeric(text_annotated$doc_id)

# replace NAs with zeros
text_annotated <- mutate(text_annotated, across(everything(), ~replace_na(.x, 0)))

View(text_annotated)

# get overall values per review
sentences_annotated <- text_annotated %>%
  group_by(doc_id) %>%
  summarise_all(list(mean = mean))

# let's order the reviews by number
sentences_annotated <- sentences_annotated[order(as.numeric(sentences_annotated$doc_id)),]

# now we can join the annotations to the original dataframe 
my_df <- cbind(my_df, sentences_annotated[,2:length(sentences_annotated)])
View(my_df)

# then the same analyses of "SA_1.R" can be performed!!

### Your Turn

# continue the analysis as in the "SA_1.R" file

# install and load package for sentiment analysis
install.packages("syuzhet")
library(syuzhet)

install.packages("reshape2")
library(reshape2)

# let's pick up two titles to compare
unique(my_df$book)

my_books <- unique(my_df$book)[c(1,5)]
my_books

# create a subset of the dataframe with just the two books
my_df_red <- my_df %>% filter(book %in% my_books)
my_df_red$review <- NULL
my_df_red$length <- NULL

# visualization 1: barplot
# calculate means
my_df_red_mean <- my_df_red %>%
  group_by(book) %>%
  summarise_all(list(mean = mean))

# melt dataframe
my_df_red_mean <- melt(my_df_red_mean)

# visualize plot
p1 <- ggplot(my_df_red_mean, aes(x=variable, y=value, fill=book))+
  geom_bar(stat="identity", position = "dodge")
p1

# save plot
ggsave(p1, filename = "figures/3.Sentiment_analysis/Goodreads_plot_01.png", height = 9, width = 16)

# better plot
p2 <- ggplot(my_df_red_mean, aes(x=variable, y=value, fill=book))+
  geom_bar(stat="identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))
p2

# save plot
ggsave(p2, filename = "figures/3.Sentiment_analysis/Goodreads_plot_02.png", height = 9, width = 16, scale = 0.5)

# visualization 2: boxplot
# melt dataframe
my_df_red_mean <- melt(my_df_red)

# make plot
p3 <- ggplot(my_df_red_mean, aes(x=variable, y=value, fill=book))+
  geom_boxplot(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))
p3

# save plot
ggsave(p3, filename = "figures/3.Sentiment_analysis/Goodreads_plot_03.png", height = 9, width = 16, scale = 0.5)

