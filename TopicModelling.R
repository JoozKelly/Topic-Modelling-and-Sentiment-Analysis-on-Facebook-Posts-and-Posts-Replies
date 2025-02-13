#install packages
install.packages("servr")

#load required packages
library(quanteda)     #used for pre-processing and analyzing textual data (tokenization)
library(topicmodels)  #used for topic modelling using Latent Dirichlet Allocation (LDA)
library(slam)         #structures and algorithms for sparse arrays and matrices
library(tm)           #to create corpus
library(tibble)       #Load tibble package
library(stringr)      #Load the stringr package

# import text data set
df <- read.csv("Facebook.csv")
text.df <- tibble(text = str_to_lower(df$message))

#Text to corpus
c = corpus(text.df)

# Text cleaning using tokens
t = tokens(c, remove_punct = T,
           remove_symbols = T,
           remove_url = T,
           remove_numbers = T,
           remove_separators = T)

t = tokens_tolower(t)
t = tokens_remove(t, pattern = stopwords("english"))

# Clean corpus created by OCR
t = tokens_select(t,
                  c("[\\d-]","[[:punct:]]", "^.{1,2}$"),
                  selection = "remove",
                  valuetype = "regex",
                  verbose = TRUE
)

#Convert tokens to dfm
dfm = dfm(t)
dfm = dfm_remove(dfm, stopwords("en"))
head(dfm_sort(dfm, decreasing = TRUE,
              margin = "both"),
              n=10)

# Generate topic modelling using LDA "Gibbs" method with different K's
dtm = convert(dfm, to = "topicmodels")
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
m

terms(m, 5)

# Generating word cloud for a topic
topic = 5
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)

library(wordcloud)
wordcloud(names(topwords), topwords)

# Visualizing using LDAvis
library(LDAvis)   

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)