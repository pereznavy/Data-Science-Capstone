
library(stringi)
library(ggplot2)
library(tm)
library(wordcloud)
library(RWeka)
library(pryr)
library(quanteda)
library(SnowballC)

##Crear un corpus


##Aplicacion al caso de los datos de US

setwd("~/Dropbox/Lab-Ram/Twitter/Ana_Texto")
blog <- readLines("en_US.blogs.txt",encoding = "UTF-8",skipNul = TRUE)
news <- readLines("en_US.news.txt",encoding = "UTF-8",warn = FALSE,skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt",encoding = "UTF-8",skipNul = TRUE)


set.seed(12345)
test_data <- c(sample(blog, length(blog) * 0.01),
               sample(news, length(news) * 0.01),
               sample(twitter, length(twitter) * 0.01)
)



corp_tm <- tm::VCorpus(tm::VectorSource(test_data))
corp_tm <-tm_map(corp_tm, content_transformer(tolower))
corp_tm <- tm_map(corp_tm, removeNumbers)
corp_tm <-  tm_map(corp_tm, removePunctuation)
corp_tm <- tm_map(corp_tm, removeWords, stopwords())

corp_quanteda <- corpus(corp_tm)  ##Aqui ahora si ya se ve con View y podemos identificar que tiene

##Ahora si se le pueden aplicar las funciones


toks_corp <- tokens(corp_quanteda, remove_punct = TRUE)

tstat_col_caps <- tokens_select(toks_corp, pattern = '^[A-Z]', 
                                valuetype = 'regex', 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 50)
head(tstat_col_caps, 20)


tstat_col2 <- tokens_select(toks_corp, pattern = '^[A-Z]', 
                            valuetype = 'regex', 
                            case_insensitive = FALSE, 
                            padding = TRUE) %>% 
  textstat_collocations(min_count = 50)
head(tstat_col2, 20)

col_corpus <- textstat_collocations(corp_quanteda, method = "lambda", size = 3, min_count = 2,
                                    smoothing = 0.5, tolower = TRUE)
