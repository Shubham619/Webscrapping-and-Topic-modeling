
library(rvest)
library(stringi)
library(dplyr)
library(xlsx)
library(cleanme)
library(slam)
library(qdap)
library(tm)
library(corpus)
library(stringr)
library(dplyr)
library(tm.plugin.webmining)
library(purrr)
library(tidytext)
library(gutenbergr)
library(ggplot2)


URL<-readline(prompt = "Please the URL")



s <- html_session(URL)
s <- s %>% follow_link("Get Help")
s <- s %>% follow_link("Phones")
s <- s %>% follow_link("Galaxy S8")





multi_pages<-function(i){
  s$url<-gsub(" ","",paste(s$url,"/page/",i,"?sort=recent"))
  webpage <- read_html(s$url)
  return(webpage)
}




htmlref<-c()
for(i in 1:5){
  l<-multi_pages(i) %>%html_nodes('h3 a') %>% html_attr("href")
  l<-gsub("/ /","/",paste(URL,l)) 
  htmlref<-append(htmlref,l)
  
}



title<-c()
for(i in 1:5){
  j<-multi_pages(i) %>%html_nodes('h3 a') %>% html_text()
  title<-append(title,j)
}




bodies<-c()
for(j in htmlref){
  j<-gsub(" ","",j)
  webpage<-read_html(j)
  body<-webpage %>% html_nodes("#messageBodyDisplay .lia-message-body-content") %>% html_text()%>%as.character %>%toString
  body<-stringr::str_squish(body)
  bodies<-append(bodies,body)
}
#messageBodyDisplay .lia-message-body-content
#messageBodyDisplay p 



slvd<-c()
for(i in 1:length(bodies)){
  if(str_detect(bodies[i],"Solved! Go to Solution")){
    l<-gsub("Solved! Go to Solution.","",bodies[i])
    slvd<-append(slvd,i)
    
  }
}
  



cleaned_body<-c()
for(i in bodies){
  l<-gsub("Solved! Go to Solution.","",i)
  cleaned_body<- append(cleaned_body,l)
}


slvdw<-rep("No",length(bodies))
slvdw[slvd]<-"Yes"


comments<-data.frame(Serial_Number=1:length(title),Title=title,Content=cleaned_body,Solved=slvdw)



write.xlsx(comments,"Product_insights.xlsx",sheetName = "Samsung Galaxy S8",row.names = FALSE)




Content<-comments$Content
Title<-comments$Title

cleanL<-function (x){
  #x <- replace_abbreviation(x)
  x<-tolower(x)
  pattern<-c("won't","n't","v'e","cannot","can't","it's","'re","It'll","what's","i'm","didn't","i'll","it'll")
  replacement<-c("will not"," not","have","can not","can not","it is","are","It will","what is ","i am","did not","I will","It will")
  x<-multigsub(pattern = pattern,replacement = replacement,x,fixed = TRUE,trailspace = TRUE,trim = TRUE)
  patrn<-c("samsung","notebook","galaxy","s8","phone","please help","help","phones","since","month","year","day","rest","hello","note","vm")
  replmnt<-""
  x<-multigsub(patrn,replmnt,x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  x <- gsub("<.*?>", " ", x)
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("[[:digit:]]", " ", x)
  x = gsub("http\\w+", " ", x)
  x = gsub(" $", " ", x)
  x <- stripWhitespace(x)
  x <- removeWords(x, c(stopwords("SMART")))
  x <- stringi::stri_trim(x, side = c("both", "left", "right"))
  x<-str_squish(x)
  return(x)
}
clean_content<-cleanL(Content)




clean_content<-cleanL(Content)
clean_content<-(clean_content[which(str_count(clean_content," ")>1)])
clean_content<-tibble(text=clean_content)



tidy_text <- clean_content %>%
  unnest_tokens(word,text) 


tidy_text<-tidy_text %>%
  count(word, sort = TRUE)  


tidy_text <- tidy_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


tidy_text %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))






library(RColorBrewer)
library(wordcloud)



set.seed(100)

pos_words <- tidy_text %>%
  filter(sentiment == "positive") %>%
  group_by(word) %>%
  tally




pos_words %>% with(wordcloud(word, n, max.words = 14))



set.seed(100)

neg_words <- tidy_text %>%
  filter(sentiment == "negative") %>%
  group_by(word) %>%
  tally


neg_words %>%
  with(wordcloud(word, n, max.words = 15, colors =  c("#56B4E9", "#E69F00")))


neg_words<-unique(tidy_text$word[tidy_text$sentiment %in% c("anger","fear","sadness","negative")])
pos_words<-c("faster","loyality","quality","performance","speed","fast")






pos_neg<-c()
for(v in 1:length(clean_content$text)){
  neg_iv<-unlist(str_split(clean_content$text[v]," ")) %in% neg_words
  pos_iv<-unlist(str_split(clean_content$text[v]," ")) %in% pos_words
  neg_cnt<-length(neg_iv[neg_iv==TRUE])
  pos_cnt<-length(pos_iv[pos_iv==TRUE])
  pos_cnt<-length(pos_cnt[pos_iv==TRUE])
  t<-(c(pos_cnt,neg_cnt))
  
  if(t[1]>=1){
    pos_neg<-append(pos_neg,"Positive")
    
  } else {
    pos_neg<- append(pos_neg,"Negative")
  }
    
}
pos_neg


corpus<-Corpus(VectorSource(clean_content$text))


# Preprocessing again
processedCorpus <- tm_map(corpus, content_transformer(tolower))

processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

minimumFrequency <- 4
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

dim(DTM)


sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- clean_content$text[sel_idx ]

require(topicmodels)
K <- 20

topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
tmResult <- posterior(topicModel)



nDocs(DTM)
beta <- tmResult$terms   
dim(beta)                



theta <- tmResult$topics 
dim(theta)  



top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

require(wordcloud)
topicToViz <- 11 
topicToViz <- grep('updat', topicNames)[1] 
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)





topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 3), 2, paste, collapse = " ")


topicNames <- apply(lda::top.topic.words(beta, 7, by.score = T), 2, paste, collapse = " ")


topicProportions <- colSums(theta) / nDocs(DTM)  
names(topicProportions) <- topicNames    
sort(topicProportions, decreasing = TRUE) 




topicNames <- apply(lda::top.topic.words(beta, 7, by.score = T), 2, paste, collapse = " ")
topicNames


library(parallel)
many_models <- mclapply(seq(2, 35, by = 1), 
                        function(x) {LDA(DTM, x, method = "Gibbs",
                                         control = list(iter = 500, verbose = 25, alpha = 0.2))} )



many_models.logLik <- as.data.frame(as.matrix(lapply(many_models, logLik)))
which.max(many_models.logLik$V1)





topicModel2 <- LDA(DTM,which.max(many_models.logLik$V1) , method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 4), 2, paste, collapse = " ")  # reset topicnames


dfa_topic_names<-data.frame(topicNames)

library(readxl)
topics_cat <- read_excel("Product_insights.xlsx",sheet = 2)

    
    
sort(topics(topicModel2),decreasing = TRUE)
dfa<-data.frame(topics(topicModel2))
row_dfa<-rownames(dfa)
dfa<-data.frame(n=row_dfa,topics=topics(topicModel2),text=clean_content[as.numeric(row_dfa)],Major=topics_cat$`Major issues`[dfa$topics])



write.xlsx(dfa,"excelops.xlsx",sheetName = "xlsx")



library(udpipe)
#during first time model download execute the below line too
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = model$file_model)
s <- udpipe_annotate(udmodel_english, Content)
x <- data.frame(s)

library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

np <- keywords_phrases(x$xpos, pattern = (""), term = x$token,is_regex = TRUE)


stats_n <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                         relevant = x$upos %in% c("VERB"))
stats_n$key <- factor(stats_n$keyword, levels = rev(stats_n$keyword))




stats_adn <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                         relevant = x$upos %in% c("ADJ"))
stats_adn$key <- factor(stats_adn$keyword, levels = rev(stats_adn$keyword))


wrds<-unique(c(stats_adn$keyword,stats_n$keyword))

wrds<-wrds[removeWords(wrds,stopwords$word)>1]








cv$V1


cv<-data.frame()
for(v in 1:length(clean_content$text)){
  iv<-unique(unlist(str_split(clean_content$text[v]," "))[unlist(str_split(clean_content$text[v]," ")) %in% wrds])
  iv<-iv[!iv %in% stats_n$keyword]
  iv<-paste(iv,collapse = ' ')
  if(iv==''){
    iv<-"error"
    cv[v,1]<-iv
    
  }
  cv[v,1]<-iv
  
  
}
