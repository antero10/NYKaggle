#Libraries
library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(plyr)

#Read the data 
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTest$Hour = NewsTest$PubDate$hour
NewsTrain2 = NewsTrain
NewsTest2 = NewsTest

NewsTest2$Test = 1
NewsTrain2$Test = 0
NewsTest2$Popular = 0

#Mergin the data
News = rbind(NewsTrain2,NewsTest2)

#Corpus

#Headline
CorpusHeadline = Corpus(VectorSource(News$Headline))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtmHeadline = DocumentTermMatrix(CorpusHeadline)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparseHeadline))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

#Abstract
CorpusAbstract = Corpus(VectorSource(News$Abstract))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.99)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))


#Snippet
CorpusSnippet = Corpus(VectorSource(News$Snippet))
CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)
dtmSnippet = DocumentTermMatrix(CorpusSnippet)
sparseSnippet = removeSparseTerms(dtmSnippet, 0.99)
SnippetWords = as.data.frame(as.matrix(sparseSnippet))
colnames(SnippetWords) = make.names(colnames(SnippetWords))

colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))
colnames(AbstractWords) = paste0("A", colnames(AbstractWords))
colnames(SnippetWords) = paste0("S", colnames(SnippetWords))


dtm = cbind(HeadlineWords, AbstractWords,SnippetWords)
dtm$Test = News$Test
dtm$Popular = News$Popular
dtm$WordCount = News$WordCount



#Split
train = subset(dtm,Test == 0)
test = subset(dtm,Test == 1)
test$Popular = NULL

#For some test purpouse

set.seed(144)
split = sample.split(train$Popular, SplitRatio = 0.7)
train2 = subset(train, split==TRUE)
test2 = subset(train,split == FALSE)

#Machine learning

#Logistic Regression

NewsLog = glm(Popular ~ ., data = train2, family = "binomial")
PredictLog = predict(NewsLog,newdata=test2,type="response")

#CART

NewsCART = rpart(Popular ~ ., data= train2, method="class")
PredictCART = predict(NewsCART,newdata=test2,type="class")