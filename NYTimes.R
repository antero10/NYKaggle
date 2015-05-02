#Libraries
library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(plyr)
library(caret)
library(e1071)

#Read the data 
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTest$Hour = NewsTest$PubDate$hour
NewsTrain$Minute = NewsTrain$PubDate$min
NewsTest$Minute = NewsTest$PubDate$min
#NewsTest$Weekday = factor(weekdays(as.Date(NewsTrain$PubDate)))
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
CorpusHeadline = tm_map(CorpusHeadline, stemDocument,language="english")
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
CorpusAbstract = tm_map(CorpusAbstract, stemDocument,language="english")
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
CorpusSnippet = tm_map(CorpusSnippet, stemDocument,language="english")
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
dtm$NewsDesk = as.factor(News$NewsDesk)
dtm$SectionName = as.factor(News$SectionName)
dtm$SubsectionName = as.factor(News$SubsectionName)
dtm$Hour = News$Hour

dtm$UniqueID = News$UniqueID
#Split
train = subset(dtm,Test == 0)
test = subset(dtm,Test == 1)
test$Popular = NULL
#numFolds = trainControl(method = "cv",number=10)
#cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
#train(Popular ~ ., data=train,method="rpart",trControl = numFolds,tuneGrid = cpGrid)

#Submision
#ExamCART = rpart(Popular ~ ., data=train,method="class",cp=0.01)
#ExamPredictCART = predict(ExamCART,newdata=test,type="class")
#ExamRandomForest = randomForest(Popular ~ ., data=train)
#ExamPredictRandomForest = predict(ExamRandomForest,newdata=test)
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = ExamPredictRandomForest)
#write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)


#Machine learning
#For some test purpouse

set.seed(144)
split = sample.split(train$Popular, SplitRatio = 0.7)
train2 = subset(train, split==TRUE)
test2 = subset(train,split == FALSE)
#Logistic Regression

#NewsLog = glm(Popular ~ ., data = train2, family = 'binomial')
#PredictLog = predict(NewsLog,newdata=test2,type='response')

#CART

NewsDeskCART = rpart(NewsDesk ~ .,data=train2,method='class')
predictDeskCART = predict(NewsDeskCART,newdata=test2,type="class")
NewsSectionNameCART = rpart(SectionName ~.,data=train2,method='class')
predictSectionNameCART = predict(NewsSectionNameCART,newdata=test2,type='class')
NewsSubSectionCART = rpart(SubsectionName ~.,data=train2,method='class')
predictSubSectionCART = predict(NewsSubSectionCART,data=train2,type="class")
#NewsCART = rpart(Popular ~ ., data= train2, method='class')
#PredictCART = predict(NewsCART,newdata=test2,type='class')

#Random Forest

#NewsRandomForest = randomForest(Popular ~ ., data= train2)
#predictForest = predict(NewsRandomForest,newdata=test2)
