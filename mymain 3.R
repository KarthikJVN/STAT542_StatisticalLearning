start.time = Sys.time()

#set.seed(8871)
library(text2vec)
library(data.table)
library(magrittr)
library(pROC)
library(glmnet)

setwd("/Users/karthikjvn/Documents/Mac_Transfer_March2018/UIUC/Fall_2018/STAT542/Project 4")

## From Piazza
all = read.table("data.tsv",stringsAsFactors = F,header = T)
all$review = gsub('<.*?>', ' ', all$review)
splits = read.table("splits.csv", header = T)
s = 1 # Here we get the 3rd training/test split. 
train = all[-which(all$new_id%in%splits[,s]),]
test = all[which(all$new_id%in%splits[,s]),]


## Vocabulary-based vectorization
# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$review, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)



## Code test Token
it_test = itoken(test$review, preprocess_function = tolower,
                 tokenizer = word_tokenizer)

#stop_words = stopwords(kind = 'en')
stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")



vocab = create_vocabulary(it_train, ngram = c(1L, 2L), 
                          stopwords = stop_words)
pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 5, 
                                doc_proportion_max = 0.5,
                                vocab_term_max = 50000)


pruned_vocab = readRDS('vocabulary.rd')
vectorizer = vocab_vectorizer(pruned_vocab)
# create dtm_train with new pruned vocabulary vectorizer

dtm_train  = create_dtm(it_train, vectorizer)
dtm_test = create_dtm(it_test, vectorizer)

v.size = dim(dtm_train)[2]
ytrain = train$sentiment

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf = create_dtm(it_test, vectorizer)
dtm_test_tfidf = transform(dtm_test_tfidf, tfidf)


 summ = matrix(0, nrow=v.size, ncol=4)
 summ[,1] = apply(dtm_train[ytrain==1, ], 2, mean) ## Use slam library instead??summ[,2] = apply(dtm_train[ytrain==1, ], 2, var)
 summ[,2] = apply(dtm_train[ytrain==1, ], 2, var)
 summ[,3] = apply(dtm_train[ytrain==0, ], 2, mean)
 summ[,4] = apply(dtm_train[ytrain==0, ], 2, var)
 n1=sum(ytrain); 
 n=length(ytrain)
 n0= n - n1
 
 myp = (summ[,1] - summ[,3])/
   sqrt(summ[,2]/n1 + summ[,4]/n0)

words = colnames(dtm_train)
id = order(abs(myp), decreasing=TRUE)[1:3000] ### Vocab Size
pos.list = words[id[myp[id]>0]]
neg.list = words[id[myp[id]<0]]

 #rownames(pruned_vocab) = NULL
 #vocab = pruned_vocab[id,]
 #saveRDS(pruned_vocab, 'vocabulary.rd')


set.seed(8871)
NFOLDS = 10
mycv = cv.glmnet(x=dtm_train[, id], y=train$sentiment, 
                 family='binomial',type.measure = "auc", 
                 nfolds = NFOLDS, alpha=0)
myfit = glmnet(x=dtm_train[, id], y=train$sentiment, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)
logit_pred = predict(myfit, dtm_test[,id], type = "response")
#logit_pred = predict(myfit, dtm_test[, (colnames(dtm_test) %in% words[id])], type = "response")

#dtm_test[, words[id]]
roc_obj = roc(test$sentiment, as.vector(logit_pred))
auc(roc_obj) 

write.table(matrix(data=c(test$new_id,logit_pred),ncol=2),file="mysubmission3.txt",row.names=FALSE,col.names=c("id","prob"),sep=',')

write.table(matrix(data=c(test$sentiment,logit_pred),ncol=2),file="sent3.csv",row.names=FALSE,col.names=c("sentiment","prob"),sep=',')
end.time = Sys.time()

