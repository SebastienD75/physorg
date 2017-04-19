
#setwd("~/Dev/R - Phys.org")

{
  library(dplyr)
  library(tidytext)
  library(data.table)
  library(stringr)
  library(tm)
  library(glmnet)
  library(wordcloud)
  require(RColorBrewer)
  library(text2vec)
  library(SnowballC)
  library(e1071)
  library(kernlab)
  library(doParallel)
}


# PARAMS ------------------------------------------------------------------
{
  protected.obj <- c("protected.obj", "bench.model", "d.art.c.bench", "d.art.c.clean.url")
  
  if(!exists('d.art.c.bench')) { 
    load('data/glmnet_cat.01.RData')
    protected.obj <- c("protected.obj", "bench.model", "d.art.c.bench", "d.art.c.clean.url")
  }
  
  rm(list = setdiff(ls(), protected.obj))
  gc()
  
  # levels(d.art$category)
  # [1] "Astronomy & Space" "Biology"           "Chemistry"         "Earth"            
  # [5] "Health"            "Nanotechnology"    "Other Sciences"    "Physics"          
  # [9] "Technology" 
  #param.categories <- c('Physics', 'Chemistry')
  
  
  param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                    'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                    'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                    'Materials Science')
  param.dorpsc <- c('')
  
  param.categories <- levels(d.art.c.bench$category)
  
  if(!exists('bench.model')) 
  {
    bench.model <- list()
  }
  
  # param.pctdata.inc <- c(0.005, 0.01, 0.03, 0.09, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  param.pctdata.inc <- c(0.03, 0.09, 0.2, 0.3, 0.4)
  param.maxmodel = 15
  
  param.dofeaturehashing = FALSE
  param.doprune = TRUE
  param.dongram = FALSE
  
  param.doparall.worker = 7
  
  init.param.pctdata = 0.03
  
  init.param.bench.glmnet.NFOLDS = 3
  init.param.bench.glmnet.THRESH = 1e-2 # (default 1E-7)
  init.param.bench.glmnet.MAXIT =  1e2 # (default 10^5)
  
  init.param.prune.term_count_min = 20
  init.param.prune.doc_proportion_max = 0.6
  init.param.prune.doc_proportion_min = 0.0005
  
  param.pctdata.init <- 0
  param.train_test <- 0.7
  init.model_num = 1
  model_desc = ''
  #param.startmodel = ceiling(length(bench.model)/2 + 1)
  param.startmodel = 1
  param.seed = 20170416
  
  set.seed(param.seed)
  registerDoParallel(param.doparall.worker)
  
}

# FUNCTIONS  -------------------------------------------------------------------
{
  save_model <- function(model_name) 
  {
    bench.model[[model_name]]$model_name <<- model_name
    bench.model[[model_name]]$model_num <<- model_num
    bench.model[[model_name]]$mode_desc <<- mode_desc
    
    bench.model[[model_name]]$param.bench.glmnet.NFOLDS <<- param.bench.glmnet.NFOLDS
    bench.model[[model_name]]$param.bench.glmnet.THRESH <<- param.bench.glmnet.THRESH
    bench.model[[model_name]]$param.bench.glmnet.MAXIT <<- param.bench.glmnet.MAXIT
    
    bench.model[[model_name]]$param.dofeaturehashing <<- param.dofeaturehashing
    bench.model[[model_name]]$param.dongram <<- param.dongram
    bench.model[[model_name]]$param.doprune <<- param.doprune
    bench.model[[model_name]]$param.prune.term_count_min <<- param.prune.term_count_min
    bench.model[[model_name]]$param.prune.doc_proportion_max <<- param.prune.doc_proportion_max
    bench.model[[model_name]]$param.prune.doc_proportion_min <<- param.prune.doc_proportion_min
    
    bench.model[[model_name]]$param.pctdata <<- param.pctdata
    bench.model[[model_name]]$param.train_test <<- param.train_test
    bench.model[[model_name]]$param.categories <<- param.categories
    bench.model[[model_name]]$param.dorpsc <<- param.dorpsc
    
    bench.model[[model_name]]$bench.train_tokens.time <<- bench.train_tokens.time
    bench.model[[model_name]]$bench.dtm_train.time <<- bench.dtm_train.time
    bench.model[[model_name]]$bench.dtm_test.time <<- bench.dtm_test.time
    bench.model[[model_name]]$bench.dtm_train.dim <<- dim(bench.dtm_train)
    bench.model[[model_name]]$bench.dtm_test.dim <<- dim(bench.dtm_test)
    bench.model[[model_name]]$bench.vectorizer <<- bench.vectorizer
    bench.model[[model_name]]$bench.glmnet_classifier.time <<- bench.glmnet_classifier.time
    bench.model[[model_name]]$bench.glmnet_classifier <<- bench.glmnet_classifier
    bench.model[[model_name]]$bench.preds.class.time <<- bench.preds.class.time
    bench.model[[model_name]]$bench.glmnet_classifier.accuracy <<- bench.glmnet_classifier.accuracy
    bench.model[[model_name]]$bench.glmnet_classifier.accuracy_cat <<- res
    
    print(sprintf("Model %s saved (%s min): pct data=%s, dim_train=(%d, %d), %s", 
                  model_name, 
                  bench.glmnet_classifier.time,
                  bench.model[[model_name]]$param.pctdata,
                  bench.model[[model_name]]$bench.dtm_train.dim[[1]],
                  bench.model[[model_name]]$bench.dtm_train.dim[[2]],
                  bench.model[[model_name]]$bench.glmnet_classifier.accuracy))
  }
  
  print_model <- function(model) {
    params <- c('model_name', 'model_num','param.pctdata','mode_desc','bench.dtm_train.dim', 'bench.glmnet_classifier.accuracy')
    lapply(model, function(m) {
      lapply(params, function(p) {
        str <- paste0(p, ':', m[[p]])
        str <- gsub('(.*)\\$.*', ':\\1',str)
        print(str)
      })
    })
  }
}

# BENCH DATA SIZE -------------------------------------------------------------------

param.pctdata <- param.pctdata.init
model_num <- init.model_num
param.maxmodel = min(c(length(param.pctdata.inc), param.maxmodel))

for(i in param.startmodel:param.maxmodel)
{
  param.pctdata <- param.pctdata.inc[[i]]
  
  cat(paste('\n\n',"Testing model with param.pctdata =", param.pctdata), '\n')
  
  bench.num_sample = ceiling(param.pctdata * dim(d.art.c.bench)[[1]])
  bench.all_ids = d.art.c.bench$id
  bench.train_ids = sample(bench.all_ids, bench.num_sample)
  d.bench <- d.art.c.bench[J(bench.train_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
  
  d.bench[,id := (.I)]
  setkey(d.bench, id)
  
  bench.num_sample = ceiling(param.train_test * dim(d.bench)[[1]])
  bench.all_ids = d.bench$id
  bench.train_ids = sample(bench.all_ids, bench.num_sample)
  bench.test_ids = setdiff(bench.all_ids, bench.train_ids)
  bench.train = d.bench[J(bench.train_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
  bench.test = d.bench[J(bench.test_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
  
  print(paste("Train nb articles =", dim(bench.train)[[1]]))
  
  tokenizer.stem = function(x) {
    tokens = word_tokenizer(x)
    lapply(tokens, SnowballC::wordStem, language="en")
  }
  
  bench.train_tokens.time <- system.time(
    bench.train_tokens <- bench.train$content %>% tokenizer.stem
  ); print(sprintf('bench.train_tokens.time: %0.2fs', bench.train_tokens.time[[3]]))
  
  bench.it_train <- itoken(bench.train_tokens, 
                           ids = bench.train$id,
                           progressbar = FALSE)
  
  bench.it_test <- bench.test$content %>% 
    word_tokenizer %>%
    itoken(ids = bench.test$id, progressbar = FALSE)
  
  if(param.dofeaturehashing) {
    
    bench.h_vectorizer = hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))
    bench.vectorizer <- bench.h_vectorizer
    
    bench.dtm_train.time <- system.time(
      bench.dtm_train<-create_dtm(bench.it_train, bench.h_vectorizer)
    ); print(sprintf('bench.dtm_train.time: %0.2fs', bench.dtm_train.time[[3]]))
    
    bench.dtm_test.time <- system.time(
      bench.dtm_test<-create_dtm(bench.it_test, bench.h_vectorizer)
    ); print(sprintf('bench.dtm_test.time: %0.2fs', bench.dtm_test.time[[3]]))
    
    
  } else {
    
    
    if(param.dongram) {
      
      bench.train.vocab.stem.time <- system.time(
        bench.train.vocab.stem<- create_vocabulary(bench.it_train, ngram = c(1L, 2L))
      ); print(sprintf('bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
      
    } else {
      
      bench.train.vocab.stem.time <- system.time(
        bench.train.vocab.stem <- create_vocabulary(bench.it_train)
      ); print(sprintf('bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
      
    }
    
    if(param.doprune) {
      
      param.prune.term_count_min = init.param.prune.term_count_min
      param.prune.doc_proportion_max = init.param.prune.doc_proportion_max
      param.prune.doc_proportion_min = init.param.prune.doc_proportion_min
      
      bench.train.vocab.stem.prune.time <- system.time(
        bench.train.vocab.stem <- prune_vocabulary(bench.train.vocab.stem,
                                                   term_count_min = param.prune.term_count_min,
                                                   doc_proportion_max = param.prune.doc_proportion_max,
                                                   doc_proportion_min = param.prune.doc_proportion_min)
      ); print(sprintf('bench.train.vocab.stem.prune.time: %0.2fs', bench.train.vocab.stem.prune.time[[3]]))
      
    }
    
    bench.vectorizer <- vocab_vectorizer(bench.train.vocab.stem)
    
    bench.dtm_train.time <- system.time(
      bench.dtm_train<-create_dtm(bench.it_train, bench.vectorizer)
    ); print(sprintf('bench.dtm_train.time: %0.2fs', bench.dtm_train.time[[3]]))
    
    bench.dtm_test.time <- system.time(
      bench.dtm_test<-create_dtm(bench.it_test, bench.vectorizer)
    ); print(sprintf('bench.dtm_test: %0.2fs', bench.dtm_test.time[[3]]))
  }
  
  gc()
  
  model_name <- paste0(as.character(i), as.character(length(bench.model) + 1))
  model_num <- as.numeric(model_name)
  
  param.bench.glmnet.THRESH <- init.param.bench.glmnet.THRESH
  param.bench.glmnet.MAXIT <- init.param.bench.glmnet.MAXIT
  param.bench.glmnet.NFOLDS <- init.param.bench.glmnet.NFOLDS
  mode_desc <- sprintf('model %d - text2vect tfidf cv.glmnet : glmnet.params = ALPHA:1, NFOLDS:%d, THRESH:%s, MAXIT:%s + featureh=%s, ngram=%s, prune=%s :  prune.params = countmin:%s, doc.prop.max:%s, doc.prop.min:%s', 
                       model_num,
                       param.bench.glmnet.NFOLDS,
                       param.bench.glmnet.THRESH,
                       param.bench.glmnet.MAXIT,
                       param.dofeaturehashing,
                       param.dongram,
                       param.doprune,
                       param.prune.term_count_min,
                       param.prune.doc_proportion_max,
                       param.prune.doc_proportion_min
                       )
  cat(mode_desc,'\n')
  tfidf = TfIdf$new()
  bench.dtm_train.tfidf = fit_transform(bench.dtm_train, tfidf)
  # tfidf modified by fit_transform() call!
  # apply pre-trained tf-idf transformation to test data
  bench.dtm_test.tfidf  = create_dtm(bench.it_test, bench.vectorizer) %>% 
    transform(tfidf)
  
  
  bench.glmnet_classifier.time <- system.time(
    bench.glmnet_classifier <- cv.glmnet(x = bench.dtm_train.tfidf, y = bench.train[['category']], 
                                         # family = 'binomial',                              
                                         family = 'multinomial', 
                                         type.multinomial="grouped", 
                                         # L1 penalty
                                         alpha = 1,
                                         # ROC curve
                                         type.measure = "auc",
                                         nfolds = param.bench.glmnet.NFOLDS,
                                         thresh = param.bench.glmnet.THRESH,
                                         maxit = param.bench.glmnet.MAXIT,
                                         parallel = TRUE)
    
  ); print(sprintf('bench.glmnet_classifier.tfidf.time: %0.2fm', bench.glmnet_classifier.time[[3]]/60))
  
  plot(bench.glmnet_classifier)
  
  bench.preds.class.time <- system.time(
    bench.test$bench.preds.class <-  predict(bench.glmnet_classifier, bench.dtm_test.tfidf, s = "lambda.min", type = 'class')
  ); print(sprintf('bench.preds.class: %0.2fs', bench.preds.class.time[[3]]))
  
  bench.glmnet_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.preds.class]))/dim(bench.test)[[1]])
  print(bench.glmnet_classifier.accuracy)
  
  res <- bench.test %>%
    mutate(accurate = ifelse(category == bench.preds.class, 1, 0)) %>%
    group_by(category) %>%
    summarise(n = n(),
              pct = 100*n/dim(bench.test)[[1]],
              accurate = sum(accurate),
              accuracy = (100*accurate/n)) %>%
    select(category, n, pct, accuracy) %>%
    arrange(-accuracy)
  
  print(res)
  
  save_model(model_name)
  
  gc()
  
}

