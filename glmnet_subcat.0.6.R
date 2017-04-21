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
  library(doParallel)
}
# env.path <- "~/Dev/R - Phys.org"
# setwd(env.path)
# d.allresults <- list()

# PARAMS ------------------------------------------------------------------
#allcat <- levels(d.art$category)
allcat <- c('Astronomy & Space','Other Sciences','Technology','Physics', 'Nanotechnology','Health', 'Biology', 'Earth','Chemistry')

protected.obj <- c("protected.obj", "i", "d.art", "allcat", "d.allresults")

param.doparall.worker = 3
registerDoParallel(param.doparall.worker)

for (i in 1:length(allcat))  {
  cat('\n\n')
  
  rm(list = setdiff(ls(), protected.obj))
  gc()
  names(d.allresults)

  env.datafile.art <- 'data/physorg.RData'
  
  load(env.datafile.art)
  rm(list = setdiff(ls(), protected.obj))
  gc()
  
  dongram = FALSE
  dobench = FALSE
  param.train_test <- 0.7
  
  bench.glmnet.NFOLDS = 5
  bench.glmnet.THRESH = 1e-3
  bench.glmnet.MAXIT =  1e4
  
  # glmnet.NFOLDS = bench.glmnet.NFOLDS * 2
  # glmnet.THRESH = bench.glmnet.THRESH / 100
  # glmnet.MAXIT =  bench.glmnet.MAXIT * 100
  # 
  
  
  glmnet.NFOLDS = bench.glmnet.NFOLDS
  glmnet.THRESH = bench.glmnet.THRESH
  glmnet.MAXIT =  bench.glmnet.MAXIT

  
  prune.term_count_min = 40
  prune.doc_proportion_max = 0.4
  prune.doc_proportion_min = 0.0008
  
  param.seed = 20170416
  
  levels(d.art$category)
  # [1] "Astronomy & Space" "Biology"           "Chemistry"         "Earth"            
  # [5] "Health"            "Nanotechnology"    "Other Sciences"    "Physics"          
  # [9] "Technology" 
  
  #param.cat <- 'Astronomy & Space'
  # param.cat <- 'Other Sciences'
  # param.cat <- 'Technology'
  # param.cat <- 'Physics'
  # param.cat <- 'Health'
  
  param.cat = allcat[[i]]
  print(param.cat)
  
  param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                    'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                    'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                    'Materials Science')
  
  param.cat.name <- param.cat %>%
    str_replace_all("[^[A-Za-z]']", "_") %>%
    str_replace_all("_+", "_")
  
  # INIT --------------------------------------------------------------------
  
  
  set.seed(param.seed)
  d.allresults[[param.cat.name]] <- list()
  
  # NETTOYAGE ---------------------------------------------------------------
  
  d.allresults$objsize.init <- format(object.size(d.art), units = 'auto')
  
  d.art.sc <- d.art %>%
    select(url, content, category, subcategory) %>%
    filter(category == param.cat) %>%
    filter(!(subcategory %in% param.dorpsc)) %>%
    mutate(subcategory = droplevels(subcategory)) %>%
    select(url, content, subcategory) %>%
    setDT()
  
  rm(d.art)
  gc()
  
  t0 <- Sys.time()
  
  # d.allresults[[param.cat.name]]$d.art.sc <- d.art.sc
  d.art.sc.size <- format(object.size(d.art.sc), units = 'auto')  
  d.allresults[[param.cat.name]]$d.art.sc.size <- d.art.sc.size
  
  print('d.art.sc.clean...')
  d.art.sc.clean.time <- system.time(
    d.art.sc.clean<-d.art.sc %>%
      # suppression des ' qui ne sont pas dans des mots
      mutate(content = str_replace_all(content, "\\s*'\\B|\\B'\\s*", "")) %>%
      # suppression des - qui ne sont pas dans des mots
      mutate(content = str_replace_all(content, "\\s*-\\B|\\B-\\s*", "")) %>%
      # suppression de tout ce qui n'est pas lettre ou ' ou -
      mutate(content = str_replace_all(content, "[^[A-Za-z]']", " ")) %>%
      # suppression des mots de une ou deux lettres
      mutate(content = str_replace_all(content, " *\\b[[:alpha:]]{1,2}\\b *", " ")) %>%
      # transformation en minuscule
      mutate(content = str_to_lower(content)) %>%
      # suppressoin des stop words
      mutate(content = removeWords(content, stopwords())) %>%
      # suppression des espaces en trop
      mutate(content = stripWhitespace(content)) %>%
      # suppression des eventuels NA
      filter(!is.na(content), !is.na(subcategory), !is.na(url)) %>%
      # suppression des factor fantoms
      mutate(subcategory = droplevels(subcategory)) %>%
      # ajout d un identifiant
      mutate(id = row_number()) %>%
      setDT() %>%
      setkey(id)
  )
  print(sprintf('%0.1f', d.art.sc.clean.time/60))
  # d.art.sc.clean$id <- as.character(d.art.sc.clean$id)
  # setkey(d.art.sc.clean, id)
  
  d.art.sc.clean.url <- d.art.sc.clean %>% select(id, url)
  d.art.sc.clean$url <- NULL
  
  d.allresults[[param.cat.name]]$d.art.sc.clean.url <- d.art.sc.clean.url
  d.allresults[[param.cat.name]]$d.art.sc.clean <- d.art.sc.clean
  d.allresults[[param.cat.name]]$d.art.sc.clean.time <- d.art.sc.clean.time
  
  format(object.size(d.art.sc.clean), units = 'auto')
  gain.clean <- sprintf("Gain nettoyage : %0.1f %%", 
                        (100/object.size(d.art.sc))*(as.numeric(object.size(d.art.sc) - object.size(d.art.sc.clean))))
  
  d.allresults[[param.cat.name]]$gain.clean <- gain.clean
  gc()
  
  
  # BENCH -------------------------------------------------------------------
  
  
  #http://text2vec.org/vectorization.html
  if(dobench == TRUE) 
  {
    d.art.sc.bench <- d.art.sc %>%
      mutate(subcategory = droplevels(subcategory)) %>%
      mutate(id = row_number()) %>%
      setDT() %>%
      setkey(id)
    
    # d.allresults[[param.cat.name]]$bench$d.art.sc.bench <- d.art.sc.bench
    
    bench.num_sample = ceiling(param.train_test * dim(d.art.sc.bench)[[1]])
    bench.all_ids = d.art.sc.bench$id
    bench.train_ids = sample(bench.all_ids, bench.num_sample)
    bench.test_ids = setdiff(bench.all_ids, bench.train_ids)
    bench.train = d.art.sc.bench[J(bench.train_ids)] %>% mutate(subcategory = droplevels(subcategory)) %>% setDT() 
    bench.test = d.art.sc.bench[J(bench.test_ids)] %>% mutate(subcategory = droplevels(subcategory)) %>% setDT() 
    
    # d.allresults[[param.cat.name]]$bench$bench.num_sample = bench.num_sample
    # d.allresults[[param.cat.name]]$bench$bench.all_ids = bench.all_ids
    # d.allresults[[param.cat.name]]$bench$bench.train_ids = bench.train_ids
    # d.allresults[[param.cat.name]]$bench$bench.test_ids = bench.test_ids
    # d.allresults[[param.cat.name]]$bench$bench.train = bench.train
    d.allresults[[param.cat.name]]$bench$bench.test = bench.test
    
    bench.train_tokens.time <- system.time(
      bench.train_tokens<-bench.train$content %>% word_tokenizer
    )
    
    # d.allresults[[param.cat.name]]$bench$bench.train_tokens <- bench.train_tokens
    d.allresults[[param.cat.name]]$bench$bench.train_tokens.time <- bench.train_tokens.time
    
    bench.it_train <- itoken(bench.train_tokens, 
                             ids = bench.train$id,
                             progressbar = FALSE)
    
    bench.it_test = bench.test$content %>% 
      word_tokenizer %>%
      itoken(ids = bench.test$id, progressbar = FALSE)
    
    # d.allresults[[param.cat.name]]$bench$bench.it_train <- bench.it_train
    # d.allresults[[param.cat.name]]$bench$bench.it_test <- bench.it_test
    
    bench.train.vocab.time <- system.time(
      bench.train.vocab<-create_vocabulary(bench.it_train)
    )
    
    d.allresults[[param.cat.name]]$bench$bench.train.vocab.time <- bench.train.vocab.time
    d.allresults[[param.cat.name]]$bench$bench.train.vocab <- bench.train.vocab
    
    format(object.size(bench.train.vocab), units = 'auto')
    
    bench.vectorizer <- vocab_vectorizer(bench.train.vocab)
    d.allresults[[param.cat.name]]$bench$bench.vectorizer <- bench.vectorizer
    
    bench.dtm_train.time <- system.time(
      bench.dtm_train<-create_dtm(bench.it_train, bench.vectorizer)
    )
    
    # d.allresults[[param.cat.name]]$bench$bench.dtm_train <- bench.dtm_train
    d.allresults[[param.cat.name]]$bench$bench.dtm_train.time <- bench.dtm_train.time
    
    bench.dtm_test.time <- system.time(
      bench.dtm_test<-create_dtm(bench.it_test, bench.vectorizer)
    )
    
    # d.allresults[[param.cat.name]]$bench$bench.dtm_test <- bench.dtm_test
    # d.allresults[[param.cat.name]]$bench$bench.dtm_test.time <- bench.dtm_test.time
    
    bench.glmnet_classifier.time <- system.time(
      bench.glmnet_classifier<-cv.glmnet(x = bench.dtm_train, y = bench.train[['subcategory']], 
                                           # family = 'binomial',                              
                                           family = 'multinomial', 
                                           type.multinomial="grouped", 
                                           # L1 penalty
                                           alpha = 1,
                                           # ROC curve
                                           type.measure = "auc",
                                           nfolds = bench.glmnet.NFOLDS,
                                           # high value is less accurate, but faster
                                           thresh = bench.glmnet.THRESH,
                                           # slower number of iterations for faster training
                                           maxit = bench.glmnet.MAXIT,
                                           param.doparall.worker = 3)
      
    )
    
    d.allresults[[param.cat.name]]$bench$bench.glmnet_classifier <- bench.glmnet_classifier
    d.allresults[[param.cat.name]]$bench$bench.glmnet_classifier.time <- bench.glmnet_classifier.time
    
    plot(bench.glmnet_classifier)
    print(paste("max AUC =", round(max(bench.glmnet_classifier$cvm), bench.glmnet.NFOLDS)))
    
    bench.glmnet_coef <- coef(bench.glmnet_classifier, s = "lambda.min")
    d.allresults[[param.cat.name]]$bench$bench.glmnet_coef <- bench.glmnet_coef
    
    bench.preds = predict(bench.glmnet_classifier, bench.dtm_test, s = "lambda.min", type = 'response')
    bench.test$bench.preds.class = predict(bench.glmnet_classifier, bench.dtm_test, s = "lambda.min", type = 'class')
    sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[subcategory != bench.preds.class]))/dim(bench.test)[[1]])
    
    d.allresults[[param.cat.name]]$bench$bench.preds <- bench.preds
    d.allresults[[param.cat.name]]$bench$bench.test <- bench.test
    
  }
  
  
  # SAMPLE DATA & TOKEN -----------------------------------------------------
  
  {
    num_sample = ceiling(param.train_test * dim(d.art.sc.clean)[[1]])
    all_ids = d.art.sc.clean$id
    train_ids = sample(all_ids, num_sample)
    test_ids = setdiff(all_ids, train_ids)
    train = d.art.sc.clean[J(train_ids)] %>% mutate(subcategory = droplevels(subcategory)) %>% setDT() 
    test = d.art.sc.clean[J(test_ids)] %>% mutate(subcategory = droplevels(subcategory)) %>% setDT() 
  }
  
  # d.allresults[[param.cat.name]]$model$num_sample = num_sample
  # d.allresults[[param.cat.name]]$model$all_ids = all_ids
  # d.allresults[[param.cat.name]]$model$train_ids = train_ids
  # d.allresults[[param.cat.name]]$model$test_ids = test_ids
  # d.allresults[[param.cat.name]]$model$train = train
  d.allresults[[param.cat.name]]$model$test = test
  
  {
    tokenizer.stem = function(x) {
      tokens = word_tokenizer(x)
      lapply(tokens, SnowballC::wordStem, language="en")
    }
    
    # d.allresults[[param.cat.name]]$model$tokenizer.stem <- tokenizer.stem
    
    # train_tokens.stem.time <- system.time(
    #   train_tokens.stem<-train$content %>% 
    #     tokenizer.stem
    # )
    
    train_tokens.stem.time <- system.time(
      train_tokens.stem<-train$content %>%
        word_tokenizer
    )
    
    # d.allresults[[param.cat.name]]$model$train_tokens.stem <- train_tokens.stem
    d.allresults[[param.cat.name]]$model$train_tokens.stem.time <- train_tokens.stem.time
    
    it_train = itoken(train$content, 
                      tokenizer = word_tokenizer, 
                      ids = train$id, 
                      progressbar = FALSE)
    
    it_test = itoken(test$content, 
                     tokenizer = word_tokenizer, 
                     ids = test$id, 
                     progressbar = FALSE)
    
    it_train.stem = itoken(train_tokens.stem, 
                           ids = train$id,
                           progressbar = FALSE)
    
    it_test.stem = test$content %>% 
      tokenizer.stem %>%
      itoken(ids = test$id, progressbar = FALSE)
    
    d.allresults[[param.cat.name]]$model$it_train.stem <- it_train.stem
    d.allresults[[param.cat.name]]$model$it_test.stem <- it_test.stem
    
  }
  
  
  # VOCAB -------------------------------------------------------------------
  {
    train.vocab = create_vocabulary(it_train)
    
    train.vocab.stem = create_vocabulary(it_train.stem)
    
    format(object.size(train.vocab.stem), units = 'auto')
    
    train.vocab.stem.pruned = prune_vocabulary(train.vocab.stem,
                                               term_count_min = prune.term_count_min,
                                               doc_proportion_max = prune.doc_proportion_max,
                                               doc_proportion_min = prune.doc_proportion_min)
    
    
    
    d.allresults[[param.cat.name]]$model$train.vocab.stem.pruned <- train.vocab.stem.pruned
    
    format(object.size(train.vocab.stem.pruned), units = 'auto')
    
    sprintf("Gain vocab.stem.pruned sur bench.vocab: %0.1f %%", 100*(1 - as.numeric(object.size(train.vocab.stem.pruned)/object.size(train.vocab))))
  }
  
  # VECTORS a DTM -----------------------------------------------------------
  
  {
    vectorizer.stem.pruned = vocab_vectorizer(train.vocab.stem.pruned)
    
    vectorizer.stem.pruned.time <- system.time(
      dtm_train.stem.pruned<-create_dtm(it_train.stem, vectorizer.stem.pruned)
    )
    
    d.allresults[[param.cat.name]]$model$vectorizer.stem.pruned <- vectorizer.stem.pruned
    d.allresults[[param.cat.name]]$model$dtm_train.stem.pruned <- dtm_train.stem.pruned
    d.allresults[[param.cat.name]]$model$vectorizer.stem.pruned.time <- vectorizer.stem.pruned.time
    
    # SDE non utilis? ?
    # dtm_test.stem.pruned = create_dtm(it_test.stem, vectorizer.stem.pruned)
    
    # d.allresults[[param.cat.name]]$model$dtm_test.stem.pruned <- dtm_test.stem.pruned
    
  }
  
  # N-GRAMS -----------------------------------------------------------------
  if(dongram == TRUE)
  {
    train.vocab.ngram.time <- system.time(
      train.vocab.ngram<- create_vocabulary(it_train, ngram = c(1L, 2L))
    )
    
    train.vocab.ngram.prune = train.vocab.ngram %>% 
      prune_vocabulary(term_count_min = 20,
                       doc_proportion_max = 0.7)
    
    # train.vocab.ngram.prune <-train.vocab.ngram
    
    bigram_vectorizer.ngram.prune = vocab_vectorizer(train.vocab.ngram.prune)
    
    dtm_train.ngram.prune.time <- system.time(
      dtm_train.ngram.prune<-create_dtm(it_train, bigram_vectorizer.ngram.prune)
    )
    
    dtm_test.ngram.prune.time <- system.time(
      dtm_test.ngram.prune<-create_dtm(it_test, bigram_vectorizer.ngram.prune)
    )
    
    glmnet_classifier.ngram.prune.time <- system.time(
      glmnet_classifier.ngram.prune<-cv.glmnet(x = dtm_train.ngram.prune, 
                                                 y = train[['subcategory']], 
                                                 # family = 'binomial',                              
                                                 family = 'multinomial', 
                                                 type.multinomial="grouped", 
                                                 # L1 penalty
                                                 alpha = 1,
                                                 type.measure = "auc",
                                                 nfolds = glmnet.NFOLDS,
                                                 thresh = glmnet.THRESH,
                                                 maxit = glmnet.MAXIT)
    )
    
    plot(glmnet_classifier.ngram.prune)
    print(paste("max AUC =", round(max(glmnet_classifier.ngram.prune$cvm), glmnet.NFOLDS)))
    
    preds.ngram.prune = predict(
      glmnet_classifier.ngram.prune, 
      dtm_test.ngram.prune, 
      s = "lambda.min", type = 'response')
    
    d.allresults[[param.cat.name]]$model$preds.ngram.prune <- preds.ngram.prune
    
    test$preds.class.ngram.prune = predict(
      glmnet_classifier.ngram.prune, 
      dtm_test.ngram.prune, 
      s = "lambda.min", type = 'class')
    
    d.allresults[[param.cat.name]]$test$preds.class.ngram.prune <- test$preds.class.ngram.prune
    
    sprintf("Accuracy : %0.2f %%", 
            100*(dim(test)[[1]] - count(test[subcategory != preds.class.ngram.prune]))/dim(test)[[1]])
    # "Accuracy : 89.92 %"
  }
  
  # GLM avec STEM -> PRUNED -> TFIDF ----------------------------------------
  
  
  # define tfidf model
  tfidf = TfIdf$new()
  
  # fit model to train data and transform train data with fitted model
  dtm_train_tfidf.stem.pruned.time <- system.time(
    dtm_train_tfidf.stem.pruned<- fit_transform(dtm_train.stem.pruned, tfidf)
  )
  
  d.allresults[[param.cat.name]]$model$dtm_train_tfidf.stem.pruned <- dtm_train_tfidf.stem.pruned
  d.allresults[[param.cat.name]]$model$dtm_train_tfidf.stem.pruned.time <- dtm_train_tfidf.stem.pruned.time
  
  # tfidf modified by fit_transform() call!
  # apply pre-trained tf-idf transformation to test data
  dtm_test_tfidf.stem.pruned.time <- system.time(
    dtm_test_tfidf.stem.pruned <-create_dtm(it_test.stem, vectorizer.stem.pruned) %>% 
      transform(tfidf)
  )
  
  d.allresults[[param.cat.name]]$model$dtm_test_tfidf.stem.pruned <- dtm_test_tfidf.stem.pruned
  d.allresults[[param.cat.name]]$model$dtm_test_tfidf.stem.pruned.time <- dtm_test_tfidf.stem.pruned.time
  
  gc()
  
  print('glmnet_classifier.tfidf.stem.pruned...')
  glmnet_classifier.tfidf.stem.pruned.time <- system.time(
    glmnet_classifier.tfidf.stem.pruned<-cv.glmnet(x = dtm_train_tfidf.stem.pruned, y = train[['subcategory']], 
                                                     # family = 'binomial',                              
                                                     family = 'multinomial', 
                                                     type.multinomial="grouped", 
                                                     # L1 penalty
                                                     alpha = 1,
                                                     # ROC curve
                                                     type.measure = "auc",
                                                     nfolds = glmnet.NFOLDS ,
                                                     # high value is less accurate, but has faster
                                                     thresh = glmnet.THRESH,
                                                     # slower number of iterations for faster training
                                                     maxit = glmnet.MAXIT
    )
  )
  print(sprintf('%0.1f', glmnet_classifier.tfidf.stem.pruned.time/60))
  # utilisateur     syst?me      ?coul? 
  # 209.53        2.34      213.73 
  
  # sans thresh et maxit !
  # utilisateur     syst?me      ?coul? 
  # 1309.89        2.08     1323.53 
  
  # Technology
  # utilisateur     syst?me      ?coul? 
  # 1129.41       24.40     1165.28 
  
  d.allresults[[param.cat.name]]$model$glmnet_classifier.tfidf.stem.pruned <- glmnet_classifier.tfidf.stem.pruned
  d.allresults[[param.cat.name]]$model$glmnet_classifier.tfidf.stem.pruned.time <- glmnet_classifier.tfidf.stem.pruned.time
  
  glmnet_coef.tfidf.stem.pruned <- coef(glmnet_classifier.tfidf.stem.pruned, s = "lambda.min")
  
  d.allresults[[param.cat.name]]$model$glmnet_coef.tfidf.stem.pruned <- glmnet_coef.tfidf.stem.pruned
  
  plot(glmnet_classifier.tfidf.stem.pruned)
  # print(paste("max AUC =", round(max(glmnet_classifier.tfidf.stem.pruned$cvm), glmnet.NFOLDS)))
  #"max AUC = 2.2306"
  # "max AUC = 4.5699451243"
  
  preds.tfidf.stem.pruned = predict(
    glmnet_classifier.tfidf.stem.pruned, 
    dtm_test_tfidf.stem.pruned, 
    s = "lambda.min", type = 'response')
  
  d.allresults[[param.cat.name]]$model$preds.tfidf.stem.pruned <- preds.tfidf.stem.pruned
  
  test$preds.class.tfidf.stem.pruned = predict(
    glmnet_classifier.tfidf.stem.pruned, 
    dtm_test_tfidf.stem.pruned, 
    s = "lambda.min", type = 'class')
  
  d.allresults[[param.cat.name]]$model$test$preds.class.tfidf.stem.pruned <- test$preds.class.tfidf.stem.pruned
  
  # sprintf("Accuracy : %0.2f %%", 
  #         100*(dim(test)[[1]] - count(test[subcategory != preds.class.tfidf.stem.pruned]))/dim(test)[[1]])
  # "Accuracy : 91.13 %"
  # "Accuracy : 79.35 %" pour Technology
  
  print(sprintf("Accuracy : %0.2f %%", 
          100*count(test[subcategory == preds.class.tfidf.stem.pruned])/dim(test)[[1]]))
  
  print(test %>% 
    mutate(accurate = ifelse(subcategory == preds.class.tfidf.stem.pruned, 1, 0)) %>%
    group_by(subcategory) %>%
    summarise(n = n(), 
              pct = 100*n/dim(test)[[1]], 
              accurate = sum(accurate),  
              accuracy = (100*accurate/n)) %>%
    arrange(-accuracy))
  
  train %>%
    group_by(subcategory) %>%
    summarise(n = n(),
              pct = 100*n/dim(test)[[1]]) %>%
    arrange(-pct)
  
  total.time <- difftime(Sys.time(), t0, units = 'sec')
  print(total.time)
  
  d.allresults[[param.cat.name]]$total.time <- total.time
  gc()
}

# ANALYSES ----------------------------------------------------------------

# #terms count
# train.vocab.stem.pruned$vocab %>% 
#   arrange(-terms_counts) %>% 
#   head(100) %>% 
#   with(wordcloud(terms, 
#                  terms_counts, 
#                  max.words = 50, 
#                  random.order = FALSE, 
#                  colors=brewer.pal(8,"Paired"))
#   )
# 
# #doc count
# train.vocab.stem.pruned$vocab %>% 
#   arrange(-doc_counts) %>% 
#   head(100) %>% 
#   with(wordcloud(terms, 
#                  doc_counts, 
#                  max.words = 50, 
#                  random.order = FALSE, 
#                  colors=brewer.pal(11,"PiYG")))

#
# glmnet.coef <- coef(bench.glmnet_classifier, s='lambda.min')
# glmnet.coef.cat <- glmnet.coef[['Genetics']]
# glmnet.coef.cat.terms <- rownames(glmnet.coef.cat)[which(glmnet.coef.cat != 0)]
# glmnet.coef.cat.terms.coef <- glmnet.coef.cat[which(glmnet.coef.cat != 0)]
# glmnet.df <- data.frame(name = glmnet.coef.cat.terms, coefficient = glmnet.coef.cat.terms.coef)

# http://stackoverflow.com/questions/27801130/extracting-coefficient-variable-names-from-glmnet-into-a-data-frame
#coef(cv.glmnet.fit, s = "lambda.min")[which(coef(cv.glmnet.fit, s = "lambda.min") != 0)]
#colnames(regression_data)[which(coef(cv.glmnet.fit, s = "lambda.min") != 0)]

# tmp_coeffs <- coef(bench.glmnet_classifier, s = "lambda.min")
# tmp_df <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i], coefficient = tmp_coeffs@x)



names(d.allresults)
# [1] "Astronomy & Space" "Biology"           "Chemistry"         "Earth"            
# [5] "Health"            "Nanotechnology"    "Other Sciences"    "Physics"          
# [9] "Technology" 
param.cat <- "Astronomy & Space"

for (i in 1:length(allcat))  {

  param.cat = allcat[[i]]

  cat.name <- param.cat %>%
    str_replace_all("[^[A-Za-z]']", "_") %>%
    str_replace_all("_+", "_")

  print(paste("Categorie:", param.cat))

  cat.test <- d.allresults[[cat.name]]$model$test 
  glmnet_classifier.tfidf.stem.pruned <- d.allresults[[cat.name]]$model$glmnet_classifier.tfidf.stem.pruned
    
  accuracy_str <- sprintf("Accuracy : %0.2f %%",
                          100*count(cat.test[subcategory == preds.class.tfidf.stem.pruned])/dim(cat.test)[[1]])
  
  x_loglambda_pos <- min(log(glmnet_classifier.tfidf.stem.pruned$lambda))  
  y_deviance_pos <- max(glmnet_classifier.tfidf.stem.pruned$cvm)
  
  

  res <- cat.test %>%
    mutate(accurate = ifelse(subcategory == preds.class.tfidf.stem.pruned, 1, 0)) %>%
    group_by(subcategory) %>%
    summarise(n = n(),
              pct = 100*n/dim(test)[[1]],
              accurate = sum(accurate),
              accuracy = (100*accurate/n)) %>%
    arrange(-accuracy)
  
  plot(glmnet_classifier.tfidf.stem.pruned)
  title(param.cat, line = 3)
  text(x_loglambda_pos + 0.7, y_deviance_pos, labels = accuracy_str)
  
  print(accuracy_str)
  print(res)
}
