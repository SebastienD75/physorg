
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
  library(textstem)
}

# LOAD DATA ---------------------------------------------------------------

{
  param.dataorg.file <- 'data/physorg.RData'
  param.clean_content.file <- 'data/glmnet_cleancontent_catsubcat.stemmatized.RData'
  
  if(!file.exists(param.clean_content.file)) {
    load(param.dataorg.file)
    rm(list = setdiff(ls(), c('d.art')))
    d.art.sc.clean.time <- system.time(
      d.art.c.bench <- d.art %>%
        select(url, content, category, subcategory) %>%
        # suppression des ' qui ne sont pas dans des mots
        mutate(content = str_replace_all(content, "\\s*'\\B|\\B'\\s*", "")) %>%
        # suppression des - qui ne sont pas dans des mots
        mutate(content = str_replace_all(content, "\\s*-\\B|\\B-\\s*", "")) %>%
        # suppression de tout ce qui n'est pas lettre ou ' ou - remplaces par espace
        mutate(content = str_replace_all(content, "[^[A-Za-z]']", " ")) %>%
        # suppression des mots de une ou deux lettres remplaces par espace
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
    
    d.art.c.bench$content <- d.art.c.bench$content %>%
        lemmatize_strings()
      
    # d.art.sc.clean$id <- as.character(d.art.sc.clean$id)
    # setkey(d.art.sc.clean, id)
    d.art.c.bench.url <- d.art.c.bench %>% select(id, url)
    d.art.c.bench$url <- NULL
  } else {
    load(param.clean_content.file)
  }
  
  protected.obj <- c("protected.obj", "bench.models", "d.art.c.bench")
  rm(list = setdiff(ls(), protected.obj))
  param.cleaninloop = c('d.bench', 'd.art.c.bench')
  gc()
}


# PARAMS ------------------------------------------------------------------
{
  
  param.mutate.subcat = TRUE
  
  param.doparall.worker = 7
  param.train_test <- 0.7
  param.seed = 20170416
  
  param.doprune = TRUE
  param.dostem = FALSE
  param.dongram = TRUE
  param.dofeaturehashing = FALSE # incompatible avec prune
  
  param.cat <- c('Astronomy & Space','Other Sciences','Technology','Physics', 'Nanotechnology','Health', 'Biology', 'Earth','Chemistry')
  param.mutate.subcat.cat <- c('Health')
  param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                    'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                    'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                    'Materials Science')
  
  param.startmodel.pctdata = 1
  param.pctdata.default = 1
  param.maxmodel.pctdata = 1
  param.pctdata.inc <- c(param.pctdata.default, 0.005, 0.01, 0.03, 0.09, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

  param.startmodel.cv.nfold <- 1
  param.cv.nfold.default = 3
  param.maxmodel.cv.nfold <- 1
  param.cv.nfold.inc <- c(param.cv.nfold.default,4:10)
  param.bench.glmnet.THRESH.default = 1e-2 # best = 1e-2 (default 1E-7)
  param.bench.glmnet.MAXIT.default =  1e2 # best = 1e2 (default 10^5)
 
  param.startmodel.prune = 2
  param.maxmodel.prune = 100
  param.prune.term_count_min.default = 80 # 80 # 40 # (default pkg 1)
  param.prune.doc_proportion_max.default = 0.8 # 0.8 # 0.4 # (default pkg 1)
  param.prune.doc_proportion_min.default = 0.0008 # 0.002 # 0.0008 # (default pkg 0)
  #param.prune.doc_proportion_min.default = # (default Inf)
  param.prune.inc <- list(term_count_min.inc = c(param.prune.term_count_min.default, ceiling(exp(seq(log(20),log(140), length.out = 40)))),
                          doc_proportion_max.inc = c(param.prune.doc_proportion_max.default, ceiling(exp(seq(log(9), log(1), length.out = 20)))/10),
                          doc_proportion_min.inc = c(param.prune.doc_proportion_min.default, ceiling(exp(seq(log(1), log(100), length.out = 20)))/100000)
  )
  
  param.hngram = 2 ** 18
}


# FUNCTIONS  -------------------------------------------------------------------
{
  save_model <- function(model_name) 
  {
    bench.models[[model_name]]$model_name <<- model_name
    bench.models[[model_name]]$model_num <<- model_num
    bench.models[[model_name]]$mode_desc <<- mode_desc
    
    bench.models[[model_name]]$param.bench.glmnet.NFOLDS <<- param.bench.glmnet.NFOLDS
    bench.models[[model_name]]$param.bench.glmnet.THRESH <<- param.bench.glmnet.THRESH
    bench.models[[model_name]]$param.bench.glmnet.MAXIT <<- param.bench.glmnet.MAXIT
    
    
    bench.models[[model_name]]$param.dostem <<- param.dostem
    bench.models[[model_name]]$param.dofeaturehashing <<- param.dofeaturehashing
    bench.models[[model_name]]$param.dongram <<- param.dongram
    bench.models[[model_name]]$param.doprune <<- param.doprune
    bench.models[[model_name]]$param.prune.term_count_min <<- param.prune.term_count_min
    bench.models[[model_name]]$param.prune.doc_proportion_max <<- param.prune.doc_proportion_max
    bench.models[[model_name]]$param.prune.doc_proportion_min <<- param.prune.doc_proportion_min
    
    bench.models[[model_name]]$param.pctdata <<- param.pctdata
    bench.models[[model_name]]$param.train_test <<- param.train_test
    bench.models[[model_name]]$param.dorpsc <<- param.dorpsc
    
    bench.models[[model_name]]$bench.train_tokens.time <<- bench.train_tokens.time
    bench.models[[model_name]]$bench.dtm_train.time <<- bench.dtm_train.time
    bench.models[[model_name]]$bench.dtm_test.time <<- bench.dtm_test.time
    bench.models[[model_name]]$bench.dtm_train.dim <<- dim(bench.dtm_train)
    bench.models[[model_name]]$bench.dtm_test.dim <<- dim(bench.dtm_test)
    bench.models[[model_name]]$bench.vectorizer <<- bench.vectorizer
    bench.models[[model_name]]$bench.glmnet_classifier.time <<- bench.glmnet_classifier.time
    bench.models[[model_name]]$bench.glmnet_classifier <<- bench.glmnet_classifier
    bench.models[[model_name]]$bench.preds.class.time <<- bench.preds.class.time
    bench.models[[model_name]]$bench.glmnet_classifier.accuracy <<- bench.glmnet_classifier.accuracy
    bench.models[[model_name]]$bench.glmnet_classifier.accuracy_cat <<- res
    
    print(sprintf("Model %s saved (%0.1f min): pct data=%s, dim_train=(%d, %d), %s", 
                  model_name, 
                  bench.glmnet_classifier.time[[3]]/60,
                  bench.models[[model_name]]$param.pctdata,
                  bench.models[[model_name]]$bench.dtm_train.dim[[1]],
                  bench.models[[model_name]]$bench.dtm_train.dim[[2]],
                  bench.models[[model_name]]$bench.glmnet_classifier.accuracy))
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

# INIT -------------------------------------------------------------------

{
  set.seed(param.seed)
  registerDoParallel(param.doparall.worker)
  
  if(!exists('bench.models')) {
    bench.models <- list()
  }
  
  if(param.mutate.subcat) {   
    param.cat <- param.mutate.subcat.cat
  }
  
  d.art.c.bench <- d.art.c.bench %>%
    filter(category %in% param.cat) %>%
    filter(!(subcategory %in% param.dorpsc)) %>%
    mutate(category = droplevels(category)) %>%
    mutate(subcategory = droplevels(subcategory)) %>%
    setDT() %>%
    setkey(id)
  
  param.maxmodel.pctdata = min(c(length(param.pctdata.inc), param.startmodel.pctdata + param.maxmodel.pctdata - 1))
  param.maxmodel.cv.nfold =  min(c(length(param.cv.nfold.inc), param.startmodel.cv.nfold + param.maxmodel.cv.nfold - 1))
  param.maxmodel.prune = min(c(length(param.prune.inc[[1]]), param.startmodel.prune + param.maxmodel.prune - 1))
  
  if(param.mutate.subcat) {
    d.art.c.bench.allcat <- d.art.c.bench
  }
}


# START BENCH -------------------------------------------------------------


for (i_cat in 1:ifelse(!param.mutate.subcat,1,length(param.cat))) 
{
  
  if(param.mutate.subcat) {
    d.art.c.bench <- d.art.c.bench.allcat %>%
      filter(category == param.cat[i_cat]) %>%
      select(-category) %>%
      mutate(category = subcategory) %>%
      select(-subcategory) %>%
      mutate(category = droplevels(category)) %>%
      setDT() %>%
      setkey(id)
  }
  
  cat('\n','------------------------------------')
  cat('\n','Categories to learn :\n')
  cat(paste0('<',levels(d.art.c.bench$category), '>'))
  
  for(i in param.startmodel.pctdata:param.maxmodel.pctdata)
  {
    param.pctdata <<- param.pctdata.inc[[i]]
    cat('\n\n',sprintf("[%d/%d]Testing model with param.pctdata = %s", i, param.maxmodel.pctdata, param.pctdata), '\n')
    
    
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
    
    if(length(param.cleaninloop) != 0 && param.startmodel.pctdata == param.maxmodel.pctdata) {
      rm(list = param.cleaninloop)  
    }
    gc()
    
    print(paste("Train nb articles =", dim(bench.train)[[1]]))
    
    tokenizer.stem = function(x) {
      tokens = word_tokenizer(x)
      lapply(tokens, SnowballC::wordStem, language="en")
    }
    
    if(param.dostem) {
      
      bench.train_tokens.time <- system.time(
        bench.train_tokens <- bench.train$content %>% tokenizer.stem
      ); print(sprintf('bench.train_tokens.time: %0.2fs', bench.train_tokens.time[[3]]))
      
    } else {
      
      bench.train_tokens.time <- system.time(
        bench.train_tokens <- bench.train$content %>% word_tokenizer
      ); print(sprintf('bench.train_tokens.time: %0.2fs', bench.train_tokens.time[[3]]))
      
    }
    
    bench.it_train <- itoken(bench.train_tokens, 
                             ids = bench.train$id,
                             progressbar = FALSE)
    
    bench.it_test <- bench.test$content %>% 
      word_tokenizer %>%
      itoken(ids = bench.test$id, progressbar = FALSE)
    
    if(param.dofeaturehashing) {
      param.doprune = FALSE
    }
    
    if(!param.doprune) {
      param.maxmodel.prune = 1
      param.startmodel.prune = 1
    }; for(i_prune in param.startmodel.prune:param.maxmodel.prune)
    {
      
      t0 = Sys.time()
      param.prune.term_count_min <<- param.prune.inc$term_count_min.inc[[i_prune]]
      param.prune.doc_proportion_max <<- param.prune.inc$doc_proportion_max.inc[[1]]#i_prune]]
      param.prune.doc_proportion_min <<- param.prune.inc$doc_proportion_min.inc[[1]]#i_prune]]

      if(param.dofeaturehashing) {
        
        bench.h_vectorizer = hash_vectorizer(hash_size = param.hngram, ngram = c(1L, 2L))
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
          ); print(sprintf('Do ngram : bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
          
        } else {
          
          bench.train.vocab.stem.time <- system.time(
            bench.train.vocab.stem <- create_vocabulary(bench.it_train)
          ); print(sprintf('bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
          
        }
        
        if(param.doprune) {
          
          cat(sprintf("[%d/%d]Testing model with prune = (count_min=%s, prop_max=%s, prop_min=%s)", 
                      i_prune, param.maxmodel.prune, param.prune.term_count_min, param.prune.doc_proportion_max, param.prune.doc_proportion_min), '\n')
          
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
      
      for(i_nfold in param.startmodel.cv.nfold:param.maxmodel.cv.nfold)
      {
        param.bench.glmnet.NFOLDS <<- param.cv.nfold.inc[[i_nfold]]
        cat(sprintf("[%d/%d]Training model with param.cv.nfold = %d", i_nfold, param.maxmodel.cv.nfold, param.bench.glmnet.NFOLDS), '\n')
        
        param.bench.glmnet.THRESH <- param.bench.glmnet.THRESH.default
        param.bench.glmnet.MAXIT <- param.bench.glmnet.MAXIT.default
        model_name <- paste0(as.character(i), as.character(length(bench.models) + 1), as.character(i_nfold))
        model_num <- as.numeric(model_name)
        
        mode_desc <- sprintf('model %d - text2vect tfidf cv.glmnet : glmnet.params = ALPHA:1, NFOLDS:%d, THRESH:%s, MAXIT:%s + featureh=%s, stem=%s, ngram=%s, prune=%s :  prune.params = countmin:%s, doc.prop.max:%s, doc.prop.min:%s', 
                             model_num,
                             param.bench.glmnet.NFOLDS,
                             param.bench.glmnet.THRESH,
                             param.bench.glmnet.MAXIT,
                             param.dofeaturehashing,
                             param.dostem,
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
        
        gc()
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
          # mutate(words = sapply(gregexpr("[[:alpha:]]+", content), function(x) sum(x > 0))) %>%
          summarise(n = n(),
                    pct = 100*n/dim(bench.test)[[1]],
                    # words = sum(words),
                    accurate = sum(accurate),
                    accuracy = (100*accurate/n)) %>%
          # select(category, n, pct, words, accuracy) %>%
          select(category, n, pct, accuracy) %>%
          arrange(-accuracy)
        
        print(res)
        
        save_model(model_name)
        
        gc()
      }
      print(difftime(Sys.time(), t0, units = 'mins'))
    }
  }
}
