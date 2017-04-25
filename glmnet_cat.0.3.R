
#setwd("~/Dev/R - Phys.org")

{
  library(dplyr)
  library(tidytext)
  library(data.table)
  library(stringr)
  library(glmnet)
  library(wordcloud)
  require(RColorBrewer)
  library(text2vec)
  library(SnowballC)
  library(doParallel)
  # library(textstem)
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
  ## -- COMPUTEUR SPECIFICS --
  param.doparall.worker = 3
  
  ## -- PIPLINE --
  param.doprune = TRUE
  param.dostem = FALSE
  param.dongram = TRUE
  param.dofeaturehashing = FALSE # incompatible avec prune
  
  ## -- CAT / SUB CAT --
  param.mutate.subcat = TRUE
  param.cat <- c('Astronomy & Space','Other Sciences','Technology','Physics', 'Nanotechnology','Health', 'Biology', 'Earth','Chemistry')
  param.mutate.subcat.cat <- c('Physics')
  param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                    'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                    'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                    'Materials Science')
  
  ## -- PCT USED DATA 
  param.pctdata.default = 1
  param.startmodel.pctdata = 1
  param.maxmodel.pctdata = 1
  param.pctdata.inc <- c(param.pctdata.default, 0.005, 0.01, 0.03, 0.09, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  param.train_test <- 0.7
  
  
  ## -- NFOLD --
  param.cv.nfold.default = 3
  param.startmodel.cv.nfold <- 1
  param.maxmodel.cv.nfold <- 1
  param.cv.nfold.inc <- c(param.cv.nfold.default,4:10)
  param.bench.glmnet.THRESH.default = 1e-2 # best = 1e-2 (default 1E-7)
  param.bench.glmnet.MAXIT.default =  1e2 # best = 1e2 (default 10^5)
 
  
  ## -- PRUNE --
  param.prune.term_count_min.default = 150 # 80 # 40 # (default pkg 1)
  param.prune.doc_proportion_max.default = 0.9 # 0.8 # 0.4 # (default pkg 1)
  param.prune.doc_proportion_min.default = 0 # 0.002 # 0.0008 # (default pkg 0)
  #param.prune.doc_proportion_min.default = # (default Inf)
  param.startmodel.prune = 1
  param.maxmodel.prune = 1
  param.prune.inc <- list(term_count_min.inc = c(param.prune.term_count_min.default, ceiling(exp(seq(log(20),log(300), length.out = 30)))),
                          doc_proportion_max.inc = c(param.prune.doc_proportion_max.default, ceiling(exp(seq(log(9), log(1), length.out = 20)))/10),
                          doc_proportion_min.inc = c(param.prune.doc_proportion_min.default, ceiling(exp(seq(log(1), log(100), length.out = 20)))/100000)
  )
  
  param.hngram = 2 ** 18
  param.seed = 20170416
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
  ## TODO enlever la valeur 1  trouver un meilleur moyen
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
      ## TODO les trois valeurs changent en meme temps: faire des boucles specifiques
      ## (pout l'instant fixer manuellement à i_prune = 1 les valeurs que l'on ne veux pas faire bouger)
      param.prune.term_count_min <<- param.prune.inc$term_count_min.inc[[i_prune]]
      param.prune.doc_proportion_max <<- param.prune.inc$doc_proportion_max.inc[[i_prune]]
      param.prune.doc_proportion_min <<- param.prune.inc$doc_proportion_min.inc[[i_prune]]

      t0 = Sys.time()
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

# ----------------- REF param.mutate.subcat.cat <- c('Physics')
#   Categories to learn : 
#   <Condensed Matter> <General Physics> <Optics & Photonics> <Plasma Physics> <Quantum Physics> <Soft Matter> <Superconductivity>
#   
#   [1/1]Testing model with param.pctdata = 1 
#   [1] "Train nb articles = 7808"
#   [1] "bench.train_tokens.time: 3.62s"
#   [1] "Do ngram : bench.train.vocab.stem.time: 4.57s"
#   [1/1]Testing model with prune = (count_min=150, prop_max=0.9, prop_min=0) 
#   [1] "bench.train.vocab.stem.prune.time: 0.27s"
#   [1] "bench.dtm_train.time: 2.23s"
#   [1] "bench.dtm_test: 0.97s"
#   [1/1]Training model with param.cv.nfold = 3 
#   model 111 - text2vect tfidf cv.glmnet : glmnet.params = ALPHA:1, NFOLDS:3, THRESH:0.01, MAXIT:100 + featureh=FALSE, stem=FALSE, ngram=TRUE, prune=TRUE :  prune.params = countmin:150, doc.prop.max:0.9, doc.prop.min:0 
#   [1] "bench.glmnet_classifier.tfidf.time: 0.38m"
#   [1] "bench.preds.class: 0.08s"
#   [1] "Accuracy : 80.03 %"
#   # A tibble: 7 × 4
#   category     n       pct  accuracy
#   <fctr> <int>     <dbl>     <dbl>
#     1    General Physics  1857 55.515695 89.553043
#   2  Superconductivity   108  3.228700 79.629630
#   3    Quantum Physics   397 11.868460 77.078086
#   4 Optics & Photonics   454 13.572496 71.145374
#   5     Plasma Physics    84  2.511211 60.714286
#   6   Condensed Matter   409 12.227205 60.146699
#   7        Soft Matter    36  1.076233  5.555556
#   [1] "Model 111 saved (0.4 min): pct data=1, dim_train=(7808, 2926), Accuracy : 80.03 %"
#   Time difference of 0.5773865 mins

## --------------- Autres models possibles --------------- 


# param.mutate.subcat.cat <- c('Physics')
# --------------- naiveBayes "Accuracy : 55.25 %", Time difference of 2.209617 mins

if(param.bench.naivebayes)
{
  library(e1071)
  t0 <- Sys.time()
  
  bench.naivebayes_classifier <- naiveBayes(x = as.matrix(bench.dtm_train.tfidf),
                                            y = bench.train[['category']])
  
  bench.test$bench.naivebayes.preds.class <-  predict(bench.naivebayes_classifier, as.matrix(bench.dtm_test.tfidf))
  head(bench.test$bench.naivebayes.preds.class)
  bench.naivebayesclassifier.accuracy <- sprintf("Accuracy : %0.2f %%", 
                                                 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.naivebayes.preds.class]))/dim(bench.test)[[1]])
  
  
  tend <- Sys.time()
  print(bench.naivebayesclassifier.accuracy)
  print(difftime(tend, to, units = 'mins'))
}


# param.mutate.subcat.cat <- c('Physics')
# --------------- xgboost : 
# 5/200   [1] "Accuracy : 80.03 %" Time difference of 2.574376 mins
# 5/300   [1] "Accuracy : 80.45 %" Time difference of 3.659111 mins
# 5/500   [1] "Accuracy : 80.75 %" Time difference of 5.820309 mins
# 6.600   [1] "Accuracy : 80.69 %" Time difference of 7.327472 mins
# 3/1000  [1] "Accuracy : 80.60 %" : Time difference of 7.901743 mins

## - https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html
## - https://gist.github.com/dkincaid/87f0fbeb912cf23816c340b4fbe30baa
## - https://www.analyticsvidhya.com/blog/2015/12/kaggle-solution-cooking-text-mining-competition/
## - https://www.analyticsvidhya.com/blog/2015/12/kaggle-solution-cooking-text-mining-competition/
## - https://www.r-bloggers.com/an-introduction-to-xgboost-r-package/
## - https://www.kaggle.com/tqchen/otto-group-product-classification-challenge/understanding-xgboost-model-on-otto-data

if(param.bench.xgboost)
{
  library(xgboost)
  library(igraph)
  library(ggplot2)
  t0 <- Sys.time()
  
  xgb_params = list(
    objective = "multi:softmax",
    num_class = length(levels(bench.train$category)) + 1,
    eta = 0.1,
    max.depth = 5,
    eval_metric = "mlogloss")
  
  bench.xgboost_classifier.trainmatrix <- xgb.DMatrix(bench.dtm_train.tfidf, label = bench.train[['category']])
  
  # Analyse exploratoire preliminaire
  # cv.nfold <- 5
  # xgb.nround <- 5
  # bench.xgboost.cv = xgb.cv(param=xgb_params, data = bench.xgboost_classifier.trainmatrix, nfold = cv.nfold, nrounds = xgb.nround)
  
  xgb_params$max.depth <- 5
  xgb.nround <- 490
  bench.xgboost_classifier <- xgboost(data = bench.xgboost_classifier.trainmatrix, params = xgb_params, nrounds = xgb.nround)
  
  # Check the feature importance
  importance_vars <- xgb.importance(model=bench.xgboost_classifier, feature_names = colnames(bench.xgboost_classifier.trainmatrix))
  head(importance_vars, 20)
  
  bench.xgboost.preds <- predict(bench.xgboost_classifier, newdata = bench.dtm_test.tfidf, type = 'class')
  bench.test$bench.xgboost.preds <- levels(bench.train$category)[bench.xgboost.preds]
  bench.xgboost_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.xgboost.preds]))/dim(bench.test)[[1]])

  xgb.importance_matrix <- xgb.importance(bench.dtm_train.tfidf@Dimnames[[2]], model = bench.xgboost_classifier)
  head(xgb.importance_matrix, 20)
  
  # xgb.ggplot.importance(head(xgb.importance_matrix,20))
  # xgb.ggplot.deepness(model = bench.xgboost_classifier)
  
  tend <- Sys.time()
  print(bench.xgboost_classifier.accuracy)
  print(difftime(tend, t0, units = 'mins'))
}



# --------------- SVM kernlab, OK moins bon resultats

# library(kernlab)
# t0 = Sys.time(); bench.ksvmclass_classifier <- ksvm(x = as.matrix(bench.dtm_train.tfidf), 
#                                                     y = as.vector(bench.train[['category']]),
#                                                     C= 10, # 3, # Physics => "Accuracy : 65.05 %" VS glmnet => "Accuracy : 80.03 %"
#                                                     prob.model=TRUE
# ); t1 = Sys.time()
# 
# print(difftime(t1, t0, units = 'mins'))
# 
# ksvm.predict <- predict(bench.ksvmclass_classifier, newdata = bench.dtm_test.tfidf, type = 'probabilities')
# 
# ksvm.predict.gather_prob <- as.data.table(ksvm.predict) %>% 
#   mutate(id = row_number()) %>% 
#   gather(category, prob, -id) %>%
#   group_by(id) %>% 
#   mutate(better_prob = max(prob)) %>% 
#   filter(prob == better_prob) %>% 
#   select(id, category, better_prob) %>% 
#   arrange(id) %>% 
#   setDT()
# 
# bench.test$ksvm.predict.class <- ksvm.predict.gather_prob$category
# bench.test$ksvm.predict.prob <- ksvm.predict.gather_prob$better_prob
# 
# 
# #tst[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
# # ksvm.predict.bool <- as.data.table(ksvm.predict)
# # ksvm.predict.bool[, (names(ksvm.predict.bool)) := lapply(.SD, function(x) ifelse(x>=0.5,1,0))]
# # bench.test$ksvm.predict.class <- ksvm.predict.bool %>% 
# #   gather(category,boolval) %>% 
# #   filter(boolval == 1) %>% 
# #   select(category)
# 
# bench.ksvm_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != ksvm.predict.class]))/dim(bench.test)[[1]])
# print(bench.ksvm_classifier.accuracy)
# 
# 
# 


# --------------- SVM e1071, KO memory et predict

# 
# # library(e1071)
# # dt.bench.dtm_train.tfidf <- as.data.frame(cbind(as.matrix(bench.dtm_train.tfidf), as.vector(bench.train[['category']])))
# # colnames(dt.bench.dtm_train.tfidf)[dim(dt.bench.dtm_train.tfidf)[2]] <- 'category'
# # bench.svm <- e1071::svm(category ~ ., dt.bench.dtm_train.tfidf) # Error: cannot allocate vector of size 50.0 Gb
# # bench.svm <- e1071::svm(as.factor(bench.train[['category']]) ~ as.matrix(bench.dtm_train.tfidf))
# # svm.predict <- predict(bench.svm, newdata = as.matrix(bench.dtm_test.tfidf))
# # KO, predict pas bon à cause de la formule => http://stackoverflow.com/questions/4462118/predict-svm-does-not-predict-new-data
# # bench.test$svm.predict.class <- svm.predict
# # 
# # dt.bench.dtm_train.tfidf <- as.data.frame(cbind(as.matrix(bench.dtm_train.tfidf), as.vector(bench.train[['category']])))
# # colnames(dt.bench.dtm_train.tfidf)[dim(dt.bench.dtm_train.tfidf)[2]] <- 'category'
# # # KO : Error: cannot allocate vector of size 50.0 Gb
# # bench.svm <- e1071::svm(category ~ ., data = dt.bench.dtm_train.tfidf)
# 

# --------------- SVM liquidSVM, KO data

## - http://www.isa.uni-stuttgart.de/software/R/demo.html

# library(liquidSVM)

# KO no data
# model <- liquidSVM::mcSVM(x = as.matrix(bench.dtm_train.tfidf),
#                y = as.vector(bench.train[['category']]),
#                mc_type="AvA_hinge"
#                # useCells=TRUE,threads=3
# )

# dt.bench.dtm_train.tfidf <- as.data.frame(cbind(as.matrix(bench.dtm_train.tfidf), as.vector(bench.train[['category']])))
# colnames(dt.bench.dtm_train.tfidf)[dim(dt.bench.dtm_train.tfidf)[2]] <- 'category'

# bench.svm <- liquidSVM::svm(category ~ ., dt.bench.dtm_train.tfidf, useCells=TRUE) # KO error
# colnames(dt.bench.dtm_train.tfidf) <- paste0('`',colnames(dt.bench.dtm_train.tfidf),'`')
# bench.svm <- liquidSVM::svm(category ~ ., dt.bench.dtm_train.tfidf, useCells=TRUE) # KO error
# f <- paste("category ~", paste(sprintf("`%s`", colnames(dt.bench.dtm_train.tfidf)), collapse="+"))
# bench.svm <- liquidSVM::svm(formula(f), dt.bench.dtm_train.tfidf, useCells=TRUE) # KO error

# KO aussi
# bench.svm <- svm(x = dt.bench.dtm_train.tfidf[-dim(dt.bench.dtm_train.tfidf)[2]], 
#                  y =  dt.bench.dtm_train.tfidf[dim(dt.bench.dtm_train.tfidf)[2]])

# KO aussi
# model <- mcSVM(formula(f), dt.bench.dtm_train.tfidf,
#                mc_type="AvA_hinge",
#                useCells=TRUE, threads=3
#                )



# 
# # To create a sparse matrix you can do this:
# #   
# #   library(Matrix)
# # library(SpareM)
# # sMatrix <- Matrix(data=as.matrix(trainData), sparse=TRUE)
# # Then you can use that on your svm model
# # 
# # svm.model <- svm(trainData[,"Target"] ~., data=sMatrix, kernel="linear")
# # To predict you should make sure your train data has the correct format and then you can use:
# #   
# #   predicted <- predict(svm.model, testData)

# --------------- NNET KO Memory

# library(nnet)
# # Error: cannot allocate vector of size 50.0 Gb
# nnet_model <- nnet(category ~ ., data = dt.bench.dtm_train.tfidf, size = 2)


# --------------- caret nn KO : There were missing values in resampled performance measures
## -- https://stats.stackexchange.com/questions/21717/how-to-train-and-validate-a-neural-network-model-in-r

# library(caret)
# library(tidyr)
# KO pareil
# caret.fit <- train(x = as.matrix(bench.dtm_train.tfidf),
#                    y = bench.train[['category']],
#                    method = "nnet", size = 5, MaxNWts = 25000,
#                    maxit = 30)

# pcanet.model <- pcaNNet(x = as.matrix(bench.dtm_train.tfidf),
#                          y = bench.train[['category']],
#                          size = 5, MaxNWts = 10000, thresh = 0.95 )
# # weights:  13302
# # initial  value 16882.917811 
# # final  value 7808.000000 
# # converged
# 
# pcanet.preds <- predict(pcanet.model, newdata = as.matrix(bench.dtm_test.tfidf))
# head(pcanet.preds)
# 
# pcanet.preds.class <- as.data.table(pcanet.preds) %>%
#   gather(category, bval) %>%
#   filter(bval == 1) %>%
#   select(category)
# 
# bench.test$pcanet.preds.class <- pcanet.preds.class
#   
# bench.pcanet.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != pcanet.preds.class]))/dim(bench.test)[[1]])
# print(bench.pcanet.accuracy)
# 
# 
# dt.bench.dtm_train.tfidf <- as.data.frame(cbind(as.matrix(bench.dtm_train.tfidf), bench.train[['category']]))
# colnames(dt.bench.dtm_train.tfidf)[dim(dt.bench.dtm_train.tfidf)[2]] <- 'category'
# 
# dt.bench.dtm_test.tfidf <- as.data.frame(cbind(as.matrix(bench.dtm_test.tfidf), bench.test[['category']]))
# colnames(dt.bench.dtm_test.tfidf)[dim(dt.bench.dtm_test.tfidf)[2]] <- 'category'
# 
# pcanet.model.2 <- multinom(category ~ ., data = dt.bench.dtm_train.tfidf, MaxNWts = 21000)
# 
# pcanet.model.2.preds <- predict(pcanet.model.2, newdata = dt.bench.dtm_test.tfidf)
# 
# pcanet.preds.2 <- factor(pcanet.model.2.preds, labels = unique(levels(bench.test$category)))
# bench.test$pcanet.preds.2 <- pcanet.preds.2
# 
# bench.pcanet.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != pcanet.preds.2]))/dim(bench.test)[[1]])
# 
# print(bench.pcanet.accuracy) # "Accuracy : 74.77 %"

