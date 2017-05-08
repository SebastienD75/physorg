# save(list = c('bench.results'), file = 'data/results_bench_glmnet_prune_Technology_10_500.RData')

####################################################
## Script created by Sébastien Desfossés (2017/04)

# setwd("~/Dev/Git/R - Phys.org")
setwd("D:/Documents/Dev/R - Phys.org")

{
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(tidytext)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(glmnet)))
  suppressWarnings(suppressMessages(library(SnowballC)))
  suppressWarnings(suppressMessages(library(doParallel)))
  suppressWarnings(suppressMessages(library(text2vec)))
  suppressWarnings(suppressMessages(library(caret)))
  
  # library (ROCR)
  # library(textstem)
}

{
  # LOAD DATA ---------------------------------------------------------------
  {
    suppressWarnings(suppressMessages(library(tm)))
    param.lemmatized = TRUE
    param.dataorg.file <- 'data/physorg.RData'
    # param.clean_lemmatized_content.file <- 'data/glmnet_cleancontent_catsubcat.lemmatized.RData'
    # param.clean_lemmatized_content.file <- 'data/glmnet_cleancontent_catsubcat.lemmatized_full.RData'
    param.clean_lemmatized_content.file <- 'data/glmnet_cleancontent_catsubcat.lemmatized_full_merged_sum.RData'
    param.clean_not_lemmatized_content.file <- 'data/glmnet_cleancontent_catsubcat.not_lemmatized.RData'
    full_subcat_sample_size <- 100
    
    if(param.lemmatized) {
      param.clean_content.file <- param.clean_lemmatized_content.file
    } else {
      param.clean_content.file <- param.clean_not_lemmatized_content.file
    }
    
    if(!file.exists(param.clean_content.file)) {
      load(param.dataorg.file)
      
      # rm(list = setdiff(ls(), c('d.art', 'param.lemmatized')))

      d.art <- d.art %>% 
        rowwise() %>%
        mutate(content = ifelse(is.na(summary),
                                content,
                                ifelse(grepl(substr(summary,5,nchar(summary) - 5), content, fixed = TRUE, useBytes = TRUE),
                                       content,
                                       paste(summary, content)
                                       )
                                )
               ) %>% setDT
      
      d.art.sc.clean.time <- system.time(
        d.art.c.bench <- d.art %>%
          select(url, content, category, subcategory) %>%
          mutate(content.org = content) %>%
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
      
      if(param.lemmatized) {
        library(textstem)
        d.art.c.bench$content.nolem <- d.art.c.bench$content
        d.art.c.bench$content <- d.art.c.bench$content %>%
          lemmatize_strings()
      }
      
      d.user.actifs <- d.user[nbcom>2 & nbart>2]
      d.com.user.actifs <- d.com[user %in% d.user.actifs$user]
      # d.com.user.actifs %>% group_by(user) %>% summarise(mean_rank = mean(rank), comments = n(), wc = sum(wc_comment)) %>% arrange(-mean_rank, -comments, -wc)
      # d.com.user.actifs %>% group_by(user) %>% summarise(comments = n(), mean_rank = mean(rank),  wc = sum(wc_comment)) %>% arrange(-comments)
      d.art.com.user.actifs <- d.art.c.bench[url %in% d.com.user.actifs$url]
      d.art.com.user.actifs$subcategory <- droplevels(d.art.com.user.actifs$subcategory)
      
      
      # d.art.c.bench.org <- d.art.c.bench
      # d.art.c.bench <- d.art.com.user.actifs
      
      dim(d.user[nbcom>2 & nbart>2])
      d.art.c.bench.sample <- d.art.c.bench[0,]
      
      for(l in levels(d.art.c.bench$subcategory)) {
        nb_lines_subcat <- min(full_subcat_sample_size, dim(d.art.c.bench[subcategory == l])[[1]])
        sample_subcat <- sample_n(d.art.c.bench[subcategory == l], nb_lines_subcat)
        d.art.c.bench.sample <- rbind(d.art.c.bench.sample, sample_subcat)
      }
      
      # d.art.sc.clean$id <- as.character(d.art.sc.clean$id)
      # setkey(d.art.sc.clean, id)
      d.art.c.bench.url <- d.art.c.bench %>% select(id, url)
      d.art.c.bench$url <- NULL
      rm(d.art)
    } 
    else 
    {
      load(param.clean_content.file)
    }
    
    d.art.c.bench[, content := removeWords(content, c('category','can','say', 'will', 'use'))]
    
    completed.obj <- c('d.user.actifs', 'd.com.user.actifs', 'd.art.com.user.actifs', 'd.art.c.bench.sample')
    protected.obj <- c(completed.obj, "protected.obj", "bench.models", "bench.results", "d.art.c.bench", 'd.art.c.bench.url','param.lemmatized')
    rm(list = setdiff(ls(), protected.obj))
    # param.cleaninloop = c('d.bench', 'd.art.c.bench')
    gc()
  }
  
  
  # PARAMS ------------------------------------------------------------------
  {
    ## -- COMPUTEUR SPECIFICS --
    param.doparall.worker = 7
    
    ## -- PIPLINE --
    param.dotfidf = TRUE
    param.dostem = FALSE
    param.dongram = TRUE
    param.dofeaturehashing = FALSE # incompatible avec prune
    param.doprune = TRUE
    
    ## -- CAT / SUB CAT --
    param.mutate.subcat.as.cat = TRUE
    
    param.cat <- c('Astronomy & Space','Other Sciences','Technology','Physics', 'Nanotechnology','Health', 'Biology', 'Earth','Chemistry')
    
    param.mutate.subcat.cat <- c('Technology')
    
    param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                      'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                      'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                      'Materials Science')
    
    
    ## -- PCT USED DATA 
    param.pctdata.default = 1
    param.startmodel.pctdata = 1
    param.maxmodel.pctdata = 1
    param.pctdata.inc <- c(param.pctdata.default, 0.005, 0.01, 0.03, 0.09, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    
    ## -- MAX DATA
    param.nblines_max.default = 1000^10
    param.startmodel.nblines_max = 1
    param.maxmodel.nblines_max = 1
    param.nblines_max.inc <- c(param.nblines_max.default, ceiling(exp(seq(log(500),log(2000), length.out = param.maxmodel.nblines_max))))
    
    param.train_test <- 0.7
    
    ## -- NFOLD --
    param.cv.nfold.default = 3
    param.startmodel.cv.nfold <- 1
    param.maxmodel.cv.nfold <- 1
    param.cv.nfold.inc <- c(param.cv.nfold.default,4:10)
    param.bench.glmnet.THRESH.default = 1e-2 # best = 1e-2 (default 1E-7)
    param.bench.glmnet.MAXIT.default =  1e2 # best = 1e2 (default 10^5)
    
    ## -- PRUNE --
    param.prune.term_count_min.default = 10 # 80 # 40 # (default pkg 1)
    param.prune.doc_proportion_max.default = 1 # 0.8 # 0.4 # (default pkg 1)
    param.prune.doc_proportion_min.default = 0 # 0.002 # 0.0008 # (default pkg 0)
    #param.prune.doc_proportion_min.default = # (default Inf)
    param.startmodel.prune = 2
    param.maxmodel.prune = 200
    param.prune.inc <- list(term_count_min.inc = c(param.prune.term_count_min.default, ceiling(exp(seq(log(10),log(500), length.out = param.maxmodel.prune)))),
                            doc_proportion_max.inc = c(param.prune.doc_proportion_max.default, ceiling(exp(seq(log(9), log(1), length.out = 20)))/10),
                            doc_proportion_min.inc = c(param.prune.doc_proportion_min.default, ceiling(exp(seq(log(1), log(100), length.out = 20)))/100000)
    )
    
    param.hngram = 2 ** 18
    param.seed = 20170416
    
    param.bench.glmnet = TRUE
    param.bench.naivebayes = FALSE
    param.bench.xgboost = FALSE
    param.bench.svmk = FALSE
    param.bench.nnet.multinom = FALSE
    param.bench.pcaNNet = FALSE
    param.bench.neuralnet = FALSE
    
    param.bench.pcaNNet.thresh = 0.99
    param.bench.pcaNNet.size = 15
    
    param.bench.neuralnet.size_hidden <- 20
    param.bench.neuralnet.threshold <- 0.05
    
    param.pca = FALSE
    param.pca.alpha.eleastic.net = 0.5
    param.pca.pct_varexp = 99
    param.pca.bench.glmnet = TRUE
    param.pca.bench.nnet.multinom = TRUE
    param.pca.bench.neuralnet = TRUE
    
    param.lda = FALSE
  }
  
  
  # FUNCTIONS  -------------------------------------------------------------------
  {
    plotresults <- function(res = bench.results)
    {
      ggplot(data = res) +
        aes(x = Sample_lines, y = Accuracy, group = Model, fill = Model, color = Model) +
        geom_line() +
        labs(x = 'Articles', y = 'Accuracy')
      
      ggplot(data = res) +
        aes(x = Sample_lines, y = ceiling(10 *  Time / 60) / 10, group = Model, fill = Model, color = Model) +
        geom_line() +
        labs(x = 'Articles', y = 'Minutes')
      
      bench.results %>% filter(Accuracy > 35, Time < 25*60) %>% ggplot() +
        aes(x = Sample_lines, y = Accuracy, group = Model, fill = Model, color = Model) +
        geom_point() + geom_smooth(span = 0.9, se = FALSE) +
        labs(x = 'Articles', y = 'Accuracy')
      
      bench.results %>% filter(Accuracy > 35, Time < 25*60) %>% ggplot() +
        aes(x = Sample_lines, y = ceiling(10 *  Time / 60) / 10, group = Model, fill = Model, color = Model) +
        geom_line() +
        labs(x = 'Articles', y = 'Minutes')
      
      # rm(list = setdiff(ls(), c('bench.results')))
      
      bench.results %>% ggplot() +
        aes(x = Sample_lines, y = F1, group = Model, fill = Model, color = Model) +
        geom_point() + geom_smooth(span = 0.9, se = FALSE) +
        labs(x = 'Articles', y = 'F1')
      
      bench.results %>% filter(Model == 'cv.glmnet') %>% 
        ggplot() +
        geom_point(aes(x = PRUNE_tcmi, y = F1, group = Model, fill = Category, color = Category)) + 
        geom_smooth(aes(x = PRUNE_tcmi, y = F1, group = Model, fill = Category, color = Category), span = 0.9, se = FALSE) +
        labs(x = 'Prune', y = 'F1') 
      
      bench.results %>% filter(Model != 'pca.neuralnet', Model != 'pcaNNet', Model != 'pca.multinom')  %>% ggplot() +
        aes(x = Sample_lines, y = F1, group = Model, fill = Model, color = Model) +
        geom_point() + geom_smooth(span = 0.9, se = FALSE) +
        labs(x = 'Articles', y = 'F1')
      
      
      bench.results %>% ggplot() +
        aes(x = Sample_lines, y = ceiling(10 *  Time / 60) / 10, group = Model, fill = Model, color = Model) +
        geom_line() +
        labs(x = 'Articles', y = 'Minutes')

      bench.results %>% filter(Model != 'pca.neuralnet', Model != 'pcaNNet', Model != 'pca.multinom') %>% ggplot() +
        aes(x = Sample_lines, y = ceiling(10 *  Time / 60) / 10, group = Model, fill = Model, color = Model) +
        geom_line() +
        labs(x = 'Articles', y = 'Minutes')
      
      # bench.results %>% filter(Accuracy > 35, Time < 25*60, Model != 'pcaNNet') %>% ggplot() +
      #   aes(x = Sample_lines, y = ceiling(10 *  Time / 60) / 10, group = Model, fill = Model, color = Model) +
      #   geom_line() +
      #   labs(x = 'Articles', y = 'Minutes')
    }
    
    num_sav <- 0
    save_results <- function()
    {
      id <- dim(bench.results)[[1]] + 1
      date.sav <- Sys.time()
      
      bench.results[id, "Session"] <<- sessioin_id
      bench.results[id, "Id"] <<- sprintf("%s-%s", sessioin_id, num_sav)
      bench.results[id, "Num"] <<- num_sav
      bench.results[id, "Timestamp"] <<- date.sav
      
      bench.results[id, "Computer"] <<- Sys.info()[[4]] 
      bench.results[id, "Worker"] <<- param.doparall.worker
      bench.results[id, "Issc"] <<- param.mutate.subcat.as.cat
      
      bench.results[id, "Category"] <<- param.cat[i_cat]
      
      # cm$byClass
      bench.results[id, "Accuracy"] <<- res.confmat$overall[['Accuracy']]
      bench.results[id, "AccuracyPValue"] <<- res.confmat$overall[['AccuracyPValue']]
      bench.results[id, "Balanced Accuracy"] <<- mean(res.confmat$byClass[,'Balanced Accuracy'])
      bench.results[id, "Precision"] <<- mean(res.confmat$byClass[,'Precision'])
      bench.results[id, "Recall"] <<- mean(res.confmat$byClass[,'Recall'])
      bench.results[id, "F1"] <<- mean(res.confmat$byClass[,'F1'])
      idx_minF1 <- which(res.confmat$byClass[,'F1'] == min(res.confmat$byClass[,'F1']))
      bench.results[id, "Min F1 class"] <<- names(idx_minF1)
      bench.results[id, "Min F1 val"] <<- res.confmat$byClass[idx_minF1,'F1']
      idx_maxF1 <- which(res.confmat$byClass[,'F1'] == max(res.confmat$byClass[,'F1']))
      bench.results[id, "Max F1 class"] <<- names(idx_maxF1)
      bench.results[id, "Max F1 val"] <<- res.confmat$byClass[idx_maxF1,'F1']
        
      # bench.results[id, "Accuracy"] <<- res.accuracy
      bench.results[id, "Time"] <<- res.time
      bench.results[id, "Model"] <<- res.model
      
      bench.results[id, "Train_lines"] <<- dim(bench.dtm_train)[[1]]
      bench.results[id, "Train_vars"] <<- dim(bench.dtm_train)[[2]]
      bench.results[id, "Sample_lines"] <<- bench.num_sample
      bench.results[id, "Ratio_TT"] <<- param.train_test
      
      bench.results[id, "LEMM"] <<- param.lemmatized
      bench.results[id, "TFIDF"] <<- param.dotfidf
      bench.results[id, "PRUNE"] <<- param.doprune
      bench.results[id, "STEM"] <<- param.dostem
      bench.results[id, "NGRAM"] <<- param.dongram
      bench.results[id, "FEATH"] <<- param.dofeaturehashing
      
      bench.results[id, "HNGRAM"] <<- param.hngram
      
      bench.results[id, "PRUNE_tcmi"] <<- param.prune.term_count_min
      bench.results[id, "PRUNE_dpma"] <<- param.prune.doc_proportion_max
      bench.results[id, "PRUNE_dpmi"] <<- param.prune.doc_proportion_min
      
      
      if(param.bench.glmnet) {
        bench.results[id, "GLM_NFOLDS"] <<- param.bench.glmnet.NFOLDS
        bench.results[id, "GLM_THRESH"] <<- param.bench.glmnet.THRESH
        bench.results[id, "GLM_MAXIT"] <<- param.bench.glmnet.MAXIT
      }
      
      if(param.bench.pcaNNet) {
        bench.results[id, "PCANNET_thresh"] <<- param.bench.pcaNNet.thresh
        bench.results[id, "PCANNET_size"] <<- param.bench.pcaNNet.size
      }
      
      if(param.bench.neuralnet) {
        bench.results[id, "NEURALNET_size_hidden"] <<- param.bench.neuralnet.size_hidden
        bench.results[id, "NEURALNET_threshold"] <<- param.bench.neuralnet.threshold
      }
      
      if(param.pca) {
        bench.results[id, "PCA_net"] <<- param.pca.alpha.eleastic.net
        bench.results[id, "PCA_varexp"] <<- param.pca.pct_varexp
      }
      
      num_sav <<- num_sav + 1
    }
    
    save_model <- function(model_name) 
    {
      print(sprintf("Model %s (%0.1f min): num sample=%s, dim_train=(%d, %d), %s", 
                    model_name, 
                    bench.glmnet_classifier.time[[3]]/60,
                    bench.models[[model_name]]$param.num_sample,
                    bench.models[[model_name]]$bench.dtm_train.dim[[1]],
                    bench.models[[model_name]]$bench.dtm_train.dim[[2]],
                    bench.models[[model_name]]$bench.glmnet_classifier.accuracy))
      
      return(0)
      
      bench.models[[model_name]]$model_name <<- model_name
      bench.models[[model_name]]$model_num <<- model_num
      bench.models[[model_name]]$model_desc <<- model_desc
      
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
    }
    
    print_model <- function(model) {
      params <- c('model_name', 'model_num','param.num_sample','model_desc','bench.dtm_train.dim', 'bench.glmnet_classifier.accuracy')
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
    
    if(!exists('bench.results')) {
      bench.results <- data.frame()
    }
    
    if(param.mutate.subcat.as.cat) {   
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
    param.maxmodel.nblines_max = min(c(length(param.nblines_max.inc), param.startmodel.nblines_max + param.maxmodel.nblines_max - 1))
    
    if(param.mutate.subcat.as.cat) {
      d.art.c.bench.allcat <- d.art.c.bench
    }
  }
  
  
  
  # START BENCH -------------------------------------------------------------
  sessioin_id <- ceiling(10 * as.numeric(Sys.time()))
  i_cat = 1
  for (i_cat in 1:ifelse(!param.mutate.subcat.as.cat,1,length(param.cat))) {
    
    if(param.mutate.subcat.as.cat) {
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
    
    i_pctdata = 1
    for(i_pctdata in param.startmodel.pctdata:param.maxmodel.pctdata)
    {
      param.pctdata <<- param.pctdata.inc[[i_pctdata]]
      cat('\n',sprintf("[%d/%d]Testing model with param.pctdata = %s", i_pctdata, param.maxmodel.pctdata, param.pctdata), '\n')
      
      i_nblm = 1
      for(i_nblm in param.startmodel.nblines_max:param.maxmodel.nblines_max)
      {
        param.nblines_max <<- param.nblines_max.inc[[i_nblm]]
        cat('\n',sprintf("[%d/%d]Testing model with param.nblines_max = %s", i_nblm, param.maxmodel.nblines_max, param.nblines_max), '\n') 
        
        param.num_sample = min(param.nblines_max, ceiling(param.pctdata * dim(d.art.c.bench)[[1]]))
        bench.all_ids = d.art.c.bench$id
        bench.train_ids = sample(bench.all_ids, param.num_sample)
        d.bench <- d.art.c.bench[J(bench.train_ids)] %>% mutate(category = droplevels(category)) %>% setDT()
        
        # SDE ?!
        # d.bench[,id := (.I)]
        setkey(d.bench, id)
        
        bench.num_sample = ceiling(param.train_test * dim(d.bench)[[1]])
        bench.all_ids = d.bench$id
        bench.train_ids = sample(bench.all_ids, bench.num_sample)
        bench.test_ids = setdiff(bench.all_ids, bench.train_ids)
        bench.train = d.bench[J(bench.train_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
        bench.test = d.bench[J(bench.test_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
        
        # if(length(param.cleaninloop) != 0 && 
        #    param.startmodel.pctdata == param.maxmodel.pctdata &&
        #    param.startmodel.nblines_max == param.maxmodel.nblines_max
        #    ) {
        #   rm(list = param.cleaninloop)
        # }
        gc()
        
        print(paste("Train nb articles =", dim(bench.train)[[1]]))
        
        tokenizer.stem = function(x) {
          tokens = word_tokenizer(x)
          lapply(tokens, SnowballC::wordStem, language="en")
        }
        
        if(param.dostem) {
          print('STEM : TRUE')
          
          bench.train_tokens.time <- system.time(
            bench.train_tokens <- bench.train$content %>% tokenizer.stem
          ); print(sprintf('bench.train_tokens.time: %0.2fs', bench.train_tokens.time[[3]]))
          
        } else {
          print('STEM : FALSE')
          
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
          param.prune.doc_proportion_max <<- param.prune.doc_proportion_max.default # param.prune.inc$doc_proportion_max.inc[[i_prune]]
          param.prune.doc_proportion_min <<- param.prune.doc_proportion_min.default # param.prune.inc$doc_proportion_min.inc[[i_prune]]
          
          t0 = Sys.time()
          if(param.dofeaturehashing) {
            print('FEATURE HASHING : TRUE')
            
            bench.h_vectorizer = hash_vectorizer(hash_size = param.hngram, ngram = c(1L, 2L))
            bench.vectorizer <- bench.h_vectorizer
            
            bench.dtm_train.time <- system.time(
              bench.dtm_train<-create_dtm(bench.it_train, bench.h_vectorizer)
            ); print(sprintf('bench.dtm_train.time: %0.2fs', bench.dtm_train.time[[3]]))
            
            bench.dtm_test.time <- system.time(
              bench.dtm_test<-create_dtm(bench.it_test, bench.h_vectorizer)
            ); print(sprintf('bench.dtm_test.time: %0.2fs', bench.dtm_test.time[[3]]))
            
            
          } else {
            print('FEATURE HASHING : FALSE')
            
            
            if(param.dongram) {
              print('NGRAM : TRUE')
              
              bench.train.vocab.stem.time <- system.time(
                bench.train.vocab.stem<- create_vocabulary(bench.it_train, ngram = c(1L, 2L))
              ); print(sprintf('Do ngram : bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
              
            } else {
              print('NGRAM : FALSE')
              
              bench.train.vocab.stem.time <- system.time(
                bench.train.vocab.stem <- create_vocabulary(bench.it_train)
              ); print(sprintf('bench.train.vocab.stem.time: %0.2fs', bench.train.vocab.stem.time[[3]]))
              
            }
            
            if(param.doprune) {
              print('PRUNE : TRUE')
              
              cat(sprintf("[%d/%d]Testing model with prune = (count_min=%s, prop_max=%s, prop_min=%s)", 
                          i_prune, param.maxmodel.prune, param.prune.term_count_min, param.prune.doc_proportion_max, param.prune.doc_proportion_min), '\n')
              
              bench.train.vocab.stem.prune.time <- system.time(
                bench.train.vocab.stem <- prune_vocabulary(bench.train.vocab.stem,
                                                           term_count_min = param.prune.term_count_min,
                                                           doc_proportion_max = param.prune.doc_proportion_max,
                                                           doc_proportion_min = param.prune.doc_proportion_min)
              ); print(sprintf('bench.train.vocab.stem.prune.time: %0.2fs', bench.train.vocab.stem.prune.time[[3]]))
              
            } else {
              print('PRUNE : FALSE')
            }
            
            bench.vectorizer <- vocab_vectorizer(bench.train.vocab.stem)
            
            bench.dtm_train.time <- system.time(
              bench.dtm_train<-create_dtm(bench.it_train, bench.vectorizer)
            ); print(sprintf('bench.dtm_train.time: %0.2fs', bench.dtm_train.time[[3]]))
            
            bench.dtm_test.time <- system.time(
              bench.dtm_test<-create_dtm(bench.it_test, bench.vectorizer)
            ); print(sprintf('bench.dtm_test: %0.2fs', bench.dtm_test.time[[3]]))
          }
          
          
          if(param.dotfidf) {
            print('TFIDF : TRUE')
            tfidf = TfIdf$new()
            bench.dtm_train = fit_transform(bench.dtm_train, tfidf)
            
            # tfidf modified by fit_transform() call!
            # apply pre-trained tf-idf transformation to test data
            bench.dtm_test = 
              create_dtm(bench.it_test, bench.vectorizer) %>% 
              transform(tfidf)
          } else {
            print('TFIDF : FALSE')
          }
          
          ## --------------- GLMNET ---------------
          
          if(param.bench.glmnet) 
          {
            
            cat('\n','------------------------------------')
            cat('\n','cv.glmnet :\n')
            
            suppressWarnings(suppressMessages(library(wordcloud2)))
            suppressWarnings(suppressMessages(library(RColorBrewer)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'cv.glmnet'
            
            for(i_nfold in param.startmodel.cv.nfold:param.maxmodel.cv.nfold)
            {
              param.bench.glmnet.NFOLDS <<- param.cv.nfold.inc[[i_nfold]]
              cat(sprintf("[%d/%d]Training model with param.cv.nfold = %d", i_nfold, param.maxmodel.cv.nfold, param.bench.glmnet.NFOLDS), '\n')
              
              param.bench.glmnet.THRESH <- param.bench.glmnet.THRESH.default
              param.bench.glmnet.MAXIT <- param.bench.glmnet.MAXIT.default
              model_name <- paste0(as.character(i_nfold), as.character(length(bench.models) + 1), as.character(i_nfold))
              model_num <- as.numeric(model_name)
              
              model_desc <- sprintf('model %d - text2vect cv.glmnet : glmnet.params = ALPHA:1, NFOLDS:%d, THRESH:%s, MAXIT:%s + featureh=%s, stem=%s, ngram=%s, prune=%s :  prune.params = countmin:%s, doc.prop.max:%s, doc.prop.min:%s', 
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
              cat(model_desc,'\n')
              
              # library("recommenderlab")
              ## bench.dtm_train.dist = dist2(bench.dtm_train)
              # bench.dtm_train.sim = sim2(bench.dtm_train)
              # bench.dt_train <- as.data.table(as.matrix(bench.dtm_train))
              # bench.dt_train.sim <- as.data.table(as.matrix(bench.dtm_train.sim))
              
              # num_train_doc = 30
              # id_doc = as.numeric(colnames(bench.dt_train.sim[,num_train_doc, with=FALSE]))
              # d.art.c.bench[id == id_doc]$content
              
              
              gc()
              bench.glmnet_classifier.time <- system.time(
                bench.glmnet_classifier <- cv.glmnet(x = bench.dtm_train, y = bench.train[['category']], 
                                                     # family = 'binomial',
                                                     # type.measure = 'deviance',
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
                
              ); print(sprintf('bench.glmnet_classifier.time: %0.2fm', bench.glmnet_classifier.time[[3]]/60))
              
              # plot(bench.glmnet_classifier)
              
              bench.preds.class.time <- system.time(
                bench.test$bench.preds.class <-  predict(bench.glmnet_classifier, bench.dtm_test, s = "lambda.min", type = 'class')
              ); print(sprintf('bench.preds.class: %0.2fs', bench.preds.class.time[[3]]))
              
              tend <- Sys.time()
              res.time <- difftime(tend, t0, units = 'secs') 
              
              res.confmat <- confusionMatrix(bench.test$bench.preds.class, bench.test$category)
              res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.preds.class]))/dim(bench.test)[[1]]
              save_results()
              
              bench.glmnet_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
              print(bench.glmnet_classifier.accuracy)
              
              # res <- bench.test %>%
              #   mutate(accurate = ifelse(category == bench.preds.class, 1, 0)) %>%
              #   group_by(category) %>%
              #   # mutate(words = sapply(gregexpr("[[:alpha:]]+", content), function(x) sum(x > 0))) %>%
              #   summarise(n = n(),
              #             pct = 100*n/dim(bench.test)[[1]],
              #             # words = sum(words),
              #             accurate = sum(accurate),
              #             accuracy = (100*accurate/n)) %>%
              #   # select(category, n, pct, words, accuracy) %>%
              #   select(category, n, pct, accuracy) %>%
              #   arrange(-accuracy)
              # 
              # print(res)
              
              save_model(model_name)
              
            }
            
          }
          
          ## --------------- LDA ---------------
          
          if(param.lda) 
          {
            
            cat('\n','------------------------------------')
            cat('\n','Latent Dirichlet Allocation :\n')
            
            suppressWarnings(suppressMessages(library(LDAvis)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'lda'
            
            param.reglda = FALSE
            param.multi_factor = 1
            # param.doc_topic_prior = 0.1
            # param.topic_word_prior = 0.01
            
            bench.lda.dtm_train = create_dtm(bench.it_train, bench.vectorizer, type = "lda_c")
            
            bench.lda.model = LDA$new(n_topics = nlevels(bench.train$category) * param.multi_factor, 
                                      # param.doc_topic_prior = 0.1, param.topic_word_prior = 0.01
                                      vocabulary = bench.train.vocab.stem)
            
            bench.lda.doc_topic_distr.train = bench.lda.model$fit_transform(bench.lda.dtm_train, 
                                                                            n_iter = 1000, 
                                                                            convergence_tol = 0.0001,
                                                                            check_convergence_every_n = 10)
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs') 
            # save_results()
            
            ## - topic probability table from text2vec LDA
            
            bench.lda.doc_topic_prob = normalize(bench.lda.doc_topic_distr.train, norm = "l1")
            # or add norm first and normalize :
            # bench.lda.doc_topic_prob = normalize(bench.lda.doc_topic_distr.train + doc_topic_prior, norm = "l1")
            
            # word_topic_counts = lda_model$get_word_vectors() 
            # topic_word_distr = t(word_topic_counts + topic_word_prior) %>% normalize('l1')
            
            bench.lda.model$plot()
            
            ## - N'apporte rien à la regression
            if(param.reglda)
            {
              
              bench.lda.doc_topic_distr.test = bench.lda.model$fit_transform(bench.lda.dtm_test, 
                                                                             n_iter = 1000, 
                                                                             convergence_tol = 0.0001,
                                                                             check_convergence_every_n = 10)
              
              bench.lda.dtm_test = create_dtm(bench.it_test, bench.vectorizer, type = "lda_c")
              # scale ou pas ?
              bench.lda.dtm_train <- cbind(bench.dtm_train, scale(bench.lda.doc_topic_distr.train))
              bench.lda.dtm_test <- cbind(bench.dtm_test, scale(bench.lda.doc_topic_distr.test))
              
              bench.lda.glmnet_classifier.time <- system.time(
                bench.lda.glmnet_classifier <- cv.glmnet(x = bench.lda.dtm_train, y = bench.train[['category']],
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
                
              ); print(sprintf('bench.glmnet_classifier.time: %0.2fm', bench.lda.glmnet_classifier.time[[3]]/60))
              
              plot(bench.lda.glmnet_classifier)
              
              bench.lda.preds.class.time <- system.time(
                bench.test$bench.preds.class <-  predict(bench.lda.glmnet_classifier, bench.lda.dtm_test, s = "lambda.min", type = 'class')
              ); print(sprintf('bench.preds.class: %0.2fs', bench.preds.class.time[[3]]))
              
              
              res.confmat <- confusionMatrix(bench.test$bench.preds.class, bench.test$category)
              res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.preds.class]))/dim(bench.test)[[1]]
              bench.lda.glmnet_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
              print(bench.lda.glmnet_classifier.accuracy)
            }
          }
          
          
          ## --------------- Autres models possibles --------------- 
          
          
          # param.mutate.subcat.cat <- c('Physics')
          # --------------- naiveBayes "Accuracy : 55.25 %", Time difference of 2.209617 mins
          
          if(param.bench.naivebayes)
          {
            cat('\n','------------------------------------')
            cat('\n','Naive Bayes :\n')
            
            suppressWarnings(suppressMessages(library(e1071)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'naiveBayes'
            
            bench.naivebayes_classifier <- naiveBayes(x = as.matrix(bench.dtm_train),
                                                      y = bench.train[['category']])
            
            bench.test$bench.naivebayes.preds.class <-  predict(bench.naivebayes_classifier, as.matrix(bench.dtm_test))
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs') 
            res.confmat <- confusionMatrix(bench.test$bench.naivebayes.preds.class, bench.test$category)
            res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.naivebayes.preds.class]))/dim(bench.test)[[1]]
            save_results()
            
            bench.naivebayesclassifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
            print(bench.naivebayesclassifier.accuracy)
            
            print(difftime(tend, t0, units = 'mins'))
          }
          
          
          # --------------- xgboost : plus long  resutlats egaux voire un peu meilleurs
          # param.mutate.subcat.cat <- c('Physics')
          # 5/200   [1] "Accuracy : 80.03 %" Time difference of 2.574376 mins
          # 5/300   [1] "Accuracy : 80.45 %" Time difference of 3.659111 mins
          # 5/500   [1] "Accuracy : 80.75 %" Time difference of 5.820309 mins
          # 6.600   [1] "Accuracy : 80.69 %" Time difference of 7.327472 mins
          # 3/1000  [1] "Accuracy : 80.60 %" : Time difference of 7.901743 mins
          
          # param.mutate.subcat.cat <- c('Health')
          # 5/500 [1] "Accuracy : 74.69 %" Time difference of 20.68896 mins
          # 3/700 [1] "Accuracy : 74.11 %" Time difference of 18.91857 mins
          # 7/300 [1] "Accuracy : 74.53 %" Time difference of 16.81269 mins 
          # 3/300 [1] "Accuracy : 73.52 %" Time difference of 8.223368 mins
          
          ## - https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html
          ## - https://gist.github.com/dkincaid/87f0fbeb912cf23816c340b4fbe30baa
          ## - https://www.analyticsvidhya.com/blog/2015/12/kaggle-solution-cooking-text-mining-competition/
          ## - https://www.analyticsvidhya.com/blog/2015/12/kaggle-solution-cooking-text-mining-competition/
          ## - https://www.r-bloggers.com/an-introduction-to-xgboost-r-package/
          ## - https://www.kaggle.com/tqchen/otto-group-product-classification-challenge/understanding-xgboost-model-on-otto-data
          
          if(param.bench.xgboost)
          {
            cat('\n','------------------------------------')
            cat('\n','Xg Boost :\n')
            
            suppressWarnings(suppressMessages(library(xgboost)))
            suppressWarnings(suppressMessages(library(igraph)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'xgboost'
            
            xgb_params = list(
              objective = "multi:softmax",
              num_class = length(levels(bench.train$category)) + 1,
              eta = 0.1,
              max.depth = 5,
              eval_metric = "mlogloss")
            
            bench.xgboost_classifier.trainmatrix <- xgb.DMatrix(bench.dtm_train, label = bench.train[['category']])
            
            # Analyse exploratoire preliminaire
            # cv.nfold <- 5
            # xgb.nround <- 5
            # bench.xgboost.cv = xgb.cv(param=xgb_params, data = bench.xgboost_classifier.trainmatrix, nfold = cv.nfold, nrounds = xgb.nround)
            
            xgb_params$max.depth <- 5
            xgb.nround <- 500
            bench.xgboost_classifier <- xgboost(data = bench.xgboost_classifier.trainmatrix, params = xgb_params, nrounds = xgb.nround, verbose = 0)
            
            # Check the feature importance
            importance_vars <- xgb.importance(model=bench.xgboost_classifier, feature_names = colnames(bench.xgboost_classifier.trainmatrix))
            head(importance_vars, 20)
            
            bench.xgboost.preds <- predict(bench.xgboost_classifier, newdata = bench.dtm_test, type = 'class')
            bench.test$bench.xgboost.preds <- levels(bench.train$category)[bench.xgboost.preds]
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            res.confmat <- confusionMatrix(bench.test$bench.xgboost.preds, bench.test$category)
            res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.xgboost.preds]))/dim(bench.test)[[1]] 
            save_results()
            
            bench.xgboost_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
            
            xgb.importance_matrix <- xgb.importance(bench.dtm_train@Dimnames[[2]], model = bench.xgboost_classifier)
            head(xgb.importance_matrix, 20)
            
            # xgb.ggplot.importance(head(xgb.importance_matrix,20))
            # xgb.ggplot.deepness(model = bench.xgboost_classifier)
            
            print(bench.xgboost_classifier.accuracy)
            print(difftime(tend, t0, units = 'mins'))
          }
          
          
          
          # --------------- SVM kernlab, OK moins bon resultats
          # Physics 
          # C = 3 : "Accuracy : 65.05 %" et 30 min VS glmnet => "Accuracy : 80.03 %"
          
          if(param.bench.svmk)
          {
            cat('\n','------------------------------------')
            cat('\n','SVM (kernlab) :\n')
            
            suppressWarnings(suppressMessages(library(kernlab)))
            suppressWarnings(suppressMessages(library(tidyr)))
            
            gc()
            t0 = Sys.time(); 
            res.model <- 'ksvm'
            
            bench.ksvmclass_classifier <- ksvm(x = as.matrix(bench.dtm_train),
                                               y = bench.train[['category']],
                                               C = 5, # 3, # Physics => "Accuracy : 65.05 %" VS glmnet => "Accuracy : 80.03 %"
                                               prob.model=TRUE
            ); t1 = Sys.time()
            
            print(difftime(t1, t0, units = 'mins'))
            
            ksvm.predict <- predict(bench.ksvmclass_classifier, newdata = bench.dtm_test, type = 'probabilities')
            
            ksvm.predict.gather_prob <- as.data.table(ksvm.predict) %>%
              mutate(id = row_number()) %>%
              gather(category, prob, -id) %>%
              group_by(id) %>%
              mutate(maxprob = max(prob)) %>%
              filter(prob == maxprob) %>%
              select(id, category, maxprob) %>%
              arrange(id) %>%
              setDT()
            
            bench.test$ksvm.predict.class <- ksvm.predict.gather_prob$category
            bench.test$ksvm.predict.prob <- ksvm.predict.gather_prob$maxprob
            
            #tst[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
            # ksvm.predict.bool <- as.data.table(ksvm.predict)
            # ksvm.predict.bool[, (names(ksvm.predict.bool)) := lapply(.SD, function(x) ifelse(x>=0.5,1,0))]
            # bench.test$ksvm.predict.class <- ksvm.predict.bool %>%
            #   gather(category,boolval) %>%
            #   filter(boolval == 1) %>%
            #   select(category)
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            res.confmat <- confusionMatrix(bench.test$ksvm.predict.class, bench.test$category)
            res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != ksvm.predict.class]))/dim(bench.test)[[1]]
            save_results()
            
            bench.ksvm_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
            
            print(bench.ksvm_classifier.accuracy)
            print(difftime(tend, t0, units = 'mins'))
          }
          
          
          
          
          # --------------- PCA: tres long pas d'amelioration des resultats à priori
          
          
          if(param.pca)
          {
            cat('\n','------------------------------------')
            cat('\n','PCA (FactoMineR) :\n')
            
            suppressWarnings(suppressMessages(library(FactoMineR)))
            suppressWarnings(suppressMessages(library(Matrix)))
            suppressWarnings(suppressMessages(library(ggplot2)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'PCA'
            
            param.do.vargraph <- FALSE
            
            dt.bench.dtm_train <-  as.data.table(as.matrix(bench.dtm_train))
            pca.dtm_train <- PCA(dt.bench.dtm_train[,!'category', with = FALSE], 
                                 graph = FALSE, 
                                 ncp = dim(dt.bench.dtm_train)[[2]] - 1
            ); tend <- Sys.time()
            
            print(difftime(tend,t0,units = 'mins')) #  3.454795 mins pour sub cat physics
            dim(pca.dtm_train$eig)
            
            pca.pred.dtm_test <- predict(pca.dtm_train, as.matrix(bench.dtm_test))
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            #save_results()
            
            df.pct_varexp <- data.frame(dim = c(0), pct_varexp = c(0))
            for (pct in 1:100) {
              
              bench.pca.cp.i = pca.dtm_train$eig %>%
                filter((100 - `cumulative percentage of variance`) > (100 - pct))
              
              df.pct_varexp <- rbind(df.pct_varexp, c(dim(bench.pca.cp.i)[1], pct))
              
            }
            
            if(param.do.vargraph) {
              head(df.pct_varexp)
              ggplot(data = df.pct_varexp, aes(x = pct_varexp, y = dim)) + 
                geom_point()  +
                geom_line(aes(x = 95)) +
                geom_line(aes(x = 98)) +
                geom_line(aes(x = 99))
            }
            
            bench.pca.cp <- pca.dtm_train$eig %>%
              filter((100 - `cumulative percentage of variance`) > (100 - param.pca.pct_varexp))
            
            dim(bench.pca.cp)
            
            pca.cp_train.mat <- as.matrix(pca.dtm_train$ind$coord[,1:dim(bench.pca.cp)[1]])
            pca.cp_train.smat <- Matrix(data = pca.cp_train.mat, sparse = TRUE) 
            pca.cp_test.mat <- as.matrix(pca.pred.dtm_test$coord[,1:dim(bench.pca.cp)[1]])
            pca.cp_test.smat <- Matrix(data = pca.cp_test.mat, sparse = TRUE) 
            
            print(paste(
              ceiling(10*100 * (dim(bench.dtm_train)[2] - dim(pca.cp_train.smat)[2])/dim(bench.dtm_train)[2])/10, 
              '% de gain sur le nombre de variable')
            )
            
            if(param.pca.bench.glmnet) 
            {
              cat('\n','------------------------------------')
              cat('\n','PCA GLMNET :\n')
              
              gc()
              t0 <- Sys.time()
              res.model <- 'pca.cv.glmnet'
              
              pca.bench.glmnet_classifier.time <- system.time(
                pca.bench.glmnet_classifier <- cv.glmnet(x = pca.cp_train.smat, y = bench.train[['category']], 
                                                         # family = 'binomial',                              
                                                         family = 'multinomial', 
                                                         type.multinomial="grouped", 
                                                         # L1 penalty
                                                         alpha = param.pca.alpha.eleastic.net,
                                                         # ROC curve
                                                         type.measure = "auc",
                                                         nfolds = param.bench.glmnet.NFOLDS,
                                                         thresh = param.bench.glmnet.THRESH,
                                                         maxit = param.bench.glmnet.MAXIT,
                                                         parallel = TRUE)
                
              ); print(sprintf('pca.bench.glmnet_classifier.time: %0.2fm', pca.bench.glmnet_classifier.time[[3]]/60))
              
              plot(pca.bench.glmnet_classifier)
              
              pca.bench.preds.class.time <- system.time(
                bench.test$bench.preds.class <-  predict(pca.bench.glmnet_classifier, pca.cp_test.smat, s = "lambda.min", type = 'class')
              ); print(sprintf('bench.preds.class: %0.2fs', bench.preds.class.time[[3]]))
              
              tend <- Sys.time()
              res.time <- difftime(tend, t0, units = 'secs')
              res.confmat <- confusionMatrix(bench.test$bench.preds.class, bench.test$category)
              res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.preds.class]))/dim(bench.test)[[1]]
              save_results()
              
              pca.bench.glmnet_classifier.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
              print(pca.bench.glmnet_classifier.accuracy) # [1] "Accuracy : 77.91 %", 3min, "Accuracy : 77.94 %" elastic, "Accuracy : 74.86 %" avec ridge
            }
            
            ##--  PCA avec Utilisation avec un reseau de neuronne
            
            if(param.pca.bench.nnet.multinom) {
              suppressWarnings(suppressMessages(library(nnet)))
              
              cat('\n','------------------------------------')
              cat('\n','PCA NNET MULTINOM :\n')
              
              gc()
              t0 <- Sys.time()
              res.model <- 'pca.multinom'
              
              nnet.size <- nlevels( bench.train[['category']]) #7
              
              nnet.pca.cp_train.mat <- as.data.frame(cbind(pca.cp_train.mat, bench.train[['category']]))
              colnames(nnet.pca.cp_train.mat)[dim(nnet.pca.cp_train.mat)[2]] <- 'category'
              
              nnet_model <- multinom(category ~ ., 
                                     data = nnet.pca.cp_train.mat, 
                                     MaxNWts = (nnet.size * (dim(nnet.pca.cp_train.mat)[2] + 1) + 1)
              )
              
              nnet.preds <- predict(nnet_model, pca.cp_test.mat, type = 'class')
              #head(nnet.preds)
              bench.test$nnet.preds.class <- factor(nnet.preds, labels = unique(levels(bench.train$category)))
              #head(bench.test$nnet.preds.class)
              
              tend <- Sys.time()
              res.time <- difftime(tend, t0, units = 'secs')
              res.confmat <- confusionMatrix(bench.test$nnet.preds.class, bench.test$category)
              res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != nnet.preds.class]))/dim(bench.test)[[1]]
              save_results()
              
              bench.pcanet.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
              print(bench.pcanet.accuracy) #  avec PCA : "Accuracy : 54.50 %"
            }
            
            if(param.pca.bench.neuralnet)
            {
              cat('\n','------------------------------------')
              cat('\n','PCA Neural Network (neuralnet) :\n')
              
              suppressWarnings(suppressMessages(library(neuralnet)))
              suppressWarnings(suppressMessages(library(nnet)))
              
              gc()
              t0 <- Sys.time()
              res.model <- 'pca.neuralnet'
              
              #levels(bench.test[['category']])  
              res.factor <- levels(bench.test[['category']]) 
              nb.factor <- length(res.factor)
              
              nbvar <- dim(pca.cp_train.mat)[[2]]
              nblines <- dim(pca.cp_train.mat)[[1]]
              cols <- colnames(pca.cp_train.mat)
              
              bench.neuralnet.train <- cbind(pca.cp_train.mat, class.ind(as.factor(bench.train[['category']])))
              bench.neuralnet.test <- cbind(pca.cp_test.mat, class.ind(as.factor(bench.test[['category']])))
              
              scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
              bench.neuralnet.train <- as.data.table(bench.neuralnet.train)
              bench.neuralnet.test <- as.data.table(bench.neuralnet.test)
              
              names(bench.neuralnet.train) <- c(colnames(pca.cp_train.mat), res.factor)
              names(bench.neuralnet.test) <- c(colnames(pca.cp_test.mat), res.factor)
              
              bench.neuralnet.train[, (cols) := lapply(.SD, scl), .SDcols=cols]
              bench.neuralnet.test[, (cols) := lapply(.SD, scl), .SDcols=cols]
              
              f <- as.formula(paste(paste(sprintf("`%s`", res.factor), collapse="+"), "~", paste(sprintf("`%s`", cols), collapse="+")))
              
              nn <- neuralnet(f,
                              data = bench.neuralnet.train,
                              # hidden = c(20, 20, nb.factor),
                              hidden = c(nbvar, param.bench.neuralnet.size_hidden, nb.factor),
                              act.fct = "logistic",
                              threshold = param.bench.neuralnet.threshold, # default = 0.01
                              #linear.output = FALSE,
                              lifesign = "full"
              );tend <- Sys.time()
              
              #summary(nn)
              bench.time <- ceiling(10*difftime(tend,t0,units = 'mins'))/10
              print(bench.time)
              
              bench.neuralnet.preds <- compute(nn, bench.neuralnet.test[, 1:nbvar])
              bench.neuralnet.preds.res <- bench.neuralnet.preds$net.result
              # head(bench.neuralnet.preds.res)
              
              original_values <- max.col(bench.neuralnet.test[, (nbvar+1):(nbvar+nb.factor)])
              bench.neuralnet.preds.res.class <- max.col(bench.neuralnet.preds.res)
              # confusionMatrix ?!
              res.confmat <- confusionMatrix(bench.neuralnet.preds.res.class, original_values)
              bench.neuralnet.acc <- mean(bench.neuralnet.preds.res.class == original_values)*100
              
              
              tend <- Sys.time()
              res.time <- difftime(tend, t0, units = 'secs')
              res.accuracy <- bench.neuralnet.acc
              save_results()
              
              bench.neuralnet_classifier.accuracy <- sprintf("[%s] nlevels = %d, size hidden = %d, thresh = %s, var = %d, lines = %d, Accuracy = %0.2f %%, time = %smin", 
                                                             param.mutate.subcat.cat, 
                                                             nb.factor,
                                                             param.bench.neuralnet.size_hidden, 
                                                             param.bench.neuralnet.threshold,
                                                             nbvar,
                                                             nblines,
                                                             bench.neuralnet.acc,
                                                             bench.time
              )
              print(bench.neuralnet_classifier.accuracy) 
              
              
            }
            
            
          }
          
          if(param.bench.pcaNNet) {
            
            cat('\n','------------------------------------')
            cat('\n','PCANET :\n')
            
            gc()
            t0 <- Sys.time()
            res.model <- 'pcaNNet'
            
            bench.pcaNNet = pcaNNet(x = as.matrix(bench.dtm_train),
                                    y = bench.train[['category']],
                                    size = param.bench.pcaNNet.size, 
                                    MaxNWts = (param.bench.pcaNNet.size * (dim(bench.dtm_train)[2] + 1) + 1), 
                                    # trace = FALSE, 
                                    thresh = param.bench.pcaNNet.thresh
            )
            
            bench.test$bench.pcaNNet.preds <- predict(bench.pcaNNet , newdata = as.matrix(bench.dtm_test), type = 'class')
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            res.confmat <- confusionMatrix(bench.test$bench.pcaNNet.preds, bench.test$category)
            res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != bench.pcaNNet.preds]))/dim(bench.test)[[1]]
            save_results()
            
            bench.pcaNNet.preds.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
            print(bench.pcaNNet.preds.accuracy) #  avec PCA : "Accuracy : 54.50 %"
          }
          
          if(param.bench.nnet.multinom) {
            suppressWarnings(suppressMessages(library(nnet)))
            
            cat('\n','------------------------------------')
            cat('\n','NNET MULTINOM :\n')
            
            gc()
            t0 <- Sys.time()
            res.model <- 'multinom'
            
            nnet.size <- nlevels( bench.train[['category']]) # 7
            
            nnet.df.dtm_train <- as.data.frame(cbind(as.matrix(bench.dtm_train), bench.train[['category']]))
            
            colnames(nnet.df.dtm_train)[dim(nnet.df.dtm_train)[2]] <- 'category'
            nnet_model <- multinom(category ~ ., data = nnet.df.dtm_train, MaxNWts = (nnet.size * (dim(nnet.df.dtm_train)[2] + 1) + 1))
            nnet.preds <- predict(nnet_model, as.matrix(bench.dtm_test), type = 'class')
            
            #head(nnet.preds)
            bench.test$nnet.preds.class <- factor(nnet.preds, labels = unique(levels(bench.train$category)))
            #head(bench.test$nnet.preds.class)
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            res.confmat <- confusionMatrix(bench.test$nnet.preds.class, bench.test$category)
            res.accuracy <- 100*(dim(bench.test)[[1]] - count(bench.test[category != nnet.preds.class]))/dim(bench.test)[[1]] 
            save_results()
            
            bench.pcanet.accuracy <- sprintf("Accuracy : %0.2f %%", res.accuracy)
            print(bench.pcanet.accuracy) #  avec PCA : "Accuracy : 54.50 %"
          }
          
          # --------------- Neural Network : neuralnet, long 13 min pour 0.5% de NAnotechology avec 60% Accuracy VS 77% cv.glmnet
          # param.mutate.subcat.cat <- c('Nanotechnology')
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 3, var = 110, lines = 1021, Accuracy = 65.90 %, time = 0.4min"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 5, var = 187, lines = 1531, Accuracy = 67.02 %, time = 1.1min (glmnet: Accuracy : 74.66 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 7, var = 187, lines = 1531, Accuracy = 62.90 %, time = 1.2min (glmnet: Accuracy : 74.66 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 7, var = 263, lines = 2041, Accuracy = 67.28 %, time = 3.8min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 3, var = 263, lines = 2041, Accuracy = 61.10 %, time = 7.4min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 10, var = 263, lines = 2041, Accuracy = 68.99 %, time = 2.2min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 10, thresh = 0.005, var = 263, lines = 2041, Accuracy = 69.34 %, time = 2.4min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 10, thresh = 0.001, var = 263, lines = 2041, Accuracy = 68.42 %, time = 3.2min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 20, thresh = 0.05, var = 263, lines = 2041, Accuracy = 70.25 %, time = 1.4min (glmnet: Accuracy : 76.20 %)"
          # [1] "[Nanotechnology] nlevels = 3, size hidden = 20, thresh = 0.05, var = 830, lines = 2041, Accuracy = 62.93 %, time = 90.7min (glmnet: Accuracy : 75.06 %)"
          
          if(param.bench.neuralnet)
          {
            cat('\n','------------------------------------')
            cat('\n','Neural Network (neuralnet) :\n')
            
            suppressWarnings(suppressMessages(library(neuralnet)))
            suppressWarnings(suppressMessages(library(nnet)))
            
            gc()
            t0 <- Sys.time()
            res.model <- 'neuralnet'
            
            #levels(bench.test[['category']])  
            res.factor <- levels(bench.test[['category']]) 
            nb.factor <- length(res.factor)
            
            nbvar <- dim(as.matrix(bench.dtm_train))[[2]]
            nblines <- dim(as.matrix(bench.dtm_train))[[1]]
            cols <- colnames(bench.dtm_train)
            
            bench.neuralnet.train <- cbind(as.matrix(bench.dtm_train), class.ind(as.factor(bench.train[['category']])))
            bench.neuralnet.test <- cbind(as.matrix(bench.dtm_test), class.ind(as.factor(bench.test[['category']])))
            
            scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
            bench.neuralnet.train <- as.data.table(bench.neuralnet.train)
            bench.neuralnet.test <- as.data.table(bench.neuralnet.test)
            
            names(bench.neuralnet.train) <- c(colnames(bench.dtm_train), res.factor)
            names(bench.neuralnet.test) <- c(colnames(bench.dtm_test), res.factor)
            
            bench.neuralnet.train[, (cols) := lapply(.SD, scl), .SDcols=cols]
            bench.neuralnet.test[, (cols) := lapply(.SD, scl), .SDcols=cols]
            
            f <- as.formula(paste(paste(sprintf("`%s`", res.factor), collapse="+"), "~", paste(sprintf("`%s`", cols), collapse="+")))
            
            nn <- neuralnet(f,
                            data = bench.neuralnet.train,
                            hidden = c(nbvar, param.bench.neuralnet.size_hidden, nb.factor),
                            # hidden = c(100, 20, nb.factor),
                            act.fct = "logistic",
                            threshold = param.bench.neuralnet.threshold, # default = 0.01
                            # linear.output = FALSE,
                            lifesign = "full"
            );tend <- Sys.time()
            
            summary(nn)
            bench.time <- ceiling(10*difftime(tend,t0,units = 'mins'))/10
            print(bench.time)
            
            bench.neuralnet.preds <- compute(nn, bench.neuralnet.test[, 1:nbvar])
            bench.neuralnet.preds.res <- bench.neuralnet.preds$net.result
            # head(bench.neuralnet.preds.res)
            
            original_values <- max.col(bench.neuralnet.test[, (nbvar+1):(nbvar+nb.factor)])
            bench.neuralnet.preds.res.class <- max.col(bench.neuralnet.preds.res)
            # confusionMatrix ?!
            res.confmat <- confusionMatrix(bench.neuralnet.preds.res.class, original_values)
            bench.neuralnet.acc <- mean(bench.neuralnet.preds.res.class == original_values)*100
            
            
            tend <- Sys.time()
            res.time <- difftime(tend, t0, units = 'secs')
            res.accuracy <- bench.neuralnet.acc 
            save_results()
            
            bench.neuralnet_classifier.accuracy <- sprintf("[%s] nlevels = %d, size hidden = %d, thresh = %s, var = %d, lines = %d, Accuracy = %0.2f %%, time = %smin", 
                                                           param.mutate.subcat.cat, 
                                                           nb.factor,
                                                           param.bench.neuralnet.size_hidden, 
                                                           param.bench.neuralnet.threshold,
                                                           nbvar,
                                                           nblines,
                                                           bench.neuralnet.acc,
                                                           bench.time
            )
            print(bench.neuralnet_classifier.accuracy) 
            
            
          }
          
          # print(difftime(Sys.time(), t0, units = 'mins'))
        }
      }
    }
  }
}



# --------------- SVM e1071, KO memory et predict

# 
# # library(e1071)
# # dt.bench.dtm_train <- as.data.frame(cbind(as.matrix(bench.dtm_train), as.vector(bench.train[['category']])))
# # colnames(dt.bench.dtm_train)[dim(dt.bench.dtm_train)[2]] <- 'category'
# # bench.svm <- e1071::svm(category ~ ., dt.bench.dtm_train) # Error: cannot allocate vector of size 50.0 Gb
# # bench.svm <- e1071::svm(as.factor(bench.train[['category']]) ~ as.matrix(bench.dtm_train))
# # svm.predict <- predict(bench.svm, newdata = as.matrix(bench.dtm_test))
# # KO, predict pas bon à cause de la formule => http://stackoverflow.com/questions/4462118/predict-svm-does-not-predict-new-data
# # bench.test$svm.predict.class <- svm.predict
# # 
# # dt.bench.dtm_train <- as.data.frame(cbind(as.matrix(bench.dtm_train), as.vector(bench.train[['category']])))
# # colnames(dt.bench.dtm_train)[dim(dt.bench.dtm_train)[2]] <- 'category'
# # # KO : Error: cannot allocate vector of size 50.0 Gb
# # bench.svm <- e1071::svm(category ~ ., data = dt.bench.dtm_train)
# 


# --------------- NNET KO Memory

# library(nnet)
# # Error: cannot allocate vector of size 50.0 Gb
# nnet_model <- nnet(category ~ ., data = dt.bench.dtm_train, size = 2)

# ## a tester : softmax : 
# ## -- http://stackoverflow.com/questions/19862478/classification-in-r-neuralnet
# idC <-class.ind(Train$y)
# NN1=nnet(Train, idC[Train], size=15, maxit = 200, softmax=TRUE)
# predict(NN1, data=Test,type = "class")

# --------------- caret nn KO : There were missing values in resampled performance measures
## -- https://stats.stackexchange.com/questions/21717/how-to-train-and-validate-a-neural-network-model-in-r

# library(caret)
# library(tidyr)
# KO pareil
# caret.fit <- train(x = as.matrix(bench.dtm_train),
#                    y = bench.train[['category']],
#                    method = "nnet", size = 5, MaxNWts = 25000,
#                    maxit = 30)

# pcanet.model <- pcaNNet(x = as.matrix(bench.dtm_train),
#                          y = bench.train[['category']],
#                          size = 5, MaxNWts = 10000, thresh = 0.95 )
# # weights:  13302
# # initial  value 16882.917811 
# # final  value 7808.000000 
# # converged
# 
# pcanet.preds <- predict(pcanet.model, newdata = as.matrix(bench.dtm_test))
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
# dt.bench.dtm_train <- as.data.frame(cbind(as.matrix(bench.dtm_train), bench.train[['category']]))
# colnames(dt.bench.dtm_train)[dim(dt.bench.dtm_train)[2]] <- 'category'
# 
# dt.bench.dtm_test <- as.data.frame(cbind(as.matrix(bench.dtm_test), bench.test[['category']]))
# colnames(dt.bench.dtm_test)[dim(dt.bench.dtm_test)[2]] <- 'category'
# 
# pcanet.model.2 <- multinom(category ~ ., data = dt.bench.dtm_train, MaxNWts = 210000)
# 
# pcanet.model.2.preds <- predict(pcanet.model.2, newdata = dt.bench.dtm_test)
# 
# pcanet.preds.2 <- factor(pcanet.model.2.preds, labels = unique(levels(bench.test$category)))
# bench.test$pcanet.preds.2 <- pcanet.preds.2
# 
# bench.pcanet.accuracy <- sprintf("Accuracy : %0.2f %%", 100*(dim(bench.test)[[1]] - count(bench.test[category != pcanet.preds.2]))/dim(bench.test)[[1]])
# 
# print(bench.pcanet.accuracy) # "Accuracy : 74.77 %"




# --------------- SVM liquidSVM, KO : pas reussi a l'utiliser, pb avec data

## - http://www.isa.uni-stuttgart.de/software/R/demo.html

# library(liquidSVM)

# KO no data
# model <- liquidSVM::mcSVM(x = as.matrix(bench.dtm_train),
#                y = as.vector(bench.train[['category']]),
#                mc_type="AvA_hinge"
#                # useCells=TRUE,threads=3
# )

# dt.bench.dtm_train <- as.data.frame(cbind(as.matrix(bench.dtm_train), as.vector(bench.train[['category']])))
# colnames(dt.bench.dtm_train)[dim(dt.bench.dtm_train)[2]] <- 'category'

# bench.svm <- liquidSVM::svm(category ~ ., dt.bench.dtm_train, useCells=TRUE) # KO error
# colnames(dt.bench.dtm_train) <- paste0('`',colnames(dt.bench.dtm_train),'`')
# bench.svm <- liquidSVM::svm(category ~ ., dt.bench.dtm_train, useCells=TRUE) # KO error
# f <- paste("category ~", paste(sprintf("`%s`", colnames(dt.bench.dtm_train)), collapse="+"))
# bench.svm <- liquidSVM::svm(formula(f), dt.bench.dtm_train, useCells=TRUE) # KO error

# KO aussi
# bench.svm <- svm(x = dt.bench.dtm_train[-dim(dt.bench.dtm_train)[2]], 
#                  y =  dt.bench.dtm_train[dim(dt.bench.dtm_train)[2]])

# KO aussi
# model <- mcSVM(formula(f), dt.bench.dtm_train,
#                mc_type="AvA_hinge",
#                useCells=TRUE, threads=3
#                )


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
#   [1] "bench.glmnet_classifier.time: 0.38m"
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


# ------------------------------------  param.mutate.subcat.cat <- c('Health')
#   Categories to learn :
#   <Cancer> <Diseases, Conditions, Syndromes> <Genetics> <Health> <HIV & AIDS> <Medical research> <Medications> <Neuroscience> <Psychology & Psychiatry>
#   
#   [1/1]Testing model with param.pctdata = 1 
#   [1] "Train nb articles = 20947"
#   [1] "bench.train_tokens.time: 8.85s"
#   [1] "Do ngram : bench.train.vocab.stem.time: 8.27s"
#   [1/1]Testing model with prune = (count_min=150, prop_max=0.9, prop_min=0) 
#   [1] "bench.train.vocab.stem.prune.time: 0.30s"
#   [1] "bench.dtm_train.time: 4.55s"
#   [1] "bench.dtm_test: 2.08s"
#   [1/1]Training model with param.cv.nfold = 3 
#   model 121 - text2vect tfidf cv.glmnet : glmnet.params = ALPHA:1, NFOLDS:3, THRESH:0.01, MAXIT:100 + featureh=FALSE, stem=FALSE, ngram=TRUE, prune=TRUE :  prune.params = countmin:150, doc.prop.max:0.9, doc.prop.min:0 
#   [1] "bench.glmnet_classifier.time: 1.30m"
#   [1] "bench.preds.class: 0.23s"
#   [1] "Accuracy : 74.36 %"
#   # A tibble: 9 × 4
#   category     n       pct accuracy
#   <fctr> <int>     <dbl>    <dbl>
#     1                          Cancer  1442 16.065062 90.08322
#   2                          Health  2055 22.894385 83.94161
#   3                      HIV & AIDS   204  2.272727 80.39216
#   4         Psychology & Psychiatry   701  7.809715 79.17261
#   5                Medical research  1933 21.535205 70.15003
#   6 Diseases, Conditions, Syndromes  1448 16.131907 65.26243
#   7                        Genetics   406  4.523173 57.14286
#   8                    Neuroscience   481  5.358734 52.80665
#   9                     Medications   306  3.409091 47.38562
#   [1] "Model 121 saved (1.3 min): pct data=1, dim_train=(20947, 6084), Accuracy : 74.36 %"
#   Time difference of 1.649866 mins
#   


## --------- Autres sav

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

## - http://www.quantide.com/multilabel-classification-neuralnet-package/
## - https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
## - https://gist.github.com/mick001/5973654a443a79b5d1b911a22c00e487



