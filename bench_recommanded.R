####################################################
## Script created by Sébastien Desfossés (2017/04)

setwd("~/Dev/Git/R - Phys.org")

{
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(tidytext)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(text2vec)))
  suppressWarnings(suppressMessages(library(tm)))
  suppressWarnings(suppressMessages(library(wordcloud2)))
  suppressWarnings(suppressMessages(library(RColorBrewer)))
  suppressWarnings(suppressMessages(library(recommenderlab)))
  suppressWarnings(suppressMessages(library(tidyr)))
  suppressWarnings(suppressMessages(library(ggplot2)))
}

# Find actif user data ---------------------------------------------------------------
{
  param.lemmatized = TRUE
  param.recommanded.real = TRUE
  param.nbmin_artcomments = 20
  # param.nbmin_usercomments = 50 #10
  # param.nbmax_usercomments = 500 #300
  # param.nbmin_userarticles = 25 #5
  # param.nbmax_userarticles = 200
  full_subcat_sample_size <- 100
  param.clean_content.file <- 'data/physorg_bagofwords_d.art.c.bench_d.user_d.com.RData'
  
  # d.art.c.bench <- d.art.c.bench.org
  load(param.clean_content.file)
  d.art.c.bench.org <- d.art.c.bench
  
  qplot(d.com$nbarticlecomments, geom='histogram', bins = 100, xlim = c(5,300))
  
  # d.user.actifs <- d.user[nbcom >= param.nbmin_usercomments & 
  #                           nbcom <= param.nbmax_usercomments &
  #                           nbart >= param.nbmin_userarticles & 
  #                           nbart <= param.nbmax_userarticles]
  
  
  d.user.actifs <- d.user
  d.user.actifs$user <- droplevels(d.user.actifs$user)
  
  d.com.user.actifs <- d.com[nbarticlecomments >= param.nbmin_artcomments &
                               user %in% d.user.actifs$user] %>%
    group_by(user, url) %>%
    summarise(n = n(), rank = mean(rank), wc = sum(wc_comment)) %>%
    setDT
  
  # d.com.user.actifs <- d.com[nbarticlecomments >= param.nbmin_artcomments & 
  #                              user %in% d.user.actifs$user]
  
  d.com.user.actifs$user <- droplevels(d.com.user.actifs$user)
  d.art.com.user.actifs <- d.art.c.bench[url %in% d.com.user.actifs$url]
  d.art.com.user.actifs$subcategory <- droplevels(d.art.com.user.actifs$subcategory)
  
  # d.com.user.actifs %>% group_by(user) %>% summarise(mean_rank = mean(rank), comments = n(), wc = sum(wc_comment)) %>% arrange(-mean_rank, -comments, -wc)
  # d.com.user.actifs %>% group_by(user) %>% summarise(comments = n(), mean_rank = mean(rank),  wc = sum(wc_comment)) %>% arrange(-comments)
  
  d.art.c.bench <- d.art.com.user.actifs
  
  d.art.c.bench.url <- d.art.c.bench %>% select(id, url)
  d.art.c.bench$url <- NULL
  
  if(!param.lemmatized) {
    d.art.c.bench$content <- d.art.c.bench$content.nolem
  } 
  
  d.art.c.bench$content.nolem <- NULL
  d.art.c.bench$content.org <- NULL
  
  d.art.c.bench.sample <- d.art.c.bench[0,]
  for(l in levels(d.art.c.bench$subcategory)) {
    nb_lines_subcat <- min(full_subcat_sample_size, dim(d.art.c.bench[subcategory == l])[[1]])
    sample_subcat <- sample_n(d.art.c.bench[subcategory == l], nb_lines_subcat)
    d.art.c.bench.sample <- rbind(d.art.c.bench.sample, sample_subcat)
  }
  
  rm(list = c('d.art.com.user.actifs','d.com','d.user'))
  
  cat('\nActif users: ',dim(d.user.actifs)[[1]])
  cat('\nDocuments of actif users: ', dim(d.art.c.bench)[[1]],'\n')
  
  gc()
}


# PARAMS ------------------------------------------------------------------
{
  ## -- PIPLINE --
  param.dotfidf = TRUE
  param.dostem = TRUE
  param.dongram = TRUE
  param.doprune = TRUE
  param.dofeaturehashing = FALSE # incompatible avec prune
  
  ## -- CAT / SUB CAT --
  param.mutate.subcat.as.cat = TRUE
  
  param.cat <- c('Astronomy & Space','Other Sciences','Technology','Physics', 'Nanotechnology','Health', 'Biology', 'Earth','Chemistry')
  
  param.mutate.subcat.cat <- c('Other Sciences')
  
  param.dorpsc <- c('Other', 'Business Hi Tech & Innovation',
                    'Health Social Sciences','Pediatrics','Overweight and Obesity','Cardiology','Sleep apnea','Medicine & Health',
                    'Ecology Biotechnology', 'Cell & Microbiology Biotechnology',
                    'Materials Science')
  
  ## -- PCT USED DATA 
  param.pctdata.default = 1
  
  ## -- MAX DATA
  param.nblines_max.default = 1000^10
  
  param.train_test <- 0.7
  
  ## -- PRUNE --
  param.prune.term_count_min.default = 80 
  param.prune.doc_proportion_max.default = 1 # (default pkg 1)
  param.prune.doc_proportion_min.default = 0 # (default pkg 0)
  
  param.hngram = 2 ** 18
  param.seed = 20170416
  
}


# FUNCTIONS  -------------------------------------------------------------------
{
  
}

# INIT -------------------------------------------------------------------

{
  set.seed(param.seed)
  
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
  
  i_cat = 1
  if(param.mutate.subcat.as.cat) {
    d.art.c.bench <- d.art.c.bench %>%
      filter(category == param.cat[i_cat]) %>%
      select(-category) %>%
      mutate(category = subcategory) %>%
      select(-subcategory) %>%
      mutate(category = droplevels(category)) %>%
      setDT() %>%
      setkey(id)
  }
}


#### TFIDF
{
  cat('\n','------------------------------------')
  cat('\n','Categories to learn :\n')
  cat(paste0('<',levels(d.art.c.bench$category), '>'))
  
  param.pctdata <<- param.pctdata.default
  
  param.nblines_max <<- param.nblines_max.default
  param.num_sample = min(param.nblines_max, ceiling(param.pctdata * dim(d.art.c.bench)[[1]]))
  bench.all_ids = d.art.c.bench$id
  bench.train_ids = sample(bench.all_ids, param.num_sample)
  d.bench <- d.art.c.bench[J(bench.train_ids)] %>% 
    mutate(category = droplevels(category)) %>% 
    setDT()
  
  # SDE ?!
  # d.bench[,id := (.I)]
  setkey(d.bench, id)
  
  bench.num_sample = ceiling(param.train_test * dim(d.bench)[[1]])
  bench.all_ids = d.bench$id
  bench.train_ids = sample(bench.all_ids, bench.num_sample)
  bench.test_ids = setdiff(bench.all_ids, bench.train_ids)
  bench.train = d.bench[J(bench.train_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
  bench.test = d.bench[J(bench.test_ids)] %>% mutate(category = droplevels(category)) %>% setDT() 
  
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
  
  param.prune.term_count_min <<- param.prune.term_count_min.default
  param.prune.doc_proportion_max <<- param.prune.doc_proportion_max.default
  param.prune.doc_proportion_min <<- param.prune.doc_proportion_min.default
  
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
}


{
  # http://stackoverflow.com/questions/30629522/error-in-using-recommenderlab-package-in-r
  # https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/
  cat('\n Recommanded sys','------------------------------------')
  
  param.test_useridx = 2
  
  pobj <- c('param.recommanded.real','afm','param.test_useridx', 'afm.bin','afm.real', 'd.recommanded.users', 'd.recommanded.bin', 'd.recommanded.real', 'd.com.user.actifs', 'd.art.c.bench.url')
  
  rm(list = setdiff(ls(), pobj))
  gc()
  
  if(param.recommanded.real) 
  {
    d.recommanded.real <- d.com.user.actifs %>% 
      left_join(d.art.c.bench.url[,c('url', 'id')]) %>% 
      # select(user, id, rank) %>%
      select(user, id, n) %>%
      unique() %>%
      group_by(user, id) %>% 
      # summarise(comments = ifelse(is.na(mean(rank, na.rm = TRUE)), NA, mean(rank, na.rm = TRUE))) %>%
      summarise(comments = ifelse(is.na(mean(n, na.rm = TRUE)), NA, mean(n, na.rm = TRUE))) %>%
      # spread(id, comments, fill = 0, convert = TRUE) %>%
      spread(id, comments) %>%
      setDT
    
    which(d.recommanded.real[param.test_useridx,-1] != 0)
    d.recommanded.real[param.test_useridx, 1 + which(d.recommanded.real[param.test_useridx,-1] != 0), with = FALSE]
    
    d.recommanded.real.mat <- as.matrix(d.recommanded.real[,2:dim(d.recommanded.real)[[2]]])
    # d.recommanded.real.mat2[d.recommanded.real.mat2 == 0] <- NA
    rownames(d.recommanded.real.mat) <- d.recommanded.real$user
    
    afm.real <- as(d.recommanded.real.mat, "realRatingMatrix")
    
    d.recommanded.users <- d.recommanded.real$user
    
    gc()
    dim(afm.real)
  }
  else 
  {
    
    # d.recommanded.bin <- copy(d.recommanded.real)
    # d.recommanded.bin[!is.na(d.recommanded.bin), -1] <- 1
    # d.recommanded.bin[!is.na(d.recommanded.bin)] <- 1
    # d.recommanded.bin[, (names(d.recommanded.bin[,-1])):=lapply(.SD, 
    #                                                             function(c) 
    #                                                               ifelse(c == 0, 1, ifelse(is.na(c),0,1))), .SDcols = names(d.recommanded.bin[,-1])]
    
    d.recommanded.bin <- d.com.user.actifs %>% 
      left_join(d.art.c.bench.url[,c('url', 'id')]) %>% 
      select(user, id, rank) %>%
      # select(user, id, n) %>%
      unique() %>%
      group_by(user, id) %>% 
      summarise(comments = ifelse(is.na(mean(rank, na.rm = TRUE)), 0, 1)) %>%
      # summarise(comments = ifelse(is.na(mean(n, na.rm = TRUE)), 0, 1)) %>%
      spread(id, comments, fill = 0, convert = TRUE) %>%
      setDT
    
    which(d.recommanded.bin[param.test_useridx,-1] != 0)
    d.recommanded.bin[param.test_useridx, 1 + which(d.recommanded.bin[param.test_useridx,-1] != 0), with = FALSE]
    
    d.recommanded.bin.mat <- as.matrix(d.recommanded.bin[,2:dim(d.recommanded.bin)[[2]]])
    rownames(d.recommanded.bin.mat) <- d.recommanded.bin$user
    
    afm.bin <- as(d.recommanded.bin.mat, "binaryRatingMatrix")
    
    d.recommanded.users <- d.recommanded.bin$user
    
    gc()
    dim(d.recommanded.bin)
  }
  
  
  ## ----------------
  
  if(param.recommanded.real) {
    afm <- afm.real
    
    reco.model <- Recommender(afm,
                              method="UBCF",
                              param=list(normalize = "Z-score",method="Cosine",nn=5)
    )
    
    summary(getRatings(afm))
    
    qplot(getRatings(afm), binwidth = .1, 
          main = "Histogram of ratings", xlab = "Rating", log = 'x')
    
    qplot(getRatings(normalize(afm, method = "Z-score")), binwidth = .1,
          main = "Histogram of normalized ratings", xlab = "Rating", log = 'x')
    
    summary(getRatings(normalize(afm, method = "Z-score")))
    
    
    image(sample(afm, 1000), main = "Raw ratings")
    
    qplot(rowCounts(afm), binwidth = .1, 
          main = "Document Rated on average", 
          xlab = "# of users", 
          ylab = "# of movies rated", 
          log = 'x')
    
    qplot(colMeans(afm), binwidth = .1, 
          main = "Mean rating of Movies", 
          xlab = "Rating", 
          ylab = "# of movies", xlim = c(0,25))
    
  }
  else 
  {
    afm <- afm.bin
    reco.model <- Recommender(afm, method = 'UBCF')
  }
  
  topitems <- predict(reco.model, afm[param.test_useridx,], n=5)
  
  topitems
  as(topitems, 'list')
  
  best3 <- bestN(topitems, n = 3)
  best3
  as(best3, 'list')
  
  
  ## ----------------
  
  recommenderRegistry$get_entry_names()
  recommenderRegistry$get_entries(dataType = "realRatingMatrix")
  recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
  
  scheme <- evaluationScheme(afm, method = "split", train = .9,
                             k = 1, given = 0, goodRating = 0)
  
  scheme
  
  algorithms <- list(
    "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
    "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
    "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                   method="Cosine",
                                                   nn=10)),
    # "SVD" = list(name="SVD", param=list(normalize = "Z-score")),
    "item-based CF" = list(name="IBCF", param=list())
  )
  
  # run algorithms, predict next n movies
  results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
  
  # Draw ROC curve
  plot(results, annotate = 1:4, legend="topleft", ylim = c(0,0.005))
  
  # See precision / recall
  plot(results, "prec/rec", annotate=3, xlim = c(0,1), ylim = c(0,1))
  
  # ## simple split with 3 items given
  # esSplit <- evaluationScheme(MSWeb10, method="split",
  #                             train = 0.9, k=1, given=3)
  # esSplit
  # 
  # ## 4-fold cross-validation with all-but-1 items for learning.
  # esCross <- evaluationScheme(MSWeb10, method="cross-validation",
  #                             k=4, given=-1)
  # esCross
}


param.statdesc = FALSE
if(param.statdesc)
{
  cat('\n Cosine similary','------------------------------------')
  gc()
  
  test_idx_doc = 100 
  test_url_doc = 'http://phys.org/news/2016-02-sustainability-social-important-profit.html'
  
  # http://phys.org/news/2014-03-chicken-bones-true-story-pacific.html
  # http://phys.org/news/2008-08-reveals-chooks.html
  
  # bench.dtm_train.dist = dist2(bench.dtm_train)
  bench.dt_train.sim <- as.data.table(as.matrix(sim2(bench.dtm_train, method = 'cosine')))
  bench.dt_train.sim$id_doc <- as.numeric(colnames(bench.dt_train.sim))
  
  ## Tests idx ##
  
  test_id_doc = as.numeric(colnames(bench.dt_train.sim[,test_idx_doc, with=FALSE]))
  
  d.art.c.bench.url[id == test_id_doc]$url
  # d.art.c.bench[id == test_id_doc]$content
  
  op <- par(mfrow = c(1, 2))
  boxplot(bench.dt_train.sim[,as.character(test_id_doc), with = FALSE])
  boxplot(bench.dt_train.sim[id_doc != test_id_doc  ,as.character(test_id_doc), with = FALSE])
  op <- par(mfrow = c(1, 1))
  
  bench.dt_train.sim[,c('id_doc',as.character(test_id_doc)), with = FALSE] %>% 
    arrange(desc(.[[2]])) %>%
    filter(between(row_number(), 2, 11)) %>%
    mutate(id_doc = as.numeric(id_doc)) %>%
    left_join(d.art.c.bench.url, by = c('id_doc' = 'id'))
  
  ## Tests url ##
  
  test_id_doc = d.art.c.bench.url[url == test_url_doc]$id
  test_url_doc
  test_id_doc
  
  op <- par(mfrow = c(1, 2))
  boxplot(bench.dt_train.sim[,as.character(test_id_doc), with = FALSE])
  boxplot(bench.dt_train.sim[id_doc != test_id_doc, as.character(test_id_doc), with = FALSE])
  op <- par(mfrow = c(1, 1))
  
  bench.dt_train.sim[,c('id_doc',as.character(test_id_doc)), with = FALSE] %>% 
    arrange(desc(.[[2]])) %>%
    filter(between(row_number(), 2, 11)) %>%
    mutate(id_doc = as.numeric(id_doc)) %>%
    left_join(d.art.c.bench.url, by = c('id_doc' = 'id'))
}