
{
  assign("last.warning", NULL, envir = baseenv())
  suppressPackageStartupMessages(library(dplyr))
  library(tidyr)
  library(ggplot2)
  library(mongolite)
  library(RJSONIO)
  library(jsonlite)
  library(data.table)
  library(tidyjson)
  library(stringr)
  library(magrittr)
  library(scales)
  library(summarytools)
  #detach("package:plyr", unload=TRUE) 
} 

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


if(!exists('d.art')) 
{
  
  {
    #---- Constantes ----
    # rm(list=ls())
    env.usebdd <- FALSE
    env.datadir <- 'data'
    env.db.url <- '127.0.0.1'
    env.db.name <- 'physorgdb'
    env.db.collection.articles <- 'articles'
    env.db.collection.comments <- 'comments'
    env.db.user <- 'user1'
    env.db.pwd <- 'pwd1'
    env.db.file.articles <- 'physorgdb_articles_20170322.json'
    env.db.file.comments <- 'physorgdb_comments_20170322.json'
    env.db.url <- paste0(env.db.user,':', env.db.pwd, '@', env.db.url, '/', env.db.name)
    
    db.rq.fields.notxt <- paste0('{"',
                                 '_id":1',
                                 ',"url":1',
                                 ',"dtpublication" : 1',
                                 ',"category":1',
                                 ',"subcategory":1',
                                 '}')
    ggp.pointsize = 3
    ggp.linesize = 1.2
    ggp.titlesize = 30
    ggp.textsize = 22
    ggp.stripsize = 25
    ggp.jitter = 100
    ggp.boxplot.txtsize = 8
    
    param.effectif_minimum_group_sousgroupe = 1
  }
  
  {
    
    #---- Chargement des donnees
    
    if (exists('d.art.org')) {
      d.art <- d.art.org
    } 
    else {
      if(env.usebdd) {
        db.art <- mongo(db = env.db.name, 
                        collection = env.db.collection.articles, 
                        url = paste0('mongodb://', env.db.url))
        # db.art.info <- db.art$info()
        d.art.org <- data.table(db.art$find('{}'))
      } 
      else {
        d.art <- flatten(stream_in(file(paste0(env.datadir,'/',env.db.file.articles))))
        #d.art <- data.table(d.art)
        d.com <- flatten(stream_in(file(paste0(env.datadir,'/',env.db.file.comments))))
        #d.comments <- data.table(flatten(stream_in(file(paste0(env.datadir,'/',env.db.file.comments)))))
      }
      
      #d.art$dtdownload <- as.POSIXct(d.art$dtdownload, origin = ISOdatetime(1970,01,01,00,00,00))
      #d.art$dtpublication <- as.POSIXct(d.art$dtpublication, origin = ISOdatetime(1970,01,01,00,00,00))
      
      d.art$subcategory_ <- ifelse(
        d.art$category == 'org/health-news/,health-news', 
        d.art$category_, 
        d.art$subcategory_)
      #d.art[category == 'org/health-news/,health-news', subcategory_ := category_]
      
      d.art$category_[d.art$category == 'org/health-news/,health-news'] <- 'Health'
      #d.art[category == 'org/health-news/,health-news', category_ := 'Health']
      
      d.art$category[d.art$category == 'org/health-news/,health-news'] <- 'Health'
      #d.art[category == 'org/health-news/,health-news', category := 'Health']
      
      d.art.org <- d.art
    }
    
    # d.art <- data.table(db.art$find('{}', fields = db.rq.fields.notxt))
    # d.art <- data.table(db.art$find('{"category":"Physics"}', sort = '{"dtdownload": -1}', limit = 1000)
    # mongoimport --port 8087 --db cities --collection l.articles --file /home/mkrawier/data/l.articles.json  
    # KO tmp.var <- read_json(paste0('data/', env.db.file.articles))
    # KO tmp.var <- fromJSON(paste0('data/', env.db.file.articles))
    
    #OK d.art <- data.table(flatten(stream_in(file(paste0('data/', env.db.file.articles)))))
    #OK d.art.cnx <- data.table(db.art$find('{}'))
    #OK d.artfull.cnx <- data.table(db.artfull$find('{}'))
    
  }
  
  if(!exists('d.art'))
  {
    #---- Enrichissements / conversion / nettoyage des donnees
    
    d.art %<>% filter(errorcode == '0' | (is.na(errorurl) & is.na(errormsg) & is.na(errorcode))) #%>% setDT()
    #d.art <- d.art[errorcode == '0' | (is.na(errorurl) & is.na(errormsg) & is.na(errorcode)),]
    
    d.art %<>% filter(category != 'news')  #%>% setDT()
    #d.art <- d.art[category != 'news',]
    
    d.art$nbshare <- ifelse(d.art$nbshare_>0, d.art$nbshare_, d.art$nbshare)
    #d.art[nbshare_>0, nbshare := nbshare_]
    
    d.art$nbshare[is.na(d.art$nbshare)] <- 0
    
    d.art$nbcomments <- ifelse(d.art$nbcomments__>0, d.art$nbcomments__, d.art$nbcomments_)
    #d.art[nbcomments__>0, nbcomments := nbcomments__]
    
    d.art$nbcomments[is.na(d.art$nbcomments)] <- 0
    
    d.art$category <- ifelse(is.na(d.art$category), d.art$category_, d.art$category)
    d.art$category_ <- NA # SDE !! bug rstudio ? affiche des warnings sinon
    #d.art[is.na(d.art$category), category := category_]
    
    d.art$summary <- ifelse(d.art$content != '', d.art$longsummary, d.art$shortsummary)
    d.art$content <- ifelse(d.art$content == '', d.art$longsummary, d.art$content)
    #d.art[content != '', summary := longsummary]
    #d.art[content == '', summary := shortsummary][, content := longsummary]
    
    d.art %<>%
      mutate(contain_slash = grepl('/',subcategory_)) %>%
      mutate(subcategory2_ = ifelse(
        contain_slash, 
        gsub("^(.*)[\r\n].*[\r\n][ \t]*(.*)$", "\\2", subcategory_), 
        NA)) %>%
      mutate(subcategory_ = ifelse(
        contain_slash, 
        gsub("^(.*)[\r\n].*[\r\n][ \t]*(.*)$", "\\1", subcategory_), 
        subcategory_)) %>%
      mutate(subcategory2_ = ifelse(
        is.na(subcategory2_) & subcategory != subcategory_, 
        str_replace_all(subcategory, subcategory_ , ""),
        subcategory2_)) %>%
      mutate(contain_slash = NULL) # %>% setDT()
    
    d.art$subcategory <- ifelse(
      !is.na(d.art$subcategory_) & d.art$subcategory_ != '', 
      d.art$subcategory_, 
      d.art$subcategory) 
    
    d.art$subcategory2 <- d.art$subcategory2_
    
    d.art$category <- as.factor(d.art$category)
    d.art$subcategory <- as.factor(d.art$subcategory)
    d.art$subcategory2 <- as.factor(d.art$subcategory2)
    
    d.art %<>% 
      group_by(category, subcategory) %>%
      mutate(total = n()) %>%
      filter(total >= param.effectif_minimum_group_sousgroupe)
    
    d.art$nbshare_ <- NULL
    d.art$nbcomments_ <- NULL
    d.art$nbcomments__ <- NULL
    d.art$category_ <- NULL
    d.art$site <- NULL
    d.art$dtdownload <- NULL
    d.art$dtdownload_ <- NULL
    d.art$archiveurl <- NULL
    d.art$nbarticlesarchive <- NULL
    d.art$`__v` <- NULL
    d.art$keywords <- NULL
    d.art$errorurl <- NULL
    d.art$errormsg <- NULL
    d.art$errorcode <- NULL
    d.art$longsummary <- NULL
    d.art$shortsummary <- NULL
    d.art$subcategory_ <- NULL
    d.art$subcategory2_ <- NULL
    #d.art[, c('category_', 'nbshare_', 'nbcomments_', 'nbcomments__', 'site', '__v') := NULL]
    #d.art[, c('dtdownload', 'dtdownload_', 'errorurl', 'errorcode', 'errormsg') := NULL]
    #d.art[, c('archiveurl', 'nbarticlesarchive',  'longsummary', 'shortsummary') := NULL]
    #d.art[, c('subcategory_', 'subcategory2_') := NULL]
    
    d.art$wc_title <- sapply(gregexpr("[[:alpha:]]+", d.art$title), function(x) sum(x > 0))
    d.art$wc_summary <- sapply(gregexpr("[[:alpha:]]+", d.art$summary), function(x) sum(x > 0))
    d.art$wc_content <- sapply(gregexpr("[[:alpha:]]+", d.art$content), function(x) sum(x > 0))
    d.art$nbtags <- lapply(d.art$tags, function(x) length(unlist(x)))
    d.art$nbtags <- as.numeric(d.art$nbtags)
    d.art$nbrelated <- lapply(d.art$related, function(x) length(unlist(x)))
    d.art$nbrelated <- as.numeric(d.art$nbrelated)
    
    d.art$category_ <- NA 
    d.art$provider <- as.factor(d.art$provider)
    d.art$publication <- as.factor(d.art$publication)
    d.art$journal <- as.factor(d.art$journal)
    d.art <- data.table(d.art)
    # d.art[, category_ := NULL]
    
    
    d.com <- data.table(d.com)
    d.com$rank <- as.numeric(d.com$rank)
    d.com[is.na(rank), rank := 0]
    d.com$user <- as.factor(d.com$user)
    d.com$wc_comment <- sapply(gregexpr("[[:alpha:]]+", d.com$comment), function(x) sum(x > 0))
    
    # today <- format(Sys.Date(), "%b %d, %Y")
    #format(as.Date(c('04 03 2017'),"%d %m %Y"),"%b %d, %Y")
    #str_replace_all(c('qsdFevmfl jMaraaezrq'), c('Jan' = 'janv.','' = 'f?vr.'))
    Sys.getlocale("LC_TIME") # "French_France.1252"
    Sys.setlocale("LC_TIME", "C")
    d.com[dtcomment %like% 'ago', dtcomment := format(dtdownload,"%b %d, %Y")]
    d.com$dtcomment <- as.POSIXct(d.com$dtcomment, format = '%b %d, %Y', origin = ISOdatetime(1970,01,01,00,00,00))
    Sys.setlocale("LC_TIME", "French_France.1252")
    d.com[, dtdownload := NULL]
    d.com[, c('__v') := NULL]
    
    d.user <- d.com %>%
      group_by(user, url) %>%
      summarize(
        ncom_by_url = n(), 
        mean_rank_by_url = mean(rank), 
        mean_word_by_com = mean(wc_comment),
        total_word_by_url = sum(wc_comment),
        min_dt_by_url = min(dtcomment),
        max_dt_by_url = max(dtcomment)) %>%
      group_by(user) %>%
      summarize(
        nbcom = sum(ncom_by_url), 
        nbart = n(),
        mean_combyart = mean(ncom_by_url), 
        mean_rank = mean(mean_rank_by_url),
        mean_wordbycom = mean(mean_word_by_com),
        mean_wordbyart = mean(total_word_by_url),
        total_word = sum(total_word_by_url),
        dt_firstcom = min(min_dt_by_url),
        dt_lastcom = max(max_dt_by_url)) %>%
      arrange(-nbcom) %>%
      setDT()
    
    d.user$nb_months <- as.numeric(d.user$dt_lastcom - d.user$dt_firstcom, units = "days") * 12 / 365
    
    rm(d.art.org, d.com.org)
  }
}


{
  
  #---------- Tests divers (non ex?cut?s)
  
  # d.art %>% 
  #   group_by(category, subcategory) %>%
  #   summarize(total = n()) %>%
  #   arrange(total)
  # 
  # tmp.var <- filter(d.art, is.na(subcategory))
  # tmp.var <- d.art %>% 
  #   filter(nbshare_>0, 10*nbshare_ != (nbshare-nbcomments), 100*nbshare_ != (nbshare-nbcomments))
  # tmp.var <- d.art %>% filter(is.na(category_))
  # tmp.var <- d.art %>% filter(!is.na(category_))
  # tmp.var <- d.art %>% filter(!is.na(category_), category_ != category)
  # tmp.var <- d.art %>% filter(!is.na(category_), category != 'Health', category_ != category)
  # tmp.var <- d.art %>% filter(!is.na(category_), as.character(category_) != as.character(category))
  # tmp.var <- d.art %>% filter(!is.na(subcategory_), as.character(subcategory_) != as.character(subcategory))
  # tmp.var <- d.art %>% filter(!is.na(subcategory_), as.character(subcategory_) == as.character(subcategory))
  # tmp.var <- tmp.var2 %>% 
  #   select(url, category, subcategory, subcategory_, subcategory2_) %>% 
  #   filter(category != 'Health', subcategory_ != subcategory)
  # tmp.var <- d.art %>% filter(grepl('/',subcategory_))
  # tmp.Var <- d.art %>% filter(!is.na(errormsg), errormsg != '')
  # tmp.var <- d.art.org %>% 
  #   select(url, category, category_, subcategory, subcategory_) %>%
  #   mutate(contain_slash = grepl('/',subcategory_)) %>%
  #   mutate(subcategory2_ = ifelse(contain_slash, 
  #                                 gsub("^(.*)[\r\n].*[\r\n][ \t]*(.*)$", "\\2", subcategory_),
  #                                 '')) %>%
  #   mutate(subcategory1_ = ifelse(contain_slash, 
  #                                 gsub("^(.*)[\r\n].*[\r\n][ \t]*(.*)$", "\\1", subcategory_), 
  #                                 subcategory_)) %>%
  #   filter(category != 'org/health-news/,health-news', subcategory1_ != subcategory)
  # 
  # tmp.var2 <- tmp.var %>% 
  #   filter(contain_slash == FALSE) %>%
  #   mutate(subcategory3_ = str_replace_all(subcategory, subcategory_ , ""))
  # 
  # tmp.var2 <- d.art %>%
  #   mutate(
  #     subcategory2_ = ifelse(is.na(subcategory2_) & subcategory != subcategory_, 
  #                            str_replace_all(subcategory, subcategory_ , ""),
  #                            subcategory2_)
  #   )
  
}