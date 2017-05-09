####################################################
## Script created by Sébastien Desfossés (2017/04)

setwd("~/Dev/Git/R - Phys.org")

{
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(tidytext)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(text2vec)))
}

# LOAD DATA ---------------------------------------------------------------
{
  
  suppressWarnings(suppressMessages(library(tm)))
  
  param.dataorg.file <- 'data/physorg.RData'
  param.clean_content.file <- 'data/glmnet_cleancontent_catsubcat.lemmatized_full_merged_sum.RData'
  
  
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
        mutate(content = str_replace_all(content, "[^[A-Za-z]'-]", " ")) %>%
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
        lemmatize_strings() %>% 
        str_replace_all(' - ', '-') #vstr_replace_all(' - ', ' ') #%>% str_replace_all(' - ', '-')
    }
    
  } 
  
  d.art.c.bench[, content := removeWords(content, c('category','can','say', 'will', 'use'))]
  
  protected.obj <- c('d.art.c.bench', 'd.com', 'd.user')
  rm(list = setdiff(ls(), protected.obj))
  
  gc()
}

