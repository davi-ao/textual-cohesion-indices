# Load functions
# Obs: these lines also install and load the packages tidyverse and wordnet
source('get_word_synonyms.R')
source('get_word_hypernyms.R')

library(SnowballC)

dir = 'oanc/'
files = list.files(dir, '*_clean.xml')

msd_keep = c('CD', 'FW', 'JJ', 'JJR', 'JJS', 'NN', 'NNS', 'NNP', 'NNPS',
             'RB', 'RBR', 'RBS', 'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')

for (f in files) {
  text_name = f %>% str_sub(0, -5)
  text_xml = read_file(paste0(dir, text_name, '.xml'))
  
  fs_tags = text_xml %>% 
    str_match_all('(?<=<fs>)(.|[\\n\\r])*?(?=<\\/fs>)') %>%
    .[[1]] %>%
    .[,1] %>%
    str_remove_all('[\\n\\r]') %>%
    str_replace_all('\\s{2,}', ' ') %>%
    str_trim()
  
  text_tibble = fs_tags %>%
    str_match_all('name="(\\S+)" value="(\\S+)"') %>%
    sapply(function(fs) {
      if (any(fs[,3] %>% str_detect('[.:?!…]'))) {
        '.'
      } else {
        paste0(fs[,2], '_', fs[,3], collapse = '|')
      }
    }) %>%
    paste0(collapse = ' ') %>%
    str_split(' . ') %>%
    .[[1]] %>%
    tibble(token = .) %>%
    mutate(sentence_id = row_number()) %>%
    separate_rows(token, sep = ' ') %>%
    mutate(token_id = row_number()) %>%
    separate_rows(token, sep = '\\|') %>% 
    separate(token, c('name', 'value'), '_') %>%
    pivot_wider(c(sentence_id, token_id), 
                names_from = name, 
                values_from = value) %>%
    select(sentence_id, base, msd, affix) %>%
    rename(clique_id = sentence_id, lemma = base, upos = msd) %>%
    mutate(stem = wordStem(lemma)) %>%
    filter(upos %in% msd_keep)
  
  synonyms_hypernyms = text_tibble %>%
    distinct(lemma, upos) %>%
    mutate(synonyms = mapply(function(l, p){get_word_synonyms(l, p)}, 
                             lemma, 
                             upos) %>% 
             unlist()) %>%
    mutate(hypernyms = mapply(function(l, p){get_word_hypernyms(l, p)}, 
                              lemma, 
                              upos) %>% 
             unlist())
  
  text_tibble = text_tibble %>%
    left_join(synonyms_hypernyms, by = c('lemma', 'upos'))
  
  write_csv(text_tibble, paste0('corpora/oanc_csv/', text_name, '.csv'))
}
