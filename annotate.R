# Install packages, if necessary
#install.packages('udpipe')
#install.packages('SnowballC')

# Load additional packages
library(udpipe)
library(SnowballC)

# Load functions
# Obs: these lines also install and load the packages tidyverse and wordnet
source('get_word_synonyms.R')
source('get_word_hypernyms.R')

# List text files
dir = 'corpus/txt/'
files = list.files(dir, pattern='^S')

for (file in files) {
  text_name = file %>% str_sub(0,-5)
  
  text_name = file %>% str_sub(0,-5)
  text = read_file(paste0(dir, file))
  
  # Data cleansing -------------------------------------------------------------
  clean_text = text %>%
    # Remove numbered external references (e.g. [1])
    str_remove_all('\\[\\d+\\]') %>%
    # Replace numbered internal references (e.g. (1)) with spaces
    str_replace_all('\\(\\d+\\)', ' ') %>%
    # Remove html elements
    str_replace_all('<[^<>]+>.*<\\/[^<>]+>', ' ') %>%
    # Remove 's
    str_remove_all('’s') %>%
    # Remove symbols (except sentence boundaries)
    str_remove_all('[‘’:%,]') %>%
    # Replace other symbols with space (e.g. hyphens)
    str_replace_all('[-–\\(\\)\\/\\n-]', ' ') %>%
    # Remove section references (e.g. 1.1, 1.1.1)
    str_remove_all('\\d+(.\\d+)+') %>%
    # Remove common abbreviations
    str_remove_all('i\\.e\\.|e\\.g\\.|et al\\.') %>%
    # Replace numbers with their names
    str_replace_all('0', 'zero') %>%
    str_replace_all('1', 'one') %>%
    str_replace_all('2', 'two') %>%
    str_replace_all('3', 'three') %>%
    str_replace_all('4', 'four') %>%
    str_replace_all('5', 'five') %>%
    str_replace_all('6', 'six') %>%
    str_replace_all('7', 'seven') %>%
    str_replace_all('8', 'eight') %>%
    str_replace_all('9', 'nine') %>%
    # Remove points inside expressions
    str_replace_all('(\\s\\w+)\\.(\\w+\\s?)', '\\1\\2') %>%
    # Remove 'i' used as numbers (i, ii, iii)
    str_replace_all('\\si+\\s', ' ') %>%
    # Replace know abbreviations with their full forms
    str_replace_all('[Ff]ig\\.', 'figure') %>%
    str_replace_all('[Ee]q\\.', 'equation') %>%
    # Remove all non-word character (except .)
    str_remove_all('[^\\w\\s\\.]') %>%
    # Replace multiple whitespaces with a single whitespace
    str_replace_all('\\s+', ' ')
  
  # Annotate text --------------------------------------------------------------
  # Select the udpipe model
  model = udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')
  
  # List the universal part of speech (upos) that will be removed
  stopupos = c('PUNCT', 'SYM', 'NUM', 'INTJ', 'X',
               'PRON', 'ADP', 'AUX', 'CCONJ', 'DET', 'PART', 'SCONJ')
  
  # Split sentences and words
  tokenized_text = clean_text %>%
    tibble(text = .) %>%
    separate_rows(text, sep = '[.:?!]') %>%
    mutate(text = text %>% str_trim() %>% str_to_lower(),
           clique = row_number()) %>%
    separate_rows(text, sep = ' ') %>%
    rename(token = text) %>%
    distinct() %>%
    filter(token != '')
  
  annotated_text = tokenized_text %>%
    .$token %>%
    udpipe_annotate(model, x = ., tokenizer = 'vertical', parser = 'none') %>%
    as_tibble() %>% 
    select(token, lemma, upos) %>%
    bind_cols(tokenized_text %>%
                select(clique)) %>%
    filter(!upos %in% stopupos & !str_detect(lemma, '\\W')) %>%
    mutate(stem = wordStem(lemma)) %>%
    group_by(clique) %>%
    distinct() %>%
    ungroup()
  
  synonyms_hypernyms = annotated_text %>%
    distinct(lemma, upos) %>%
    mutate(synonyms = mapply(function(l, p){get_word_synonyms(l, p)}, 
                             lemma, 
                             upos) %>% 
             unlist()) %>%
    mutate(hypernyms = mapply(function(l, p){get_word_hypernyms(l, p)}, 
                              lemma, 
                              upos) %>% 
             unlist())
  
  annotated_text = annotated_text %>%
    left_join(synonyms_hypernyms, by = c('lemma', 'upos'))
  
  write_csv(annotated_text, paste0('corpus/csv/', text_name, '.csv'))
}
