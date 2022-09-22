#####################################################################
# Calculate Cohesion Indices for a Text based on lexical repetition #
# Author: Davi Alves Oliveira                                       #
# Date: 22/09/2022                                                  #
#####################################################################

# Load functions
# Obs: these lines also install and load the packages tidyverse and wordnet
source('get_word_synonyms.R')
source('get_word_hypernyms.R')

# Install packages, if necessary
#install.packages('udpipe')
#install.packages('SnowballC')

# Load additional packages
library(udpipe)
library(SnowballC)

# Load text
text = read_file('text1.txt')

# Data cleansing-----------------------------------------------------------------
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

# Annotate text ----------------------------------------------------------------
# Select the udpipe model
model = udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

# List the universal part of speech (upos) that will be removed
stopupos = c('PUNCT', 'SYM', 'NUM', 'INTJ', 'X',
             'PRON', 'ADP', 'AUX', 'CCONJ', 'DET', 'PART', 'SCONJ')

annotated_text = clean_text %>%
  udpipe_annotate(model, .) %>%
  as_tibble() %>% 
  select(sentence_id, token, lemma, upos) %>%
  filter(!upos %in% stopupos & !str_detect(lemma, '\\W')) %>%
  mutate(lemma = lemma %>% str_to_lower(),
         token = token %>% str_to_lower()) %>%
  mutate(stem = wordStem(lemma)) %>%
  group_by(sentence_id) %>%
  distinct() %>%
  ungroup() %>%
  rename(clique = sentence_id)

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

# Global Backward Cohesion Index -----------------------------------------------
data = annotated_text %>%
  group_by(clique) %>%
  distinct(stem, .keep_all = T) %>%
  mutate(synonyms = synonyms %>% str_remove_all('\\(a\\)|\\(p\\)'))

global_backward_cohesion = tibble(
  text = character(),
  clique = integer(),
  r = integer(),
  m_v = integer(),
  n_qi = integer(),
  n_iprev = integer(),
  g = double()
)

for(q in (data %>% .$clique %>% unique())) {
  q_i = data %>% filter(clique == q)
  G_i = data %>% filter(clique < q)
  
  global_backward_cohesion = global_backward_cohesion %>%
    bind_rows(tibble(
      text = 'text1',
      clique = q,
      r = intersect(q_i %>% .$stem, G_i %>% .$stem) %>% length(),
      m_v = (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma) %in% 
        (c(G_i %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           G_i %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      n_qi = q_i %>% nrow(),
      n_iprev = G_i %>% nrow(),
      g = (r + m_v)/(n_qi + ((n_qi - r) * (n_iprev - r)))))
}

# Local Backward Cohesion Index ------------------------------------------------
q = data %>% .$clique %>% unique()

local_backward_cohesion = tibble(
  text = character(),
  clique = integer(),
  r = integer(),
  m_v = integer(),
  n_qi = integer(),
  n_qj = integer(),
  b = double()
)

for(i in 1:length(q)) {
  q_i = data %>% filter(clique == q[i])
  
  if (i > 1) {
    q_j = data %>% filter(clique == q[i - 1])
  } else {
    q_j = tibble(clique = integer(),
                 token = character(),
                 lemma = character(),
                 upos = character(),
                 stem = character(),
                 synonyms = character(),
                 hypernyms = character(),
                 article = character())
  }
  
  local_backward_cohesion = local_backward_cohesion %>%
    bind_rows(tibble(
      text = 'text1',
      clique = q[i],
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
      m_v = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      n_qi = q_i %>% nrow(),
      n_qj = q_j %>% nrow(),
      b = (r + m_v)/(n_qi + ((n_qi - r) * (n_qj - r)))))
}

# Local Forward Cohesion Index -------------------------------------------------
local_forward_cohesion = tibble(
  text = character(),
  clique = integer(),
  r = integer(),
  m_v = integer(),
  n_qi = integer(),
  n_qj = integer(),
  f = double()
)

for(i in 1:length(q)) {
  q_i = data %>% filter(clique == q[i])
  
  if (i < length(q)) {
    q_j = data %>% filter(clique == q[i + 1])
  } else {
    q_j = tibble(clique = integer(),
                 token = character(),
                 lemma = character(),
                 upos = character(),
                 stem = character(),
                 synonyms = character(),
                 hypernyms = character(),
                 article = character())
  }
  
  local_forward_cohesion = local_forward_cohesion %>%
    bind_rows(tibble(
      text = 'text1',
      clique = q[i],
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
      m_v = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      n_qi = q_i %>% nrow(),
      n_qj = q_j %>% nrow(),
      f = (r + m_v)/(n_qi + ((n_qi - r) * (n_qj - r)))))
}

# Mean Pairwise Cohesion Index -------------------------------------------------
mean_pairwise_cohesion = tibble(
  text = character(),
  clique = integer(),
  p = double()
)

for(i in 1:length(q)) {
  q_i = data %>% filter(clique == q[i])
  
  indices = double()
  n_qi = q_i %>% nrow()
  
  for (j in 1:length(q)) {
    if (j != i) {
      q_j = data %>% filter(clique == q[j])
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length()
      m_v = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum()
      n_qj = q_j %>% nrow()
      indices = c(indices, (r + m_v)/(n_qi + ((n_qi - r) * (n_qj - r))))
    }
  }
  
  mean_pairwise_cohesion = mean_pairwise_cohesion %>%
    bind_rows(tibble(
      text = 'text1',
      clique = q[i],
      p = mean(indices)))
}
