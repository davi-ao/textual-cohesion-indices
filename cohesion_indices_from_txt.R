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
library(jtools)

# Set APA theme for the plots
theme_set(theme_apa())

# Load text
text_name = 'text2'
text = read_file(paste0(text_name, '.txt'))

# Data cleansing ---------------------------------------------------------------
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

# Global Backward Cohesion Indices ---------------------------------------------
data = annotated_text %>%
  group_by(clique) %>%
  distinct(stem, .keep_all = T) %>%
  mutate(synonyms = synonyms %>% str_remove_all('\\(a\\)|\\(p\\)'))

global_backward_cohesion = tibble(
  text = character(),
  clique = integer(),
  r = integer(),
  m_c = integer(),
  q_n = integer(),
  n_iprev = integer(),
  v = double(),
  e = double()
)

for(q in (data %>% .$clique %>% unique())) {
  q_i = data %>% filter(clique == q)
  G_i = data %>% filter(clique < q)
  
  global_backward_cohesion = global_backward_cohesion %>%
    bind_rows(tibble(
      text = text_name,
      clique = q,
      r = intersect(q_i %>% .$stem, G_i %>% .$stem) %>% length(),
      m_c = (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma) %in% 
        (c(G_i %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           G_i %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      q_n = q_i %>% nrow(),
      n_iprev = G_i %>% nrow(),
      v = ifelse(
        n_iprev == 0,
        0,
        ifelse(
          q_n > 0 && r < q_n && r < n_iprev,
          r/q_n,
          1
        )),
      e = ifelse(
        q_n > 0 && n_iprev > 0 && r < q_n && r < n_iprev, 
        m_c/((q_n - r)*(n_iprev - r)), 
        0)
    ))
}

# Local Backward Cohesion Indices ----------------------------------------------
q = data %>% .$clique %>% unique()

local_backward_cohesion = tibble(
  text = character(),
  clique = integer(),
  r = integer(),
  m_c = integer(),
  q_n = integer(),
  q_n_iprev = integer(),
  v = double(),
  e = double()
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
      text = text_name,
      clique = q[i],
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
      m_c = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      q_n = q_i %>% nrow(),
      q_n_iprev = q_j %>% nrow(),
      v = ifelse(
        q_n_iprev == 0,
        0,
        ifelse(
          q_n > 0 && r < q_n && r < q_n_iprev,
          r/q_n,
          1
        )),
      e = ifelse(
        q_n > 0 && q_n_iprev > 0 && r < q_n && r < q_n_iprev, 
        m_c/((q_n - r)*(q_n_iprev - r)), 
        0)
    ))
}

# Mean Pairwise Cohesion Indices -----------------------------------------------
mean_pairwise_cohesion = tibble(
  text = character(),
  clique = integer(),
  v = double(),
  e = double()
)

pairwise_indices_v = matrix(rep(NA, length(q)^2), length(q), length(q))
rownames(pairwise_indices_v) = q
colnames(pairwise_indices_v) = q
pairwise_indices_e = matrix(rep(NA, length(q)^2), length(q), length(q))
rownames(pairwise_indices_e) = q
colnames(pairwise_indices_e) = q

for(i in 1:length(q)) {
  q_i = data %>% filter(clique == q[i])
  
  q_n = q_i %>% nrow()
  
  for (j in 1:length(q)) {
    if (j > i) {
      q_j = data %>% filter(clique == q[j])
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length()
      m_c = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum()
      n_j = q_j %>% nrow()
      pairwise_indices_v[i,j] = r/q_n
      pairwise_indices_v[j,i] = r/q_n
      pairwise_indices_e[i,j] = m_c/((q_n - r)*(n_j - r))
      pairwise_indices_e[j,i] = m_c/((q_n - r)*(n_j - r))
    }
  }
}

for(i in 1:length(q)) {
  mean_pairwise_cohesion = mean_pairwise_cohesion %>%
    bind_rows(tibble(
      text = text_name,
      clique = q[i],
      v = mean(pairwise_indices_v[i,], na.rm = T),
      e = mean(pairwise_indices_e[i,], na.rm = T)
    ))
}

# Create the indices table if it doesn't exist
#indices = tibble(
#  text = character(),
#  clique = numeric(),
#  index = character(),
#  v = numeric(),
#  e = numeric()
#)

# Table with all the indices
indices = indices %>%
  bind_rows(
    global_backward_cohesion %>%
      select(text, clique, v, e) %>%
      mutate(index = 'global')) %>%
  bind_rows(
    local_backward_cohesion %>%
      select(text, clique, v, e) %>%
      mutate(index = 'local')) %>%
  bind_rows(
    mean_pairwise_cohesion %>%
      select(text, clique, v, e) %>%
      mutate(index = 'pairwise'))

# Indices for the texts
indices %>%
  group_by(text, index) %>%
  summarise(v = mean(v),
            e = mean(e))

# Plot of cohesion indices
indices %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(text = text %>% 
           as_factor() %>%
           recode('text1' = 'Text 1', 'text2' = 'Text 2'),
         index = index %>%
           as_factor() %>%
           recode('global' = 'Θ - Global Backward Cohesion',
                  'local' = 'Ξ - Local Backward Cohesion',
                  'pairwise' = 'Η - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edges Cohesion',
                  'v' = 'Vertices Cohesion')) %>%
  ggplot(aes(clique, value, color = index, linetype = index)) +
  facet_grid(rows = vars(type), cols = vars(text), scales = 'free_y') +
  geom_line() +
  scale_x_continuous(breaks = 1:10) + 
  xlab('Clique') +
  ylab('Value') +
  theme(legend.position = 'bottom', legend.direction = 'vertical') +
  scale_color_brewer(palette = 'Dark2')

ggsave('Figura6.png', 
       device = 'png', 
       width = 16, 
       height = 16, 
       units = 'cm', 
       dpi = 300)
