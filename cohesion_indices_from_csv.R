#####################################################################
# Calculate Cohesion Indices for a Text based on lexical repetition #
# Author: Davi Alves Oliveira                                       #
# Date: 22/09/2022                                                  #
#####################################################################

# Load packages
library(tidyverse)

# List csv files
dir = 'corpora/oanc_csv/'
files = list.files(dir)

for (file in files) {
  # Create the indices table
  indices = tibble(
    text = character(),
    clique_id = character(),
    index = character(),
    v = numeric(),
    e = numeric()
  )
  
  text_name = file %>% str_sub(0,-5)
  annotated_text = read_csv(paste0(dir, file)) %>%
    mutate(clique_id = clique_id %>% as.character())
  
  # Global Backward Cohesion Indices ---------------------------------------------
  data = annotated_text %>%
    group_by(clique_id) %>%
    distinct(stem, .keep_all = T) %>%
    mutate(synonyms = synonyms %>% str_remove_all('\\(a\\)|\\(p\\)'))
  
  global_backward_cohesion = tibble(
    text = character(),
    clique_id = character(),
    r = integer(),
    m_c = integer(),
    q_n = integer(),
    n_iprev = integer(),
    v = double(),
    e = double()
  )
  
  for(q in (data %>% .$clique_id %>% unique())) {
    q_i = data %>% filter(clique_id == q)
    G_i = data %>% filter(clique_id < q)
    
    global_backward_cohesion = global_backward_cohesion %>%
      bind_rows(tibble(
        text = text_name,
        clique_id = q,
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
  q = data %>% .$clique_id %>% unique()
  
  local_backward_cohesion = tibble(
    text = character(),
    clique_id = character(),
    r = integer(),
    m_c = integer(),
    q_n = integer(),
    q_n_iprev = integer(),
    v = double(),
    e = double()
  )
  
  for(i in 1:length(q)) {
    q_i = data %>% filter(clique_id == q[i])
    
    if (i > 1) {
      q_j = data %>% filter(clique_id == q[i - 1])
    } else {
      q_j = tibble(clique_id = character(),
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
        clique_id = q[i],
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
    clique_id = character(),
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
    q_i = data %>% filter(clique_id == q[i])
    
    q_n = q_i %>% nrow()
    
    for (j in 1:length(q)) {
      if (j > i) {
        q_j = data %>% filter(clique_id == q[j])
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
        clique_id = q[i],
        v = mean(pairwise_indices_v[i,], na.rm = T),
        e = mean(pairwise_indices_e[i,], na.rm = T)
      ))
  }
  
  # Table with all the indices
  indices = indices %>%
    bind_rows(
      global_backward_cohesion %>%
        select(text, clique_id, v, e) %>%
        mutate(index = 'global')) %>%
    bind_rows(
      local_backward_cohesion %>%
        select(text, clique_id, v, e) %>%
        mutate(index = 'local')) %>%
    bind_rows(
      mean_pairwise_cohesion %>%
        select(text, clique_id, v, e) %>%
        mutate(index = 'pairwise'))
  
  write_csv(indices, paste0('corpora/indices_oanc/', file))
}
