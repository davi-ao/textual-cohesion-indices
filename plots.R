library(tidyverse)
library(jtools)
library(ggpattern)

theme_set(theme_apa())

indices_oanc = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/oanc_indices/'
files_indices_oanc = list.files(dir)

for (f in files_indices_oanc) {
  indices_oanc = indices_oanc %>%
    bind_rows(read_csv(paste0(dir, f)))
}

indices_pseudotext = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/oanc_indices_pseudotexts/'
files_indices_pseudo = list.files(dir, 'pseudotext_*')

for (f in files_indices_pseudo) {
  indices_pseudotext = indices_pseudotext %>%
    bind_rows(read_csv(paste0(dir, f)))
}

# Prepare tibble for plotting
indices = indices_oanc %>%
  mutate(genre = text %>% 
           str_match('([a-z]+)_') %>% 
           .[,2],
         corpus = 'text') %>%
  bind_rows(indices_pseudotext %>%
              mutate(genre = 'pseudo',
                     corpus = 'pseudo')) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         value = ifelse(value == Inf, 0, value))

# Plot mean indices
indices %>%
  mutate(index = index %>%
           recode('θ - Global Backward Cohesion' = 'Text Global Backward Cohesion',
                  'λ - Local Backward Cohesion' = 'Text Local Backward Cohesion',
                  'ρ - Mean Pairwise Cohesion' = 'Text Mean Pairwise Cohesion'),
         corpus = corpus %>%
           as_factor() %>%
           recode('text' = 'Texts',
                  'pseudo' = 'Pseudotexts'),
         genre = genre %>%
           as_factor() %>%
           recode('berlitz' = 'Travel guides',
                  'biomed' = 'Biomedical research articles',
                  'icic' = 'Letters',
                  'media' = 'Government media',
                  'plos' = 'Scientific and medical articles',
                  'pseudo' = 'Pseudotexts',
                  'slate' = 'Magazine articles')) %>%
  group_by(text, index, corpus, type, genre) %>%
  summarize(`Mean indices` = mean(value)) %>%
  ggplot(aes(corpus, `Mean indices`, fill = genre)) +
  geom_boxplot() +
  facet_wrap(type ~ index, scales = 'free') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('')

ggsave('figure6.png',
       device = 'png',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

ggsave('figure6.svg',
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Plot distribution of cohesion indices
# Density
indices %>%
  mutate(corpus = corpus %>%
           as_factor() %>%
           recode('text' = 'Texts',
                  'pseudo' = 'Pseudotexts')) %>%
  ggplot(aes(value, 
             fill = corpus, 
             group = corpus, 
             pattern = corpus)) +
  facet_wrap(type ~ index, scales = 'free') +
  geom_density_pattern(alpha = .5,
                       pattern_fill = 'black',
                       pattern_color = 'black') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('Value') +
  ylab('Density')

ggsave('figure7.png', 
       device = 'png', 
       width = 24.7, 
       height = 16, 
       units = 'cm', 
       dpi = 300)

ggsave('figure7.svg', 
       device = 'svg', 
       width = 24.7, 
       height = 16, 
       units = 'cm', 
       dpi = 300)

# Empirical probabilities of global vertex cohesion indices
for (p in seq(.1, .9, .1)) {
  indices %>%
    filter(index == 'θ - Global Backward Cohesion', 
           type == 'Vertex Cohesion') %>%
    group_by(corpus) %>%
    summarise(p = p, Mean = mean(value >= p)) %>%
    pivot_wider(names_from = 'corpus', values_from = Mean) %>%
    mutate(diff = pseudo - text) %>%
    print()
}

# Empirical probabilities of local vertex cohesion indices
for (p in seq(.1, .9, .1)) {
  indices %>%
    filter(index == 'λ - Local Backward Cohesion', 
           type == 'Vertex Cohesion') %>%
    group_by(corpus) %>%
    summarise(p = p, Mean = mean(value >= p)) %>%
    pivot_wider(names_from = 'corpus', values_from = Mean) %>%
    mutate(diff = pseudo - text) %>%
    print()
}

# Empirical probabilities of parwise vertex cohesion indices
for (p in seq(.1, .4, length.out = 9)) {
  indices %>%
    filter(index == 'ρ - Mean Pairwise Cohesion', 
           type == 'Vertex Cohesion') %>%
    group_by(corpus) %>%
    summarise(p = p, Mean = mean(value >= p)) %>%
    pivot_wider(names_from = 'corpus', values_from = Mean) %>%
    mutate(diff = pseudo - text) %>%
    print()
}

# Partial text indices
indices_partial = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double(),
  group_id = double()
)

dir = 'corpora/oanc_indices_partial_global_local/'
files = list.files(dir)

for (f in files) {
  indices_partial = indices_partial %>%
    bind_rows(
      read_csv(paste0(dir, f)) %>%
        mutate(sentences = f %>% str_sub(1, 3) %>% str_remove_all('_')))
}

dir = 'corpora/oanc_indices_partial_pairwise/'
files = list.files(dir)

for (f in files) {
  indices_partial = indices_partial %>%
    bind_rows(
      read_csv(paste0(dir, f)) %>%
        mutate(sentences = f %>% str_sub(1, 3) %>% str_remove_all('_')))
}

# Global Backward Cohesion
indices_partial %>%
  mutate(sentences = sentences %>% as.numeric(),
         corpus = ifelse(text %>% str_detect('pseudotext'), 
                         'Pseudotexts', 
                         'Texts') %>%
           as_factor() %>%
           relevel('Texts')) %>%
  filter(index == 'global' & sentences <= 60) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         value = ifelse(value == Inf, 0, value)) %>%
  ggplot(aes(value,
             fill = corpus, 
             group = corpus)) +
  facet_grid(type ~ sentences, scales = 'free') +
  geom_histogram(colour = 'black') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('Value') +
  ylab('Frequency')

ggsave('figure8_histograms.png', 
       device = 'png', 
       width = 24.7, 
       height = 12.35,
       units = 'cm', 
       dpi = 300)

# Local Backward Cohesion
indices_partial %>%
  mutate(sentences = sentences %>% as.numeric(),
         corpus = ifelse(text %>% str_detect('pseudotext'), 
                         'Pseudotexts', 
                         'Texts') %>%
           as_factor() %>%
           relevel('Texts')) %>%
  filter(index == 'local' & sentences <= 60) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         value = ifelse(value == Inf, 0, value)) %>%
  ggplot(aes(value,
             fill = corpus, 
             group = corpus)) +
  facet_grid(type ~ sentences, scales = 'free') +
  geom_histogram(colour = 'black') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('Value') +
  ylab('Frequency')

ggsave('figure9_histograms.png', 
       device = 'png', 
       width = 24.7, 
       height = 12.35,
       units = 'cm', 
       dpi = 300)

# Mean Pairwise Cohesion
indices_partial %>%
  mutate(sentences = sentences %>% as.numeric(),
         corpus = ifelse(text %>% str_detect('pseudotext'), 
                         'Pseudotexts', 
                         'Texts') %>%
           as_factor() %>%
           relevel('Texts')) %>%
  filter(index == 'pairwise' & sentences <= 60) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         value = ifelse(value == Inf, 0, value)) %>%
  ggplot(aes(value,
             fill = corpus, 
             group = corpus)) +
  facet_grid(type ~ sentences, scales = 'free') +
  geom_histogram(colour = 'black') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('Value') +
  ylab('Frequency')

ggsave('figure10_histograms.png', 
       device = 'png', 
       width = 24.7, 
       height = 12.35,
       units = 'cm', 
       dpi = 300)
