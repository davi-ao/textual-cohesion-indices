library(tidyverse)

indices_oanc = tibble(
  text = character(),
  clique_id = double(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/indices_oanc/'
files_indices_oanc = list.files(dir)

for (f in files_indices_oanc) {
  indices_oanc = indices_oanc %>%
    bind_rows(read_csv(paste0(dir, f)))
}

indices_pseudotext = tibble(
  text = character(),
  clique_id = character(),
  index = character(),
  v = double(),
  e = double()
)

dir = 'corpora/indices_pseudotexts/'
files_indices_pseudo = list.files(dir)

for (f in files_indices_pseudo) {
  indices_pseudotext = indices_pseudotext %>%
    bind_rows(read_csv(paste0(dir, f)) %>%
                select(-clique))
}

# Plot of cohesion indices (distribution)
indices_oanc %>%
  mutate(clique_id = clique_id %>% as.character(),
         corpus = 'oanc') %>%
  bind_rows(indices_pseudotext %>%
              mutate(corpus = 'pseudo')) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion')) %>%
  ggplot(aes(value, ..scaled.., fill = corpus)) +
  facet_wrap(type ~ index, scales = 'free') +
  geom_density(alpha = .5) +
  theme(legend.position = 'bottom', legend.direction = 'vertical') +
  scale_color_brewer(palette = 'Dark2')

#ggsave('FiguraX.png', 
#       device = 'png', 
#       width = 16, 
#       height = 16, 
#       units = 'cm', 
#       dpi = 300)