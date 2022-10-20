library(tidyverse)

text1_indices = read_csv('corpus/indices/S0020025508004520.csv') %>%
  group_by(clique) %>%
  mutate(clique_id = cur_group_id()) %>%
  ungroup()
pseudotext1_indices = read_csv('corpus/indices_pseudotexts/S0020025508004520.csv') %>%
  group_by(clique_id) %>%
  mutate(clique_id = cur_group_id()) %>%
  ungroup()

indices = text1_indices %>%
  select(-clique) %>%
  bind_rows(pseudotext1_indices %>%
              select(-clique))

# Plot of cohesion indices
indices %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value') %>%
  mutate(text = text %>% 
           as_factor() %>%
           recode('text1' = 'Text 1', 'text2' = 'Text 2'),
         index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion'),
         type = type %>%
           as_factor() %>%
           recode('e' = 'Edges Cohesion',
                  'v' = 'Vertices Cohesion')) %>%
  ggplot(aes(clique, value, color = index, linetype = index, shape = index)) +
  facet_grid(rows = vars(type), cols = vars(text), scales = 'free_y') +
  geom_line() +
  geom_point() +
  xlab('Clique') +
  ylab('Value') +
  theme(legend.position = 'bottom', legend.direction = 'vertical') +
  scale_color_brewer(palette = 'Dark2')

#ggsave('FiguraX.png', 
#       device = 'png', 
#       width = 16, 
#       height = 16, 
#       units = 'cm', 
#       dpi = 300)