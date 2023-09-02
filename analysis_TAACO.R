library(tidyverse)
library(jtools)

theme_set(theme_apa())

taaco = read_delim('TAACO_results.csv', 
                   delim = ';', 
                   locale = locale(decimal_mark = ',')) %>%
  mutate(corpus = corpus %>%
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
  pivot_longer(-c(corpus, genre, Filename), names_to = 'index', values_to = 'value') %>%
  mutate(index = index %>%
           recode('adjacent_overlap_cw_sent' = str_wrap('Adjacent sentence overlap content lemmas', 32),
                  'adjacent_overlap_cw_sent_div_seg' = str_wrap('Adjacent sentence overlap content lemmas (sentence normed)', 32),
                  'adjacent_overlap_binary_cw_sent' = str_wrap('Binary adjacent sentence overlap content lemmas', 32),
                  'syn_overlap_sent_noun' = str_wrap('Synonym overlap (sentence, noun)', 32),
                  'syn_overlap_sent_verb' = str_wrap('Synonym overlap (sentence, verb)', 32)))

taaco %>%
  ggplot(aes(genre, value, fill = genre)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point',
               shape = 22,
               size = 2,
               color = 'black',
               fill = 'white',
               position = position_dodge(width = 1)) +
  facet_wrap(. ~ index, scales = 'free') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('') +
  theme(axis.text.x = element_blank()) +
  ylab('Index value')

ggsave('figure6B.png',
       device = 'png',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

ggsave('figure6B.svg',
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

taaco_indices = taaco %>%
  group_by(Filename, index, corpus, genre) %>%
  summarize(`Mean indices` = mean(value)) %>%
  group_by(index, genre) %>%
  summarize(`Overall Mean indices` = mean(`Mean indices`),
            SD = sd(`Mean indices`),
            Median = median(`Mean indices`))

distances = mean_indices %>%
  unite(index, c(index, type)) %>%
  bind_rows(taaco_indices) %>%
  group_by(index) %>%
  summarise(Median_SD = sd(Median))
