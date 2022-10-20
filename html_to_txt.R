library(tidyverse)
library(rvest)

directory = 'corpus/html/'
files = list.files(directory)

for (i in 1:length(files)) {
  html = read_html(paste0(directory, files[i]), 
                   encoding = 'UTF-8')
  
  paragraphs = tibble(id = html %>%
           html_elements('p') %>%
           html_attr('id'),
         text = html %>%
           html_elements('p') %>%
           html_text2()) %>% 
    filter(str_detect(id, '^(p|par|para)\\d{4}$'))
  
  if (nrow(paragraphs) > 0) {
    paragraphs %>%
      .$text %>%
      str_c(collapse = '\n') %>%
      write_file(paste0('corpus/txt/', str_sub(files[i], 0, -5), 'txt'))
  }
}

