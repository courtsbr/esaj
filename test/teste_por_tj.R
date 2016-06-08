library(dplyr)

lista_sc <- readRDS('/home/fcorrea/Documentos/p_tjsc_convertido_mini.rds') %>%
  with(n_processo[1:100])

lista_sp <- list.files('/home/fcorrea/Projects/senasp_tjal/tjsp_amostra/')[1:60] %>%
  gsub(pattern = '.html|.rds',replacement = '') %>%
  unique

lista_al <- list.files('/home/fcorrea/Documentos/tjal_cpopg/')[1:44] %>%
  gsub(pattern = '.html|.rds',replacement = '')

lista_al <- lista_al[which(nchar(lista_al) == 20)]

path = '~/testes'

dir.create(path)

lista_al[1:15] %>%
  esaj::cpo_pg(path, tj = 'TJAL', .parallel = T)

lista_al[16:30] %>%
  esaj::cpo_pg(path, tj = 'TJAL', .parallel = F)

lista_sc[1:15] %>%
  esaj::cpo_pg(path, tj = 'TJSC', .parallel = F)

lista_sc[40:60] %>%
  cpo_pg(path, tj = 'TJSC', .parallel = T)

lista_sp[1:15] %>%
  esaj::cpo_pg(path, tj = 'TJSP', .parallel = T)

lista_sp[16:30] %>%
  esaj::cpo_pg(path, tj = 'TJSP', .parallel = F)


