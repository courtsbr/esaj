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

system.time({
lista_al[16:30] %>%
  esaj::cpo_pg(path, tj = 'TJAL', .parallel = F)
})

system.time({
lista_sc[1:15] %>%
  esaj::cpo_pg(path, tj = 'TJSC', .parallel = F)
})
