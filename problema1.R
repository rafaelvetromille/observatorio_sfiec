# Carrega os pacotes
library("basedosdados")
library("tidyverse")
library("geobr")
library("pdftools")

# Defina o seu projeto no Google Cloud
set_billing_id("observatoriosfiec")

# Para carregar o dado direto no R
query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  dplyr::filter(ano == 2020, sigla_uf == "CE") %>% 
  dplyr::select(ano, sigla_uf, valor_remuneracao_media, 
                nacionalidade, tipo_vinculo, id_municipio, 
                cnae_2, cbo_2002, sexo, grau_instrucao_apos_2005) 
  
# Realizar o query
rais <- bd_collect(query_rais) 

# Pergunta 1: 
# Em quais municípios a indústria de calçados mais emprega no estado do Ceará?

dplyr::left_join(
  
  rais %>% 
    dplyr::filter(stringr::str_detect(cnae_2, '153')) %>% 
    dplyr::group_by(id_municipio) %>%
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::top_n(10) %>% 
    dplyr::rename(code_muni = id_municipio) %>% 
    dplyr::mutate(code_muni = as.numeric(code_muni)) %>% 
    dplyr::arrange(desc(n)), 
  
  geobr::read_municipality(), 
  
  by = 'code_muni'
  
)

# Pergunta 2: 
# Qual a média salarial e número de empregados da indústria de calçados nestes municípios?

dplyr::left_join(

  rais %>%
    dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
    dplyr::group_by(id_municipio) %>%
    dplyr::summarise(
      n = dplyr::n(),
      remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(code_muni = id_municipio) %>%
    dplyr::mutate(code_muni = as.numeric(code_muni)),
  
  geobr::read_municipality(),
  
  by = 'code_muni'

)

# Pergunta 3:
# Quais as principais ocupações empregadas no setor (indústria calçadista) e qual a média salarial delas?

dplyr::left_join(
  
  rais %>% 
    dplyr::filter(stringr::str_detect(cnae_2, '153')) %>% 
    dplyr::group_by(cbo_2002) %>%
    dplyr::summarise(
      n = dplyr::n(), 
      remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
    ) %>% 
    dplyr::arrange(desc(n)),
  
  readr::read_delim(file = 'CBO2002 - Ocupacao.csv', delim = ';', 
                    locale = locale(encoding = 'Latin1'), 
                    show_col_types = FALSE) %>% 
    janitor::clean_names() %>%
    dplyr::rename(cbo_2002 = codigo, ocupacao = titulo),
  
  by = "cbo_2002"
  
)

# Pergunta 4: 
# O setor (indústria calçadista) emprega mais homens ou mulheres e qual a média salarial deles?

dplyr::left_join(
  
  rais %>% 
    dplyr::filter(stringr::str_detect(cnae_2, '153')) %>% 
    dplyr::group_by(sexo) %>%
    dplyr::summarise(
      n = dplyr::n(), 
      remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
    ) %>% 
    dplyr::arrange(desc(n)), 
  
  tribble(
    ~ sexo, ~ sexo_nome, 
    "1", "Homem", 
    "2", "Mulher"
  ), 
  
  by = 'sexo'
  
)

# Pergunta 5: 
# Qual o nível de escolaridade dos empregados neste setor (indústria calçadista)?

dic <- bdplyr("br_me_rais.dicionario") %>% bd_collect()

dplyr::left_join(
  
  rais %>% 
    dplyr::select(ano, sigla_uf, valor_remuneracao_media, 
                  nacionalidade, tipo_vinculo, id_municipio, 
                  cnae_2, cbo_2002, sexo, grau_instrucao_apos_2005) %>%
    dplyr::filter(stringr::str_detect(cnae_2, '153')) %>% 
    dplyr::group_by(grau_instrucao_apos_2005) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>% 
    dplyr::arrange(dplyr::desc(n)) %>% 
    dplyr::rename(chave = grau_instrucao_apos_2005),
  
  dic %<>% 
    dplyr::filter(nome_coluna == "grau_instrucao_apos_2005") %>%
    dplyr::select(grau_instrucao_apos_2005 = valor, chave),
  
  by = 'chave'
  
)
