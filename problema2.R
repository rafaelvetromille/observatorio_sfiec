# Case 1 - Vaga: Pesquisador Credenciado 
# Empresa: Observatório SFIEC
# Candidato: Rafael Vetromille
# Assunto: Análise dos dados da COMEX (http://comexstat.mdic.gov.br/pt/home) para o estado do Ceará (2019 e 2021)

# Carregar pacotes
library(tidyverse)

# 2º problema - A presidência da FIEC solicitou ao Observatório da Indústria um relatório sobre o
# impacto da COVID-19 sobre o comércio internacional do Ceará em 2021 comparado a 2019.
# Coube a você atender essa demanda!

# NCM (Nomenclatura Comum do Mercosul) - Código dos produtos
ncm <- readr::read_csv2(
    file = 'https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv',
    locale = locale(encoding = 'Latin1'),
    show_col_types = FALSE
  ) %>%
  dplyr::select(CO_NCM, NO_NCM_POR)

# A. Quais os top 10 produtos exportados pelo Ceará em 2019 e o desempenho do
# comércio desses produtos em 2021, houve uma queda ou crescimento das exportações
# desses produtos?

## 2019
exp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2019.csv'
  )

dplyr::left_join(
  
  exp_2019 %>% 
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>% 
    dplyr::group_by(CO_NCM) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2019')), 
  
  ncm, 
  
  by = 'CO_NCM'

)


## 2021
exp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2021.csv'
)

dplyr::left_join(
  
  exp_2021 %>% 
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>% 
    dplyr::group_by(CO_NCM) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2021')), 
  
  ncm, 
  
  by = 'CO_NCM'
  
)

# B. Quais os top 10 produtos importados pelo Ceará em 2019 e o desempenho do
# comércio desses produtos em 2021, houve uma queda ou crescimento das importações
# desses produtos?

# 2019
imp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2019.csv'
)

dplyr::left_join(
  
  imp_2019 %>% 
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>% 
    dplyr::group_by(CO_NCM) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2019')), 
  
  
  ncm, 
  
  by = 'CO_NCM'
  
)


# 2021
imp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2021.csv'
)

dplyr::left_join(
  
  imp_2021 %>% 
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>% 
    dplyr::group_by(CO_NCM) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2021')), 

  ncm, 
  
  by = 'CO_NCM'
  
)

# Países - Código dos países e nome
paises <- readr::read_delim(
    file = 'https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv',
    delim = ';',
    locale = locale(encoding = 'Latin1'),
    show_col_types = FALSE
  ) %>%
  dplyr::select(CO_PAIS, NO_PAIS)


### Quais os top 5 destinos (países) dos produtos exportados pelo Ceará em 2019? Esses
### países perderam ou ganharam participação em 2021?


### 2019 
left_join(
  
  exp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::group_by(CO_PAIS) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(KG_LIQUIDO)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>%
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2019')), 
  
  paises,
  
  by = 'CO_PAIS'

)

### 2021
left_join(
  
  exp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::group_by(CO_PAIS) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(KG_LIQUIDO)) %>%
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2021')), 

  paises,
  
  by = 'CO_PAIS'
  
)


### Quais as top 5 origens (países) dos produtos importados pelo Ceará em 2019? Esses
### países perderam ou ganharam participação em 2021?

### 2019

left_join(
  
  imp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::group_by(CO_PAIS) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2019')),
  
  paises,
  
  by = 'CO_PAIS'
  
)


### 2021

left_join(
  
  imp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>% 
    dplyr::group_by(CO_PAIS) %>% 
    dplyr::summarise(
      across(.cols = c(KG_LIQUIDO, VL_FOB), .fns = sum)
    ) %>% 
    dplyr::arrange(desc(VL_FOB)) %>% 
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>% 
    dplyr::rename_with(.cols = c(KG_LIQUIDO, VL_FOB), .fn = ~paste0(.x, '_2021')), 
  
  paises,
  
  by = 'CO_PAIS'
  
)
