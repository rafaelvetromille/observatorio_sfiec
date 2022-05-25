# Case 1 - Vaga: Pesquisador Credenciado 
# Empresa: Observatório SFIEC
# Candidato: Rafael Vetromille
# Assunto: Análise dos dados da COMEX (http://comexstat.mdic.gov.br/pt/home) para o estado do Ceará (2019 e 2021)

# Carregar pacotes
library(tidyverse)

# 2º problema - A presidência da FIEC solicitou ao Observatório da Indústria um relatório sobre o
# impacto da COVID-19 sobre o comércio internacional do Ceará em 2021 comparado a 2019.
# Coube a você atender essa demanda!

# A. Quais os top 10 produtos 'exportados' pelo Ceará em 2019 e o desempenho do
# comércio desses produtos em 2021, houve uma queda ou crescimento das exportações
# desses produtos?

# NCM (Nomenclatura Comum do Mercosul) - Código dos produtos
url <- 'https://balanca.economia.gov.br/balanca/bd/tabelas/TABELAS_AUXILIARES.xlsx'
ncm <- rio::import(file = url, which = 5) %>% 
  dplyr::select(1,2,12,16)

# EXPORTAÇÕES 2019
exp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2019.csv'
  )

df1 <- dplyr::left_join(
  
  exp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_NCM) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(
      PART_2019_FOB = scales::percent(VL_FOB / sum(VL_FOB), 0.01)
    ) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2019')
    ),
  
  ncm,
  
  by = 'CO_NCM'
  
)

# EXPORTAÇÕES 2021
exp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2021.csv'
)

df2 <- dplyr::left_join(
  
  exp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_NCM) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(
      PART_2021_FOB = scales::percent(VL_FOB / sum(VL_FOB), 0.01)
    ) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2021')
    ),
  
  ncm,
  
  by = 'CO_NCM'
  
)

# Participação das Exportações por Atividade Econômica – 2019-2021
# Atividade Econômica: Agropecuária, Indústria de Transformação, Indústria Extrativa & Outros Produtos

dplyr::full_join(
  
  df1 %>% 
    dplyr::group_by(NO_ISIC_SECAO) %>% 
    dplyr::summarise(VL_FOB_2019 = sum(VL_FOB_2019)),
  
  df2 %>% 
    dplyr::group_by(NO_ISIC_SECAO) %>% 
    dplyr::summarise(VL_FOB_2021 = sum(VL_FOB_2021)),
  
  by = 'NO_ISIC_SECAO'

) %>% 
  dplyr::mutate(
    PART_2019 = VL_FOB_2019/sum(VL_FOB_2019),
    PART_2021 = VL_FOB_2021/sum(VL_FOB_2021)  
    ) %>% 
  dplyr::arrange(
    desc(PART_2021)
  ) %>% 
  janitor::adorn_totals(where = 'row', name = 'Ceará') %>% 
  dplyr::mutate(
    VAR_PERCT = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019
  ) %>%
  dplyr::mutate(
    across(.cols = 4:6, .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 

# B. Quais os top 10 produtos 'importados' pelo Ceará em 2019 e o desempenho do
# comércio desses produtos em 2021, houve uma queda ou crescimento das importações
# desses produtos?

# IMPORTAÇÕES 2019
imp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2019.csv'
)

df3 <- dplyr::left_join(
  
  imp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>%
    dplyr::group_by(CO_NCM) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2019')
    ),
  
  ncm,
  
  by = 'CO_NCM'
  
)

# IMPORTAÇÕES 2021
imp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2021.csv'
)

df4 <- dplyr::left_join(
  
  imp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::mutate(DATE = lubridate::make_date(year = CO_ANO, month = CO_MES)) %>%
    dplyr::group_by(CO_NCM) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(PART = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2021')
    ),
  
  ncm,
  
  by = 'CO_NCM'
  
)

# Participação das Exportações por Atividade Econômica – 2019-2021
# Atividade Econômica: Agropecuária, Indústria de Transformação, Indústria Extrativa & Outros Produtos

dplyr::full_join(
  
  df3 %>% 
    dplyr::group_by(NO_ISIC_SECAO) %>% 
    dplyr::summarise(VL_FOB_2019 = sum(VL_FOB_2019)),
  
  df4 %>% 
    dplyr::group_by(NO_ISIC_SECAO) %>% 
    dplyr::summarise(VL_FOB_2021 = sum(VL_FOB_2021)),
  
  by = 'NO_ISIC_SECAO'
  
) %>% 
  dplyr::mutate(
    PART_2019 = VL_FOB_2019/sum(VL_FOB_2019),
    PART_2021 = VL_FOB_2021/sum(VL_FOB_2021)  
  ) %>% 
  dplyr::arrange(
    desc(PART_2021)
  ) %>% 
  janitor::adorn_totals(where = 'row', name = 'Ceará') %>% 
  dplyr::mutate(
    VAR_PERCT = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019
  ) %>%
  dplyr::mutate(
    across(.cols = 4:6, .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 


# C. Quais os top 5 destinos (países) dos produtos 'exportados' pelo Ceará em 2019? Esses
# países perderam ou ganharam participação em 2021?

# Países - Código dos países e nome
paises <- readr::read_delim(
  file = 'https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv',
  delim = ';',
  locale = locale(encoding = 'Latin1'),
  show_col_types = FALSE
) %>%
  dplyr::select(CO_PAIS, NO_PAIS)


# EXP 2019 
df5 <- left_join(
  
  exp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_PAIS) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(PART_2019 = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2019')
    ),
  
  paises,
  
  by = 'CO_PAIS'
  
)

# EXP 2021
df6 <- left_join(
  
  exp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_PAIS) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::mutate(PART_2021 = scales::percent(VL_FOB / sum(VL_FOB), 0.01)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2021')
    ),
  
  paises,
  
  by = 'CO_PAIS'
  
)

# Esses países perderam ou ganharam participação nas exportações de 2021?

df7 <- dplyr::full_join(df5, df6) %>% 
  dplyr::arrange(desc(VL_FOB_2021)) %>% 
  dplyr::relocate(NO_PAIS, .after = CO_PAIS) %>% 
  dplyr::relocate(VL_FOB_2021, .after = VL_FOB_2019) %>% 
  dplyr::mutate(VAR_PERC = scales::percent((VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019, accuracy = 0.01)) %>% 
  dplyr::rename(
    `Código País` = 1, 
    `Nome País` = 2, 
    `US$ 2019` = 3, 
    `US$ 2021` = 4,
    `Participação (%) 2019` = 5,
    `Participação (%) 2021` = 6,
    `Variação Pecentual (2021/2019)` = 7,
  )

# Quais as top 5 origens (países) dos produtos 'importados' pelo Ceará em 2019? Esses
# países perderam ou ganharam participação em 2021?

# IMP 2019

df8 <- left_join(
  
  imp_2019 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_PAIS) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2019')
    ),
  
  paises,
  
  by = 'CO_PAIS'
  
)

# IMP 2021

df9 <- left_join(
  
  imp_2021 %>%
    dplyr::filter(SG_UF_NCM == "CE") %>%
    dplyr::group_by(CO_PAIS) %>%
    dplyr::summarise(across(
      .cols = c(VL_FOB), .fns = sum
    )) %>%
    dplyr::arrange(desc(VL_FOB)) %>%
    dplyr::rename_with(
      .cols = c(VL_FOB),
      .fn = ~ paste0(.x, '_2021')
    ),
  
  paises,
  
  by = 'CO_PAIS'
  
)

# Esses países perderam ou ganharam participação nas importações de 2021?

df10 <- dplyr::bind_rows(
  
  dplyr::full_join(df8, df9, by = c('CO_PAIS', 'NO_PAIS')) %>% 
    dplyr::arrange(desc(VL_FOB_2021)) %>% 
    dplyr::relocate(NO_PAIS, .after = CO_PAIS) %>% 
    dplyr::relocate(VL_FOB_2021, .after = VL_FOB_2019) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T),
                  PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T)) %>% 
    dplyr::filter(row_number() %in% 1:5), 
  
  dplyr::full_join(df8, df9, by = c('CO_PAIS', 'NO_PAIS')) %>% 
    dplyr::arrange(desc(VL_FOB_2021)) %>% 
    dplyr::relocate(NO_PAIS, .after = CO_PAIS) %>% 
    dplyr::relocate(VL_FOB_2021, .after = VL_FOB_2019) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T),
                  PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T)) %>% 
    dplyr::filter(row_number() %in% 6:n()) %>% 
    dplyr::summarise(across(where(is.numeric), .fns = ~sum(.x, na.rm = T))) %>%
    dplyr::mutate(CO_PAIS = 'Demais Países', NO_PAIS = 'Demais Países', .before = everything())
  
) %>% 
  dplyr::bind_rows(summarise(.,
                             across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Ceará"))) %>%  
  dplyr::mutate(VAR_PERC  = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019) %>% 
  dplyr::rename(
    `Código País` = 1, 
    `Nome País` = 2, 
    `US$ 2019` = 3, 
    `US$ 2021` = 4,
    `Participação (%) 2019` = 5,
    `Participação (%) 2021` = 6,
    `Variação Pecentual (2021/2019)` = 7,
  ) %>% 
  dplyr::select(-1) %>% 
  dplyr::mutate(
    across(.cols = 4:6, .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 
  

# Taxa de crescimento (%) das exportações
df9 <- dplyr::full_join(
  df1, df2, by = c('CO_NCM', 'NO_NCM_POR')
) %>% 
  dplyr::relocate(where(is.numeric), .after = 'CO_NCM') %>% 
  dplyr::relocate(NO_NCM_POR, .after = 'CO_NCM') %>% 
  dplyr::relocate(PART_2019_KG, .before = dplyr::last_col()) %>% 
  dplyr::relocate(starts_with('KG'), .after = 2)

# Taxa de crescimento (%) das importações
df10 <- dplyr::full_join(
  df3, df4, by = c('CO_NCM', 'NO_NCM_POR')
) %>% 
  dplyr::relocate(where(is.numeric), .after = 'CO_NCM') %>% 
  dplyr::relocate(NO_NCM_POR, .after = 'CO_NCM') %>% 
  dplyr::relocate(PART_2019_KG, .before = dplyr::last_col()) %>% 
  dplyr::relocate(starts_with('KG'), .after = 2)


# Produtos (Setor)

url <- 'https://balanca.economia.gov.br/balanca/bd/tabelas/TABELAS_AUXILIARES.xlsx'








