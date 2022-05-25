# Case 1 - Vaga: Pesquisador Credenciado 
# Empresa: Observatório SFIEC
# Candidato: Rafael Vetromille
# Assunto: Análise dos dados da COMEX (http://comexstat.mdic.gov.br/pt/home) para o estado do Ceará (2019 e 2021)

# Carregar pacotes
library(tidyverse)

# 2º problema - A presidência da FIEC solicitou ao Observatório da Indústria um relatório sobre o
# impacto da COVID-19 sobre o comércio internacional do Ceará em 2021 comparado a 2019.
# Coube a você atender essa demanda!

# Como produto utilizei a seleção: NO_CUCI_GRUPO

# NCM (Nomenclatura Comum do Mercosul) - Código dos produtos
url1 <- 'https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv'
ncm <- rio::import(file = url1) %>% 
  tibble::as_tibble()

# CUCI (Classificação Uniforme do Comércio Internacional) - Nome mais geral dos produtos
url2 <- 'https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_CUCI.csv'
cuci <- rio::import(file = url2) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(CO_CUCI_ITEM, NO_CUCI_ITEM, NO_CUCI_GRUPO)

# JOIN (NCM & CUCI)
ncm <- ncm %>% 
  dplyr::left_join(
    cuci, by = 'CO_CUCI_ITEM'
  )

# A.1. Quais os top 10 produtos 'exportados' pelo Ceará em 2019?

# EXPORTAÇÕES 2019
exp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2019.csv'
  )

# BASE DE DADOS TRATADA (2019)
df1 <- exp_2019 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_NCM) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = sum)) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::mutate(CO_NCM = as.numeric(CO_NCM), ) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2019')) %>%
  dplyr::left_join(ncm, by = 'CO_NCM') %>%
  dplyr::select(1, 2, 3, 17) %>%
  dplyr::group_by(NO_CUCI_GRUPO) %>%
  dplyr::summarise(VL_FOB_2019 = sum(VL_FOB_2019)) %>%
  dplyr::arrange(desc(VL_FOB_2019))

head(df1, 10)

# A.2. E o desempenho do comércio 'desses produtos' em 2021?

# EXPORTAÇÕES 2021
exp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_2021.csv'
)

# BASE DE DADOS TRATADA (2021)
df2 <- exp_2021 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_NCM) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = sum)) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::mutate(CO_NCM = as.numeric(CO_NCM), ) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2021')) %>%
  dplyr::left_join(ncm, by = 'CO_NCM') %>%
  dplyr::select(1, 2, 3, 17) %>%
  dplyr::group_by(NO_CUCI_GRUPO) %>%
  dplyr::summarise(VL_FOB_2021 = sum(VL_FOB_2021)) %>%
  dplyr::arrange(desc(VL_FOB_2021))

head(df2, 12) # oléo deixou de estar no top 10

# A.3. Houve uma queda ou crescimento nas exportações desses produtos?

df3 <- dplyr::bind_rows(
  
  dplyr::full_join(df1, df2, by = 'NO_CUCI_GRUPO') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 1:10), 
  
  dplyr::full_join(df1, df2, by = 'NO_CUCI_GRUPO') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 11:n()) %>% 
    dplyr::summarise(across(where(is.numeric), .fns = ~sum(.x, na.rm = T))) %>%
    dplyr::mutate(NO_CUCI_GRUPO = 'Demais Produtos', .before = everything())
  
) %>% 
  dplyr::bind_rows(summarise(.,
                             across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Ceará (Todos os produtos)"))) %>%  
  dplyr::mutate(VAR_PERC  = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019) %>% 
  dplyr::rename(
    `Produto` = 1, 
    `US$ 2019` = 2,
    `Participação (%) 2019` = 3,
    `US$ 2021` = 4,
    `Participação (%) 2021` = 5,
    `Variação Pecentual (2021/2019)` = 6,
  ) %>% 
  dplyr::mutate(
    across(.cols = c(3,5,6), .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 

# B.1. Quais os top 10 produtos 'importados' pelo Ceará em 2019?

# IMPORTAÇÕES 2019
imp_2019 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2019.csv'
)

# BASE DE DADOS TRATADA (2019)
df4 <- imp_2019 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_NCM) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = sum)) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::mutate(CO_NCM = as.numeric(CO_NCM), ) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2019')) %>%
  dplyr::left_join(ncm, by = 'CO_NCM') %>%
  dplyr::select(1, 2, 3, 17) %>%
  dplyr::group_by(NO_CUCI_GRUPO) %>%
  dplyr::summarise(VL_FOB_2019 = sum(VL_FOB_2019)) %>%
  dplyr::arrange(desc(VL_FOB_2019))

head(df4, 10)

# B.2. E o desempenho do comércio desses produtos em 2021?

# IMPORTAÇÕES 2021
imp_2021 <- readr::read_csv2(
  file = 'https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_2021.csv'
)

# BASE DE DADOS TRATADA (2021)
df5 <- imp_2021 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_NCM) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = sum)) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::mutate(CO_NCM = as.numeric(CO_NCM), ) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2021')) %>%
  dplyr::left_join(ncm, by = 'CO_NCM') %>%
  dplyr::select(1, 2, 3, 17) %>%
  dplyr::group_by(NO_CUCI_GRUPO) %>%
  dplyr::summarise(VL_FOB_2021 = sum(VL_FOB_2021)) %>%
  dplyr::arrange(desc(VL_FOB_2021))

head(df5, 12)

# B.3. Houve uma queda ou crescimento das importações desses produtos?

df6 <- dplyr::bind_rows(
  
  dplyr::full_join(df4, df5, by = 'NO_CUCI_GRUPO') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 1:10), 
  
  dplyr::full_join(df4, df5, by = 'NO_CUCI_GRUPO') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 11:n()) %>% 
    dplyr::summarise(across(where(is.numeric), .fns = ~sum(.x, na.rm = T))) %>%
    dplyr::mutate(NO_CUCI_GRUPO = 'Demais Produtos', .before = everything())
  
) %>% 
  dplyr::bind_rows(summarise(.,
                             across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Ceará (Todos os produtos)"))) %>%  
  dplyr::mutate(VAR_PERC  = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019) %>% 
  dplyr::rename(
    `Produto` = 1, 
    `US$ 2019` = 2,
    `Participação (%) 2019` = 3,
    `US$ 2021` = 4,
    `Participação (%) 2021` = 5,
    `Variação Pecentual (2021/2019)` = 6,
  ) %>% 
  dplyr::mutate(
    across(.cols = c(3,5,6), .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 

# C.1. Quais os top 5 destinos (países) dos produtos 'exportados' pelo Ceará em 2019? 

# Países - Código dos países e nome
paises <- readr::read_delim(
  file = 'https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv',
  delim = ';',
  locale = locale(encoding = 'Latin1'),
  show_col_types = FALSE) %>%
  dplyr::select(CO_PAIS, NO_PAIS)

# BASE DE DADOS TRATADA (2019) 
df7 <- exp_2019 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_PAIS) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = ~ sum(.x, na.rm = T))) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2019')) %>%
  dplyr::left_join(paises, by = 'CO_PAIS') %>% 
  dplyr::select(-1) %>% 
  dplyr::relocate(2, 1)

head(df7, 10) # por conta da Canadá
  
# C.2. Esses países perderam ou ganharam participação em 2021? - Parte 1

# BASE DE DADOS TRATADA (2021) 
df8 <- exp_2021 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_PAIS) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = ~ sum(.x, na.rm = T))) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2021')) %>%
  dplyr::left_join(paises, by = 'CO_PAIS') %>% 
  dplyr::select(-1) %>% 
  dplyr::relocate(2, 1)

head(df8, 5)

# C.2. Esses países perderam ou ganharam participação em 2021? - Parte 2

df9 <- dplyr::bind_rows(
  
  dplyr::full_join(df7, df8, by = 'NO_PAIS') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 1:5), 
  
  dplyr::full_join(df7, df8, by = 'NO_PAIS') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 6:n()) %>% 
    dplyr::summarise(across(where(is.numeric), .fns = ~sum(.x, na.rm = T))) %>%
    dplyr::mutate(NO_PAIS = 'Demais Países', .before = everything())
  
) %>% 
  dplyr::bind_rows(summarise(.,
                             across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Ceará (Todos os países)"))) %>%  
  dplyr::mutate(VAR_PERC  = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019) %>% 
  dplyr::rename(
    `País` = 1, 
    `US$ 2019` = 2,
    `Participação (%) 2019` = 3,
    `US$ 2021` = 4,
    `Participação (%) 2021` = 5,
    `Variação Pecentual (2021/2019)` = 6,
  ) %>% 
  dplyr::mutate(
    across(.cols = c(3,5,6), .fns = ~scales::percent(.x, accuracy = 0.01))
  ) 

# D.1. Quais as top 5 origens (países) dos produtos 'importados' pelo Ceará em 2019? 

# BASE DE DADOS TRATADA (2019) 
df10 <- imp_2019 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_PAIS) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = ~ sum(.x, na.rm = T))) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2019')) %>%
  dplyr::left_join(paises, by = 'CO_PAIS') %>% 
  dplyr::select(-1) %>% 
  dplyr::relocate(2, 1)

head(df10, 5)

# D.2. Esses países perderam ou ganharam participação em 2021? - Parte 1

df11 <- imp_2021 %>%
  dplyr::filter(SG_UF_NCM == "CE") %>%
  dplyr::group_by(CO_PAIS) %>%
  dplyr::summarise(across(.cols = c(VL_FOB), .fns = ~ sum(.x, na.rm = T))) %>%
  dplyr::arrange(desc(VL_FOB)) %>%
  dplyr::rename_with(.cols = c(VL_FOB), .fn = ~ paste0(.x, '_2021')) %>%
  dplyr::left_join(paises, by = 'CO_PAIS') %>% 
  dplyr::select(-1) %>% 
  dplyr::relocate(2, 1)

head(df11, 6) # por conta da Rússia

# D.2. Esses países perderam ou ganharam participação em 2021? - Parte 2

df12 <- dplyr::bind_rows(
  
  dplyr::full_join(df10, df11, by = 'NO_PAIS') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 1:5), 
  
  dplyr::full_join(df10, df11, by = 'NO_PAIS') %>% 
    dplyr::arrange(desc(VL_FOB_2019)) %>% 
    dplyr::mutate(PART_2019 = VL_FOB_2019/sum(VL_FOB_2019, na.rm = T), .after = 'VL_FOB_2019') %>%
    dplyr::mutate(PART_2021 = VL_FOB_2021/sum(VL_FOB_2021, na.rm = T), .after = 'VL_FOB_2021') %>%
    dplyr::filter(row_number() %in% 6:n()) %>% 
    dplyr::summarise(across(where(is.numeric), .fns = ~sum(.x, na.rm = T))) %>%
    dplyr::mutate(NO_PAIS = 'Demais Países', .before = everything())
  
) %>% 
  dplyr::bind_rows(summarise(.,
                             across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Ceará (Todos os países)"))) %>%  
  dplyr::mutate(VAR_PERC  = (VL_FOB_2021 - VL_FOB_2019)/VL_FOB_2019) %>% 
  dplyr::rename(
    `País` = 1, 
    `US$ 2019` = 2,
    `Participação (%) 2019` = 3,
    `US$ 2021` = 4,
    `Participação (%) 2021` = 5,
    `Variação Pecentual (2021/2019)` = 6,
  ) %>% 
  dplyr::mutate(
    across(.cols = c(3,5,6), .fns = ~scales::percent(.x, accuracy = 0.01))
  )