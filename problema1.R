# Case 1 - Vaga: Pesquisador Credenciado 
# Empresa: Observatório SFIEC
# Candidato: Rafael Vetromille
# Assunto: Análise dos dados da RAIS 2020 para o estado do Ceará - Setor: Indústria Calçadista

# Carrega os pacotes
library(basedosdados)
library(tidyverse)
library(geobr)
library(ggspatial)

# Temas de todos os mapas
tema_mapa <- theme_bw() + 
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      size = 8
    ),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    panel.grid.major = element_line(
      color = gray(0.9),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white") +
      annotation_scale(location = "br", width_hint = 0.30)
  )

# Defina o seu projeto no Google Cloud
set_billing_id('observatoriosfiec')

# Para carregar o dado direto no R
query_rais <- bdplyr('br_me_rais.microdados_vinculos') %>%
  dplyr::filter(ano == 2020, sigla_uf == "CE") %>% 
  dplyr::select(ano, sigla_uf, valor_remuneracao_media, 
                nacionalidade, tipo_vinculo, id_municipio, 
                cnae_2, cbo_2002, sexo, grau_instrucao_apos_2005) 

# Realizar o query
rais <- bd_collect(query_rais) 

# 1º problema – Recebemos uma demanda informacional do sindicado de produtos calçadistas
# do Ceará, solicitando um relatório com informações sobre o setor no estado. Os solicitantes
# buscam responder as seguintes perguntas:

# Pergunta 1: 
# Em quais municípios a indústria de calçados mais emprega no estado do Ceará?
df1 <- geobr::read_municipality(year = '2020') %>%
  dplyr::full_join(
    rais %>%
      dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
      dplyr::group_by(id_municipio) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(
        code_muni = as.numeric(id_municipio),
        .keep = 'unused',
        .before = dplyr::everything()
      ) %>%
      dplyr::arrange(dplyr::desc(n)),
    by = 'code_muni'
  ) %>%
  dplyr::filter(abbrev_state == 'CE')

# Mapa 1 - Os munícipios que mais empregram no setor de calçados
df1 %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(direction = -1) + # escala de cores
  labs(fill = "Qtd.",
       title = "Número de funcionários no setor de calçados, por município, CE",
       subtitle = "Dados da RAIS, para o ano de 2020", 
       caption = "Fonte: RAIS") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa 

# Tabela 1 - Os 10 munícipios que mais empregram no setor de calçados
df1 %>%
  tibble::as_tibble() %>% 
  dplyr::select(name_muni, n) %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::rename(
    `Nome do Município` = 1, 
    `Quantidade de funcionários no setor` = 2
  ) %>% 
  dplyr::filter(row_number() %in% 1:10)

# Gráfico 1 - Os 10 munícipios que mais empregram no setor de calçados
df1 %>%
  tibble::as_tibble() %>% 
  dplyr::select(name_muni, n) %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::rename(
    `Nome do Município` = 1, 
    `Quantidade de funcionários no setor` = 2
  ) %>% 
  dplyr::filter(row_number() %in% 1:10) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = reorder(`Nome do Município`, `Quantidade de funcionários no setor`), 
               y = `Quantidade de funcionários no setor`) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Quantidade de funcionários no setor de calçados, por município, CE",
    subtitle = "Dados da RAIS, para o ano de 2020",
    caption = "Fonte: RAIS",
    y = "Qtd.",
    x = "Municípios"
  )

# Pergunta 2: 
# Qual a média salarial e número de empregados da indústria de calçados nestes municípios?
df2 <- geobr::read_municipality(year = '2020') %>%
  dplyr::full_join(
    rais %>%
      dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
      dplyr::group_by(id_municipio) %>%
      dplyr::summarise(
        n = dplyr::n(),
        remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
      ) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(
        code_muni = as.numeric(id_municipio),
        .keep = 'unused',
        .before = dplyr::everything()
      ) %>%
      dplyr::arrange(desc(n)),
    by = 'code_muni'
  ) %>%
  dplyr::filter(abbrev_state == 'CE')

# Mapa 2 - Média salarial dos municípios
df2 %>%
  ggplot() +
  geom_sf(aes(fill = remuneracao_media)) +
  scale_fill_viridis_c(direction = -1) + # escala de cores
  labs(fill = "R$",
       title = "Remuneração média no setor de calçados, por município, CE",
       subtitle = "Dados da RAIS, para o ano de 2020", 
       caption = "Fonte: RAIS") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

# Tabela 2 - Remuneração média dos 10 municípios que mais empregram 
df2 %>% 
  tibble::as_tibble() %>% 
  dplyr::select(name_muni, n, remuneracao_media) %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n)) %>%  
  dplyr::mutate(remuneracao_media = scales::dollar(remuneracao_media, prefix = 'R$ ', decimal.mark = ',')) %>% 
  dplyr::rename(
    `Nome do município` = 1, 
    `Quantidade de funcionários no setor` = 2, 
    `Remuneração Média (R$)` = 3 
  ) %>% 
  dplyr::filter(row_number() %in% 1:10)

# Pergunta 3:
# Quais as principais ocupações empregadas no setor (indústria calçadista) e qual a média salarial delas?
df3 <- rais %>%
  dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
  dplyr::group_by(cbo_2002) %>%
  dplyr::summarise(
    n = dplyr::n(),
    remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::left_join(
    readr::read_delim(
      file = 'CBO2002 - Ocupacao.csv',
      delim = ';',
      locale = locale(encoding = 'Latin1'),
      show_col_types = FALSE
    ) %>%
      janitor::clean_names() %>%
      dplyr::rename(cbo_2002 = codigo, ocupacao = titulo),
    by = 'cbo_2002'
  ) 

# Tabela 3 - Quantidade e remuneração média por ocupação (10 maiores)
df3 %>% 
  dplyr::select(4, 2, 3) %>% 
  dplyr::mutate(remuneracao_media = scales::dollar(remuneracao_media, prefix = 'R$ ', decimal.mark = ',')) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::rename(
    `Ocupação` = 1, 
    `Qtd.` = 2, 
    `Remuneração Média (R$)` = 3
  ) %>% 
  dplyr::filter(row_number() %in% 1:10)
  
# Pergunta 4: 
# O setor (indústria calçadista) emprega mais homens ou mulheres e qual a média salarial deles?
df4 <- rais %>%
  dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
  dplyr::group_by(sexo) %>%
  dplyr::summarise(
    n = dplyr::n(),
    remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::left_join(tribble( ~ sexo, ~ sexo_nome, '1', 'Homem', '2', 'Mulher'), by = 'sexo')

# Tabela 4 - Número de funcionário no setor, por sexo 
df4 %>% 
  dplyr::select(sexo_nome, n, remuneracao_media) %>% 
  dplyr::rename(
    `Sexo` = 1, 
    `Qtd.` = 2, 
    `Remuneração Média (R$)` = 3
  ) %>% 
  dplyr::bind_rows(summarise(.,
                             across(.cols = 2, ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Total"), 
                             across(.cols = 3, ~mean(.x, na.rm = TRUE))))

# Gráfico 2 - Número de funcionário no setor, por sexo 
df4 %>% 
  dplyr::select(sexo_nome, n, remuneracao_media) %>% 
  dplyr::rename(
    `Sexo` = 1, 
    `Qtd.` = 2, 
    `Remuneração Média (R$)` = 3
  ) %>% 
  dplyr::bind_rows(summarise(.,
                             across(.cols = 2, ~sum(.x, na.rm = TRUE)),
                             across(where(is.character), ~"Total"), 
                             across(.cols = 3, ~mean(.x, na.rm = TRUE)))) %>% 
  ggplot2::ggplot() + 
  ggplot2::aes(x = reorder(`Sexo`, -`Qtd.`), y = `Qtd.`) +
  ggplot2::geom_bar(stat = 'identity') + 
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Quantidade de funcionários, por sexo, no setor calçadista, CE",
    subtitle = "Dados da RAIS, para o ano de 2020",
    caption = "Fonte: RAIS", 
    x = ""
  )
  

# Pergunta 5: 
# Qual o nível de escolaridade dos empregados neste setor (indústria calçadista)?
df5 <- rais %>%
  dplyr::filter(stringr::str_detect(cnae_2, '153')) %>%
  dplyr::group_by(grau_instrucao_apos_2005) %>%
  dplyr::summarise(
    n = dplyr::n(),
    remuneracao_media = mean(valor_remuneracao_media, na.rm = T)
  ) %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::rename(chave = grau_instrucao_apos_2005) %>%
  dplyr::left_join(
    bdplyr('br_me_rais.dicionario') %>%
      bd_collect() %>%
      dplyr::filter(nome_coluna == 'grau_instrucao_apos_2005') %>%
      dplyr::select(grau_instrucao = valor, chave),
    by = 'chave'
  ) %>% 
  dplyr::mutate(
    grau_instrucao = case_when(
      grau_instrucao == 'MEDIO COMPL' ~ 'Ensino Médio Completo', 
      grau_instrucao == 'MEDIO INCOMP' ~ 'Ensimo Médio Incompleto', 
      grau_instrucao == 'FUND COMPL' ~ 'Fundamental Completo', 
      grau_instrucao == '6. A 9. FUND' ~ 'De 6º ao 9º do Ensino Fundamental', 
      grau_instrucao == 'ATE 5.A INC' ~ 'Até 5º ano Incompleto', 
      grau_instrucao == 'SUP. INCOMP' ~ 'Ensino Superior Incompleto', 
      grau_instrucao == 'SUP. COMP' ~ 'Ensio Superior Completo', 
      grau_instrucao == '5.A CO FUND' ~ 'Até o 5º Completo', 
      grau_instrucao == 'ANALFABETO' ~ 'Analfabeto', 
      grau_instrucao == 'MESTRADO' ~ 'Mestrado', 
      grau_instrucao == 'DOUTORADO' ~ 'Doutorado'
    )
  )

# Tabela 5 - Grau de instrução, quantidade de funcionários e média salarial
df5 %>%
  tibble::add_row(
    chave = NA, 
    n     = sum(df2$n, na.rm = T), 
    remuneracao_media = mean(df2$remuneracao_media, na.rm = T), 
    grau_instrucao = 'Total'
  ) %>% 
  dplyr::mutate(remuneracao_media = scales::dollar(remuneracao_media, prefix = 'R$ ', decimal.mark = ',')) %>% 
  dplyr::select(4, 2, 3) %>% 
  dplyr::rename(
    `Grau de Instrução` = 1, 
    `Qtd.` = 2, 
    `Remuneração Média (R$)` = 3
  )

# Gráfico 5 - Grau de instrução, quantidade de funcionários
df5 %>%
  dplyr::mutate(remuneracao_media = scales::dollar(
    remuneracao_media,
    prefix = 'R$ ',
    decimal.mark = ','
  )) %>%
  dplyr::select(4, 2, 3) %>%
  dplyr::rename(
    `Grau de Instrução` = 1,
    `Qtd.` = 2,
    `Remuneração Média (R$)` = 3
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = reorder(`Grau de Instrução`, `Qtd.`), y = `Qtd.`) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    fill = "R$",
    title = "Grau de instrução no setor de calçados",
    subtitle = "Dados da RAIS, para o ano de 2020",
    caption = "Fonte: RAIS"
  )

# Gráfico 6 - Grau de instrução, remuneração média
df5 %>%
  dplyr::select(4, 2, 3) %>%
  dplyr::rename(
    `Grau de Instrução` = 1,
    `Qtd.` = 2,
    `Remuneração Média (R$)` = 3
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = reorder(`Grau de Instrução`, -`Remuneração Média (R$)`), y = `Remuneração Média (R$)`) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Grau de instrução no setor de calçados & Remuneração Média",
    subtitle = "Dados da RAIS, para o ano de 2020",
    caption = "Fonte: RAIS",
    y = "Remuneração Média (R$)",
    x = "Grau de Instrução"
  )
