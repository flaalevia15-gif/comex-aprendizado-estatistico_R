# ==============================================================================
# SCRIPT MODULAR: ANÁLISE EXPLORATÓRIA E MÉTRICAS ESPACIAIS/TEMPORAIS (R/eda.R)
# ==============================================================================
# 
# ÍNDICE DE SEÇÕES (Use Ctrl + F com as TAGs abaixo):
# 
# 1. [TAG: LIMPEZA_SANITIZACAO] - Funções de Limpeza Textual e Conversão Numérica
# 2. [TAG: RECORTE_GEOGRAFICO]  - Rankings Regionais, Assiduidade e Participação Geral
# 3. [TAG: TOP_ROTAS]          - Identificação e Exportação das Rotas Comerciais (Pareto)
# 4. [TAG: SAZONALIDADE_GERAL]  - Visualização Gráfica da Sazonalidade Mensal
# 5. [TAG: OUTLIERS_ROTAS_IQR]  - Detecção Contínua de Outliers (IQR) nas Rotas Líderes
# ==============================================================================

library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)


# ==============================================================================
# 1. [TAG: LIMPEZA_SANITIZACAO] FUNÇÕES DE LIMPEZA E SANITIZAÇÃO
# ==============================================================================

#' Remove espaços em branco redundantes em todas as colunas de texto
#' @param df Data frame de entrada
#' @return Data frame tratado
limpar_strings <- function(df) {
  df |> mutate(across(where(is.character), str_squish))
}

#' Converte colunas formatadas em padrão numérico brasileiro (pt-BR) para tipo numeric
#' @param df Data frame de entrada
#' @param colunas Vetor de nomes de colunas a serem convertidas
#' @return Data frame com métricas numéricas convertidas
converter_metricas_numericas <- function(df, colunas = c("Valor US$ FOB")) {
  loc_br <- locale(decimal_mark = ",", grouping_mark = ".")
  df |> mutate(across(any_of(colunas), ~ parse_number(as.character(.x), locale = loc_br)))
}


# ==============================================================================
# 2. [TAG: RECORTE_GEOGRAFICO] RANKINGS REGIONAIS E ASSIDUIDADE HISTÓRICA
# ==============================================================================

#' Calcula o ranking anual e a participação percentual monetária por município
#' @param df Data frame com dados de comércio exterior
#' @param fluxo_alvo Sentido comercial ("Exportação" ou "Importação")
#' @return Tibble com rankings anuais[cite: 11]
gerar_ranking_anual <- function(df, fluxo_alvo) {
  df |>
    filter(Fluxo == fluxo_alvo) |>
    group_by(Ano, Município) |>
    summarise(`Valor US$ FOB` = sum(`Valor US$ FOB`, na.rm = TRUE), .groups = "drop_last") |>
    mutate(
      total_ano = sum(`Valor US$ FOB`, na.rm = TRUE),
      `Participacao_%` = round((`Valor US$ FOB` / total_ano) * 100, 2),
      Ranking = min_rank(desc(`Valor US$ FOB`))
    ) |>
    ungroup() |>
    select(Ano, Ranking, Município, `Valor US$ FOB`, `Participacao_%`) |>
    arrange(Ano, Ranking)
}

#' Analisa a consistência e assiduidade dos municípios no Top do ranking regional[cite: 11]
#' @param df_top Tibble contendo os municípios filtrados no topo do ranking[cite: 11]
#' @param df_ranking_anual Tibble gerado por gerar_ranking_anual()[cite: 11]
#' @return Lista contendo: frequencia_top e matriz_posicoes[cite: 11]
gerar_analise_consistencia <- function(df_top, df_ranking_anual) {
  total_anos <- n_distinct(df_ranking_anual$Ano)
  frequencia_top <- df_top |>
    group_by(Município) |>
    summarise(
      anos_no_top = n(),
      participacao_media_top = round(mean(`Participacao_%`, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    mutate(frequencia_pct = round((anos_no_top / total_anos) * 100, 2)) |>
    arrange(desc(anos_no_top), desc(participacao_media_top))
  
  matriz_posicoes <- df_ranking_anual |>
    select(Ano, Município, Ranking) |>
    pivot_wider(names_from = Ano, values_from = Ranking, values_fill = NA)
  
  list(frequencia_top = frequencia_top, matriz_posicoes = matriz_posicoes)
}

#' Consolida o ranking geral acumulado de todo o período histórico por fluxo[cite: 11]
#' @param df Data frame com dados consolidados[cite: 11]
#' @param fluxo_alvo Sentido comercial ("Exportação" ou "Importação")[cite: 11]
#' @return Tibble ordenado por relevância com participação acumulada[cite: 11]
gerar_ranking_geral <- function(df, fluxo_alvo) {
  df |>
    filter(Fluxo == fluxo_alvo) |>
    group_by(Município) |>
    summarise(`Valor US$ FOB` = sum(`Valor US$ FOB`, na.rm = TRUE), .groups = "drop") |>
    mutate(
      total_geral = sum(`Valor US$ FOB`, na.rm = TRUE),
      Participacao_Acumulada_pct = round((`Valor US$ FOB` / total_geral) * 100, 2),
      Ranking_Geral = min_rank(desc(`Valor US$ FOB`))
    ) |>
    select(Município, `Valor US$ FOB`, Ranking_Geral, `Participacao_Acumulada_%` = Participacao_Acumulada_pct) |>
    arrange(Ranking_Geral)
}


# ==============================================================================
# 3. [TAG: TOP_ROTAS] IDENTIFICAÇÃO E EXPORTAÇÃO DAS ROTAS COMERCIAIS (PARETO)
# ==============================================================================

#' Identifica o Top N de rotas (País x Produto) por volume físico com curva de Pareto[cite: 11]
#' @param df_transacional Tibble transacional tratado[cite: 11]
#' @param df_dimensao_sh4 Tibble da dimensão de produtos[cite: 11]
#' @param top_n Quantidade de rotas no ranking por município/fluxo (padrão: 3)[cite: 11]
#' @param caminho_exportacao_csv Caminho opcional para gravação em disco[cite: 11]
#' @return Tibble com rankings, volume acumulado e representatividade percentual[cite: 11]
gerar_top_rotas <- function(df_transacional, df_dimensao_sh4, top_n = 3, caminho_exportacao_csv = NULL) {
  df_rotas <- df_transacional |>
    left_join(df_dimensao_sh4, by = "Código SH4") |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    summarise(
      volume_total_kg = sum(`Quilograma Líquido`, na.rm = TRUE),
      valor_total_fob = sum(`Valor US$ FOB`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(Fluxo, Município) |>
    mutate(
      total_municipio_kg = sum(volume_total_kg, na.rm = TRUE),
      participacao_pct = (volume_total_kg / total_municipio_kg) * 100
    ) |>
    arrange(Fluxo, Município, desc(volume_total_kg)) |>
    mutate(ranking = row_number(), participacao_acumulada_pct = cumsum(participacao_pct)) |>
    filter(ranking <= top_n) |>
    ungroup() |>
    select(
      Fluxo, Município, ranking, País, `Código SH4`, `Descrição SH4`,
      volume_total_kg, valor_total_fob, participacao_pct, participacao_acumulada_pct
    )
  
  if (!is.null(caminho_exportacao_csv)) {
    dir.create(dirname(caminho_exportacao_csv), showWarnings = FALSE, recursive = TRUE)
    write_csv2(df_rotas, caminho_exportacao_csv)
    message(paste("Tabela de Top Rotas salva em:", caminho_exportacao_csv))
  }
  return(df_rotas)
}

#' Isola e exporta as bases transacional e sazonal exclusivamente para as rotas líderes (Top 1)[cite: 11]
#' @param df_transacional Data frame transacional completo[cite: 11]
#' @param top_rotas Data frame retornado por gerar_top_rotas()[cite: 11]
#' @param pasta_saida Pasta destino para os arquivos CSV (padrão: "data/processed")[cite: 11]
#' @return Lista contendo os dois data frames das rotas líderes[cite: 11]
exportar_rotas_lideres_csv <- function(df_transacional, top_rotas, pasta_saida = "data/processed") {
  top1_definicao <- top_rotas |>
    filter(ranking == 1) |>
    select(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    distinct()
  
  df_trans_lideres <- df_transacional |>
    inner_join(top1_definicao, by = c("Fluxo", "Município", "País", "Código SH4"))
  
  df_sazonal_lideres <- df_trans_lideres |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`, Ano, mes_numero, mes_nome) |>
    summarise(
      `Valor US$ FOB` = sum(`Valor US$ FOB`, na.rm = TRUE),
      `Quilograma Líquido` = sum(`Quilograma Líquido`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Fluxo, Município, Ano, mes_numero)
  
  dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)
  write_csv2(df_trans_lideres, file.path(pasta_saida, "df_transacional_lideres.csv"))
  write_csv2(df_sazonal_lideres, file.path(pasta_saida, "df_sazonal_lideres.csv"))
  
  list(df_transacional_lideres = df_trans_lideres, df_sazonal_lideres = df_sazonal_lideres)
}


# ==============================================================================
# 4. [TAG: SAZONALIDADE_GERAL] VISUALIZAÇÃO GRÁFICA DA SAZONALIDADE MENSAL
# ==============================================================================

#' Plota as curvas sazonais intra-anuais de volume físico por fluxo e município[cite: 11]
#' @param df_sazonal Tibble contendo as agregações sazonais[cite: 11]
#' @return Objeto ggplot facetado[cite: 11]
plotar_sazonalidade_volume <- function(df_sazonal) {
  df_plot <- df_sazonal |>
    mutate(
      Ano = as.factor(Ano),
      mes_nome_abrev = factor(
        mes_nome,
        levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      )
    )
  
  ggplot(df_plot, aes(x = mes_nome_abrev, y = `Quilograma Líquido` / 1e6, group = Ano, color = Ano)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    geom_point(size = 1.6) +
    facet_grid(Fluxo ~ Município, scales = "free_y") +
    scale_color_viridis_d(option = "plasma") +
    scale_y_continuous(labels = label_number(suffix = " M kg", big.mark = ".")) +
    labs(
      title = "Curvas de Sazonalidade Mensal do Volume Físico (2019–2026)",
      subtitle = "Comportamento intra-anual de movimentação de carga por fluxo e município",
      x = "Mês",
      y = "Volume Físico (Milhões de Kg)",
      color = "Ano"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}


# ==============================================================================
# 5. [TAG: OUTLIERS_ROTAS_IQR] DETECÇÃO CONTÍNUA DE OUTLIERS (IQR) NAS ROTAS LÍDERES
# ==============================================================================

#' Constrói série mensal contínua (com zero nos meses vazios) e detecta outliers via IQR[cite: 11]
#' @param df_transacional Data frame transacional[cite: 11]
#' @param top_rotas Data frame retornado por gerar_top_rotas()[cite: 11]
#' @param fator_iqr Multiplicador de Tukey (padrão: 1.5)[cite: 11]
#' @return Lista contendo: resumo_iqr, meses_outliers e series[cite: 11]
detectar_outliers_rotas_iqr <- function(df_transacional, top_rotas, fator_iqr = 1.5) {
  rotas_selecionadas <- top_rotas |>
    filter(ranking == 1) |>
    select(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    distinct()
  
  periodos_validos <- df_transacional |>
    select(Ano, mes_numero) |>
    distinct() |>
    arrange(Ano, mes_numero)
  
  lista_resumo_iqr <- list()
  lista_meses_outliers <- list()
  lista_series <- list()
  
  for (i in seq_len(nrow(rotas_selecionadas))) {
    rota <- rotas_selecionadas[i, ]
    df_rota_mes <- df_transacional |>
      filter(Município == rota$Município, Fluxo == rota$Fluxo, País == rota$País, `Código SH4` == rota$`Código SH4`) |>
      group_by(Ano, mes_numero) |>
      summarise(volume_kg = sum(`Quilograma Líquido`, na.rm = TRUE), .groups = "drop")
    
    df_serie <- periodos_validos |>
      left_join(df_rota_mes, by = c("Ano", "mes_numero")) |>
      mutate(
        volume_kg = if_else(is.na(volume_kg), 0, volume_kg),
        Município = rota$Município,
        Fluxo = rota$Fluxo,
        País = rota$País,
        `Código SH4` = rota$`Código SH4`,
        `Descrição SH4` = rota$`Descrição SH4`
      ) |>
      arrange(Ano, mes_numero)
    
    q1 <- quantile(df_serie$volume_kg, 0.25, na.rm = TRUE, names = FALSE)
    q3 <- quantile(df_serie$volume_kg, 0.75, na.rm = TRUE, names = FALSE)
    iqr <- q3 - q1
    limite_sup <- q3 + (fator_iqr * iqr)
    outliers <- df_serie |> filter(volume_kg > limite_sup)
    
    lista_resumo_iqr[[i]] <- tibble(
      Município = rota$Município,
      Fluxo = rota$Fluxo,
      País = rota$País,
      `Código SH4` = rota$`Código SH4`,
      `Descrição SH4` = rota$`Descrição SH4`,
      Total_Meses = nrow(df_serie),
      Q1_Ton = round(q1 / 1e3, 2),
      Q3_Ton = round(q3 / 1e3, 2),
      IQR_Ton = round(iqr / 1e3, 2),
      Limite_Superior_Ton = round(limite_sup / 1e3, 2),
      Qtd_Outliers = nrow(outliers)
    )
    
    if (nrow(outliers) > 0) {
      lista_meses_outliers[[i]] <- outliers |>
        mutate(
          Volume_Real_Ton = round(volume_kg / 1e3, 2),
          Limite_Sup_Ton = round(limite_sup / 1e3, 2),
          Excesso_Ton = round((volume_kg - limite_sup) / 1e3, 2),
          Ano = as.integer(Ano),
          Mês = as.integer(mes_numero)
        ) |>
        select(
          Município, Fluxo, País, `Código SH4`, `Descrição SH4`,
          Ano, Mês, Volume_Real_Ton, Limite_Sup_Ton, Excesso_Ton
        )
    }
    
    df_serie$limite_sup <- limite_sup
    df_serie$is_outlier <- df_serie$volume_kg > limite_sup
    lista_series[[i]] <- df_serie
  }
  
  list(
    resumo_iqr = bind_rows(lista_resumo_iqr),
    meses_outliers = bind_rows(lista_meses_outliers),
    series = bind_rows(lista_series)
  )
}

#' Plota as séries temporais das rotas líderes com o teto IQR e os meses anômalos[cite: 11]
#' @param df_transacional Data frame transacional[cite: 11]
#' @param resultado_iqr Objeto retornado por detectar_outliers_rotas_iqr()[cite: 11]
#' @return Objeto ggplot facetado com estética consistente[cite: 11]
plotar_series_rotas_com_outliers <- function(df_transacional, resultado_iqr) {
  df_series <- resultado_iqr$series |>
    mutate(
      Data = as.Date(paste(Ano, mes_numero, "01", sep = "-")),
      Volume_Mil_Ton = volume_kg / 1e6,
      Limite_Sup_Mil_Ton = limite_sup / 1e6
    )
  
  df_rotulos <- resultado_iqr$resumo_iqr |>
    mutate(
      rotulo_faceta = sprintf(
        "%s | %s\n%s — SH4 %s\n[Teto: %.1fk ton | Outliers: %d]",
        Município, Fluxo, País, `Código SH4`, Limite_Superior_Ton / 1e3, Qtd_Outliers
      )
    )
  
  df_plot <- df_series |>
    left_join(
      df_rotulos |> select(Município, Fluxo, País, `Código SH4`, rotulo_faceta),
      by = c("Município", "Fluxo", "País", "Código SH4")
    )
  
  ggplot(df_plot, aes(x = Data, y = Volume_Mil_Ton)) +
    geom_hline(
      aes(yintercept = Limite_Sup_Mil_Ton),
      color = "#d62728",
      linetype = "dashed",
      linewidth = 0.75
    ) +
    geom_line(color = "#1f77b4", linewidth = 0.8) +
    geom_point(
      data = df_plot |> filter(is_outlier),
      aes(x = Data, y = Volume_Mil_Ton),
      color = "#d62728",
      size = 2.4,
      shape = 16
    ) +
    facet_wrap(~ rotulo_faceta, scales = "free_y", ncol = 2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = label_number(suffix = "k ton", big.mark = ".")) +
    labs(
      title = "Série Temporal Mensal e Detecção de Outliers nas Rotas Líderes (Top 1)",
      subtitle = "Linha tracejada vermelha representa o Teto Superior do IQR (Q3 + 1.5×IQR). Pontos vermelhos são meses com volumes anômalos.",
      x = NULL,
      y = "Milhares de Toneladas (k ton)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 9, color = "#212529", lineheight = 1.1),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e9ecef")
    )
}