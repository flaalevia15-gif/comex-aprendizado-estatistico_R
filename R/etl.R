# ==============================================================================
# SCRIPT MODULAR: PIPELINE DE ETL E MODELAGEM DIMENSIONAL (R/etl.R)
# ==============================================================================
# 
# ÍNDICE DE SEÇÕES (Use Ctrl + F com as TAGs abaixo):
# 
# 1. [TAG: LIMPEZA_SANITIZACAO]   - Limpeza Textual e Conversão de Métricas pt-BR
# 2. [TAG: INGESTAO_PARTICIONADA] - Leitura Unificada e Validação dos Microdados
# 3. [TAG: MODELO_DIMENSIONAL]    - Construção das Tabelas Fato, Dimensão e Carga
# ==============================================================================

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(fs)


# ==============================================================================
# 1. [TAG: LIMPEZA_SANITIZACAO] LIMPEZA TEXTUAL E CONVERSÃO DE MÉTRICAS PT-BR
# ==============================================================================

#' Remove espaços em branco redundantes em todas as colunas textuais do data frame[cite: 12]
#' @param df Data frame de entrada[cite: 12]
#' @return Data frame tratado[cite: 12]
limpar_strings <- function(df) {
  df |> mutate(across(where(is.character), ~ str_squish(.x)))
}

#' Converte colunas numéricas no padrão pt-BR (vírgula decimal/ponto de milhar) para numeric[cite: 12]
#' @param df Data frame com dados brutos[cite: 12]
#' @param colunas Vetor com os nomes das colunas a serem parseadas[cite: 12]
#' @return Data frame com métricas numéricas convertidas[cite: 12]
converter_metricas_numericas <- function(df, colunas = c("Valor US$ FOB", "Quilograma Líquido")) {
  loc_br <- locale(decimal_mark = ",", grouping_mark = ".")
  df |> mutate(across(any_of(colunas), ~ parse_number(as.character(.x), locale = loc_br)))
}


# ==============================================================================
# 2. [TAG: INGESTAO_PARTICIONADA] LEITURA UNIFICADA E VALIDAÇÃO DOS MICRODADOS
# ==============================================================================

#' Lê e concatena dinamicamente arquivos particionados de microdados CSV[cite: 12]
#' @param caminhos_particoes Vetor de caminhos dos arquivos particionados[cite: 12]
#' @return Tibble consolidado com leitura de tipos textuais padrão[cite: 12]
ler_microdados_particionados <- function(caminhos_particoes) {
  arquivos_existentes <- caminhos_particoes[file.exists(caminhos_particoes)]
  if (length(arquivos_existentes) == 0) {
    stop("Nenhum arquivo de microdados foi encontrado no caminho especificado.")
  }
  map_dfr(arquivos_existentes, function(arq) {
    df <- read_delim(
      file = arq,
      delim = ";",
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
    message(paste0("Arquivo '", path_file(arq), "': ", nrow(df), " linhas."))
    df
  })
}


# ==============================================================================
# 3. [TAG: MODELO_DIMENSIONAL] CONSTRUÇÃO DAS TABELAS FATO, DIMENSÃO E CARGA
# ==============================================================================

#' Executa o pipeline de ETL completo e gera o esquema dimensional em memória e em disco[cite: 12]
#' @param caminhos_particoes Vetor com caminhos dos arquivos brutos[cite: 12]
#' @param caminho_saida Caminho opcional para exportar a base consolidada tratada[cite: 12]
#' @return Lista contendo: df_transacional, df_dimensao_sh4 e df_sazonal[cite: 12]
executar_pipeline_etl <- function(caminhos_particoes, caminho_saida = "data/processed/dados_completos_2019_2026.csv") {
  message("--- Lendo e validando arquivos de entrada ---")
  df_bruto <- ler_microdados_particionados(caminhos_particoes)
  
  message("\n--- Aplicando limpeza e conversões de tipos ---")
  df_tratado <- df_bruto |>
    limpar_strings() |>
    converter_metricas_numericas(colunas = c("Valor US$ FOB", "Quilograma Líquido")) |>
    mutate(
      mes_numero = as.integer(str_extract(Mês, "^\\d+")),
      mes_nome = ifelse(str_detect(Mês, "^\\d+\\."), str_trim(str_remove(Mês, "^\\d+\\.\\s*")), Mês),
      Ano = as.integer(Ano),
      `Código SH4` = str_pad(str_extract(`Código SH4`, "\\d+"), width = 4, pad = "0")
    ) |>
    select(-Mês)
  
  if (!is.null(caminho_saida)) {
    dir_create(path_dir(caminho_saida))
    write_csv2(df_tratado, caminho_saida)
    message(paste("Arquivo consolidado salvo em:", caminho_saida))
  }
  
  message("\n--- Construindo Estruturas Dimensionais ---")
  
  # 1. Dimensão Produto (SH4)
  df_dimensao_sh4 <- df_tratado |>
    select(`Código SH4`, `Descrição SH4`) |>
    distinct(`Código SH4`, .keep_all = TRUE) |>
    arrange(`Código SH4`)
  
  # 2. Fato Transacional
  colunas_fato <- c(
    "Fluxo", "Ano", "mes_numero", "mes_nome", "Município", 
    "País", "Código SH4", "Valor US$ FOB", "Quilograma Líquido"
  )
  df_transacional <- df_tratado |> select(any_of(colunas_fato))
  
  # 3. Fato Agregada Sazonal
  df_sazonal <- df_tratado |>
    group_by(Fluxo, Ano, mes_numero, mes_nome, Município) |>
    summarise(
      `Valor US$ FOB` = sum(`Valor US$ FOB`, na.rm = TRUE),
      `Quilograma Líquido` = sum(`Quilograma Líquido`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Fluxo, Ano, mes_numero, Município)
  
  message(paste0("1. df_transacional : ", nrow(df_transacional), " linhas."))
  message(paste0("2. df_dimensao_sh4  : ", nrow(df_dimensao_sh4), " produtos únicos."))
  message(paste0("3. df_sazonal       : ", nrow(df_sazonal), " agregações temporais."))
  
  list(
    df_transacional = df_transacional,
    df_dimensao_sh4 = df_dimensao_sh4,
    df_sazonal = df_sazonal
  )
}