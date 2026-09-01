# ==============================================================================
# SCRIPT MODULAR: MODELAGEM ECONOMÉTRICA E CLASSIFICAÇÃO (R/regressoes.R)
# ==============================================================================
# 
# ÍNDICE DE SEÇÕES (Use Ctrl + F com as TAGs abaixo):
# 
# 1. [TAG: REGRESSAO_SIMPLES]  - Regressão Linear Simples por Rota (Volume ~ Valor FOB)
# 2. [TAG: DIAGNOSTICO_OLS]    - Diagnósticos Multifacetados do Modelo OLS Simples
# 3. [TAG: MODELO_DUMMIES]     - Regressão com Dummies Mensais e Tendência Temporal
# 4. [TAG: DIAGNOSTICO_DUMMIES]- Diagnóstico de Resíduos do Modelo Sazonal com Dummies
# 5. [TAG: LOGISTICA_BINARIA]  - Regressão Logística e Avaliação ROC/AUC
# ==============================================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)


# ==============================================================================
# 1. [TAG: REGRESSAO_SIMPLES] REGRESSÃO LINEAR SIMPLES POR ROTA LÍDER (TOP 1)
# ==============================================================================

#' Ajusta modelos de regressão linear simples (Volume ~ Valor FOB) para as rotas líderes
#' @param df_sazonal_lideres Tibble contendo os dados mensais agregados das rotas líderes
#' @return Lista contendo: metricas, modelos (objetos lm) e dados formatados
ajustar_regressao_top1_rotas <- function(df_sazonal_lideres) {
  df_modelagem <- df_sazonal_lideres |>
    mutate(
      volume_mil_ton = `Quilograma Líquido` / 1e6,
      valor_milhoes_fob = `Valor US$ FOB` / 1e6
    )
  
  resultados <- df_modelagem |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_split()
  
  lista_metricas <- list()
  lista_lm <- list()
  
  for (i in seq_along(resultados)) {
    dados_rota <- resultados[[i]]
    rota_id <- paste(dados_rota$Município[1], dados_rota$Fluxo[1], dados_rota$País[1], sep = " | ")
    
    modelo <- lm(volume_mil_ton ~ valor_milhoes_fob, data = dados_rota)
    resumo <- summary(modelo)
    
    lista_metricas[[i]] <- tibble(
      Município = dados_rota$Município[1],
      Fluxo = dados_rota$Fluxo[1],
      País = dados_rota$País[1],
      `Código SH4` = dados_rota$`Código SH4`[1],
      `Descrição SH4` = dados_rota$`Descrição SH4`[1],
      N = nrow(dados_rota),
      Intercepto_b0 = round(coef(modelo)[1], 4),
      Inclinacao_b1 = round(coef(modelo)[2], 4),
      `R2` = round(resumo$r.squared, 4),
      `R2_Ajustado` = round(resumo$adj.r.squared, 4),
      `RMSE_Mil_Ton` = round(sqrt(mean(resumo$residuals^2)), 4),
      `p_valor` = format.pval(resumo$coefficients[2, 4], eps = 0.001, digits = 3)
    )
    lista_lm[[rota_id]] <- modelo
  }
  
  list(
    metricas = bind_rows(lista_metricas),
    modelos = lista_lm,
    dados = df_modelagem
  )
}

#' Plota os diagramas de dispersão com as retas de regressão OLS simples para as 6 rotas
#' @param resultado_regressao Objeto retornado por ajustar_regressao_top1_rotas()
#' @return Objeto ggplot facetado
plotar_regressao_top1_rotas <- function(resultado_regressao) {
  df_dados <- resultado_regressao$dados
  df_rotulos <- resultado_regressao$metricas |>
    mutate(
      rotulo = sprintf(
        "%s | %s\n%s (SH4 %s)\nR² = %.2f | b1 = %.2f | RMSE = %.2f",
        Município, Fluxo, País, `Código SH4`,
        R2, Inclinacao_b1, RMSE_Mil_Ton
      )
    )
  
  df_plot <- df_dados |>
    left_join(
      df_rotulos |> select(Município, Fluxo, País, `Código SH4`, rotulo),
      by = c("Município", "Fluxo", "País", "Código SH4")
    )
  
  ggplot(df_plot, aes(x = valor_milhoes_fob, y = volume_mil_ton)) +
    geom_smooth(method = "lm", formula = y ~ x, color = "#c0392b", fill = "#e74c3c", alpha = 0.2, linewidth = 0.9) +
    geom_point(color = "#2c3e50", alpha = 0.7, size = 1.8) +
    facet_wrap(~ rotulo, scales = "free", ncol = 2) +
    scale_x_continuous(labels = label_number(suffix = " M$", big.mark = ".")) +
    scale_y_continuous(labels = label_number(suffix = "k ton", big.mark = ".")) +
    labs(
      title = "Regressão Linear: Volume Físico vs. Valor Comercial (FOB)",
      subtitle = "Modelo OLS Simples: Volume (k ton) ~ Valor FOB (Milhões US$)",
      x = "Valor Comercial (Milhões de US$ FOB)",
      y = "Volume Físico (Milhares de Toneladas)"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      panel.grid.minor = element_blank()
    )
}


# ==============================================================================
# 2. [TAG: DIAGNOSTICO_OLS] DIAGNÓSTICOS MULTIFACETADOS DO MODELO OLS SIMPLES
# ==============================================================================

#' Extrai e consolida resíduos e séries temporais estimadas de todas as rotas (OLS Simples)
#' @param resultado_regressao Objeto retornado por ajustar_regressao_top1_rotas()
#' @return Tibble com valores observados, ajustados, resíduos e rótulos
obter_dados_diagnostico_rotas <- function(resultado_regressao) {
  df_dados <- resultado_regressao$dados
  
  df_diagnostico <- df_dados |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_modify(~ {
      mod <- lm(volume_mil_ton ~ valor_milhoes_fob, data = .x)
      .x |>
        mutate(
          Data = as.Date(paste(Ano, mes_numero, "01", sep = "-")),
          Volume_Real = volume_mil_ton,
          Ajuste_OLS = fitted(mod),
          Residuos = resid(mod)
        )
    }) |>
    ungroup() |>
    mutate(
      rotulo_faceta = sprintf("%s | %s\n%s — SH4 %s", Município, Fluxo, País, `Código SH4`)
    )
  
  return(df_diagnostico)
}

#' Painel 1 (OLS): Gráfico Resíduos vs. Valores Ajustados para todas as rotas (6 subplots)
#' @param df_diagnostico Tibble retornado por obter_dados_diagnostico_rotas()
plotar_todos_residuos_vs_ajustados <- function(df_diagnostico) {
  ggplot(df_diagnostico, aes(x = Ajuste_OLS, y = Residuos)) +
    geom_hline(yintercept = 0, color = "#d62728", linetype = "dashed", linewidth = 0.8) +
    geom_point(color = "#1f77b4", fill = "#1f77b4", alpha = 0.8, size = 2, shape = 21, stroke = 0.5) +
    facet_wrap(~ rotulo_faceta, scales = "free", ncol = 2) +
    labs(
      title = "Resíduos vs. Valores Ajustados por Rota Líder (OLS Simples)",
      subtitle = "Avaliação de homocedasticidade e linearidade por município e fluxo",
      x = expression("Valores Ajustados (" * hat(Y)[t] * ") [k ton]"),
      y = expression("Resíduos (" * e[t] * ") [k ton]")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      panel.grid.major = element_line(color = "#e9ecef"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#dee2e6", fill = NA, linewidth = 0.6),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9.5, color = "#6c757d", margin = margin(b = 10))
    )
}

#' Painel 2 (OLS): Série Histórica Real vs. Ajustada no Tempo para todas as rotas (6 subplots)
#' @param df_diagnostico Tibble retornado por obter_dados_diagnostico_rotas()
plotar_todas_series_reais_vs_ajustadas <- function(df_diagnostico) {
  ggplot(df_diagnostico, aes(x = Data)) +
    geom_line(aes(y = Volume_Real, color = "Volume Real"), linewidth = 0.8) +
    geom_line(aes(y = Ajuste_OLS, color = "Ajuste OLS (Y_hat)"), linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~ rotulo_faceta, scales = "free_y", ncol = 2) +
    scale_color_manual(
      name = NULL,
      values = c("Volume Real" = "#1f77b4", "Ajuste OLS (Y_hat)" = "#ff7f0e"),
      labels = c("Volume Real", expression("Ajuste OLS (" * hat(Y) * ")"))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = label_number(suffix = "k ton", big.mark = ".")) +
    labs(
      title = "Série Histórica Real vs. Ajuste OLS por Rota Líder",
      subtitle = "Linha contínua: Volume Mensal Real | Linha tracejada: Volume Estimado pelo Modelo OLS",
      x = NULL,
      y = "Milhares de Toneladas (k ton)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8.5),
      panel.grid.major = element_line(color = "#e9ecef"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#dee2e6", fill = NA, linewidth = 0.6),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9.5, color = "#6c757d", margin = margin(b = 10))
    )
}


# ==============================================================================
# 3. [TAG: MODELO_DUMMIES] REGRESSÃO SAZONAL COM DUMMIES MENSAIS E TENDÊNCIA
# ==============================================================================

#' Ajusta modelos OLS com dummies mensais (Janeiro como base) e tendência temporal
#' @param df_sazonal_lideres Tibble contendo os dados mensais agregados das rotas líderes
#' @return Lista contendo: metricas, modelos (objetos lm) e dados formatados
ajustar_regressao_dummies_rotas <- function(df_sazonal_lideres) {
  df_modelagem <- df_sazonal_lideres |>
    arrange(Fluxo, Município, Ano, mes_numero) |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    mutate(
      Data = as.Date(paste(Ano, mes_numero, "01", sep = "-")),
      tempo = row_number(),
      volume_mil_ton = `Quilograma Líquido` / 1e6,
      mes_nome_fator = factor(
        mes_nome,
        levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
      )
    ) |>
    ungroup()

  resultados <- df_modelagem |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_split()

  lista_metricas <- list()
  lista_lm <- list()

  for (i in seq_along(resultados)) {
    dados_rota <- resultados[[i]]
    rota_id <- paste(dados_rota$Município[1], dados_rota$Fluxo[1], dados_rota$País[1], sep = " | ")

    modelo <- lm(volume_mil_ton ~ tempo + mes_nome_fator, data = dados_rota)
    resumo <- summary(modelo)

    f_stat <- resumo$fstatistic
    p_val_mod <- if (!is.null(f_stat)) {
      pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    } else {
      NA_real_
    }

    lista_metricas[[i]] <- tibble(
      Município = dados_rota$Município[1],
      Fluxo = dados_rota$Fluxo[1],
      País = dados_rota$País[1],
      `Código SH4` = dados_rota$`Código SH4`[1],
      `Descrição SH4` = dados_rota$`Descrição SH4`[1],
      N = nrow(dados_rota),
      `R²` = round(resumo$r.squared, 4),
      `R²_Ajustado` = round(resumo$adj.r.squared, 4),
      `RMSE_Mil_Ton` = round(sqrt(mean(resumo$residuals^2)), 4),
      `F_Estatistica` = round(f_stat[1], 2),
      `p_valor` = format.pval(p_val_mod, eps = 0.001, digits = 3)
    )
    lista_lm[[rota_id]] <- modelo
  }

  list(
    metricas = bind_rows(lista_metricas),
    modelos = lista_lm,
    dados = df_modelagem
  )
}

#' Plota os valores reais versus valores ajustados pelo modelo com dummies mensais (6 rotas)
#' @param resultado_dummies Objeto retornado por ajustar_regressao_dummies_rotas()
#' @return Objeto ggplot facetado
plotar_regressao_dummies_rotas <- function(resultado_dummies) {
  df_dados <- resultado_dummies$dados
  
  df_plot <- df_dados |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_modify(~ {
      mod <- lm(volume_mil_ton ~ tempo + mes_nome_fator, data = .x)
      .x |>
        mutate(
          Ajuste_Dummies = fitted(mod),
          Residuos = resid(mod)
        )
    }) |>
    ungroup() |>
    mutate(
      rotulo = sprintf("%s | %s\n%s (SH4 %s)", Município, Fluxo, País, `Código SH4`)
    )

  ggplot(df_plot, aes(x = Data)) +
    geom_line(aes(y = volume_mil_ton, color = "Volume Real"), linewidth = 0.8) +
    geom_line(aes(y = Ajuste_Dummies, color = "Ajuste Dummies"), linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~ rotulo, scales = "free_y", ncol = 2) +
    scale_color_manual(
      name = NULL,
      values = c("Volume Real" = "#1f77b4", "Ajuste Dummies" = "#2ca02c")
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = label_number(suffix = "k ton", big.mark = ".")) +
    labs(
      title = "Série Histórica Real vs. Ajuste Sazonal com Dummies Mensais",
      subtitle = "Modelo: Volume Mensal ~ Tendência Temporal + Dummies de Mês (Janeiro como base)",
      x = NULL,
      y = "Milhares de Toneladas (k ton)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8.5),
      panel.grid.major = element_line(color = "#e9ecef"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9.5, color = "#6c757d", margin = margin(b = 10))
    )
}


# ==============================================================================
# 4. [TAG: DIAGNOSTICO_DUMMIES] DIAGNÓSTICO DE RESÍDUOS DO MODELO COM DUMMIES
# ==============================================================================

#' Extrai e consolida resíduos e valores ajustados do modelo com dummies mensais
#' @param resultado_dummies Objeto retornado por ajustar_regressao_dummies_rotas()
#' @return Tibble com valores reais, ajustados, resíduos e rótulos
obter_dados_diagnostico_dummies <- function(resultado_dummies) {
  df_dados <- resultado_dummies$dados
  
  df_diagnostico <- df_dados |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_modify(~ {
      mod <- lm(volume_mil_ton ~ tempo + mes_nome_fator, data = .x)
      .x |>
        mutate(
          Ajuste_Dummies = fitted(mod),
          Residuos = resid(mod)
        )
    }) |>
    ungroup() |>
    mutate(
      rotulo_faceta = sprintf("%s | %s\n%s — SH4 %s", Município, Fluxo, País, `Código SH4`)
    )
  
  return(df_diagnostico)
}

#' Painel 1 (Dummies): Resíduos vs. Valores Ajustados para as 6 rotas líderes
#' @param df_diagnostico_dummies Tibble retornado por obter_dados_diagnostico_dummies()
plotar_residuos_vs_ajustados_dummies <- function(df_diagnostico_dummies) {
  ggplot(df_diagnostico_dummies, aes(x = Ajuste_Dummies, y = Residuos)) +
    geom_hline(yintercept = 0, color = "#d62728", linetype = "dashed", linewidth = 0.8) +
    geom_point(color = "#1f77b4", fill = "#1f77b4", alpha = 0.8, size = 2, shape = 21, stroke = 0.5) +
    facet_wrap(~ rotulo_faceta, scales = "free", ncol = 2) +
    labs(
      title = "Resíduos vs. Valores Ajustados (Modelo Sazonal com Dummies)",
      subtitle = "Diagnóstico de homocedasticidade e independência dos resíduos por rota líder",
      x = expression("Valores Ajustados (" * hat(Y)[t] * ") [k ton]"),
      y = expression("Resíduos (" * e[t] * ") [k ton]")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      panel.grid.major = element_line(color = "#e9ecef"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#dee2e6", fill = NA, linewidth = 0.6),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9.5, color = "#6c757d", margin = margin(b = 10))
    )
}


# ==============================================================================
# 5. [TAG: LOGISTICA_BINARIA] REGRESSÃO LOGÍSTICA BINÁRIA (OPÇÕES 2 E 4 DA AULA)
# ==============================================================================

#' Prepara a base transacional com a variável binária (Exportação = 1, Importação = 0)
preparar_dados_logistica <- function(df_transacional) {
  df_transacional |>
    mutate(
      y = if_else(Fluxo == "Exportação", 1L, 0L),
      volume_mil_ton = `Quilograma Líquido` / 1e6,
      valor_milhoes_fob = `Valor US$ FOB` / 1e6,
      mes_fator = factor(
        mes_nome,
        levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
      )
    ) |>
    filter(!is.na(y), !is.na(volume_mil_ton), !is.na(valor_milhoes_fob))
}

#' Opção 2: Regressão Logística Simples por Volume Físico
ajustar_logistica_opcao2 <- function(df_logit) {
  modelo <- glm(y ~ volume_mil_ton, family = binomial, data = df_logit)
  coefs <- coef(modelo)
  
  ponto_virada <- as.numeric(-coefs[1] / coefs[2])
  
  list(
    modelo = modelo,
    ponto_virada_k_ton = ponto_virada,
    odds_ratio = exp(coefs),
    resumo = summary(modelo)
  )
}

#' Opção 4: Regressão Logística com Valor Financeiro e Sazonalidade (Dummies Mensais)
ajustar_logistica_opcao4 <- function(df_logit) {
  modelo <- glm(y ~ valor_milhoes_fob + mes_fator, family = binomial, data = df_logit)
  
  list(
    modelo = modelo,
    odds_ratio = exp(coef(modelo)),
    resumo = summary(modelo)
  )
}

#' Avalia o classificador nos 3 limiares da aula (0.3, 0.5 e 0.7) com AUC otimizada via Mann-Whitney
avaliar_metricas_limiares <- function(modelo, df_logit, limiares = c(0.3, 0.5, 0.7)) {
  p <- predict(modelo, type = "response")
  y_real <- df_logit$y
  
  lista_tabelas <- list()
  lista_metricas <- list()
  
  for (limiar in limiares) {
    yhat <- as.integer(p > limiar)
    matriz <- table(Real = y_real, Previsto = yhat)
    
    vn <- sum(y_real == 0 & yhat == 0)
    fp <- sum(y_real == 0 & yhat == 1)
    fn <- sum(y_real == 1 & yhat == 0)
    vp <- sum(y_real == 1 & yhat == 1)
    
    acuracia <- (vp + vn) / (vp + vn + fp + fn)
    precisao <- if ((vp + fp) > 0) vp / (vp + fp) else 0
    recall   <- if ((vp + fn) > 0) vp / (vp + fn) else 0
    especificidade <- if ((vn + fp) > 0) vn / (vn + fp) else 0
    
    lista_tabelas[[as.character(limiar)]] <- matriz
    lista_metricas[[as.character(limiar)]] <- tibble(
      Limiar = limiar,
      `Acurácia` = round(acuracia, 4),
      `Precisão` = round(precisao, 4),
      `Recall (Sensibilidade)` = round(recall, 4),
      `Especificidade` = round(especificidade, 4),
      `VP` = vp, `FP` = fp, `FN` = fn, `VN` = vn
    )
  }
  
  # Cálculo de AUC analítico otimizado (Estatística U de Mann-Whitney / Wilcoxon)
  n1 <- as.numeric(sum(y_real == 1))
  n0 <- as.numeric(sum(y_real == 0))
  ranks <- rank(p)
  soma_postos_positivos <- sum(ranks[y_real == 1])
  auc <- (soma_postos_positivos - n1 * (n1 + 1) / 2) / (n1 * n0)
  
  list(
    probabilidades = p,
    y_real = y_real,
    matrizes = lista_tabelas,
    metricas = bind_rows(lista_metricas),
    auc = auc
  )
}
